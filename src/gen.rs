use std::{fmt::Display, slice};

use cranelift::prelude::{
    types::I64, AbiParam, ExtFuncData, ExternalName, InstBuilder, Signature, Value as ClifValue,
};
use cranelift_codegen::{
    ir::{condcodes::IntCC, FuncRef, Function, UserExternalName, UserFuncName},
    isa::CallConv,
    verifier::VerifierResult,
    verify_function, Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::{
    ast::{Block as AstBlock, Expr},
    symbols::{GlobalSymbols, LocalContext},
};

/// A value in SIMP lang that could contain zero, one, or more clif values
#[derive(Clone, PartialEq, Eq, Debug)]
enum Value {
    Empty,
    Single([ClifValue; 1]),
    Tuple(Vec<ClifValue>),
    Never,
}

impl Value {
    /// Returns `true` if the value is [`Empty`].
    ///
    /// [`Empty`]: Value::Empty
    #[must_use]
    fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    fn as_single(&self) -> Option<ClifValue> {
        if let &Self::Single(v) = self {
            Some(v[0])
        } else {
            None
        }
    }

    fn expect_single(&self) -> ClifValue {
        match self.as_single() {
            Some(val) => val,
            None => {
                panic!(
                    "Expects a singular value, but {} is provided",
                    self.display()
                );
            }
        }
    }

    fn expect_empty(&self) {
        if !self.is_empty() {
            panic!(
                "Expects no return value, but {} is provided",
                self.display()
            )
        }
    }

    #[allow(dead_code)]
    fn as_tuple(&self) -> Option<&Vec<ClifValue>> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_slice(&self) -> &[ClifValue] {
        match self {
            Self::Empty => &[],
            Self::Single(val) => val.as_slice(),
            Self::Tuple(fields) => &fields,
            Self::Never => &[],
        }
    }

    fn values(&self) -> Values {
        match self {
            Self::Empty => Values::Empty,
            &Self::Single(val) => Values::Singular(val[0]),
            Self::Tuple(vals) => Values::Tuple(vals.iter()),
            Self::Never => Values::Empty,
        }
    }

    /// Returns `true` if the value is [`Single`].
    ///
    /// [`Single`]: Value::Single
    #[allow(dead_code)]
    #[must_use]
    fn is_single(&self) -> bool {
        matches!(self, Self::Single(..))
    }

    /// Returns whether or not it's valid for `other` to be assigned to `self`.
    /// - `Never` can be coerced to any types
    /// - `Tuple`s of 1 value can be coerced to `Single`
    /// - `Tuple`s with 0 values can be coerced to `Empty`
    fn matches_val_type(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Empty, Self::Empty)
            | (Self::Single(..), Self::Single(..)) => true,
            (Self::Empty, Self::Tuple(vals)) if vals.is_empty() => true,
            (Self::Tuple(vals), Self::Empty) if vals.is_empty() => true,
            (Self::Single(..), Self::Tuple(vals)) if vals.len() == 1 => true,
            (Self::Tuple(vals), Self::Single(..)) if vals.len() == 1 => true,
            (Self::Tuple(l), Self::Tuple(r)) if l.len() == r.len() => true,
            _ => false,
        }
    }

    /// Checks whether or not it's valid for value of type `self` to be assigned to expression of
    /// length `len`.
    fn can_be_assigned_to_expr_of_len(&self, len: usize) -> bool {
        match (self, len) {
            (Value::Empty, 0) => true,
            (Value::Single(..), 1) => true,
            (Value::Tuple(vals), len) => vals.len() == len,
            (Value::Never, _) => false, // never can never be assigned
            _ => false,
        }
    }

    /// Returns a temporary `ValueDisplay` object for printing a description of SIMP values,
    /// used in error messages.
    /// Formats to
    /// - `"nothing"` for `Value::Empty`
    /// - `"a single integer"` for `Value::Single(..)`
    /// - `"a tuple of {len} fields"` for `Value::Tuple(len)`
    fn display(&self) -> ValueDisplay {
        match self {
            Self::Empty => ValueDisplay::Empty,
            Self::Single(..) => ValueDisplay::Single,
            Self::Tuple(vals) => ValueDisplay::Tuple(vals.len()),
            Self::Never => ValueDisplay::Empty,
        }
    }

    /// Returns `true` if the value is [`Never`].
    ///
    /// [`Never`]: Value::Never
    #[must_use]
    fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }

    #[must_use]
    fn ty(&self) -> ValueType {
        match self {
            Value::Empty => ValueType::Empty,
            Value::Single(_) => ValueType::Single,
            Value::Tuple(vals) => ValueType::Tuple(vals.len()),
            Value::Never => ValueType::Never,
        }
    }
}

/// A hollow value with only the type
#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Empty,
    Single,
    Tuple(usize),
    Never,
}

impl ValueType {
    /// Returns whether or not it's valid for `other` to be assigned to `self`.
    /// - `Never` can be coerced to any types
    /// - `Tuple`s of 1 value can be coerced to `Single`
    /// - `Tuple`s with 0 values can be coerced to `Empty`
    pub fn matches(self, other: Self) -> bool {
        match (self, other) {
            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Empty, Self::Empty)
            | (Self::Single, Self::Single) => true,
            (Self::Empty, Self::Tuple(0)) => true,
            (Self::Tuple(0), Self::Empty) => true,
            (Self::Single, Self::Tuple(1)) => true,
            (Self::Tuple(1), Self::Single) => true,
            (Self::Tuple(l), Self::Tuple(r)) if l == r => true,
            _ => false,
        }
    }
}

/// An iterator that iterates through the `ClifValue`s contained in a SIMP `Value`.
/// Can be created by `simp_val.values()`.
#[derive(Debug, Clone)]
enum Values<'short> {
    Empty,
    Singular(ClifValue),
    Tuple(slice::Iter<'short, ClifValue>),
}

impl Iterator for Values<'_> {
    type Item = ClifValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Singular(val) => {
                let val = *val;
                *self = Self::Empty;
                Some(val)
            }
            Self::Tuple(iter) => iter.next().copied(),
        }
    }
}

/// A temporary type for printing a description of SIMP Value, used in error messages.
/// Can be created by `simp_val.description()`.
/// Formats to
/// - `"nothing"` for `Value::Empty`
/// - `"a single integer"` for `Value::Single(..)`
/// - `"a tuple of {len} fields"` for `Value::Tuple(len)`
#[derive(Debug, Clone, Copy)]
enum ValueDisplay {
    Empty,
    Single,
    Tuple(usize),
}

impl Display for ValueDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueDisplay::Empty => write!(f, "nothing"),
            ValueDisplay::Single => write!(f, "a single integer"),
            ValueDisplay::Tuple(len) => write!(f, "a tuple of {len} fields"),
        }
    }
}

impl From<ClifValue> for Value {
    fn from(val: ClifValue) -> Self {
        Self::Single([val])
    }
}

impl From<Vec<ClifValue>> for Value {
    fn from(vals: Vec<ClifValue>) -> Self {
        Self::Tuple(vals)
    }
}

/// Declares a new variable in the local symbol space and in the function builder
#[inline(always)]
fn declare_var<'e>(
    builder: &mut FunctionBuilder<'_>,
    local: &mut LocalContext<'e>,
    name: &'e str,
) -> Variable {
    let var = local.create_var(name);
    builder.declare_var(var, I64);
    var
}

/// Codegen for things that can be used as an operand
/// e.g. number literal, identifier, lhs + rhs, if-else block, ...
fn gen_operand<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    expr: &'e Expr,
) -> Value {
    macro_rules! bin_op {
        ($f:ident, $lhs:expr, $rhs:expr $(,)?) => {{
            let lhs = gen_operand(builder, global, local, $lhs.as_ref()).expect_single();
            let rhs = gen_operand(builder, global, local, $rhs.as_ref()).expect_single();
            builder.ins().$f(lhs, rhs).into()
        }};
    }
    macro_rules! cmp {
        ($cmp:path, $lhs:expr, $rhs:expr $(,)?) => {{
            let lhs = gen_operand(builder, global, local, $lhs.as_ref()).expect_single();
            let rhs = gen_operand(builder, global, local, $rhs.as_ref()).expect_single();
            builder.ins().icmp($cmp, lhs, rhs).into()
        }};
    }
    match expr {
        Expr::Id(id) => builder.use_var(local.expect_var(&id)).into(),
        &Expr::Num(num) => builder.ins().iconst(I64, num).into(),
        Expr::Eq(lhs, rhs) => cmp!(IntCC::Equal, lhs, rhs),
        Expr::NEq(lhs, rhs) => cmp!(IntCC::NotEqual, lhs, rhs),
        Expr::Le(lhs, rhs) => cmp!(IntCC::SignedLessThan, lhs, rhs),
        Expr::LeEq(lhs, rhs) => cmp!(IntCC::SignedLessThanOrEqual, lhs, rhs),
        Expr::Gr(lhs, rhs) => cmp!(IntCC::SignedGreaterThan, lhs, rhs),
        Expr::GrEq(lhs, rhs) => cmp!(IntCC::SignedLessThanOrEqual, lhs, rhs),
        Expr::Not(_) => todo!(),
        Expr::UnaryAdd(e) => {
            let val = gen_operand(builder, global, local, e);
            val.expect_single();
            val
        }
        Expr::UnarySub(e) => {
            let val = gen_operand(builder, global, local, e).expect_single();
            builder.ins().ineg(val).into()
        }
        Expr::Add(lhs, rhs) => bin_op!(iadd, lhs, rhs),
        Expr::Sub(lhs, rhs) => bin_op!(isub, lhs, rhs),
        Expr::Mul(lhs, rhs) => bin_op!(imul, lhs, rhs),
        Expr::Div(lhs, rhs) => bin_op!(sdiv, lhs, rhs),
        Expr::Mod(lhs, rhs) => bin_op!(srem, lhs, rhs),
        Expr::And(_, _) => todo!(),
        Expr::Or(_, _) => todo!(),
        Expr::Block(body) => gen_block(builder, global, local, body),
        Expr::IfElse(cond, if_block, else_block) => gen_if_else(
            builder,
            global,
            local,
            cond,
            if_block.as_block().unwrap(),
            else_block.as_deref().map(|expr| expr.as_block().unwrap()),
        ),
        Expr::Loop(block) => gen_loop(builder, global, local, block.as_block().unwrap()).into(),
        Expr::Call(callee, args) => gen_call(builder, global, local, callee, args).into(),
        Expr::Tuple(fields) => fields
            .iter()
            .map(|expr| {
                gen_operand(builder, global, local, expr)
                    .as_single()
                    .expect("Nested tuple is not allowed")
            })
            .collect::<Vec<ClifValue>>()
            .into(),
        e => panic!("Expression {e:?} not allowed as an operand"),
    }
}

/// Generate an assignment statement
fn gen_assign<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e Expr,
    rhs: &'e Expr,
) {
    match lhs {
        Expr::Id(name) => gen_assign_var(builder, global, local, &name, rhs),
        Expr::Tuple(fields) => gen_assign_tuple(builder, global, local, &fields, rhs),
        _ => panic!("Expression cannot be used as LHS of assignment"),
    }
}

/// Generate an assignment statement with one variable as lhs, called by `gen_assign`
#[inline(always)]
fn gen_assign_var<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e str,
    rhs: &'e Expr,
) {
    let lhs = local.expect_var(lhs);
    let rhs = gen_operand(builder, global, local, rhs).expect_single();
    builder.def_var(lhs, rhs);
}

/// Generate an assignment statement with a tuple as lhs, called by `gen_assign`
#[allow(unused_variables)]
#[inline(always)]
fn gen_assign_tuple<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e [Expr],
    rhs: &'e Expr,
) {
    match rhs {
        Expr::Tuple(rhs) => {
            lhs.iter().zip(rhs).for_each(|(lhs, rhs)| {
                let lhs = local.expect_var(lhs.as_id().expect(
                    "Only identifier or tuple of identifiers is allowed as LHS of an assignment",
                ));
                let rhs = gen_operand(builder, global, local, rhs)
                    .as_single()
                    .expect("Nested tuples are not allowed, flatten it first before assigning");
                builder.def_var(lhs, rhs);
            });
        }
        expr => {
            let rhs = gen_operand(builder, global, local, expr);
            lhs.iter()
                .map(|e|e.as_id().expect("Only identifier or tuple of identifiers is allowed as LHS of an assignment"))
                .zip(rhs.values())
                .for_each(|(id, rhs)| {
                    let var = local.expect_var("id");
                    builder.def_var(var, rhs);
                });
            if !rhs.can_be_assigned_to_expr_of_len(lhs.len()) {
                panic!(
                    "The LHS is {} but the RHS is {}",
                    match lhs.len() {
                        0 => "nothing".to_string(),
                        1 => "one variable".to_string(),
                        x => format!("a tuple of {x} variables"),
                    },
                    rhs.display()
                );
            }
        }
    }
}

/// Generate a `let` statement
fn gen_let<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e Expr,
    rhs: &'e Expr,
) {
    match lhs {
        Expr::Id(name) => gen_let_var(builder, global, local, &name, rhs),
        Expr::Tuple(fields) => gen_let_tuple(builder, global, local, &fields, rhs),
        _ => panic!("Expression cannot be used as LHS of assignment"),
    }
}

/// Generate a `let` statement with one variable as lhs, called by `gen_let`
#[inline(always)]
fn gen_let_var<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e str,
    rhs: &'e Expr,
) {
    let rhs = gen_operand(builder, global, local, rhs).expect_single();
    let var = declare_var(builder, local, lhs);
    builder.def_var(var, rhs);
}

/// Generate a `let` statement with a tuple as lhs, called by `gen_let`
#[allow(unused_variables)]
#[inline(always)]
fn gen_let_tuple<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    lhs: &'e [Expr],
    rhs: &'e Expr,
) {
    match rhs {
        Expr::Tuple(rhs) => {
            lhs.iter().zip(rhs).for_each(|(lhs, rhs)| {
                gen_let_var(
                    builder,
                    global,
                    local,
                    lhs.as_id().expect("Nested tuples are not allowed"),
                    rhs,
                );
            });
        }
        expr => {
            let rhs = gen_operand(builder, global, local, expr);
            lhs.iter()
                .map(|e|e.as_id().expect("Only identifier or tuple of identifiers is allowed as lhs of an assignment"))
                .zip(rhs.values())
                .for_each(|(id, rhs)| {
                    let var = declare_var(builder, local, id);
                    builder.def_var(var, rhs);
                });
            if !rhs.can_be_assigned_to_expr_of_len(lhs.len()) {
                panic!(
                    "The LHS is {} but the rhs is {}",
                    match lhs.len() {
                        0 => "nothing".to_string(),
                        1 => "one variable".to_string(),
                        x => format!("a tuple of {x} variables"),
                    },
                    rhs.display()
                );
            }
        }
    }
}

fn gen_loop<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    block: &'e AstBlock,
) -> Value {
    let loop_block = builder.create_block();
    let break_block = builder.create_block();

    builder.ins().jump(loop_block, &[]);

    local.enters_loop(break_block, loop_block);

    // loop block
    builder.switch_to_block(loop_block);
    let mut is_terminated = false;
    for expr in &block.body {
        if !gen_statement(builder, global, local, expr) {
            is_terminated = true;
            break;
        }
    }
    if !is_terminated {
        builder.ins().jump(loop_block, &[]);
    }
    builder.seal_block(loop_block);

    // break block
    builder.switch_to_block(break_block);
    builder.seal_block(break_block);
    let loop_info = unsafe { local.parent_loop().unwrap_unchecked() };
    let break_val = match loop_info.val_ty {
        None | Some(ValueType::Empty) => Value::Empty,
        Some(ValueType::Single) => builder.append_block_param(break_block, I64).into(),
        Some(ValueType::Tuple(len)) => (0..len)
            .into_iter()
            .map(|_| builder.append_block_param(break_block, I64))
            .collect::<Vec<ClifValue>>()
            .into(),
        Some(ValueType::Never) => Value::Never,
    };

    local.leaves_loop();

    break_val
}

#[inline(always)]
fn gen_if_else<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    cond: &'e Expr,
    if_ast_block: &'e AstBlock,
    else_ast_block: Option<&'e AstBlock>,
) -> Value {
    let if_block = builder.create_block();
    let else_block = builder.create_block();
    let merged_block = builder.create_block();

    // cmp
    let cond_val = gen_operand(builder, global, local, cond).expect_single();
    builder.ins().brif(cond_val, if_block, &[], else_block, &[]);

    // if block
    let if_result = {
        builder.switch_to_block(if_block);
        builder.seal_block(if_block);
        gen_block(builder, global, local, if_ast_block)
    };
    if !if_result.is_never() {
        builder.ins().jump(merged_block, if_result.as_slice());
    }

    // else block
    let else_result = {
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        match else_ast_block {
            Some(block) => gen_block(builder, global, local, block),
            None => {
                match if_result {
                    Value::Empty | Value::Never=> (),
                    _ => panic!("Type of the return value from `if` block doesn't match the value from `else` block, the if block returns {} but the else block returns nothing", if_result.display()),
                }
                Value::Empty
            }
        }
    };
    if !else_result.is_never() {
        builder.ins().jump(merged_block, if_result.as_slice());
    }

    // merged block
    // check number of results
    if !if_result.matches_val_type(&else_result) {
        panic!(
            "Expects same type of value from `if` block and `else` block, but found the if block returns {} and the else block returns {}",
            if_result.display(),
            else_result.display());
    }
    builder.switch_to_block(merged_block);
    builder.seal_block(merged_block);
    let result = match (if_result, else_result) {
        (Value::Never, Value::Never) => return Value::Never,
        (x, Value::Never) => x,
        (Value::Never, x) => x,
        (x, _) => x,
    };
    match result {
        Value::Empty => Value::Empty,
        Value::Single(_) => builder.append_block_param(merged_block, I64).into(),
        Value::Tuple(vals) => (0..vals.len())
            .map(|_| builder.append_block_param(merged_block, I64))
            .collect::<Vec<ClifValue>>()
            .into(),
        Value::Never => Value::Never,
    }
}

/// Imports an user-defined external function to a function, returns the signature of the imported
/// function and the result `FuncRef`, if the function is already imported, returns the result
/// stored in `local`
fn import_func_if_needed<'a, 'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &'a GlobalSymbols,
    local: &mut LocalContext<'e>,
    name: &'e str,
) -> (&'a Signature, FuncRef) {
    let (index, sig) = global.func(name).expect(&format!(
        "Trying to call the function `{name}` which does not exist, symbols: {global:?}"
    ));
    let func_ref = local.import_func_if_needed(name, || {
        let sig_ref = builder.import_signature(sig.clone());
        let name_ref = builder
            .func
            .declare_imported_user_function(UserExternalName::new(0, *index));
        let func_ref = builder.import_function(ExtFuncData {
            name: ExternalName::user(name_ref),
            signature: sig_ref,
            colocated: false,
        });
        func_ref
    });
    (sig, func_ref)
}

/// Generate a call instruction
/// Returns the value in which the result of the call is stored
fn gen_call<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    callee: &'e Expr,
    args: &'e Vec<Expr>,
) -> ClifValue {
    let name = callee
        .as_id()
        .expect("Dynamic function calling is not supported yet");
    let (sig, func_ref) = import_func_if_needed(builder, global, local, name);
    if args.len() != sig.params.len() {
        panic!(
            "The function `{name}` requires {} arguments, but only {} are provided",
            args.len(),
            sig.params.len()
        );
    }
    let args: Vec<ClifValue> = args
        .iter()
        .map(|e| gen_operand(builder, global, local, e).expect_single())
        .collect();
    let inst = builder.ins().call(func_ref, args.as_ref());
    *builder.inst_results(inst).iter().next().unwrap()
}

/// Generate IR for a block, if the block has a tail, return the value of the tail, otherwise
/// return iconst 0
fn gen_block<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    block: &'e AstBlock,
) -> Value {
    match block.body.as_slice() {
        [] => Value::Empty,
        [expr] => match expr {
            Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
            expr => match gen_statement(builder, global, local, &expr) {
                true => Value::Empty,
                false => Value::Never,
            },
        },
        body => {
            local.enters_block();
            // first generate for expressions except the last one ...
            for expr in unsafe { body.get_unchecked(0..body.len() - 1) }.iter() {
                match gen_statement(builder, global, local, &expr) {
                    true => continue,
                    false => return Value::Never,
                }
            }
            // ... and then if the last one is a tail, return the value of the tail, otherwise
            // return `Empty`.
            let last = unsafe { body.get_unchecked(body.len() - 1) };
            let val = match last {
                Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
                expr => match gen_statement(builder, global, local, &expr) {
                    true => Value::Empty,
                    false => Value::Never,
                },
            };
            local.leaves_block();
            val
        }
    }
}

/// Generate IR for a statement.
/// Returns `false` if encountering a `Never` statement (`break` and `continue`).
fn gen_statement<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    expr: &'e Expr,
) -> bool {
    match expr {
        Expr::Let(lhs, rhs) => gen_let(builder, global, local, lhs, rhs),
        Expr::Call(callee, args) => {
            gen_call(builder, global, local, callee, args);
        }
        Expr::Block(body) => {
            gen_block(builder, global, local, &body).expect_empty();
        }
        Expr::IfElse(cond, if_body, else_body) => {
            gen_if_else(
                builder,
                global,
                local,
                cond,
                if_body.as_block().unwrap(),
                else_body.as_ref().map(|e| e.as_block().unwrap()),
            )
            .expect_empty();
        }
        Expr::Loop(body) => {
            gen_loop(builder, global, local, body.as_block().unwrap()).expect_empty();
        }
        Expr::Break(expr) => {
            let val = expr.as_ref().map_or(Value::Empty, |expr| {
                gen_operand(builder, global, local, &expr)
            });
            let parent_loop = local
                .parent_loop_mut()
                .expect("Using `break` outside of a loop");
            parent_loop.check_break_val(val.ty());
            let break_block = parent_loop.break_block;
            builder.ins().jump(break_block, val.as_slice());
            return false;
        }
        Expr::Continue => {
            let continue_block = local
                .parent_loop()
                .expect("Using `continue` outside of a loop")
                .continue_block;
            builder.ins().jump(continue_block, &[]);
            return false;
        }
        Expr::Assign(lhs, rhs) => gen_assign(builder, global, local, &lhs, &rhs),
        e => panic!("Expression not allowed as a statement: {e:?}"),
    }
    true
}

#[inline(always)]
#[must_use]
fn make_func_signature(args: &Vec<String>) -> Signature {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.reserve(args.len());
    (0..args.len()).for_each(|_| {
        sig.params.push(AbiParam::new(I64));
    });
    sig.returns.push(AbiParam::new(I64));
    sig
}

#[inline(always)]
fn add_func_to_module(func_id: FuncId, func: Function, module: &mut ObjectModule) {
    let mut ctx = Context::for_function(func);
    module.define_function(func_id, &mut ctx).unwrap();
}

pub fn compile_func(
    module: &mut ObjectModule,
    global: &mut GlobalSymbols,
    builder_ctx: &mut FunctionBuilderContext,
    (name, arg_names, body): (String, Vec<String>, Option<Box<Expr>>),
) -> VerifierResult<()> {
    let sig = make_func_signature(&arg_names);
    let func_id = module
        .declare_function(&name, Linkage::Export, &sig)
        .unwrap();
    let func_index = global
        .add_func(name, sig.clone())
        .expect("Redefinition of function");
    let body = match body {
        Some(x) => x,
        None => return Ok(()),
    };
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_index), sig);
    let mut builder = FunctionBuilder::new(&mut func, builder_ctx);
    let mut local = LocalContext::default();
    let entry_block = {
        let b = builder.create_block();
        builder.append_block_params_for_function_params(b);
        builder.switch_to_block(b);
        builder.seal_block(b);
        b
    };
    arg_names.iter().enumerate().for_each(|(i, name)| {
        let var = local.create_var(name);
        let val = *unsafe { builder.block_params(entry_block).get_unchecked(i) };
        builder.declare_var(var, I64);
        builder.def_var(var, val);
    });

    let return_val = gen_operand(&mut builder, global, &mut local, &body);
    builder.ins().return_(return_val.as_slice());

    builder.finalize();
    println!("{}", func.display());
    verify_function(&func, module.isa().flags())?;
    add_func_to_module(func_id, func, module);
    Ok(())
}
