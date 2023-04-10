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
    ast::Expr,
    symbols::{GlobalSymbols, LocalContext},
};

/// A value in SIMP lang that could contain zero, one, or more clif values
#[derive(Clone, PartialEq, Eq, Debug)]
enum Value {
    Empty,
    Single([ClifValue; 1]),
    Tuple(Vec<ClifValue>),
}

impl Value {
    /// Returns `true` if the value is [`Empty`].
    ///
    /// [`Empty`]: Value::Empty
    #[allow(dead_code)]
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
        }
    }

    fn values(&self) -> Values {
        match self {
            Self::Empty => Values::Empty,
            &Self::Single(val) => Values::Singular(val[0]),
            Self::Tuple(vals) => Values::Tuple(vals.iter()),
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

    fn len(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Single(..) => 1,
            Self::Tuple(vals) => vals.len(),
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
            let lhs = gen_operand(builder, global, local, $lhs.as_ref())
                .as_single()
                .expect("Unary addition operator cannot be used on a tuple");
            let rhs = gen_operand(builder, global, local, $rhs.as_ref())
                .as_single()
                .expect("Unary addition operator cannot be used on a tuple");
            builder.ins().$f(lhs, rhs).into()
        }};
    }
    macro_rules! cmp {
        ($cmp:path, $lhs:expr, $rhs:expr $(,)?) => {{
            let lhs = gen_operand(builder, global, local, $lhs.as_ref())
                .as_single()
                .expect("Unary addition operator cannot be used on a tuple");
            let rhs = gen_operand(builder, global, local, $rhs.as_ref())
                .as_single()
                .expect("Unary addition operator cannot be used on a tuple");
            builder.ins().icmp($cmp, lhs, rhs).into()
        }};
    }
    match expr {
        Expr::Id(id) => builder.use_var(local.expect_var(id.as_str())).into(),
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
        Expr::IfElse(cond, if_body, else_body) => gen_if_else(
            builder,
            global,
            local,
            cond,
            if_body.as_block().unwrap(),
            else_body
                .as_ref()
                .expect("Requires `else` block when using if-else block as value")
                .as_block()
                .unwrap(),
        ),
        Expr::Loop(body) => gen_loop(builder, global, local, body.as_block().unwrap()).into(),
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
    local: &mut LocalContext<'e>,
    global: &mut GlobalSymbols,
    lhs: &'e Expr,
    rhs: &'e Expr,
) {
    match lhs {
        Expr::Id(name) => gen_assign_var(builder, local, global, &name, rhs),
        Expr::Tuple(fields) => gen_assign_tuple(builder, local, global, &fields, rhs),
        _ => panic!("Expression cannot be used as LHS of assignment"),
    }
}

/// Generate an assignment with one variable as lhs, called by `gen_assign`
#[inline(always)]
fn gen_assign_var<'e>(
    builder: &mut FunctionBuilder<'_>,
    local: &mut LocalContext<'e>,
    global: &mut GlobalSymbols,
    lhs: &'e str,
    rhs: &'e Expr,
) {
    gen_operand(builder, global, local, rhs)
        .values()
        .for_each(|rhs| {
            let var = declare_var(builder, local, lhs);
            builder.def_var(var, rhs);
        });
}

/// Generate an assignment with a tuple as lhs, called by `gen_assign`
#[allow(unused_variables)]
#[inline(always)]
fn gen_assign_tuple<'e>(
    builder: &mut FunctionBuilder<'_>,
    local: &mut LocalContext<'e>,
    global: &mut GlobalSymbols,
    lhs: &'e [Expr],
    rhs: &'e Expr,
) {
    match rhs {
        Expr::Tuple(rhs) => {
            lhs.iter().zip(rhs).for_each(|(lhs, rhs)| {
                gen_assign(builder, local, global, lhs, rhs);
            });
        }
        Expr::IfElse(cond, if_block, else_block) => {
            let rhs = gen_if_else(
                builder,
                global,
                local,
                cond,
                if_block.as_block().unwrap(),
                else_block
                    .as_ref()
                    .map(|e| e.as_block().unwrap())
                    .unwrap_or(&[]),
            );
            lhs.iter()
                .map(|e|e.as_id().expect("Only identifier or tuple of identifiers is allowed as lhs of an assignment"))
                .zip(rhs.values())
                .for_each(|(id,rhs)| {
                    let var = declare_var(builder, local, id);
                    builder.def_var(var, rhs);
                });
        }
        _ => todo!(),
    }
}

fn gen_loop<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    body: &'e [Expr],
) -> ClifValue {
    let loop_block = builder.create_block();
    let break_block = builder.create_block();
    let break_val = builder.append_block_param(break_block, I64);

    local.enters_loop(break_block, loop_block);

    // loop block
    builder.switch_to_block(loop_block);
    let mut is_terminated = false;
    for expr in body {
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

    local.leaves_loop();

    break_val
}

#[inline(always)]
fn gen_if_else_no_tail<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    cond: &'e Expr,
    if_body: &'e [Expr],
    else_body: &'e [Expr],
) {
    let if_block = builder.create_block();
    let else_block = builder.create_block();
    let merged_block = builder.create_block();

    // cmp
    let cond_val = gen_operand(builder, global, local, cond).expect_single();
    builder.ins().brif(cond_val, if_block, &[], else_block, &[]);

    // if block
    builder.switch_to_block(if_block);
    builder.seal_block(if_block);
    gen_block_no_tail(builder, global, local, if_body);
    builder.ins().jump(merged_block, &[]);

    // else block
    builder.switch_to_block(else_block);
    builder.seal_block(else_block);
    gen_block_no_tail(builder, global, local, else_body);
    builder.ins().jump(merged_block, &[]);

    // merged block
    builder.switch_to_block(merged_block);
    builder.seal_block(merged_block);
}

#[inline(always)]
fn gen_if_else<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    cond: &'e Expr,
    if_body: &'e [Expr],
    else_body: &'e [Expr],
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
        gen_block(builder, global, local, if_body)
    };
    builder.ins().jump(merged_block, if_result.as_slice());

    // else block
    let else_result = {
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        gen_block(builder, global, local, else_body)
    };
    builder.ins().jump(merged_block, else_result.as_slice());

    // merged block
    // check number of results
    if if_result.len() != else_result.len() {
        panic!(
            "Expects same type of value from `if` block and `else` block, but found the if block returns {} and the else block returns {}",
            if_result.display(),
            else_result.display());
    }
    builder.switch_to_block(merged_block);
    builder.seal_block(merged_block);
    match if_result {
        Value::Empty => Value::Empty,
        Value::Single(_) => builder.append_block_param(merged_block, I64).into(),
        Value::Tuple(vals) => (0..vals.len())
            .map(|_| builder.append_block_param(merged_block, I64))
            .collect::<Vec<ClifValue>>()
            .into(),
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
    let (index, sig) = global.func(name).expect(
        format!("Trying to call the function `{name}` which does not exist, symbols: {global:?}")
            .as_str(),
    );
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

/// Generate IR for a block, requiring it to not have a tail
fn gen_block_no_tail<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    body: &'e [Expr],
) {
    body.iter()
        .take_while(|expr| gen_statement(builder, global, local, expr))
        .for_each(|_| ());
}

/// Generate IR for a block, if the block has a tail, return the value of the tail, otherwise
/// return iconst 0
fn gen_block<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    body: &'e [Expr],
) -> Value {
    match body {
        [] => Value::Empty,
        [expr] => match expr {
            Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
            expr => {
                gen_statement(builder, global, local, &expr);
                Value::Empty
            }
        },
        body => {
            local.enters_block();
            unsafe { body.get_unchecked(0..body.len() - 1) }
                .iter()
                .take_while(|expr| gen_statement(builder, global, local, expr))
                .for_each(|_| ());
            let last = unsafe { body.get_unchecked(body.len() - 1) };
            let val = match last {
                Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
                expr => {
                    gen_statement(builder, global, local, &expr);
                    Value::Empty
                }
            };
            local.leaves_block();
            val
        }
    }
}

/// Generate IR for a statement
/// Returns `false` if encountering a terminating statement (e.g. break, continue)
fn gen_statement<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalContext<'e>,
    expr: &'e Expr,
) -> bool {
    match expr {
        Expr::Assign(lhs, rhs) => gen_assign(builder, local, global, lhs, rhs),
        Expr::Call(callee, args) => {
            gen_call(builder, global, local, callee, args);
        }
        Expr::Block(body) => gen_block_no_tail(builder, global, local, &body),
        Expr::IfElse(cond, if_body, else_body) => gen_if_else_no_tail(
            builder,
            global,
            local,
            cond,
            if_body.as_block().unwrap(),
            else_body
                .as_ref()
                .map(|e| e.as_block().unwrap())
                .unwrap_or(&[]),
        ),
        Expr::Loop(body) => {
            gen_loop(builder, global, local, body.as_block().unwrap());
        }
        Expr::Break(expr) => {
            let val = gen_operand(builder, global, local, &expr);
            let break_block = local
                .parent_loop()
                .expect("Using `break` outside of a loop")
                .break_block;
            builder.ins().jump(break_block, val.as_slice());
            return false;
        }
        Expr::Continue => {
            let continue_block = local
                .parent_loop()
                .expect("Using `break` outside of a loop")
                .continue_block;
            builder.ins().jump(continue_block, &[]);
            return false;
        }
        _ => panic!("Expression not allowed as a statement"),
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
