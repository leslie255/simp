#![allow(dead_code)]

use cranelift::prelude::{
    types::I64, AbiParam, ExtFuncData, ExternalName, InstBuilder, Signature, Value,
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
    symbols::{GlobalSymbols, LocalSymbols},
};

/// Declares a new variable in the local symbol space and in the function builder
#[inline(always)]
fn declare_var<'e>(
    builder: &mut FunctionBuilder<'_>,
    local: &mut LocalSymbols<'e>,
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
    local: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) -> Value {
    macro_rules! bin_op {
        ($f:ident, $lhs:expr, $rhs:expr $(,)?) => {{
            let lhs = gen_operand(builder, global, local, $lhs.as_ref());
            let rhs = gen_operand(builder, global, local, $rhs.as_ref());
            builder.ins().$f(lhs, rhs)
        }};
    }
    macro_rules! cmp {
        ($cmp:path, $lhs:expr, $rhs:expr $(,)?) => {{
            let lhs = gen_operand(builder, global, local, $lhs.as_ref());
            let rhs = gen_operand(builder, global, local, $rhs.as_ref());
            builder.ins().icmp($cmp, lhs, rhs)
        }};
    }
    match expr {
        Expr::Id(id) => builder.use_var(local.expect_var(id.as_str())),
        &Expr::Num(num) => builder.ins().iconst(I64, num),
        Expr::Eq(lhs, rhs) => cmp!(IntCC::Equal, lhs, rhs),
        Expr::NEq(lhs, rhs) => cmp!(IntCC::NotEqual, lhs, rhs),
        Expr::Le(lhs, rhs) => cmp!(IntCC::SignedLessThan, lhs, rhs),
        Expr::LeEq(lhs, rhs) => cmp!(IntCC::SignedLessThanOrEqual, lhs, rhs),
        Expr::Gr(lhs, rhs) => cmp!(IntCC::SignedGreaterThan, lhs, rhs),
        Expr::GrEq(lhs, rhs) => cmp!(IntCC::SignedLessThanOrEqual, lhs, rhs),
        Expr::Not(_) => todo!(),
        Expr::UnaryAdd(e) => gen_operand(builder, global, local, e),
        Expr::UnarySub(e) => {
            let val = gen_operand(builder, global, local, e);
            builder.ins().ineg(val)
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
        Expr::Call(callee, args) => gen_call(builder, global, local, callee, args),
        e => panic!("Expression {e:?} not allowed as rhs of assignment"),
    }
}

fn gen_assign<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    local: &mut LocalSymbols<'e>,
    global: &mut GlobalSymbols,
    lhs: &'e Expr,
    rhs: &'e Expr,
) {
    let name = lhs
        .as_id()
        .expect("Expression not allowed as lhs of assignment");
    declare_var(builder, local, name);
    let rhs = gen_operand(builder, global, local, rhs);
    builder.def_var(local.expect_var(name), rhs)
}

#[allow(unused_variables)]
#[inline(always)]
fn gen_if_else<'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &mut GlobalSymbols,
    local: &mut LocalSymbols<'e>,
    cond: &'e Expr,
    if_body: &'e [Expr],
    else_body: &'e [Expr],
) -> Value {
    let if_block = builder.create_block();
    let else_block = builder.create_block();
    let merged_block = builder.create_block();

    // cmp
    let cond_val = gen_operand(builder, global, local, cond);
    builder.ins().brif(cond_val, if_block, &[], else_block, &[]);

    // if block
    let if_result = {
        builder.switch_to_block(if_block);
        builder.seal_block(if_block);
        gen_block(builder, global, local, if_body)
    };
    builder.ins().jump(merged_block, &[if_result]);

    // else block
    let else_result = {
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        gen_block(builder, global, local, else_body)
    };
    builder.ins().jump(merged_block, &[else_result]);

    // merged block
    builder.switch_to_block(merged_block);
    builder.seal_block(merged_block);
    builder.append_block_param(merged_block, I64)
}

/// Imports an user-defined external function to a function, returns the signature of the imported
/// function and the result `FuncRef`, if the function is already imported, returns the result
/// stored in `local`
fn import_func_if_needed<'a, 'e>(
    builder: &mut FunctionBuilder<'_>,
    global: &'a GlobalSymbols,
    local: &mut LocalSymbols<'e>,
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
    local: &mut LocalSymbols<'e>,
    callee: &'e Expr,
    args: &'e Vec<Expr>,
) -> Value {
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
    let args: Vec<Value> = args
        .iter()
        .map(|e| gen_operand(builder, global, local, e))
        .collect();
    let inst = builder.ins().call(func_ref, args.as_ref());
    *builder.inst_results(inst).iter().next().unwrap()
}

fn gen_tail<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) {
    let val = gen_operand(builder, global, local, expr);
    builder.ins().return_(&[val]);
}

fn gen_block<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalSymbols<'e>,
    body: &'e [Expr],
) -> Value {
    match body {
        [] => builder.ins().iconst(I64, 0),
        [expr] => match expr {
            Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
            expr => {
                gen_statement(builder, global, local, &expr);
                builder.ins().iconst(I64, 0)
            }
        },
        body => {
            local.enters_block();
            unsafe { body.get_unchecked(0..body.len() - 1) }
                .iter()
                .for_each(|expr| {
                    gen_statement(builder, global, local, expr);
                });
            let last = unsafe { body.get_unchecked(body.len() - 1) };
            let val = match last {
                Expr::Tail(expr) => gen_operand(builder, global, local, &expr),
                expr => {
                    gen_statement(builder, global, local, &expr);
                    builder.ins().iconst(I64, 0)
                }
            };
            local.leaves_block();
            val
        }
    }
}

fn gen_statement<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    global: &mut GlobalSymbols,
    local: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) {
    match expr {
        Expr::Assign(lhs, rhs) => gen_assign(builder, local, global, lhs, rhs),
        Expr::Call(callee, args) => {
            gen_call(builder, global, local, callee, args);
        }
        Expr::Block(_) => todo!(),
        Expr::IfElse(cond, if_body, else_body) => {
            gen_if_else(
                builder,
                global,
                local,
                cond,
                if_body.as_block().unwrap(),
                else_body
                    .as_ref()
                    .map(|e| e.as_block().unwrap())
                    .unwrap_or(&[]),
            );
        }
        Expr::While(_, _) => todo!(),
        _ => panic!("Expression not allowed as a statement"),
    }
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
    let mut local = LocalSymbols::default();
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
    builder.ins().return_(&[return_val]);

    builder.finalize();
    println!("{}", func.display());
    verify_function(&func, module.isa().flags())?;
    add_func_to_module(func_id, func, module);
    Ok(())
}
