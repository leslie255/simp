#![allow(dead_code)]

use cranelift::prelude::{
    types::I64, AbiParam, ExtFuncData, ExternalName, InstBuilder, Signature, Value,
};
use cranelift_codegen::{
    ir::{FuncRef, Function, UserExternalName, UserFuncName},
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
    symbols: &mut GlobalSymbols,
    var_table: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) -> Value {
    match expr {
        Expr::Id(id) => builder.use_var(var_table.expect_var(id.as_str())),
        &Expr::Num(num) => builder.ins().iconst(I64, num),
        Expr::Eq(_, _) => todo!(),
        Expr::NEq(_, _) => todo!(),
        Expr::Le(_, _) => todo!(),
        Expr::LeEq(_, _) => todo!(),
        Expr::Gr(_, _) => todo!(),
        Expr::GrEq(_, _) => todo!(),
        Expr::Not(_) => todo!(),
        Expr::UnaryAdd(e) => gen_operand(builder, symbols, var_table, e),
        Expr::UnarySub(_) => todo!(),
        Expr::Add(lhs, rhs) => {
            let lhs = gen_operand(builder, symbols, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, symbols, var_table, rhs.as_ref());
            builder.ins().iadd(lhs, rhs)
        }
        Expr::Sub(lhs, rhs) => {
            let lhs = gen_operand(builder, symbols, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, symbols, var_table, rhs.as_ref());
            builder.ins().isub(lhs, rhs)
        }
        Expr::Mul(lhs, rhs) => {
            let lhs = gen_operand(builder, symbols, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, symbols, var_table, rhs.as_ref());
            builder.ins().imul(lhs, rhs)
        }
        Expr::Div(lhs, rhs) => {
            let lhs = gen_operand(builder, symbols, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, symbols, var_table, rhs.as_ref());
            builder.ins().sdiv(lhs, rhs)
        }
        Expr::Mod(lhs, rhs) => {
            let lhs = gen_operand(builder, symbols, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, symbols, var_table, rhs.as_ref());
            builder.ins().srem(lhs, rhs)
        }
        Expr::And(_, _) => todo!(),
        Expr::Or(_, _) => todo!(),
        Expr::Block(_) => todo!(),
        Expr::IfElse(_, _, _) => todo!(),
        Expr::Call(callee, args) => gen_call(builder, symbols, var_table, callee, args),
        e => panic!("Expression {e:?} not allowed as rhs of assignment"),
    }
}

fn gen_assign<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    local: &mut LocalSymbols<'e>,
    symbols: &mut GlobalSymbols,
    lhs: &'e Expr,
    rhs: &'e Expr,
) {
    let name = lhs
        .as_id()
        .expect("Expression not allowed as lhs of assignment");
    declare_var(builder, local, name);
    let rhs = gen_operand(builder, symbols, local, rhs);
    builder.def_var(local.expect_var(name), rhs)
}

/// Imports an user-defined external function to a function, returns the signature of the imported
/// function and the result `FuncRef`, if the function is already imported, returns the result
/// stored in `var_table`
fn import_func_if_needed<'a, 'e>(
    builder: &mut FunctionBuilder<'_>,
    symbols: &'a GlobalSymbols,
    var_table: &mut LocalSymbols<'e>,
    name: &'e str,
) -> (&'a Signature, FuncRef) {
    let (index, sig) = symbols.func(name).expect(
        format!("Trying to call the function `{name}` which does not exist, symbols: {symbols:?}")
            .as_str(),
    );
    let func_ref = var_table.import_func_if_needed(name, || {
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
    symbols: &mut GlobalSymbols,
    var_table: &mut LocalSymbols<'e>,
    callee: &'e Expr,
    args: &'e Vec<Expr>,
) -> Value {
    let name = callee
        .as_id()
        .expect("Dynamic function calling is not supported yet");
    let (sig, func_ref) = import_func_if_needed(builder, symbols, var_table, name);
    if args.len() != sig.params.len() {
        panic!(
            "The function `{name}` requires {} arguments, but only {} are provided",
            args.len(),
            sig.params.len()
        );
    }
    let args: Vec<Value> = args
        .iter()
        .map(|e| gen_operand(builder, symbols, var_table, e))
        .collect();
    let inst = builder.ins().call(func_ref, args.as_ref());
    *builder.inst_results(inst).iter().next().unwrap()
}

fn gen_tail<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    symbols: &mut GlobalSymbols,
    var_table: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) {
    let val = gen_operand(builder, symbols, var_table, expr);
    builder.ins().return_(&[val]);
}

fn gen_statement<'f, 'e>(
    builder: &mut FunctionBuilder<'f>,
    symbols: &mut GlobalSymbols,
    var_table: &mut LocalSymbols<'e>,
    expr: &'e Expr,
) {
    match expr {
        Expr::Assign(lhs, rhs) => gen_assign(builder, var_table, symbols, lhs, rhs),
        Expr::Call(callee, args) => {
            gen_call(builder, symbols, var_table, callee, args);
        }
        Expr::Block(_) => todo!(),
        Expr::IfElse(_, _, _) => todo!(),
        Expr::While(_, _) => todo!(),
        Expr::Tail(expr) => gen_tail(builder, symbols, var_table, expr),
        _ => panic!("Expression not allowed as a statement"),
    }
}

#[inline(always)]
#[must_use]
fn make_func_signature(args: &Vec<String>) -> Signature {
    let mut sign = Signature::new(CallConv::SystemV);
    sign.params.reserve(args.len());
    (0..args.len()).for_each(|_| {
        sign.params.push(AbiParam::new(I64));
    });
    sign.returns.push(AbiParam::new(I64));
    sign
}

pub fn compile_func(
    module: &mut ObjectModule,
    symbols: &mut GlobalSymbols,
    (name, arg_names, body): (String, Vec<String>, Option<Box<Expr>>),
) -> VerifierResult<FuncId> {
    let sig = make_func_signature(&arg_names);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let func_id = module
        .declare_function(&name, Linkage::Export, &sig)
        .unwrap();
    let func_index = symbols
        .add_func(name, sig.clone())
        .expect("Redefinition of function");
    let body = match body {
        Some(x) => x,
        None => return Ok(func_id),
    };
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_index), sig);
    let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
    let mut local = LocalSymbols::default();
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    arg_names.iter().enumerate().for_each(|(i, name)| {
        let var = local.create_var(name);
        let val = *unsafe { builder.block_params(entry_block).get_unchecked(i) };
        builder.declare_var(var, I64);
        builder.def_var(var, val);
    });
    body.as_block()
        .expect("Single expression functions isn't supported yet")
        .into_iter()
        .for_each(|e| gen_statement(&mut builder, symbols, &mut local, e));

    builder.finalize();
    println!("{}", func.display());
    verify_function(&func, module.isa().flags())?;
    let mut ctx = Context::for_function(func);
    module.define_function(func_id, &mut ctx).unwrap();
    Ok(func_id)
}
