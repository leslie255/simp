#![allow(dead_code)]

use cranelift::prelude::{types::I64, AbiParam, Signature};
use cranelift_codegen::{
    ir::{Function, UserFuncName},
    isa::CallConv,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

use crate::{ast::Expr, scan::VarTable};

#[allow(unused_variables)]
pub fn compile_func(name: String, args: Vec<String>, body: Expr) -> Function {
    let mut sign = Signature::new(CallConv::SystemV);
    sign.params.reserve(args.len());
    (0..args.len()).for_each(|_| {
        sign.params.push(AbiParam::new(I64));
    });
    sign.returns.push(AbiParam::new(I64));
    let mut var_table = VarTable::scan_from(&body);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sign);
    let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
    var_table.iter().for_each(|(_, var)|fn_builder.declare_var(var, I64));
    println!("{}", func.display());
    todo!()
}
