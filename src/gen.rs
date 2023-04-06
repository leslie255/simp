#![allow(dead_code)]

use cranelift::prelude::{types::I64, AbiParam, InstBuilder, Signature, Value};
use cranelift_codegen::{
    ir::{Function, UserFuncName},
    isa::CallConv,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

use crate::{ast::Expr, scan::{VarTable, scan_func}};

/// Codegen for things that can be used as an operand
/// e.g. number literal, identifier, lhs + rhs, if-else block, ...
fn gen_operand<'f>(
    builder: &mut FunctionBuilder<'f>,
    var_table: &VarTable<'_>,
    expr: &Expr,
) -> Value {
    match expr {
        Expr::Id(id) => builder.use_var(var_table.expect_exist(id.as_str())),
        &Expr::Num(num) => builder.ins().iconst(I64, num),
        Expr::Eq(_, _) => todo!(),
        Expr::NEq(_, _) => todo!(),
        Expr::Le(_, _) => todo!(),
        Expr::LeEq(_, _) => todo!(),
        Expr::Gr(_, _) => todo!(),
        Expr::GrEq(_, _) => todo!(),
        Expr::Not(_) => todo!(),
        Expr::UnaryAdd(_) => todo!(),
        Expr::UnarySub(_) => todo!(),
        Expr::Add(lhs, rhs) => {
            let lhs = gen_operand(builder, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, var_table, rhs.as_ref());
            builder.ins().iadd(lhs, rhs)
        }
        Expr::Sub(lhs, rhs) => {
            let lhs = gen_operand(builder, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, var_table, rhs.as_ref());
            builder.ins().isub(lhs, rhs)
        }
        Expr::Mul(lhs, rhs) => {
            let lhs = gen_operand(builder, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, var_table, rhs.as_ref());
            builder.ins().imul(lhs, rhs)
        }
        Expr::Div(lhs, rhs) => {
            let lhs = gen_operand(builder, var_table, lhs.as_ref());
            let rhs = gen_operand(builder, var_table, rhs.as_ref());
            builder.ins().sdiv(lhs, rhs)
        }
        Expr::Mod(_, _) => todo!(),
        Expr::And(_, _) => todo!(),
        Expr::Or(_, _) => todo!(),
        Expr::Block(_) => todo!(),
        Expr::IfElse(_, _, _) => todo!(),
        Expr::Call(_, _) => todo!(),
        e => panic!("Expression {e:?} not allowed as rhs of assignment"),
    }
}

fn gen_assign<'f>(
    builder: &mut FunctionBuilder<'f>,
    var_table: &VarTable<'_>,
    lhs: &Expr,
    rhs: &Expr,
) {
    let name = lhs
        .as_id()
        .expect("Expression not allowed as lhs of assignment");
    let rhs = gen_operand(builder, var_table, rhs);
    builder.def_var(var_table.expect_exist(name.as_str()), rhs)
}

fn gen_tail<'f>(
    builder: &mut FunctionBuilder<'f>,
    var_table: &VarTable<'_>,
    expr: &Expr,
) {
    let val = [gen_operand(builder, var_table, expr)];
    builder.ins().return_(&val);
}

fn gen_statement<'f>(builder: &mut FunctionBuilder<'f>, var_table: &VarTable<'_>, expr: &Expr) {
    match expr {
        Expr::Assign(lhs, rhs) => gen_assign(builder, var_table, lhs, rhs),
        Expr::Block(_) => todo!(),
        Expr::IfElse(_, _, _) => todo!(),
        Expr::While(_, _) => todo!(),
        Expr::Call(_, _) => todo!(),
        Expr::Tail(expr) => gen_tail(builder, var_table, expr),
        _ => panic!("Expression not allowed as a statement"),
    }
}

pub fn compile_func((_name, args, body): (String, Vec<String>, Option<Box<Expr>>)) -> Function {
    let body = body.expect("TODO: Function declaration without body");
    let mut sign = Signature::new(CallConv::SystemV);
    sign.params.reserve(args.len());
    (0..args.len()).for_each(|_| {
        sign.params.push(AbiParam::new(I64));
    });
    sign.returns.push(AbiParam::new(I64));
    let var_table = scan_func(&body);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sign);
    let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
    var_table
        .iter()
        .for_each(|(_, var)| fn_builder.declare_var(var, I64));
    let block0 = fn_builder.create_block();
    fn_builder.append_block_params_for_function_params(block0);
    fn_builder.switch_to_block(block0);
    fn_builder.seal_block(block0);
    body.as_block()
        .expect("Single instruction functions isn't supported yet")
        .into_iter()
        .for_each(|e| gen_statement(&mut fn_builder, &var_table, e));
    fn_builder.finalize();
    func
}
