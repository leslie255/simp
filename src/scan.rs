use std::collections::HashMap;

use cranelift::prelude::EntityRef;
use cranelift_frontend::Variable;

use crate::ast::Expr;

#[derive(Debug, Clone, Default)]
pub struct VarTable<'e> {
    set: HashMap<&'e str, Variable>,
}

impl<'e> VarTable<'e> {
    pub fn scan_from(body: &'e Expr) -> Self {
        let mut result = Self::default();
        let mut id = 0usize;
        recursive_scan_assign(body, &mut |name| {
            result.set.insert(name, Variable::new(id));
            id += 1;
        });
        result
    }

    pub fn iter<'a: 'e>(&'a self) -> impl Iterator<Item = (&'e str, Variable)> {
        self.set.iter().map(|(&name, &var)| (name, var))
    }

    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.set.get(name).copied()
    }
}

fn recursive_scan_assign<'e>(expr: &'e Expr, f: &mut impl FnMut(&'e str)) {
    match expr {
        Expr::Assign(lhs, _) => {
            let str = lhs.as_id().expect("Expects a variable name as rhs of `=`");
            f(str.as_str());
        }
        Expr::Block(body) => body.iter().for_each(|e| recursive_scan_assign(e, f)),
        Expr::Fn(_, _, Some(expr)) | Expr::IfElse(_, expr, None) => recursive_scan_assign(expr, f),
        Expr::IfElse(_, if_block, Some(else_block)) => {
            recursive_scan_assign(if_block, f);
            recursive_scan_assign(else_block, f);
        }
        Expr::While(_, _) => todo!(),
        _ => (),
    }
}
