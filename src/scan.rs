use std::collections::HashMap;

use cranelift::prelude::EntityRef;
use cranelift_frontend::Variable;

use crate::ast::Expr;

#[derive(Clone, Default)]
pub struct VarTable<'e> {
    set: HashMap<&'e str, Variable>,
}

impl<'e> std::fmt::Debug for VarTable<'e> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.set.fmt(f)
    }
}

impl<'e> VarTable<'e> {
    pub fn iter<'a: 'e>(&'a self) -> impl Iterator<Item = (&'e str, Variable)> {
        self.set.iter().map(|(&name, &var)| (name, var))
    }

    #[allow(dead_code)]
    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.set.get(name).copied()
    }

    pub fn expect_exist(&self, name: &'e str) -> Variable {
        self.set.get(name).copied().unwrap_or_else(|| {
            panic!(
                "Variable does not exist: `{}`, var table: {:?}",
                name.escape_debug(),
                self.set
            )
        })
    }
}

fn recursive_scan_assign<'e>(expr: &'e Expr, f: &mut impl FnMut(&'e str)) {
    match expr {
        Expr::Assign(lhs, _) => {
            let str = lhs.as_id().expect("Expects a variable name as lhs of `=`");
            f(str);
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

pub fn scan_func<'e>(body: &'e Expr) -> VarTable<'e> {
    let mut result = VarTable::<'e>::default();
    let mut id = 0usize;
    recursive_scan_assign(body, &mut |name| {
        result.set.insert(name, Variable::new(id));
        id += 1;
    });
    result
}
