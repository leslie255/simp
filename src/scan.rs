use std::collections::{hash_map::Entry, HashMap};

use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::FuncRef;
use cranelift_frontend::Variable;

use crate::ast::Expr;

#[derive(Clone, Default)]
pub struct VarTable<'e> {
    imported_funcs: HashMap<&'e str, FuncRef>,
    vars: HashMap<&'e str, Variable>,
}

impl<'e> std::fmt::Debug for VarTable<'e> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.vars.fmt(f)
    }
}

impl<'e> VarTable<'e> {
    pub fn iter<'a: 'e>(&'a self) -> impl Iterator<Item = (&'e str, Variable)> {
        self.vars.iter().map(|(&name, &var)| (name, var))
    }

    #[allow(dead_code)]
    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.vars.get(name).copied()
    }

    pub fn expect_exist(&self, name: &'e str) -> Variable {
        self.vars.get(name).copied().unwrap_or_else(|| {
            panic!(
                "Variable does not exist: `{}`, var table: {:?}",
                name.escape_debug(),
                self.vars
            )
        })
    }

    /// If a function is already imported, return the index and ref to that function, if not,
    /// execute the closure, add the returned index and ref to the map and return it
    #[inline(always)]
    #[must_use]
    pub fn import_func_if_needed(
        &mut self,
        name: &'e str,
        mut f: impl FnMut() -> FuncRef,
    ) -> FuncRef {
        match self.imported_funcs.entry(name) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => *v.insert(f()),
        }
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
        result.vars.insert(name, Variable::new(id));
        id += 1;
    });
    result
}
