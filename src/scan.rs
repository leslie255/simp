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
    #[allow(dead_code)]
    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.vars.get(name).copied()
    }

    /// Append a new variable into the vars map, assuming that it did not exist in the map
    pub fn append_var(&mut self, name: &'e str, var: Variable) {
        self.vars.insert(name, var);
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

    /// Recursively through a function and creates a table of all its variables, while executing a
    /// function for every variable definition it finds
    pub fn scan_func(&mut self, body: &'e Expr, mut f: impl FnMut(&'e str, Variable)) {
        let mut id = self.vars.len();
        recursive_scan_assign(body, &mut |name| {
            let var = Variable::new(id);
            self.vars.insert(name, var);
            f(name, var);
            id += 1;
        });
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
