use std::collections::{hash_map::Entry, HashMap};

use cranelift::prelude::{EntityRef, Signature};
use cranelift_codegen::ir::FuncRef;
use cranelift_frontend::Variable;

/// Keep track of symbols inside a function, includeing variables and imported functions
#[derive(Clone, Default)]
pub struct LocalSymbols<'e> {
    imported_funcs: HashMap<&'e str, FuncRef>,
    vars: HashMap<&'e str, Variable>,
    next_var_id: usize,
}

impl<'e> std::fmt::Debug for LocalSymbols<'e> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.vars.fmt(f)
    }
}

impl<'e> LocalSymbols<'e> {
    /// Returns a variable if it exists
    #[allow(dead_code)]
    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.vars.get(name).copied()
    }

    /// Creates a new variable and add that to the symbols
    pub fn create_var(&mut self,name: &'e str) -> Variable {
        let var = Variable::new(self.next_var_id);
        self.vars.insert(name, var);
        self.next_var_id += 1;
        var
    }

    pub fn expect_var(&self, name: &'e str) -> Variable {
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

/// Keeps track of global symbols
#[derive(Debug, Clone, Default)]
pub struct GlobalSymbols {
    /// Map from function name to Id and signature of the function
    funcs: HashMap<String, (u32, Signature)>,
    next_index: u32,
}

impl GlobalSymbols {
    /// Get the signature of a function by its name
    pub fn func(&self, name: &str) -> Option<&(u32, Signature)> {
        self.funcs.get(name)
    }

    /// Add a new function to the symbols, returns index of the function
    /// returns `Err(())` if the symbol existed, otherwise returns `Ok(index)`
    #[inline(always)]
    pub fn add_func(&mut self, name: String, sig: Signature) -> Result<u32, ()> {
        let index = self.next_index;
        self.next_index += 1;
        match self.funcs.insert(name, (index, sig)) {
            Some(..) => Err(()),
            None => Ok(index),
        }
    }
}
