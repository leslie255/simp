use std::collections::{hash_map::Entry, HashMap};

use cranelift::prelude::{EntityRef, Signature};
use cranelift_codegen::ir::FuncRef;
use cranelift_frontend::Variable;

/// Keep track of symbols inside a function, includeing variables and imported functions
#[derive(Clone, Debug)]
pub struct LocalSymbols<'e> {
    imported_funcs: HashMap<&'e str, FuncRef>,
    vars: Vec<HashMap<&'e str, Variable>>,
    next_var_id: usize,
}

impl<'e> Default for LocalSymbols<'e> {
    fn default() -> Self {
        Self {
            imported_funcs: HashMap::default(),
            vars: vec![HashMap::default()],
            next_var_id: 0,
        }
    }
}

impl<'e> LocalSymbols<'e> {
    fn var_stack_top_mut(&mut self) -> &mut HashMap<&'e str, Variable> {
        self.vars.last_mut().expect("Local symbol stack is empty")
    }

    fn _var_stack_top(&self) -> &HashMap<&'e str, Variable> {
        self.vars.last().expect("Local symbol stack is empty")
    }

    /// Returns a variable if it exists
    pub fn var(&self, name: &'e str) -> Option<Variable> {
        self.vars
            .iter()
            .rev()
            .find_map(|vars| vars.get(name))
            .copied()
    }

    pub fn enters_block(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn leaves_block(&mut self) {
        self.vars.pop();
    }

    /// Creates a new variable and add that to the symbols
    pub fn create_var(&mut self, name: &'e str) -> Variable {
        let var = Variable::new(self.next_var_id);
        self.var_stack_top_mut().insert(name, var);
        self.next_var_id += 1;
        var
    }

    /// Returns the variable of `name`, or exits with an error message if a variable of the
    /// provided name is not found
    pub fn expect_var(&self, name: &'e str) -> Variable {
        self.var(name).unwrap_or_else(|| {
            println!("Variable does not exist: `{}`", name);
            std::process::exit(255);
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
