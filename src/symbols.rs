use std::collections::{hash_map::Entry, HashMap};

use cranelift::prelude::{Block, EntityRef, Signature};
use cranelift_codegen::ir::FuncRef;
use cranelift_frontend::Variable;

use crate::gen::ValueType;

/// Information about the parent loop block, stored inside `LocalContext` as a stack
#[derive(Clone, Copy, Debug)]
pub struct LoopInfo {
    pub break_block: Block,
    pub continue_block: Block,
    /// Type of value the block returns, used for checking if all the `break` statements in this
    /// loop carrys the same type of value.
    pub val_ty: Option<ValueType>,
}

impl LoopInfo {
    pub fn new(break_block: Block, continue_block: Block) -> Self {
        Self {
            break_block,
            continue_block,
            val_ty: None,
        }
    }

    /// If there was a `break` statement previously encountered, check if `new_count` matches the
    /// old `val_count`, otherwise, set the `val_count` to `new_count`.
    pub fn check_break_val(&mut self, new_ty: ValueType) -> bool {
        match self.val_ty {
            Some(prev) => prev.matches(new_ty),
            None => false,
        }
    }
}

/// Keep track of symbols inside a function, includeing variables and imported functions
#[derive(Clone, Debug)]
pub struct LocalContext<'e> {
    imported_funcs: HashMap<&'e str, FuncRef>,
    vars: Vec<HashMap<&'e str, Variable>>,
    next_var_id: usize,
    loops: Vec<LoopInfo>,
}

impl<'e> Default for LocalContext<'e> {
    fn default() -> Self {
        Self {
            imported_funcs: HashMap::default(),
            vars: vec![HashMap::default()],
            next_var_id: 0,
            loops: Vec::default(),
        }
    }
}

impl<'e> LocalContext<'e> {
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

    /// Called when entering a loop.
    /// Returns the `LoopInfo` object created
    pub fn enters_loop<'a>(&'a mut self, break_block: Block, continue_block: Block) {
        self.loops.push(LoopInfo::new(break_block, continue_block));
        self.enters_block();
    }

    pub fn leaves_loop(&mut self) {
        self.loops.pop();
        self.leaves_block();
    }

    /// Get the parent loop
    pub fn parent_loop(&mut self) -> Option<LoopInfo> {
        self.loops.last().copied()
    }

    /// Get a mutable reference to the  parent loop
    pub fn parent_loop_mut(&mut self) -> Option<&mut LoopInfo> {
        self.loops.last_mut()
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
