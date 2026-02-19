use crate::vartype::VarType;
use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Symbol<'ctx> {
    pub varType: VarType,
    pub ptr: PointerValue<'ctx>,
}

#[derive(Clone)]
pub struct Symbols<'ctx> {
    pub symbols: HashMap<String, Symbol<'ctx>>,
    pub parent: Option<Box<Symbols<'ctx>>>,
}

impl<'ctx> Symbols<'ctx> {
    pub fn new(parent: Option<Symbols<'ctx>>) -> Symbols<'ctx> {
        let parentBoxed = match parent {
            Some(symbol) => Some(Box::new(symbol)),
            None => None,
        };

        Symbols {
            symbols: HashMap::new(),
            parent: parentBoxed,
        }
    }

    pub fn insert(&mut self, name: String, varType: VarType, ptr: PointerValue<'ctx>) {
        // insert only in current scope
        self.symbols.insert(name, Symbol { varType, ptr });
    }

    pub fn get(&self, name: &str) -> Result<Symbol<'ctx>, String> {
        // Check in current scope
        if let Some(symbol) = self.symbols.get(name) {
            return Ok(symbol.clone());
        }

        // Check in parent scope (recursively)
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }

        return Err(format!("[UNEXPECTED] Undefined variable: {}", name));
    }

    pub fn setPtr(&mut self, name: &str, ptr: PointerValue<'ctx>) {
        if let Some(symbol) = self.symbols.get_mut(name) {
            symbol.ptr = ptr;
        }
    }

    pub fn getPtr(&self, name: &str) -> Result<PointerValue<'ctx>, String> {
        match self.symbols.get(name) {
            Some(symbol) => return Ok(symbol.ptr.clone()),
            Null => return Err(format!("Undefined variable: {}", name)),
        }
    }
}
