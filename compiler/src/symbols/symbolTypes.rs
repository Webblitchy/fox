/*

*/
use crate::vartype::VarType;
use std::collections::HashMap;

#[derive(Clone)]
pub struct SymbolType {
    pub varType: VarType,
    pub isConstant: bool,
    pub isBuiltIn: bool,
    pub isAssigned: bool, // value defined after declaration
    pub isParam: bool,
}

#[derive(Clone)]
pub struct SymbolTypes {
    pub symbols: HashMap<String, SymbolType>,
    pub parent: Option<Box<SymbolTypes>>,
}

impl SymbolTypes {
    pub fn new(parent: Option<SymbolTypes>) -> SymbolTypes {
        let parent = match parent {
            Some(p) => Some(Box::new(p)),
            None => None,
        };

        SymbolTypes {
            symbols: HashMap::new(),
            parent: parent,
        }
    }

    pub fn insert(&mut self, name: &str, symbolType: SymbolType) -> Result<(), String> {
        if let Some(foundSymbol) = self.symbols.get(name) {
            if foundSymbol.isParam {
                return Err(format!("Variable \"{}\" already defined as a function parameter", name));
            } else {
                return Err(format!("Variable \"{}\" already defined in the same block", name));
            }
        }

        self.symbols.insert(String::from(name), symbolType);

        return Ok(());
    }

    pub fn get(&self, name: &str) -> Result<SymbolType, String> {
        // Check in current scope
        if let Some(symbol) = self.symbols.get(name) {
            return Ok(symbol.clone());
        }

        // Check in parent scope (recursively)
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }

        return Err(String::from("Undefined variable"));
    }

    pub fn getAsMut(&mut self, name: &str) -> Result<&mut SymbolType, String> {
        // Check in current scope
        if let Some(symbol) = self.symbols.get_mut(name) {
            return Ok(symbol);
        }

        // Check in parent scope (recursively)
        if let Some(parent) = &mut self.parent {
            return parent.getAsMut(name);
        }

        return Err(format!("Undefined variable: {}", name));
    }

    // returns false also if value not in symbol table
    pub fn isAssigned(&self, name: &str) -> bool {
        match self.get(name) {
            Ok(symbol) => return symbol.isAssigned,
            Err(_) => return false,
        }
    }
}
