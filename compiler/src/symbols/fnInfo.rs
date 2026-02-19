use crate::{
    _2_parsing::literal::Literal,
    symbols::builtins::BuiltinFn,
    vartype::{self, VarType},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FnIndex(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum FnKind {
    //TODO: inutilisé ?
    User,
    Builtin(BuiltinFn),
}

#[derive(Clone, Debug)]
pub struct ParamType {
    pub name: String,
    pub typ: VarType,
    // pub defaultValue: Literal, // TODO: plus tard (Nullable)
}

#[derive(Clone)]
pub struct FnInfo {
    pub name: String,
    pub typ: VarType,
    pub params: Vec<ParamType>,
    pub isThrowable: bool,
    pub panics: bool, // Functions that always panics
    pub kind: FnKind, // User or one of builtin
}

#[derive(Clone)]
pub struct FnTable {
    pub functions: Vec<FnInfo>,
    indexMap: HashMap<String, FnIndex>,
}

impl FnTable {
    pub fn getIndex(&self, name: &str) -> Result<FnIndex, String> {
        if let Some(fnIndex) = self.indexMap.get(name) {
            return Ok(fnIndex.clone());
        }

        return Err(format!("Undefined function: {}", name));
    }

    pub fn getInfo(&self, fnIndex: &FnIndex) -> FnInfo {
        return self.functions[fnIndex.0].clone();
    }

    pub fn insert(&mut self, name: &str, fnInfo: FnInfo) -> Result<FnIndex, String> {
        let index = FnIndex(self.functions.len());
        if self.indexMap.contains_key(name) {
            return Err(format!("Function \"{}\" already defined", name));
        }

        self.functions.push(fnInfo.clone());
        self.indexMap.insert(fnInfo.name.clone(), index);

        return Ok(index);
    }

    pub fn new() -> FnTable {
        return FnTable {
            functions: Vec::new(),
            indexMap: HashMap::new(),
        };
    }
}
