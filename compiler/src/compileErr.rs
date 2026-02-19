use crate::{
    _3_checking::typedAST::{TypedExpr, TypedStmt},
    metadata::Metadata,
};

#[derive(Debug, Clone)]
pub struct CompileErr {
    pub errType: String,
    pub msg: String,
    pub metadata: Metadata,
    // help: String, // TODO: maybe later
}
