use crate::compileErr::CompileErr;
use crate::literal::Literal;
use crate::metadata::{self, Metadata};
use crate::symbols::fnInfo::{FnIndex, ParamType};
use crate::token::Token;
use crate::untypedAST::Expr;
use crate::vartype::VarType;

#[derive(Clone, Debug)]
pub enum TypedStmt {
    ExprStmt {
        expr: TypedExpr,
        metadata: Metadata,
    },
    VarDecl {
        name: String,
        expr: Option<TypedExpr>,
        typ: VarType, // used when no expr
        isConstant: bool,
        metadata: Metadata,
    },
    VarAssign {
        name: String,
        expr: TypedExpr,
        metadata: Metadata,
    },
    FnDecl {
        fnIndex: FnIndex,
        parameters: Vec<ParamType>,
        body: Box<TypedStmt>, // TypedStmt::Block
        fnType: VarType,
        metadata: Metadata,
    },
    StandaloneFnCall {
        // Function call alone (for void and ignored returns)
        expr: TypedExpr, // TypedExpr::FnCall
        metadata: Metadata,
    },
    Block {
        statements: Vec<TypedStmt>,
        metadata: Metadata,
    },
    If {
        condition: TypedExpr,
        ifBlock: Box<TypedStmt>,            // TypedStmt::Block
        elseBranch: Option<Box<TypedStmt>>, // else if: TypedStmt::If / else: TypedStmt::Block / nothing: None
        alwaysTerminates: bool,
        metadata: Metadata,
    },
    While {
        condition: TypedExpr,
        body: Box<TypedStmt>, // TypedStmt::Block
        label: Option<String>,
        metadata: Metadata,
    },
    Return {
        value: Option<TypedExpr>,
        metadata: Metadata,
    },
    Break {
        label: Option<String>,
        metadata: Metadata,
    },
    Continue {
        label: Option<String>,
        metadata: Metadata,
    },
    CompileErr,
}

#[derive(Clone, Debug)]
pub enum TypedExpr {
    Literal {
        value: Literal, // INT | DEC | STRING | BOOL | None
        typ: VarType,
        metadata: Metadata,
    },
    UnaryExpr {
        operator: Token, // - !
        right: Box<TypedExpr>,
        typ: VarType,
        metadata: Metadata,
    },
    BinaryExpr {
        left: Box<TypedExpr>,
        operator: Token, // == != < <= > >= + - * / %
        right: Box<TypedExpr>,
        typ: VarType,
        metadata: Metadata,
    },
    LogicalExpr {
        left: Box<TypedExpr>,
        operator: Token, // && ||
        right: Box<TypedExpr>,
        typ: VarType,
        metadata: Metadata,
    },
    Grouping {
        // expr in parenthesis
        expr: Box<TypedExpr>,
        typ: VarType,
        metadata: Metadata,
    },
    FnCall {
        fnIndex: FnIndex,
        typ: VarType,
        arguments: Vec<TypedExpr>,
        metadata: Metadata,
    },
    CompileErr,
}

impl TypedExpr {
    pub fn typ(&self) -> VarType {
        match self {
            TypedExpr::Literal { typ, .. } => typ.clone(),
            TypedExpr::UnaryExpr { typ, .. } => typ.clone(),
            TypedExpr::BinaryExpr { typ, .. } => typ.clone(),
            TypedExpr::LogicalExpr { typ, .. } => typ.clone(),
            TypedExpr::Grouping { typ, .. } => typ.clone(),
            TypedExpr::FnCall { typ, .. } => typ.clone(),
            TypedExpr::CompileErr => VarType::CompileErr,
        }
    }

    pub fn metadata(&self) -> Metadata {
        match self {
            TypedExpr::Literal { metadata, .. } => metadata.clone(),
            TypedExpr::UnaryExpr { metadata, .. } => metadata.clone(),
            TypedExpr::BinaryExpr { metadata, .. } => metadata.clone(),
            TypedExpr::LogicalExpr { metadata, .. } => metadata.clone(),
            TypedExpr::Grouping { metadata, .. } => metadata.clone(),
            TypedExpr::FnCall { metadata, .. } => metadata.clone(),
            TypedExpr::CompileErr => Metadata::empty(),
        }
    }
}

impl TypedStmt {
    pub fn metadata(&self) -> Metadata {
        match self {
            TypedStmt::ExprStmt { metadata, .. } => metadata.clone(),
            TypedStmt::VarDecl { metadata, .. } => metadata.clone(),
            TypedStmt::VarAssign { metadata, .. } => metadata.clone(),
            TypedStmt::FnDecl { metadata, .. } => metadata.clone(),
            TypedStmt::StandaloneFnCall { metadata, .. } => metadata.clone(),
            TypedStmt::Block { metadata, .. } => metadata.clone(),
            TypedStmt::If { metadata, .. } => metadata.clone(),
            TypedStmt::While { metadata, .. } => metadata.clone(),
            TypedStmt::Return { metadata, .. } => metadata.clone(),
            TypedStmt::Break { metadata, .. } => metadata.clone(),
            TypedStmt::Continue { metadata, .. } => metadata.clone(),
            TypedStmt::CompileErr { .. } => Metadata::empty(),
        }
    }
}
