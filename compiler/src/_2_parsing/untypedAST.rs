#![allow(dead_code)]
use crate::compileErr::CompileErr;
use crate::literal::Literal;
use crate::metadata::Metadata;
use crate::symbols::fnInfo::ParamType;
use crate::token::{Token, TokenType};
use crate::vartype::VarType;

#[derive(Clone, Debug)]
pub struct UntypedArgument {
    pub name: Option<String>, // Optionnally named argument
    pub data: Expr,
    pub metadata: Metadata,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    ExprStmt {
        expr: Expr,
        metadata: Metadata,
    },
    VarDecl {
        name: String,
        expr: Option<Expr>,              // can be assigned after
        typeAnnotation: Option<VarType>, // can be deducted (inference)
        isConstant: bool,
        metadata: Metadata,
    },
    VarAssign {
        name: String,
        expr: Expr,
        operator: Token, // +=, *=, ++
        metadata: Metadata,
    },
    FnDecl {
        name: String,
        parameters: Vec<ParamType>,
        body: Box<Stmt>, // block
        fnType: VarType,
        metadata: Metadata,
    },
    StandaloneFnCall {
        // Function call alone (for void and ignored returns)
        expr: Expr, // Expr::FnCall
        metadata: Metadata,
    },
    Block {
        statements: Vec<Stmt>,
        metadata: Metadata,
    },
    If {
        condition: Expr,
        ifBlock: Box<Stmt>,            // Block
        elseBranch: Option<Box<Stmt>>, // else if: Some(Stmt::If) / else: Some(Stmt::Block) / nothing: None
        metadata: Metadata,
    },
    While {
        condition: Expr,
        body: Box<Stmt>, // block
        label: Option<String>,
        metadata: Metadata,
    },
    Return {
        value: Option<Expr>,
        metadata: Metadata,
    },
    Break {
        label: Option<String>,
        metadata: Metadata,
    },
    Continue {
        label: Option<String>,
        metadata: Metadata,
    }, // Can contain a label
    CompileErr,
}

impl Stmt {
    pub fn metadata(&self) -> Metadata {
        match self {
            Stmt::ExprStmt { metadata, .. } => metadata.clone(),
            Stmt::VarDecl { metadata, .. } => metadata.clone(),
            Stmt::VarAssign { metadata, .. } => metadata.clone(),
            Stmt::FnDecl { metadata, .. } => metadata.clone(),
            Stmt::StandaloneFnCall { metadata, .. } => metadata.clone(),
            Stmt::Block { metadata, .. } => metadata.clone(),
            Stmt::If { metadata, .. } => metadata.clone(),
            Stmt::While { metadata, .. } => metadata.clone(),
            Stmt::Return { metadata, .. } => metadata.clone(),
            Stmt::Break { metadata, .. } => metadata.clone(),
            Stmt::Continue { metadata, .. } => metadata.clone(),
            Stmt::CompileErr { .. } => Metadata::empty(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: Literal, // INT | DEC | STRING | BOOL | None
        metadata: Metadata,
    },
    UnaryExpr {
        operator: Token, // - !
        right: Box<Expr>,
        metadata: Metadata,
    },
    BinaryExpr {
        left: Box<Expr>,
        operator: Token, // "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/"
        right: Box<Expr>,
        metadata: Metadata,
    },
    Grouping {
        // expr in brackets
        expr: Box<Expr>,
        metadata: Metadata,
    },
    LogicalExpr {
        left: Box<Expr>,
        operator: Token, // || &&
        right: Box<Expr>,
        metadata: Metadata,
    },
    FnCall {
        name: String,
        arguments: Vec<UntypedArgument>,
        metadata: Metadata,
    },
    CompileErr,
}

impl Expr {
    pub fn metadata(&self) -> Metadata {
        match self {
            Expr::Literal { metadata, .. } => metadata.clone(),
            Expr::UnaryExpr { metadata, .. } => metadata.clone(),
            Expr::BinaryExpr { metadata, .. } => metadata.clone(),
            Expr::LogicalExpr { metadata, .. } => metadata.clone(),
            Expr::Grouping { metadata, .. } => metadata.clone(),
            Expr::FnCall { metadata, .. } => metadata.clone(),
            Expr::CompileErr { .. } => Metadata::empty(),
        }
    }
}
