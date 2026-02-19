#![allow(dead_code)]
#![allow(non_snake_case)]

use std::fmt;

use crate::metadata::Metadata;

#[derive(Debug, Clone)]
pub struct Token {
    pub tokenType: TokenType,
    pub lexeme: String, // string in user code (used in errors)
    pub metadata: Metadata,
}

impl Token {
    pub fn print(&self) {
        print!("{} ", self.tokenType);
    }
    pub fn empty() -> Token {
        Token {
            tokenType: TokenType::Eof,
            lexeme: String::new(),
            metadata: Metadata::empty(),
        }
    }
    pub fn invalid() -> Token {
        Token {
            tokenType: TokenType::InvalidToken,
            lexeme: String::new(),
            metadata: Metadata::empty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Comma,

    // One or two character tokens.
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Bang,
    BangEqual,
    Equal,
    TwoEquals,
    BigArrow,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    TwoAmpersands,
    TwoPipes,
    Minus,
    MinusEqual,
    Arrow,
    Plus,
    PlusEqual,
    PlusPercent,
    Dot,
    TwoDots,
    Colon,
    TwoColons,
    Asterisk,
    AsteriskEqual,

    // Literals.
    Str(String),
    Int(i64),
    Dec(f64),
    Bool(bool),
    Byte(u8),
    Identifier(String), // variable name
    Null,

    // Keywords.
    Struct,
    Fn,
    For,
    If,
    Else,
    Return,
    Break,
    Continue,
    Var,
    Const,
    While,
    As,
    Import,
    From,
    IntType,
    DecType,
    BoolType,
    StrType,
    ByteType,

    // End of
    Eol,
    Eof,

    InvalidToken, // For errors
}

// To use println!("{}", TokenType);
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", format!("{}", self).to_lowercase()); // TODO: ? {:?}
    }
}
