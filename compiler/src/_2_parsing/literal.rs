#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Dec(f64),
    Str(String),
    Byte(u8),
    Identifier(String), // name defined by user
    Bool(bool),
    Null,
}
