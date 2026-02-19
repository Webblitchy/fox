use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub enum VarType {
    Int,
    Byte,
    Dec,
    Bool,
    Str,
    Null,
    Void, // Only for functions
    CompileErr,
    All, // Only for builtins
}

// To use println!("{}", );
impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}
