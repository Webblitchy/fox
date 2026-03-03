#[derive(Debug, Clone, PartialEq)]
pub enum ScanError {
    UnterminatedStr,
    SingleQuoteStr,
    InvalidInt,
    InvalidDec,
    InvalidByte,
    UnterminatedCommentBlock,
    InvalidToken,
}
