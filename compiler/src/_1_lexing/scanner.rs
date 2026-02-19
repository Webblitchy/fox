#![allow(dead_code)]
#![allow(non_snake_case)]

use crate::compileErr::CompileErr;
use crate::metadata::{self, Metadata};
use crate::token::*;
use TokenType::*;
use std::collections::HashMap;
use std::num::IntErrorKind;
use std::sync::OnceLock;

#[derive(PartialEq)]
enum NbrType {
    IntegerBase2,
    IntegerBase8,
    IntegerBase10,
    IntegerBase16,
    Decimal,
}

pub struct Scanner {
    source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    pub errors: Vec<CompileErr>,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: String::from(source),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            errors: Vec::new(),
        }
    }

    pub fn scanTokens(&mut self) {
        while !self.isAtEnd() {
            self.start = self.current;
            self.scanToken();
        }

        // Add end of file token at the end
        self.tokens.push(Token {
            tokenType: Eof,
            lexeme: String::new(),
            metadata: Metadata {
                line: self.line,
                column: self.column,
                start: self.current,
                end: self.current + 1,
            },
        });
    }

    fn isAtEnd(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn scanToken(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.addToken(LeftParen),
            ')' => self.addToken(RightParen),
            '{' => self.addToken(LeftCurly),
            '}' => self.addToken(RightCurly),
            '[' => self.addToken(LeftBracket),
            ']' => self.addToken(RightBracket),
            ',' => self.addToken(Comma),
            '+' => {
                if self.nextMatches('=') {
                    self.addToken(PlusEqual);
                } else if self.nextMatches('%') {
                    self.addToken(PlusPercent);
                } else {
                    self.addToken(Plus);
                }
            }
            '-' => {
                if self.nextMatches('>') {
                    self.addToken(Arrow);
                } else if self.nextMatches('=') {
                    self.addToken(MinusEqual);
                } else {
                    self.addToken(Minus);
                }
            }
            '*' => {
                if self.nextMatches('=') {
                    self.addToken(AsteriskEqual);
                } else {
                    self.addToken(Asterisk);
                }
            }
            '%' => {
                if self.nextMatches('=') {
                    self.addToken(PercentEqual);
                } else {
                    self.addToken(Percent);
                }
            }
            '/' => {
                if self.nextMatches('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.isAtEnd() {
                        self.advance();
                    }
                } else if self.nextMatches('*') {
                    // multi line comment
                    while self.peek() != '*' && self.peekNext() != '/' && !self.isAtEnd() {
                        self.advance();
                    }
                    self.advance(); // skip last '*'
                    self.advance(); // skip last '/'
                } else if self.nextMatches('=') {
                    self.addToken(SlashEqual);
                } else {
                    self.addToken(Slash);
                }
            }
            '.' => {
                if self.nextMatches('.') {
                    self.addToken(TwoDots);
                } else {
                    self.addToken(Dot);
                }
            }
            '!' => {
                if self.nextMatches('=') {
                    self.addToken(BangEqual);
                } else {
                    self.addToken(Bang);
                }
            }
            '=' => {
                if self.nextMatches('=') {
                    self.addToken(TwoEquals);
                } else if self.nextMatches('>') {
                    self.addToken(BigArrow);
                } else {
                    self.addToken(Equal);
                }
            }
            '<' => {
                if self.nextMatches('=') {
                    self.addToken(LessEqual);
                } else {
                    self.addToken(Less);
                }
            }
            '>' => {
                if self.nextMatches('=') {
                    self.addToken(GreaterEqual);
                } else {
                    self.addToken(Greater);
                }
            }
            ':' => {
                if self.nextMatches(':') {
                    self.addToken(TwoColons)
                } else {
                    self.addToken(Colon)
                }
            }
            '&' => {
                if self.nextMatches('&') {
                    self.addToken(TwoAmpersands);
                } else {
                    self.unexpectedCharErr();
                }
            }
            '|' => {
                if self.nextMatches('|') {
                    self.addToken(TwoPipes);
                } else {
                    self.unexpectedCharErr();
                }
            }
            '\\' => {
                if self.nextMatches('\n') {
                    // increment line without adding Eol
                    self.line += 1;
                    self.column = 1;
                }
                // else : ignore
            }
            '\n' => {
                self.addToken(Eol);
                self.line += 1;
                self.column = 1;
            }

            ' ' | '\r' | '\t' | '\0' => { /* ignore */ }

            '"' => self.handleString(),

            '\'' => match self.handleByte() {
                Ok(()) => {}
                Err(err) => {
                    self.errors.push(err);
                    self.addToken(InvalidToken);
                }
            },

            otherChar => {
                if otherChar.is_ascii_digit() {
                    match self.handleNumber() {
                        Ok(()) => {}
                        Err(err) => {
                            self.errors.push(err);
                            self.addToken(InvalidToken);
                        }
                    }
                } else if otherChar.is_ascii_alphabetic() || otherChar == '_' {
                    self.handleIdentifier();
                } else {
                    self.unexpectedCharErr();
                }
            }
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.column += 1;
        return self.sourceCharAt(self.current - 1);
    }

    fn addToken(&mut self, tokenType: TokenType) {
        // to report errors to user
        let length = self.current - self.start;
        let mut lexeme: String = self
            .source
            .chars()
            .skip(self.start)
            .take(length) // length to take
            .collect();

        if lexeme == "\n" {
            lexeme = String::from("\\n");
        } else if lexeme == "\t" {
            lexeme = String::from("\\t");
        }

        self.tokens.push(Token {
            metadata: Metadata {
                line: self.line,
                column: self.column - length, // start column
                start: self.start,
                end: self.current,
            },
            tokenType: tokenType,
            lexeme: lexeme,
        });
    }

    /**
     * Also consume char if is matching
     */
    fn nextMatches(&mut self, expected: char) -> bool {
        if self.isAtEnd() {
            return false;
        }

        if self.sourceCharAt(self.current) != expected {
            return false;
        }

        self.current += 1;
        self.column += 1;

        return true;
    }

    // Like advance but don't consume char
    fn peek(&self) -> char {
        if self.isAtEnd() {
            return '\0';
        }
        return self.sourceCharAt(self.current);
    }

    fn peekNext(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        return self.sourceCharAt(self.current + 1);
    }

    fn previous(&self) -> char {
        if self.current == 0 {
            return '\0';
        }
        return self.sourceCharAt(self.current - 1);
    }

    fn sourceCharAt(&self, index: usize) -> char {
        return match self.source.chars().nth(index) {
            Some(c) => c,
            None => '\0',
        };
    }

    fn handleString(&mut self) {
        let startOfString = self.current - 1;
        let startColumn = self.column - 1;
        let stringDelimiter = self.sourceCharAt(startOfString); // can be "
        while self.peek() != stringDelimiter {
            if self.peek() == '\n' || self.isAtEnd() {
                self.errors.push(CompileErr {
                    errType: String::from("Unterminated string"),
                    msg: format!("Missing {}", stringDelimiter),
                    metadata: Metadata {
                        line: self.line,
                        column: startColumn,
                        start: startOfString,
                        end: self.current + 1,
                    },
                });
                self.addToken(InvalidToken);
                return;
            }
            self.advance();
        }

        // Ignore the closing "
        self.advance();

        // Remove the start and end delimiters
        let value: String = self
            .source
            .chars()
            .skip(self.start + 1)
            .take(self.current - self.start - 2) // number of char to take
            .collect();

        self.addToken(Str(value));
    }

    fn getNumberBase(&mut self) -> NbrType {
        let mut nbrType = NbrType::IntegerBase10; // Default base

        // Integer not in base 10
        if self.previous() == '0' && ['x', 'b', 'o'].contains(&self.peek()) {
            let formatChar = self.advance(); // consume format char

            match formatChar {
                'x' => {
                    nbrType = NbrType::IntegerBase16;
                }
                'o' => {
                    nbrType = NbrType::IntegerBase8;
                }
                'b' => {
                    nbrType = NbrType::IntegerBase2;
                }
                _ => unreachable!(),
            }
        }

        return nbrType;
    }

    fn handleNumber(&mut self) -> Result<(), CompileErr> {
        let mut nbrType = self.getNumberBase();

        // read every chars (later conversion will handle errors)
        loop {
            let c = self.peek();
            // was not a special base
            if c == 'e' && nbrType == NbrType::IntegerBase10 {
                // decimal format \d+e(+|-)?\d+(\.\d+)?
                nbrType = NbrType::Decimal;
                self.advance();
                if ['+', '-'].contains(&self.peek()) {
                    self.advance();
                }
                break;
            } else if !c.is_ascii_alphanumeric() {
                break;
            }
            self.advance();
        }

        // Decimal number in format \d+e\d+
        if nbrType == NbrType::Decimal {
            while self.peek().is_ascii_alphanumeric() {
                self.advance();
            }
        }
        // Decimal number in format \d+.\d+ or \d+.\d+e\d+
        else if nbrType == NbrType::IntegerBase10 && self.peek() == '.' {
            nbrType = NbrType::Decimal;
            self.advance(); // consume '.'

            // read every chars (later conversion will handle errors)
            loop {
                let c = self.peek();
                if c == 'e' {
                    self.advance();
                    if ['+', '-'].contains(&self.peek()) {
                        self.advance();
                    }
                } else if c.is_ascii_alphanumeric() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let strLen = self.current - self.start;
        let lastMetadata = Metadata {
            line: self.line,
            column: self.column - strLen,
            start: self.start,
            end: self.current,
        };

        let numberStr: String = self
            .source
            .chars()
            .skip(self.start)
            .take(strLen) // nb chars
            .collect();

        // Convert string to number
        match nbrType {
            NbrType::IntegerBase10 => {
                let number: i64 = match numberStr.parse::<i64>() {
                    Ok(n) => n,
                    Err(e) => {
                        let typeMsg = match e.kind() {
                            IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => "Overflowing",
                            _ => "Invalid",
                        };
                        return Err(CompileErr {
                            errType: "Invalid number".to_string(),
                            msg: format!("{} number", typeMsg),
                            metadata: lastMetadata,
                        });
                    }
                };
                self.addToken(Int(number));
            }
            NbrType::Decimal => {
                let number: f64 = match numberStr.parse::<f64>() {
                    Ok(n) => {
                        if n.is_infinite() {
                            return Err(CompileErr {
                                errType: "Invalid number".to_string(),
                                msg: "Overflowing decimal".to_string(),
                                metadata: lastMetadata,
                            });
                        }
                        n
                    }
                    Err(_) => {
                        return Err(CompileErr {
                            errType: "Invalid number".to_string(),
                            msg: "Invalid decimal".to_string(),
                            metadata: lastMetadata,
                        });
                    }
                };
                self.addToken(Dec(number));
            }
            NbrType::IntegerBase16 => {
                let number = convertFromOtherBase(16, &numberStr, lastMetadata)?;
                self.addToken(Int(number));
            }
            NbrType::IntegerBase8 => {
                let number = convertFromOtherBase(8, &numberStr, lastMetadata)?;
                self.addToken(Int(number));
            }
            NbrType::IntegerBase2 => {
                let number = convertFromOtherBase(2, &numberStr, lastMetadata)?;
                self.addToken(Int(number));
            }
        }
        return Ok(());
    }

    fn handleByte(&mut self) -> Result<(), CompileErr> {
        // Ignore the '
        self.advance();

        let byteBase = self.getNumberBase();

        let mut wrongStr = false;
        // read every chars (later conversion will handle errors)
        loop {
            let c = self.peek();
            if !c.is_ascii_alphanumeric() {
                if c == '\'' {
                    wrongStr = true; // user declared a Str like 'hello'
                }
                break;
            }
            self.advance();
        }

        let strLen = self.current - self.start;
        let lastMetadata = Metadata {
            line: self.line,
            column: self.column - strLen,
            start: self.start,
            end: self.current,
        };

        let numberStr: String = self
            .source
            .chars()
            .skip(self.start + 1)
            .take(strLen - 1) // nb chars
            .collect();

        if wrongStr {
            return Err(CompileErr {
                errType: "Invalid byte".to_string(),
                msg: format!("' is used to declare Byte: '123\nUse \" to declare a Str"),
                metadata: lastMetadata,
            });
        }

        // Convert string to number
        let numberI64: i64;
        match byteBase {
            NbrType::IntegerBase10 => {
                numberI64 = match numberStr.parse::<i64>() {
                    Ok(n) => n,
                    Err(e) => {
                        let typeMsg = match e.kind() {
                            IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => "Overflowing",
                            _ => "Invalid",
                        };
                        return Err(CompileErr {
                            errType: "Invalid number".to_string(),
                            msg: format!("{} number", typeMsg),
                            metadata: lastMetadata,
                        });
                    }
                };
            }
            NbrType::IntegerBase16 => {
                numberI64 = convertFromOtherBase(16, &numberStr, lastMetadata.clone())?;
            }
            NbrType::IntegerBase8 => {
                numberI64 = convertFromOtherBase(8, &numberStr, lastMetadata.clone())?;
            }
            NbrType::IntegerBase2 => {
                numberI64 = convertFromOtherBase(2, &numberStr, lastMetadata.clone())?;
            }
            NbrType::Decimal => {
                return Err(CompileErr {
                    errType: "Invalid byte number".to_string(),
                    msg: format!("{} is not a byte number: an integer between 0 and 255", numberStr),
                    metadata: lastMetadata,
                });
            }
        }

        if numberI64 < 0 || numberI64 > 255 {
            return Err(CompileErr {
                errType: "Invalid number".to_string(),
                msg: format!("{} is not a byte number: an integer between 0 and 255", numberStr),
                metadata: lastMetadata,
            });
        }

        let numberU8 = numberI64 as u8;

        self.addToken(Byte(numberU8));
        return Ok(());
    }

    fn handleIdentifier(&mut self) {
        let keywords = getKeywords();
        loop {
            let c = self.peek();
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let identifierStr: String = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start) // number of char to take
            .collect();

        match keywords.get(identifierStr.as_str()) {
            // Get reserved keyword
            Some(keywordToken) => self.addToken(keywordToken.clone()),

            // Create new identifier (variable)
            None => self.addToken(Identifier(identifierStr)),
        };
    }

    fn unexpectedCharErr(&mut self) {
        let strLen = self.current - self.start;
        let metadata = Metadata {
            line: self.line,
            column: self.column - strLen,
            start: self.start,
            end: self.current,
        };

        let err = CompileErr {
            metadata,
            errType: String::from("Unexpected token"),
            msg: format!("Unexpected character"),
        };

        self.errors.push(err);
        self.addToken(TokenType::InvalidToken);
    }
}

static KEYWORDS: OnceLock<HashMap<&'static str, TokenType>> = OnceLock::new();
fn getKeywords() -> &'static HashMap<&'static str, TokenType> {
    KEYWORDS.get_or_init(|| {
        let mut m = HashMap::new();
        m.insert("if", If);
        m.insert("else", Else);
        m.insert("struct", Struct);
        m.insert("true", Bool(true));
        m.insert("false", Bool(false));
        m.insert("fn", Fn);
        m.insert("for", For);
        m.insert("null", Null);
        m.insert("return", Return);
        m.insert("break", Break);
        m.insert("continue", Continue);
        m.insert("var", Var);
        m.insert("const", Const);
        m.insert("while", While);
        m.insert("as", As);
        m.insert("import", Import);
        m.insert("from", From);
        m.insert("Int", IntType);
        m.insert("Dec", DecType);
        m.insert("Bool", BoolType);
        m.insert("Str", StrType);
        m.insert("Byte", ByteType);

        return m;
    })
}

fn convertFromOtherBase(base: u32, strNbr: &str, metadata: Metadata) -> Result<i64, CompileErr> {
    let withoutPrefix: String = strNbr.chars().skip(2).collect();
    match i64::from_str_radix(withoutPrefix.as_str(), base) {
        Ok(n) => return Ok(n),
        Err(e) => {
            let typeMsg = match e.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => "Overflowing",
                _ => "Invalid",
            };

            let baseStr = match base {
                2 => "binary".to_string(),
                8 => "octal".to_string(),
                16 => "hexadecimal".to_string(),
                _ => format!("base {}", base),
            };
            return Err(CompileErr {
                errType: "Invalid number".to_string(),
                msg: format!("{} {} number", typeMsg, baseStr),
                metadata,
            });
        }
    };
}
