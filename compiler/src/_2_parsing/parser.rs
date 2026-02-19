use crate::{
    _2_parsing::untypedAST::UntypedArgument,
    compileErr::CompileErr,
    literal::Literal,
    metadata::{self, Metadata},
    symbols::fnInfo::ParamType,
    token::{Token, TokenType},
    untypedAST::{Expr, Stmt},
    vartype::VarType,
};
use TokenType::*;

type Res<T> = Result<T, CompileErr>;

pub struct GenericFnCall {
    pub name: String,
    pub arguments: Vec<UntypedArgument>,
    pub metadata: Metadata,
}

const TOP_LEVEL_TOKENS: &[TokenType] = &[
    TokenType::Fn,
    TokenType::Struct,
    // TokenType::Enum,
];

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub errors: Vec<CompileErr>,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = Vec::new();
        loop {
            println!("PARSE");
            match self.peek().tokenType {
                TokenType::Eof => break,
                TokenType::Eol => _ = self.advance(), // Ignore empty lines
                _ => {
                    let stmt = self.statement();
                    statements.push(stmt);
                }
            }
        }

        return statements;
    }

    fn statement(&mut self) -> Stmt {
        println!("STMT");
        match self.peek().tokenType {
            TokenType::Var | TokenType::Const => self.declaration(),
            TokenType::Identifier(_) => {
                // Identifier followed by "=", +=, ...
                let following = self.peekNext();
                match following.tokenType {
                    TokenType::Equal
                    | TokenType::PlusEqual
                    | TokenType::MinusEqual
                    | TokenType::AsteriskEqual
                    | TokenType::SlashEqual
                    | TokenType::PercentEqual => return self.assignment(),
                    TokenType::LeftParen => return self.fnCallStandaloneStmt(),
                    _ => return self.exprStmt(),
                };
            }
            TokenType::If => self.ifStmt(),
            TokenType::While => self.whileStmt(),
            TokenType::Break | TokenType::Continue => self.breakContinueStmt(),
            TokenType::Fn => self.fnDecl(),
            TokenType::For => {
                // TODO:
                unimplemented!();
            }
            TokenType::Return => self.returnStmt(),
            TokenType::InvalidToken => Stmt::CompileErr, // TODO: Théoriquement il faudrait rien mettre
            _ => self.exprStmt(),
        }
    }

    fn declaration(&mut self) -> Stmt {
        println!("DECLARATION");
        let firstToken = self.advance();
        let isConst = match firstToken.tokenType {
            TokenType::Var => false,
            TokenType::Const => true,
            _ => unreachable!(),
        };

        let identifierToken = self.advance();
        let identifier = match identifierToken.tokenType {
            TokenType::Identifier(name) => name,
            _ => {
                self.errors.push(CompileErr {
                    errType: String::from("Invalid variable declaration"),
                    msg: format!("The following name is invalid"),
                    metadata: identifierToken.metadata,
                });
                return Stmt::CompileErr;
            }
        };

        let mut token = self.advance();

        // If type is defined
        let mut typeAnnotation = None;
        if token.tokenType == TokenType::Colon {
            let annotationToken = self.advance();
            typeAnnotation = match annotationToken.tokenType {
                TokenType::IntType => Some(VarType::Int),
                TokenType::DecType => Some(VarType::Dec),
                TokenType::StrType => Some(VarType::Str),
                TokenType::BoolType => Some(VarType::Bool),
                _ => {
                    self.errors.push(CompileErr {
                        errType: String::from("Invalid variable declaration"),
                        msg: format!("Expected a type, found {}", annotationToken.lexeme.clone()),
                        metadata: annotationToken.metadata,
                    });
                    return Stmt::CompileErr;
                }
            };
            token = self.advance(); // get =
        }

        if token.tokenType == TokenType::Eol {
            if typeAnnotation == None {
                self.errors.push(CompileErr {
                    errType: String::from("Invalid variable declaration"),
                    msg: format!("Expected type annotation \": <type>\" for a declaration without value"),
                    metadata: token.metadata,
                });
                return Stmt::CompileErr;
            } else {
                return Stmt::VarDecl {
                    name: identifier,
                    expr: None, // will be filled in an assignment
                    typeAnnotation: typeAnnotation,
                    isConstant: isConst,
                    metadata: Metadata {
                        line: firstToken.metadata.line,
                        column: firstToken.metadata.column,
                        start: firstToken.metadata.start,
                        end: token.metadata.end - 1, // ignore EOL
                    },
                };
            }
        }

        if token.tokenType != TokenType::Equal {
            let errMsg;
            if typeAnnotation != None {
                errMsg = "Expected \"= <value>\"";
            } else {
                errMsg = "Expected \": <type>\" or \"= <value>\"";
            }
            self.errors.push(CompileErr {
                errType: String::from("Invalid variable declaration"),
                msg: errMsg.to_string(),
                metadata: token.metadata,
            });
            return Stmt::CompileErr;
        }

        let expr = self.expression();

        let lastToken;
        if self.peek().tokenType == Eof {
            lastToken = self.advance();
        } else {
            lastToken = match self.consume(Eol, "Unexpected token", "Invalid variable declaration") {
                Ok(t) => t,
                Err(()) => return Stmt::CompileErr,
            }
        }

        return Stmt::VarDecl {
            name: identifier,
            expr: Some(expr),
            typeAnnotation: typeAnnotation,
            isConstant: isConst,
            metadata: Metadata {
                line: firstToken.metadata.line,
                column: firstToken.metadata.column,
                start: firstToken.metadata.start,
                end: lastToken.metadata.end - 1, // Ignore EOL
            },
        };
    }

    // When variable is assigned to value (not decalaration)
    fn assignment(&mut self) -> Stmt {
        println!("ASSIGNMENT");
        let identifierToken = self.advance(); // Already checked

        // Const reassignement is verified during type checking
        let identifier: String = match identifierToken.tokenType {
            TokenType::Identifier(name) => name,
            _ => unreachable!(),
        };

        let operator = self.advance(); // =, +=, ...

        let expr = self.expression();

        let lastToken;
        if self.peek().tokenType != Eof {
            lastToken = match self.consume(Eol, "Unexpected token", "Invalid variable assignment") {
                Ok(t) => t,
                Err(()) => return Stmt::CompileErr,
            }
        } else {
            lastToken = self.advance();
        }

        return Stmt::VarAssign {
            name: identifier,
            expr: expr,
            operator: operator,
            metadata: Metadata {
                line: identifierToken.metadata.line,
                column: identifierToken.metadata.column,
                start: identifierToken.metadata.start,
                end: lastToken.metadata.end - 1, // remove EOL
            },
        };
    }

    fn ifStmt(&mut self) -> Stmt {
        println!("IF STMT");
        let firstToken = self.advance();
        match firstToken.tokenType {
            TokenType::If => {
                let condition = self.expression();

                // consume EOL before block if present
                self.ignore(Eol);

                let ifBlock = self.getBlock();

                let elseBranch: Option<Box<Stmt>>;
                // Has else or else if branch
                if self.peek().tokenType == TokenType::Else {
                    elseBranch = Some(Box::new(self.ifStmt()));
                } else {
                    elseBranch = None;
                }

                let lastToken = self.previous();

                return Stmt::If {
                    condition: condition,
                    ifBlock: Box::new(ifBlock),
                    elseBranch: elseBranch,
                    metadata: Metadata {
                        line: firstToken.metadata.line,
                        column: firstToken.metadata.column,
                        start: firstToken.metadata.start,
                        end: lastToken.metadata.end,
                    },
                };
            }
            TokenType::Else => {
                // ELSE IF
                if self.peek().tokenType == TokenType::If {
                    return self.ifStmt(); // recursive call
                }
                // Final ELSE
                else {
                    // consume EOL before block if present
                    self.ignore(Eol);

                    let block = self.getBlock(); // Else block
                    return block;
                }
            }
            _ => unreachable!(),
        }
    }

    fn whileStmt(&mut self) -> Stmt {
        println!("WHILE STMT");
        let firstToken = self.advance(); // consume "while"
        let condition = self.expression();

        // Check for a potiential label
        let label;
        if self.ignore(TokenType::Colon) {
            let labelToken = self.advance();
            label = match labelToken.tokenType {
                TokenType::Identifier(name) => Some(name),
                _ => {
                    self.errors.push(CompileErr {
                        errType: String::from("Invalid while syntax"),
                        msg: String::from("Expected a label here"),
                        metadata: labelToken.metadata,
                    });
                    return Stmt::CompileErr;
                }
            };
        } else {
            label = None;
        }

        // consume EOL before block if present
        self.ignore(Eol);

        let body = self.getBlock();

        let lastToken = self.previous();

        return Stmt::While {
            condition: condition,
            body: Box::new(body),
            label: label,
            metadata: Metadata {
                line: firstToken.metadata.line,
                column: firstToken.metadata.column,
                start: firstToken.metadata.start,
                end: lastToken.metadata.end,
            },
        };
    }

    fn breakContinueStmt(&mut self) -> Stmt {
        println!("BREAK CONTINUE STMT");
        let stmt = self.advance();

        // Check for a potiential label
        let label = match self.peek().tokenType {
            TokenType::Identifier(name) => {
                self.advance();
                Some(name)
            }
            _ => None,
        };

        let lastToken = match self.consume(
            TokenType::Eol,
            format!("Expected block EOL after {} statement", stmt.lexeme).as_str(),
            format!("Invalid {} statement", stmt.lexeme).as_str(),
        ) {
            Ok(t) => t,
            Err(()) => return Stmt::CompileErr,
        };

        let breakContinue = match stmt.tokenType {
            TokenType::Break => Stmt::Break {
                label: label,
                metadata: Metadata {
                    line: stmt.metadata.line,
                    column: stmt.metadata.column,
                    start: stmt.metadata.start,
                    end: lastToken.metadata.end - 1, // Remove the \n
                },
            },
            TokenType::Continue => Stmt::Continue {
                label: label,
                metadata: Metadata {
                    line: stmt.metadata.line,
                    column: stmt.metadata.column,
                    start: stmt.metadata.start,
                    end: lastToken.metadata.end - 1, // Remove the \n
                },
            },
            _ => unreachable!(),
        };

        return breakContinue;
    }

    fn returnStmt(&mut self) -> Stmt {
        println!("RETURN STMT");
        // TODO: multi return value ?
        let firstToken = self.advance(); // = return keyword

        if self.ignore(TokenType::Eol) {
            return Stmt::Return {
                value: None,
                metadata: Metadata {
                    line: firstToken.metadata.line,
                    column: firstToken.metadata.column,
                    start: firstToken.metadata.start,
                    end: firstToken.metadata.end,
                },
            };
        } else {
            let returnValue = self.expression();
            let lastToken = match self.consume(TokenType::Eol, "Unexpected token", "Invalid return statement") {
                Ok(t) => t,
                Err(()) => return Stmt::CompileErr,
            };
            return Stmt::Return {
                value: Some(returnValue),
                metadata: Metadata {
                    line: firstToken.metadata.line,
                    column: firstToken.metadata.column,
                    start: firstToken.metadata.start,
                    end: lastToken.metadata.end - 1, // Remove the \n
                },
            };
        }
    }

    fn fnDecl(&mut self) -> Stmt {
        println!("FN DECL");
        let mut parameters = Vec::new();

        let firstToken = self.advance(); // Skip "FN"

        let identifierToken = self.advance();
        let identifier = match identifierToken.tokenType {
            TokenType::Identifier(name) => name,
            _ => {
                self.errors.push(CompileErr {
                    errType: String::from("Invalid function declaration"),
                    msg: String::from("Bad function name"),
                    metadata: identifierToken.metadata,
                });
                return Stmt::CompileErr;
            }
        };

        if self
            .consume(TokenType::LeftParen, "Expected \")\"", "Invalid function declaration")
            .is_err()
        {
            return Stmt::CompileErr;
        }

        let mut firstRound = true;
        let mut splitSyntax = SplitSyntax::CommaOneLine; // by default
        loop {
            if firstRound {
                if self.ignore(RightParen) {
                    // No parameters
                    break;
                }

                if self.ignore(Eol) {
                    splitSyntax = SplitSyntax::MultiLinesMaybeComma;
                }
                firstRound = false;
            }

            let token = self.advance();
            let paramName = match token.tokenType {
                TokenType::Identifier(name) => name,
                // TokenType::RightParen => break, // TODO: tester
                _ => {
                    self.errors.push(CompileErr {
                        errType: String::from("Invalid function declaration"),
                        msg: String::from("Bad parameter name"),
                        metadata: token.metadata,
                    });
                    return Stmt::CompileErr;
                }
            };

            if self
                .consume(
                    TokenType::Colon,
                    "Expected \":<type>\" after a parameter name",
                    "Invalid function declaration",
                )
                .is_err()
            {
                return Stmt::CompileErr;
            }

            let paramTypeToken = self.advance();
            let paramType = match paramTypeToken.tokenType {
                TokenType::IntType => VarType::Int,
                TokenType::DecType => VarType::Dec,
                TokenType::StrType => VarType::Str,
                TokenType::BoolType => VarType::Bool,
                _ => {
                    self.errors.push(CompileErr {
                        errType: String::from("Invalid function declaration"),
                        msg: String::from("Expected a parameter type"),
                        metadata: paramTypeToken.metadata,
                    });
                    return Stmt::CompileErr;
                }
            };

            parameters.push(ParamType {
                name: paramName,
                typ: paramType,
            });

            let isLast = match self.handleSplitSeparator(&mut splitSyntax, "Invalid function declaration") {
                Ok(last) => last,
                Err(()) => return Stmt::CompileErr,
            };
            if isLast {
                break;
            }
        }

        let fnType: VarType;

        // returns something
        if self.peek().tokenType == TokenType::Arrow {
            self.advance(); // consume arrow

            let fnTypeToken = self.advance();
            fnType = match fnTypeToken.tokenType {
                TokenType::IntType => VarType::Int,
                TokenType::DecType => VarType::Dec,
                TokenType::StrType => VarType::Str,
                TokenType::BoolType => VarType::Bool,
                _ => {
                    self.errors.push(CompileErr {
                        errType: String::from("Invalid function declaration"),
                        msg: String::from("Expected a return type for function"),
                        metadata: fnTypeToken.metadata,
                    });
                    return Stmt::CompileErr;
                }
            };
        } else {
            fnType = VarType::Void;
        }
        let lastSignatureToken = self.previous();

        // consume EOL before block if present
        self.ignore(Eol);

        let fnBody = self.getBlock();

        return Stmt::FnDecl {
            name: identifier,
            parameters: parameters,
            body: Box::new(fnBody),
            fnType: fnType,
            metadata: Metadata {
                line: firstToken.metadata.line,
                column: firstToken.metadata.column,
                start: firstToken.metadata.start,
                end: lastSignatureToken.metadata.end,
            },
        };
    }

    fn fnCallExpr(&mut self) -> Expr {
        println!("FN CALL EXPR");
        let fnNameToken = self.previous();

        let mut arguments: Vec<UntypedArgument> = Vec::new();

        let fnName = match fnNameToken.tokenType {
            TokenType::Identifier(name) => name,
            _ => {
                println!("{:?}", fnNameToken.tokenType);
                self.errors.push(CompileErr {
                    errType: String::from("Invalid function call"),
                    msg: String::from("Not a function name"),
                    metadata: fnNameToken.metadata,
                });
                return Expr::CompileErr;
            }
        };

        self.advance(); // skip leftParen (already checked)

        let mut firstRound = true;
        let mut splitSyntax = SplitSyntax::CommaOneLine; // by default
        let mut alreadyHasNamedArgs = false;
        loop {
            if firstRound {
                if self.ignore(RightParen) {
                    // No arguments
                    break;
                }

                if self.ignore(Eol) {
                    splitSyntax = SplitSyntax::MultiLinesMaybeComma;
                }
                firstRound = false;
            }

            let mut nameToken = None;

            // If has named arguments
            if self.peekNext().tokenType == TokenType::Colon {
                alreadyHasNamedArgs = true;
                nameToken = Some(self.advance());
                self.advance(); // Skip Colon
            } else if alreadyHasNamedArgs {
                let token = self.advance();
                self.errors.push(CompileErr {
                    errType: String::from("Invalid function call"),
                    msg: String::from("After one named argument, all must be named"),
                    metadata: token.metadata,
                });
                return Expr::CompileErr;
            }

            let arg = self.expression();

            match nameToken {
                Some(nTok) => match nTok.tokenType {
                    TokenType::Identifier(name) => {
                        arguments.push(UntypedArgument {
                            name: Some(name),
                            metadata: Metadata {
                                line: nTok.metadata.line,
                                column: nTok.metadata.column,
                                start: nTok.metadata.start,
                                end: arg.metadata().end,
                            },
                            data: arg,
                        });
                    }
                    _ => {
                        self.errors.push(CompileErr {
                            errType: String::from("Invalid function call"),
                            msg: String::from("Bad parameter name"),
                            metadata: nTok.metadata,
                        });
                        return Expr::CompileErr;
                    }
                },
                None => {
                    arguments.push(UntypedArgument {
                        name: None,
                        metadata: arg.metadata(),
                        data: arg,
                    });
                }
            }

            let isLast = match self.handleSplitSeparator(&mut splitSyntax, "Invalid function call") {
                Ok(isLast) => isLast,
                Err(()) => {
                    return Expr::CompileErr;
                }
            };
            if isLast {
                break;
            }
        }

        let lastToken = self.previous();

        return Expr::FnCall {
            name: fnName,
            arguments: arguments,
            metadata: Metadata {
                line: fnNameToken.metadata.line,
                column: fnNameToken.metadata.column,
                start: fnNameToken.metadata.start,
                end: lastToken.metadata.end,
            },
        };
    }

    fn fnCallStandaloneStmt(&mut self) -> Stmt {
        match self.expression() {
            Expr::CompileErr => return Stmt::CompileErr,
            fnCall => {
                return Stmt::StandaloneFnCall {
                    metadata: fnCall.metadata(),
                    expr: fnCall,
                };
            }
        }
    }

    //--------------------------------------------------

    fn exprStmt(&mut self) -> Stmt {
        println!("EXPR STMT");
        let expr = self.expression();
        if self.peek().tokenType == Eol {
            self.advance();
        }
        return Stmt::ExprStmt {
            metadata: expr.metadata(),
            expr: expr,
        };
    }

    // get {...statements...}
    fn getBlock(&mut self) -> Stmt {
        println!("GET BLOCK");
        let firstBracket = match self.consume(LeftCurly, "Expected \"{\"", "Invalid block") {
            Ok(t) => t,
            Err(()) => return Stmt::CompileErr,
        };

        // Ignore Eol if present
        self.ignore(Eol);

        let mut block: Vec<Stmt> = Vec::new();

        loop {
            println!("LOOP BLOCK");
            match self.peek().tokenType {
                TokenType::RightCurly => break,
                TokenType::Eol => _ = self.advance(), // Ignore empty lines
                TokenType::Eof => {
                    let eofToken = self.advance();
                    self.errors.push(CompileErr {
                        msg: String::from("Expected \"}\""),
                        errType: String::from("Invalid block"),
                        metadata: eofToken.metadata,
                    });
                    break; // add block anyway
                }
                TokenType::Fn | TokenType::Struct => { // TODO: Top level statments ?
                    // TODO: Ici c'est forcément plus dans un block
                }
                _ => {
                    let stmt = self.statement();
                    match stmt {
                        Stmt::CompileErr => {
                            block.push(stmt); // TODO: Incorrect
                            break;
                        }
                        _ => block.push(stmt),
                    }
                }
            }
        }

        let lastBracket = self.advance(); // consume RightCurly

        // Ignore Eol if present
        self.ignore(Eol);

        return Stmt::Block {
            statements: block,
            metadata: Metadata {
                line: firstBracket.metadata.line,
                column: firstBracket.metadata.column,
                start: firstBracket.metadata.start,
                end: lastBracket.metadata.end,
            },
        };
    }

    // -----------------------------------------------------------------------
    // ------ Priority Tree of operators in expressions (low -> high) --------
    // -----------------------------------------------------------------------

    fn expression(&mut self) -> Expr {
        println!("EXPRESSION");
        return self.or();
    }

    fn or(&mut self) -> Expr {
        println!("OR");
        let mut expr = self.and(); // Consume left hand side (as and)

        while self.peek().tokenType == TokenType::TwoPipes {
            let orToken = self.advance();
            let right = self.and();
            expr = Expr::LogicalExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: orToken,
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn and(&mut self) -> Expr {
        println!("AND");
        let mut expr = self.equality();

        while self.peek().tokenType == TokenType::TwoAmpersands {
            let andToken = self.advance();
            let right = self.and();
            expr = Expr::LogicalExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: andToken,
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn equality(&mut self) -> Expr {
        println!("Equality");
        let mut expr: Expr = self.comparison();

        while self.matchType(&[BangEqual, TwoEquals]) {
            let operator: Token = self.advance();
            let right: Expr = self.comparison();
            expr = Expr::BinaryExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn comparison(&mut self) -> Expr {
        println!("Comparison");
        let mut expr: Expr = self.term();

        while self.matchType(&[Greater, GreaterEqual, Less, LessEqual]) {
            self.advance(); // consume Token
            let operator: Token = self.previous();
            let right: Expr = self.term();
            expr = Expr::BinaryExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }

        return expr;
    }

    fn term(&mut self) -> Expr {
        println!("TERM");
        let mut expr: Expr = self.factor();

        // TODO: MinusPercent
        while self.matchType(&[Minus, Plus, PlusPercent]) {
            self.advance(); // consume Token
            let operator: Token = self.previous();
            let right: Expr = self.factor();
            expr = Expr::BinaryExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }

        return expr;
    }

    fn factor(&mut self) -> Expr {
        println!("FACTOR");
        let mut expr = self.unary();

        while self.matchType(&[Slash, Asterisk, Percent]) {
            self.advance(); // consume Token
            let operator: Token = self.previous();
            let right: Expr = self.unary();
            expr = Expr::BinaryExpr {
                metadata: Metadata {
                    line: expr.metadata().line,
                    column: expr.metadata().column,
                    start: expr.metadata().start,
                    end: right.metadata().end,
                },
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            }
        }

        return expr;
    }

    fn unary(&mut self) -> Expr {
        println!("UNARY");
        if self.matchType(&[Bang, Minus]) {
            self.advance(); // consume Token
            let operator = self.previous();
            let right = self.unary();
            let expr = Expr::UnaryExpr {
                metadata: Metadata {
                    line: operator.metadata.line,
                    column: operator.metadata.column,
                    start: operator.metadata.start,
                    end: right.metadata().end,
                },
                operator: operator,
                right: Box::new(right),
            };
            return expr;
        }

        return self.primary();
    }

    fn primary(&mut self) -> Expr {
        println!("PRIMARY");
        let currToken = self.advance();
        let literal = match currToken.tokenType {
            TokenType::Bool(boolVal) => Literal::Bool(boolVal),
            TokenType::Null => Literal::Null,
            TokenType::Int(val) => Literal::Int(val),
            TokenType::Dec(val) => Literal::Dec(val),
            TokenType::Str(val) => Literal::Str(val),
            TokenType::Byte(val) => Literal::Byte(val),
            TokenType::Identifier(name) => {
                if self.peek().tokenType == LeftParen {
                    return self.fnCallExpr();
                } else {
                    Literal::Identifier(name)
                }
            }
            TokenType::LeftParen => {
                self.advance(); // consume Token
                let expr = self.expression();
                if self.consume(RightParen, "Expected \")\"", "Invalid primary").is_err() {
                    return Expr::CompileErr;
                }

                return Expr::Grouping {
                    expr: Box::new(expr),
                    metadata: currToken.metadata,
                };
            }

            _ => {
                self.errors.push(CompileErr {
                    msg: String::from("Unexpected Token"),
                    errType: String::from("Invalid primary"),
                    metadata: currToken.metadata,
                });
                return Expr::CompileErr;
            }
        };

        return Expr::Literal {
            value: literal,
            metadata: currToken.metadata,
        };
    }

    // -----------------------------------------------------------------------

    fn handleSplitSeparator(&mut self, splitSyntax: &mut SplitSyntax, errType: &str) -> Result<bool, ()> {
        match splitSyntax {
            SplitSyntax::MultiLinesMaybeComma => {
                if self.ignore(Comma) {
                    *splitSyntax = SplitSyntax::CommaMultiLines;
                } else {
                    *splitSyntax = SplitSyntax::MultiLines;
                }
            }
            SplitSyntax::MultiLines => {
                if self.consume(Eol, "Expected a return", "Invalid function declaration").is_err() {
                    return Err(());
                }

                if self.ignore(RightParen) {
                    return Ok(true); // is end = true
                }
            }
            SplitSyntax::CommaMultiLines => {
                let haveComma = self.consume(Comma, "Expected a comma and a return", "Invalid function declaration");

                if self
                    .consume(Eol, "Expected a comma and a return", "Invalid function declaration")
                    .is_err()
                {
                    return Err(());
                }
                if self.ignore(RightParen) {
                    // don't need a comma at the last arg
                    return Ok(true); // is end = true
                }
                match haveComma {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }
            }
            SplitSyntax::CommaOneLine => {
                if self.ignore(RightParen) {
                    return Ok(true); // is end = true
                }
                if self.consume(Comma, "Expected a \",\"", "Invalid function declaration").is_err() {
                    return Err(());
                }
            }
        }
        return Ok(false); // is end = false
    }

    fn consume(&mut self, tokenType: TokenType, message: &str, errType: &str) -> Result<Token, ()> {
        let nextToken = self.peek();
        if nextToken.tokenType != Eof && nextToken.tokenType == tokenType {
            return Ok(self.advance());
        }

        self.errors.push(CompileErr {
            metadata: nextToken.metadata,
            msg: String::from(message),
            errType: String::from(errType),
        });
        return Err(());
    }

    fn ignore(&mut self, tokenType: TokenType) -> bool {
        if self.peek().tokenType == tokenType {
            self.advance();
            // println!("IGNORING {}", nextToken.lexeme);
            return true;
        }
        return false;
    }

    fn syncStmt(&mut self) {
        while self.peek().tokenType != Eof {
            if self.peek().tokenType == Eol {
                self.advance();
                return;
            }
            self.advance();
        }
    }

    fn syncBlock(&mut self) {
        let recoveryToken = [Fn, Var, Const, If, For, While, Return, RightCurly];

        self.advance();

        while self.peek().tokenType != Eof {
            if recoveryToken.contains(&self.peek().tokenType) {
                return;
            }
            self.advance();
        }
    }

    fn advance(&mut self) -> Token {
        if self.peek().tokenType != Eof {
            // is not at end
            self.current += 1;
        }
        // println!("ADVANCE: {}", self.previous().lexeme);
        return self.previous();
    }

    fn matchType(&mut self, types: &[TokenType]) -> bool {
        let currTokenType = self.peek().tokenType;

        if types.contains(&currTokenType) {
            return true;
        }

        return false;
    }

    //---------------------------

    fn peek(&self) -> Token {
        return self.getToken(self.current);
    }

    fn peekNext(&self) -> Token {
        return self.getToken(self.current + 1);
    }

    fn previous(&self) -> Token {
        return self.getToken(self.current - 1);
    }

    fn getToken(&self, index: usize) -> Token {
        match self.tokens.get(index) {
            Some(token) => {
                return token.clone();
            }
            None => Token::invalid(),
        }
    }
}

// How to split: params, args, arrays,...
#[derive(PartialEq, Clone)]
enum SplitSyntax {
    CommaOneLine,
    MultiLinesMaybeComma, // before knowing
    MultiLines,
    CommaMultiLines,
}
