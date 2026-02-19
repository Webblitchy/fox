use crate::_2_parsing::untypedAST::UntypedArgument;
use crate::compileErr::CompileErr;
use crate::literal::Literal;
use crate::metadata::{self, Metadata};
use crate::symbolTypes::SymbolTypes;
use crate::symbols::builtins::BuiltinFn;
use crate::symbols::fnInfo::{FnIndex, FnInfo, FnKind, FnTable, ParamType};
use crate::symbols::symbolTypes::SymbolType;
use crate::token::{Token, TokenType};
use crate::typedAST::{TypedExpr, TypedStmt};
use crate::untypedAST::{Expr, Stmt};
use crate::vartype::VarType;

use std::collections::HashSet;

#[derive(Clone, PartialEq, Debug)]
enum FlowState {
    Normal,
    Return,
    Panic,
    Break,
    Continue,
    Throw,
}

pub struct TypeChecker {
    pub symbolTypes: SymbolTypes, // changes during the checks
    pub fnTable: FnTable,
    symbolsBeforeIf: Option<SymbolTypes>, // to keep state before an if/else chain
    loopLevel: u32,
    insideFn: bool,
    flowState: FlowState,
    currReturnedType: Option<(VarType, Metadata)>, // return metadata for errors
    pub errors: Vec<CompileErr>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            symbolTypes: SymbolTypes::new(None),
            fnTable: FnTable::new(),
            symbolsBeforeIf: None,
            loopLevel: 0, // not in a loop
            insideFn: false,
            flowState: FlowState::Normal,
            currReturnedType: None,
            errors: Vec::new(),
        }
    }

    pub fn afterChecks(&mut self) {
        // Check if main() is present (type already checked)
        if self.fnTable.getIndex("main").is_err() {
            self.errors.push(CompileErr {
                msg: String::from(""), // not displayed
                errType: String::from("Missing main function"),
                metadata: Metadata::empty(),
            });
        }
    }

    // Declare global scope statment
    // (useful to use fn before declaring in code and check redefinitions)
    pub fn declarationPass(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FnDecl {
                name,
                parameters,
                body,
                fnType,
                metadata,
            } => {
                // Insert parameters in function table (fnTable)
                let mut paramTypes: Vec<ParamType> = Vec::new();
                let mut paramNames: HashSet<String> = HashSet::new();

                for parameter in parameters {
                    if !paramNames.insert(parameter.name.clone()) {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: "Parameter already existing".to_string(),
                            errType: "Invalid function declaration".to_string(),
                        });
                    }
                    paramTypes.push(parameter.clone());
                }

                // TODO: handle throwable after
                let fnInfo = FnInfo {
                    name: name.clone(),
                    typ: fnType.clone(),
                    params: paramTypes,
                    isThrowable: false,
                    panics: false,
                    kind: FnKind::User,
                };
                match self.fnTable.insert(name, fnInfo) {
                    Ok(_) => {
                        println!("Decalare fn {}", name);
                    }
                    // Insertion can fail because of redeclaration
                    Err(err) => {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: err,
                            errType: String::from("Invalid function declaration"),
                        });
                    }
                }
            }
            _ => (), // Ignore every other things
        }
    }

    // ---------------------------

    pub fn exprTyped(&mut self, expr: &Expr) -> TypedExpr {
        match expr {
            Expr::Literal { value, metadata } => match value {
                Literal::Int(v) => TypedExpr::Literal {
                    value: Literal::Int(*v),
                    typ: VarType::Int,
                    metadata: metadata.clone(),
                },
                Literal::Byte(v) => TypedExpr::Literal {
                    value: Literal::Byte(*v),
                    typ: VarType::Byte,
                    metadata: metadata.clone(),
                },
                Literal::Dec(v) => TypedExpr::Literal {
                    value: Literal::Dec(*v),
                    typ: VarType::Dec,
                    metadata: metadata.clone(),
                },
                Literal::Bool(v) => TypedExpr::Literal {
                    value: Literal::Bool(*v),
                    typ: VarType::Bool,
                    metadata: metadata.clone(),
                },
                Literal::Str(v) => TypedExpr::Literal {
                    value: Literal::Str(v.clone()),
                    typ: VarType::Str,
                    metadata: metadata.clone(),
                },
                Literal::Null => TypedExpr::Literal {
                    value: Literal::Null,
                    typ: VarType::Null,
                    metadata: metadata.clone(),
                },
                Literal::Identifier(name) => {
                    // identifier (var name) in expression
                    let symbol = match self.symbolTypes.get(name) {
                        Ok(s) => s,
                        Err(errStr) => {
                            self.errors.push(CompileErr {
                                metadata: metadata.clone(),
                                msg: errStr,
                                errType: String::from("Name resolution error"),
                            });
                            return TypedExpr::CompileErr; // node itself is unexploitable
                        }
                    };

                    // variable can be assigned after declaration but always before use
                    if !symbol.isAssigned {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: "Variable was never assigned before use".to_string(),
                            errType: "Use of uninitialized before".to_string(),
                        });
                        return TypedExpr::CompileErr;
                    }

                    return TypedExpr::Literal {
                        value: Literal::Identifier(name.clone()),
                        typ: symbol.varType.clone(),
                        metadata: metadata.clone(),
                    };
                }
            },
            Expr::BinaryExpr {
                left,
                operator,
                right,
                metadata,
            } => {
                let typedLeft = self.exprTyped(left);
                if matches!(typedLeft, TypedExpr::CompileErr { .. }) {
                    return typedLeft;
                }

                let typedRight = self.exprTyped(right);
                if matches!(typedRight, TypedExpr::CompileErr { .. }) {
                    return typedRight;
                }

                let leftType: VarType = typedLeft.typ();
                let rightType: VarType = typedRight.typ();
                if leftType != rightType {
                    self.errors.push(CompileErr {
                        msg: format!("The two values are not the same type: {} != {}", leftType, rightType),
                        errType: "Mismatching types".to_string(),
                        metadata: metadata.clone(),
                    });
                    return TypedExpr::CompileErr;
                }

                let wholeExpType;
                match operator.tokenType {
                    TokenType::TwoEquals
                    | TokenType::BangEqual
                    | TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual => {
                        wholeExpType = VarType::Bool; // type become bool
                    }
                    _ => wholeExpType = leftType,
                }

                return TypedExpr::BinaryExpr {
                    left: Box::new(typedLeft),
                    operator: operator.clone(),
                    right: Box::new(typedRight),
                    metadata: metadata.clone(),
                    typ: wholeExpType,
                };
            }
            Expr::UnaryExpr { operator, right, metadata } => {
                let typedExpr = self.exprTyped(right);

                if matches!(typedExpr, TypedExpr::CompileErr { .. }) {
                    return typedExpr;
                }

                return TypedExpr::UnaryExpr {
                    operator: operator.clone(),
                    metadata: metadata.clone(),
                    typ: typedExpr.typ(),
                    right: Box::new(typedExpr),
                };
            }
            Expr::LogicalExpr {
                left,
                operator,
                right,
                metadata,
            } => {
                let typedLeft = self.exprTyped(left);
                if matches!(typedLeft, TypedExpr::CompileErr { .. }) {
                    return typedLeft;
                }

                let typedRight = self.exprTyped(right);
                if matches!(typedRight, TypedExpr::CompileErr { .. }) {
                    return typedRight;
                }

                let leftType: VarType = typedLeft.typ();
                let rightType: VarType = typedRight.typ();

                if leftType != VarType::Bool {
                    self.errors.push(CompileErr {
                        metadata: left.metadata(),
                        msg: "Value must be a bool".to_string(),
                        errType: "Bad comparison type".to_string(),
                    });
                    return TypedExpr::CompileErr;
                } else if rightType != VarType::Bool {
                    self.errors.push(CompileErr {
                        metadata: right.metadata(),
                        msg: "Value must be a bool".to_string(),
                        errType: "Bad comparison type".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }

                return TypedExpr::LogicalExpr {
                    left: Box::new(typedLeft),
                    operator: operator.clone(),
                    right: Box::new(typedRight),
                    metadata: metadata.clone(),
                    typ: VarType::Bool,
                };
            }
            Expr::Grouping { expr, metadata } => self.exprTyped(&*expr),
            Expr::FnCall {
                name: fnName,
                arguments,
                metadata,
            } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Function call must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }

                if fnName == "main" {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Cannot call main function".to_string(),
                        errType: "Invalid function call".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }

                let mut arguments = arguments.clone();

                let builtinFnIndex = match self.callBuiltins(fnName, &mut arguments, &metadata) {
                    Ok(index) => index,
                    Err(()) => return TypedExpr::CompileErr,
                };

                let fnIndex = match builtinFnIndex {
                    Some(index) => index,
                    None => match self.fnTable.getIndex(fnName) {
                        // Get non builtin fn index
                        Ok(v) => v,
                        Err(_) => {
                            self.errors.push(CompileErr {
                                metadata: metadata.clone(),
                                msg: "Function not declared".to_string(),
                                errType: "Invalid function call".to_string(),
                            });
                            return TypedExpr::CompileErr;
                        }
                    },
                };

                let fnInfo: FnInfo = self.fnTable.getInfo(&fnIndex);

                // TODO: handle optionnal arguments later (before checking number)
                // HERE

                if arguments.len() > fnInfo.params.len() {
                    let mut parameters = Vec::new();
                    for arg in fnInfo.params {
                        parameters.push(format!("{}: {}", arg.name, arg.typ));
                    }
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: format!("Too many arguments for: fn {}({})", fnName, parameters.join(", ")),
                        errType: "Invalid function call".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }

                let mut foundParams: Vec<bool> = vec![false; fnInfo.params.len()];

                let placeholder = TypedExpr::Literal {
                    value: Literal::Null,
                    typ: VarType::Void,
                    metadata: Metadata::empty(),
                };

                let mut typedArgs: Vec<TypedExpr> = vec![placeholder; arguments.len()];
                for (i, arg) in arguments.iter().enumerate() {
                    println!("{}: {:?}", i, arg);
                    let typedArg = self.exprTyped(&arg.data);
                    let argType = typedArg.typ();
                    let mut parameterIndex = -1;

                    if arg.name.is_none() {
                        // Positionnal argument
                        let paramType = fnInfo.params[i].typ.clone();
                        if !validParamTyp(&paramType, &argType) {
                            self.errors.push(CompileErr {
                                metadata: arg.data.metadata(),
                                msg: format!("Invalid argument type: {}, expected {}", argType, paramType),
                                errType: "Invalid function call".to_string(),
                            });
                            return TypedExpr::CompileErr;
                        }
                        foundParams[i] = true;
                        parameterIndex = i as i32;
                    } else {
                        // Named argument
                        let argName = arg.name.clone().unwrap(); // already checked
                        // println!("ARG {}", argName);
                        for (j, param) in fnInfo.params.iter().enumerate() {
                            if argName == param.name {
                                if !validParamTyp(&param.typ, &argType) {
                                    //TODO: pas return mais continuer les params ?
                                    self.errors.push(CompileErr {
                                        metadata: arg.data.metadata(),
                                        msg: format!("Invalid argument type: {}, expected {}", argType, param.typ),
                                        errType: "Invalid function call".to_string(),
                                    });
                                    return TypedExpr::CompileErr;
                                }
                                parameterIndex = j as i32;
                                foundParams[j] = true;
                                break;
                            }
                        }
                        if parameterIndex == -1 {
                            // not found
                            let mut badParams = Vec::new();
                            for arg in fnInfo.params {
                                badParams.push(format!("{}: {}", arg.name, arg.typ));
                            }
                            self.errors.push(CompileErr {
                                metadata: arg.metadata.clone(),
                                msg: format!("Unknown argument name for: fn {}({})", fnName, badParams.join(", ")),
                                errType: "Invalid function call".to_string(),
                            });
                            return TypedExpr::CompileErr;
                        }
                    }
                    typedArgs[parameterIndex as usize] = typedArg; // cannot be -1 here
                }

                let mut nbMissing = 0;
                let mut missingParams = Vec::new();
                for (i, foundParam) in foundParams.iter().enumerate() {
                    if foundParam == &false {
                        missingParams.push(format!("{}: {}", fnInfo.params[i].name, fnInfo.params[i].typ));
                        nbMissing += 1;
                    } else {
                        missingParams.push(String::from("..."));
                    }
                }

                if nbMissing != 0 {
                    let mut msg = String::from("Missing argument");
                    if nbMissing > 1 {
                        msg += "s";
                    }
                    msg += format!(": {}(", fnName).as_str();
                    msg += missingParams.join(", ").as_str();
                    msg += ")";

                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg,
                        errType: "Invalid function call".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }

                if fnInfo.panics {
                    self.flowState = FlowState::Panic;
                }

                return TypedExpr::FnCall {
                    fnIndex: fnIndex,
                    arguments: typedArgs,
                    metadata: metadata.clone(),
                    typ: fnInfo.typ.clone(),
                };

                /*
                //TODO: check that type is not void
                // FAIRE Ailleurs ?
                if fnCall.typ == VarType::Void {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Function without return value cannot be called in an expression".to_string(),
                        errType: "Invalid function call".to_string(),
                    });
                    return TypedExpr::CompileErr;
                }
                */
            }
            Expr::CompileErr => TypedExpr::CompileErr,
        }
    }

    pub fn checkType(&mut self, untypedStmt: &Stmt) -> TypedStmt {
        match untypedStmt {
            Stmt::ExprStmt { expr, metadata } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Expression must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                    return TypedStmt::CompileErr;
                }

                let exprTyped = self.exprTyped(expr);
                match exprTyped {
                    TypedExpr::CompileErr => {
                        return TypedStmt::CompileErr;
                    }
                    _ => {
                        return TypedStmt::ExprStmt {
                            expr: exprTyped,
                            metadata: metadata.clone(),
                        };
                    }
                }
            }
            Stmt::VarDecl {
                name,
                typeAnnotation,
                expr,
                isConstant,
                metadata,
            } => {
                // TODO: après voir plus tard pour les globales
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Declaration must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                match expr {
                    Some(expr) => {
                        // println!("Declaring {} cith value", name.clone());
                        let typedExpr = self.exprTyped(expr);

                        let exprType = typedExpr.typ(); // in case of CompileErr, type is VarType::CompileErr
                        match typeAnnotation {
                            Some(annotation) => {
                                if *annotation != exprType && exprType != VarType::CompileErr {
                                    self.errors.push(CompileErr {
                                        metadata: metadata.clone(),
                                        msg: String::from("Types are not matching"),
                                        errType: "Invalid variable declaration".to_string(),
                                    });
                                }
                            }
                            None => {}
                        }

                        match self.symbolTypes.insert(
                            name,
                            SymbolType {
                                varType: exprType.clone(),
                                isConstant: *isConstant,
                                isAssigned: true,
                                isBuiltIn: false,
                                isParam: false,
                            },
                        ) {
                            Ok(()) => {}
                            Err(err) => {
                                self.errors.push(CompileErr {
                                    metadata: metadata.clone(),
                                    msg: err,
                                    errType: "Invalid variable declaration".to_string(),
                                });
                            } // Insertion can fail because of redeclaration in same block
                        }

                        // recreate the typed version of VarDecl
                        // (even if variable is an error: typed as CompileErr)
                        return TypedStmt::VarDecl {
                            name: name.clone(),
                            expr: Some(typedExpr),
                            typ: exprType,
                            isConstant: isConstant.clone(),
                            metadata: metadata.clone(),
                        };
                    }
                    None => {
                        // println!("Declaring {} without value", name.clone());
                        let typ = match typeAnnotation {
                            Some(t) => t,
                            None => unreachable!(), // checked by the parser
                        };

                        match self.symbolTypes.insert(
                            name,
                            SymbolType {
                                varType: typ.clone(),
                                isConstant: *isConstant,
                                isAssigned: false,
                                isBuiltIn: false,
                                isParam: false,
                            },
                        ) {
                            Ok(()) => {}
                            Err(err) => {
                                self.errors.push(CompileErr {
                                    metadata: metadata.clone(),
                                    msg: err,
                                    errType: "Invalid variable declaration".to_string(),
                                });
                            } // Insertion can fail because of redeclaration in same block
                        }
                        return TypedStmt::VarDecl {
                            name: name.clone(),
                            expr: None,
                            typ: typ.clone(),
                            isConstant: isConstant.clone(),
                            metadata: metadata.clone(),
                        };
                    }
                }
            }
            Stmt::VarAssign {
                name,
                expr,
                operator,
                metadata,
            } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Assignement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                let mut typedExpr = self.exprTyped(expr);

                // println!("Assigning {} to {}", name.clone(), expr.metadata().lexeme);

                let mut variable = match self.symbolTypes.get(name) {
                    Ok(symbol) => symbol,
                    Err(errStr) => {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: errStr,
                            errType: String::from("Invalid variable assignment"),
                        });
                        return TypedStmt::CompileErr; // Cannot get variable so returns
                    }
                };

                if variable.isConstant && variable.isAssigned {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Constant cannot be reassigned"),
                        errType: String::from("Invalid variable assignment"),
                    });
                    return TypedStmt::CompileErr;
                }

                if variable.isConstant && self.loopLevel != 0 {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Constant cannot be assigned in a loop"),
                        errType: String::from("Invalid variable assignment"),
                    });
                    return TypedStmt::CompileErr;
                }

                if variable.isParam {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Parameters cannot be reassigned"),
                        errType: String::from("Invalid variable assignment"),
                    });
                    return TypedStmt::CompileErr;
                }

                if variable.varType != typedExpr.typ() && variable.varType != VarType::CompileErr && typedExpr.typ() != VarType::CompileErr {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: format!(
                            "Variable of type \"{}\" cannot be reassigned to type \"{}\"",
                            variable.varType,
                            typedExpr.typ()
                        ),
                        errType: String::from("Invalid variable assignment"),
                    });
                    return TypedStmt::CompileErr;
                }

                // transform i += 2 -> i = i + 2
                if operator.tokenType != TokenType::Equal {
                    let addedIdentifier = TypedExpr::Literal {
                        value: Literal::Identifier(name.clone()),
                        typ: variable.varType.clone(),
                        metadata: typedExpr.metadata(),
                    };

                    let replacedOp = match operator.tokenType {
                        TokenType::PlusEqual => TokenType::Plus,
                        TokenType::MinusEqual => TokenType::Minus,
                        TokenType::AsteriskEqual => TokenType::Asterisk,
                        TokenType::SlashEqual => TokenType::Slash,
                        TokenType::PercentEqual => TokenType::Percent,
                        _ => unreachable!(),
                    };

                    typedExpr = TypedExpr::BinaryExpr {
                        metadata: typedExpr.metadata(),
                        typ: typedExpr.typ(),
                        left: Box::new(addedIdentifier),
                        operator: Token {
                            tokenType: replacedOp,
                            lexeme: operator.lexeme.clone(),
                            metadata: metadata.clone(),
                        },
                        right: Box::new(typedExpr),
                    }
                }

                // Empty variable reassigned
                if !variable.isAssigned {
                    variable.isAssigned = true;
                    // replace variable with edited one
                    match self.symbolTypes.getAsMut(name) {
                        Ok(symbolRef) => *symbolRef = variable,
                        Err(errMsg) => {
                            self.errors.push(CompileErr {
                                metadata: metadata.clone(),
                                msg: errMsg,
                                errType: String::from("Unexpected compiler error"),
                            });
                            return TypedStmt::CompileErr;
                        }
                    };
                }

                // recreate the typed version of VarAssign
                return TypedStmt::VarAssign {
                    name: name.clone(),
                    expr: typedExpr,
                    metadata: metadata.clone(),
                };
            }
            Stmt::Block { statements, metadata } => {
                let mut typedBlockStmts: Vec<TypedStmt> = Vec::new();

                // Enter new Scope
                let parent = Some(self.symbolTypes.clone());
                let symbols = SymbolTypes::new(parent);
                self.symbolTypes = symbols;

                // Check Types
                for untypedStmt in statements {
                    typedBlockStmts.push(self.checkType(untypedStmt));
                }

                // Exist Scope
                match self.symbolTypes.parent.take() {
                    Some(parent) => self.symbolTypes = *parent,
                    None => {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: String::from("Cannot exit global scope"),
                            errType: String::from("Invalid block"),
                        });
                    }
                }

                return TypedStmt::Block {
                    statements: typedBlockStmts,
                    metadata: metadata.clone(),
                };
            }
            Stmt::If {
                condition,
                ifBlock,
                elseBranch,
                metadata,
            } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "If statement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                let typedCondition = self.exprTyped(condition);
                if ![VarType::Bool, VarType::CompileErr].contains(&typedCondition.typ()) {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Condition must be a bool"),
                        errType: String::from("Invalid if statement"),
                    });
                }

                let mut isFirstIf = false;
                // Parallel contexts between branches: save previous context
                if self.symbolsBeforeIf.is_none() {
                    // it is the first if (not else if)
                    self.symbolsBeforeIf = Some(self.symbolTypes.clone());
                    isFirstIf = true;
                    println!("Entering first if");
                } else {
                    println!("Entering else if");
                }

                // Save flow state to verify after if there is a return
                let flowBeforeIf = self.flowState.clone();
                self.flowState = FlowState::Normal;

                // Check if block
                let typedIfBlock = self.checkType(ifBlock);

                // Restore flow state
                let flowAfterIf = self.flowState.clone();
                self.flowState = flowBeforeIf;

                // Save symbols state after if/elseif
                let symbolsAfterIf = self.symbolTypes.clone();
                println!("Save symbols after if/elseif block");

                let typedElseBranch;
                let symbolsAfterElse;
                let flowAfterElse: Option<FlowState>;

                match elseBranch {
                    Some(stmtBox) => {
                        // restore symbols after if/elseif
                        self.symbolTypes = self.symbolsBeforeIf.clone().unwrap();
                        println!("Restore symbols before another elseif block");

                        // restore flow after if/elseif
                        let flowBeforeElse = self.flowState.clone();
                        self.flowState = FlowState::Normal;

                        let typedElse = self.checkType(&**stmtBox);

                        // Save symbols state after else
                        symbolsAfterElse = Some(self.symbolTypes.clone());

                        flowAfterElse = Some(self.flowState.clone());
                        self.flowState = flowBeforeElse;

                        println!("save symbols after else (if?)");
                        typedElseBranch = Some(Box::new(typedElse));
                    }
                    None => {
                        typedElseBranch = None;
                        symbolsAfterElse = None;
                        flowAfterElse = None;
                        println!("emptying symbols because no else");
                    }
                };

                self.symbolTypes = self.symbolsBeforeIf.clone().unwrap(); // restore state before first if
                println!("restoring symbols between branches");

                // Check if a branch terminates
                let ifTerminates = matches!(
                    flowAfterIf,
                    FlowState::Return | FlowState::Panic | FlowState::Throw | FlowState::Break | FlowState::Continue
                );
                let elseTerminates = match &flowAfterElse {
                    Some(flow) => matches!(
                        flow,
                        FlowState::Return | FlowState::Panic | FlowState::Throw | FlowState::Break | FlowState::Continue
                    ),
                    None => false,
                };

                // Used by irGen
                let alwaysTerminates = ifTerminates && elseTerminates;

                // Compare symbols between ifs to verify if all variables are assigned
                for (name, symbol) in self.symbolTypes.symbols.iter_mut() {
                    let inIf = symbolsAfterIf.isAssigned(name);
                    let inElse = match &symbolsAfterElse {
                        Some(elseSym) => elseSym.isAssigned(name),
                        None => false, // no else
                    };

                    symbol.isAssigned = match (ifTerminates, elseTerminates) {
                        // If one branches terminates check only the other
                        (true, false) => inElse,
                        (false, true) => inIf,
                        // Check both if no one terminates
                        (false, false) => inIf && inElse,
                        // Ignore if both terminate
                        (true, true) => true,
                    };
                }

                // Check if an IF terminates in all cases
                self.flowState = match (flowAfterIf, flowAfterElse) {
                    (FlowState::Return, Some(FlowState::Return)) => FlowState::Return,
                    (FlowState::Throw, Some(FlowState::Throw)) => FlowState::Throw,
                    (FlowState::Panic, Some(FlowState::Panic)) => FlowState::Panic,
                    _ => FlowState::Normal,
                };

                // empty symbols before if
                if isFirstIf {
                    self.symbolsBeforeIf = None;
                    println!("Emptying symbolsBeforeIf at the end");
                }

                return TypedStmt::If {
                    condition: typedCondition,
                    ifBlock: Box::new(typedIfBlock),
                    elseBranch: typedElseBranch,
                    alwaysTerminates: alwaysTerminates,
                    metadata: metadata.clone(),
                };
            }
            Stmt::Return {
                value: returnedVal,
                metadata,
            } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Return statement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                self.flowState = FlowState::Return;

                let retVarType: Option<(VarType, Metadata)>;
                let typedRetVal = match returnedVal {
                    Some(val) => {
                        let typedStmt = self.exprTyped(val);
                        retVarType = Some((typedStmt.typ(), metadata.clone()));
                        Some(typedStmt)
                    }
                    None => {
                        retVarType = None;
                        None
                    }
                };

                // To check after the type in functionDecl
                self.currReturnedType = retVarType;

                return TypedStmt::Return {
                    value: typedRetVal,
                    metadata: metadata.clone(),
                };
            }
            Stmt::While {
                condition,
                body,
                label,
                metadata,
            } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Return statement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                let typedCondition = self.exprTyped(condition);
                if ![VarType::Bool, VarType::CompileErr].contains(&typedCondition.typ()) {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Condition must be a bool"),
                        errType: String::from("Invalid while statement"),
                    });
                }

                self.loopLevel += 1;
                let typedBody = self.checkType(body);
                self.loopLevel -= 1;

                return TypedStmt::While {
                    condition: typedCondition,
                    body: Box::new(typedBody),
                    label: label.clone(),
                    metadata: metadata.clone(),
                };
            }
            Stmt::Break { label, metadata } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Break statement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                self.flowState = FlowState::Break;
                return TypedStmt::Break {
                    label: label.clone(),
                    metadata: metadata.clone(),
                };
            }
            Stmt::Continue { label, metadata } => {
                if !self.insideFn {
                    self.errors.push(CompileErr {
                        metadata: metadata.clone(),
                        msg: "Continue statement must be in a function".to_string(),
                        errType: "Unexpected item out of a function".to_string(),
                    });
                }

                self.flowState = FlowState::Continue;
                return TypedStmt::Continue {
                    label: label.clone(),
                    metadata: metadata.clone(),
                };
            }
            Stmt::FnDecl {
                name,
                parameters,
                body,
                fnType,
                metadata,
            } => {
                // Save previous return type
                let prevReturnedType = self.currReturnedType.clone();
                self.currReturnedType = None; //= fnType.clone();

                // Save previous flow state
                let prevFlowState = self.flowState.clone();
                self.flowState = FlowState::Normal;

                // Enter new Scope
                let parent = Some(self.symbolTypes.clone());
                let symbols = SymbolTypes::new(parent);
                self.symbolTypes = symbols;

                // Insert parameters in function scope
                for parameter in parameters {
                    match self.symbolTypes.insert(
                        parameter.name.as_str(),
                        SymbolType {
                            varType: parameter.typ.clone(),
                            isConstant: false,
                            isAssigned: true,
                            isBuiltIn: false,
                            isParam: true,
                        },
                    ) {
                        Ok(()) => {}
                        Err(_) => unreachable!(), // already checked in declarationPass
                    }
                }

                self.insideFn = true;
                // Check body Types
                let mut typedBlockStmts: Vec<TypedStmt> = Vec::new();
                match **body {
                    Stmt::Block {
                        ref statements,
                        ref metadata,
                    } => {
                        for untypedStmt in statements {
                            typedBlockStmts.push(self.checkType(&untypedStmt));
                        }
                    }
                    _ => unreachable!(),
                }
                self.insideFn = false;

                // Exist Scope
                match self.symbolTypes.parent.take() {
                    Some(parent) => self.symbolTypes = *parent,
                    None => {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: String::from("Cannot exit global scope"),
                            errType: String::from("Invalid block"),
                        });
                    }
                }

                // Check if return type is correct
                println!("returnType {:?}, ", fnType);
                match &fnType {
                    VarType::Void => match &self.currReturnedType {
                        Some((currType, returnMetadata)) => {
                            self.errors.push(CompileErr {
                                metadata: returnMetadata.clone(),
                                msg: String::from("Function must return nothing"),
                                errType: String::from("Invalid return type"),
                            });
                        }
                        None => {
                            // Void function without any "return"
                            if self.flowState != FlowState::Return {
                                // Add artificial return to void function
                                println!("Adding return at the end of {}", name);
                                typedBlockStmts.push(TypedStmt::Return {
                                    value: None,
                                    metadata: Metadata::empty(),
                                });
                            }
                        }
                    },
                    expectType => {
                        // Non void function
                        match &self.currReturnedType {
                            Some((currType, returnMetadata)) => {
                                if &currType != expectType && currType != &VarType::CompileErr {
                                    // println!("PRINTING RETURN ERROR METADATA");
                                    // returnMetadata.print();
                                    self.errors.push(CompileErr {
                                        metadata: returnMetadata.clone(),
                                        msg: format!("Expected to return {} but returns {}", expectType, currType),
                                        errType: String::from("Invalid return type"),
                                    });
                                }
                            }
                            None => {
                                self.errors.push(CompileErr {
                                    metadata: metadata.clone(),
                                    msg: format!("Expected to return {} but returns nothing", expectType),
                                    errType: String::from("Invalid return type"),
                                });
                                return TypedStmt::CompileErr;
                            }
                        }
                        // Check if every path returns
                        if self.flowState != FlowState::Return && self.flowState != FlowState::Panic {
                            self.errors.push(CompileErr {
                                metadata: metadata.clone(),
                                msg: format!("Function does not return a value on all code paths"),
                                errType: String::from("Invalid function declaration"),
                            });
                        }
                    }
                }

                self.currReturnedType = prevReturnedType;
                self.flowState = prevFlowState;

                // Check main function
                if name == "main" {
                    println!("FUNCTION MAIN");
                    if fnType != &VarType::Void {
                        self.errors.push(CompileErr {
                            msg: String::from("No return type expected"),
                            errType: String::from("Invalid \"main()\" function syntax"),
                            metadata: metadata.clone(),
                        });
                    }
                    if !parameters.is_empty() {
                        self.errors.push(CompileErr {
                            msg: String::from("There must be no parameter"),
                            errType: String::from("Invalid \"main()\" function syntax"),
                            metadata: metadata.clone(),
                        });
                    }

                    // is patched to be i32 and return 0 in IRGen (replaces the returns)
                }

                let typedBlock = match **body {
                    Stmt::Block {
                        ref statements,
                        ref metadata,
                    } => TypedStmt::Block {
                        statements: typedBlockStmts,
                        metadata: metadata.clone(),
                    },
                    _ => unreachable!(),
                };

                let fnIndex = match self.fnTable.getIndex(name) {
                    Ok(i) => i,
                    Err(e) => {
                        self.errors.push(CompileErr {
                            metadata: metadata.clone(),
                            msg: format!("Function index not found"),
                            errType: String::from("Unexpected compiler error"),
                        });
                        return TypedStmt::CompileErr;
                    }
                };

                return TypedStmt::FnDecl {
                    fnIndex: fnIndex,
                    parameters: parameters.clone(),
                    body: Box::new(typedBlock),
                    fnType: fnType.clone(),
                    metadata: metadata.clone(),
                };
            }
            Stmt::StandaloneFnCall { expr, metadata } => match self.exprTyped(expr) {
                TypedExpr::CompileErr => return TypedStmt::CompileErr,
                fnCall => {
                    return TypedStmt::StandaloneFnCall {
                        expr: fnCall,
                        metadata: metadata.clone(),
                    };
                }
            },
            Stmt::CompileErr => return TypedStmt::CompileErr,
        }
    }
}

fn validParamTyp(param: &VarType, arg: &VarType) -> bool {
    return param == arg || param == &VarType::All;
}
