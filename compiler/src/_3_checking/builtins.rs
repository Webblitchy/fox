use crate::_2_parsing::literal::Literal;
use crate::_2_parsing::untypedAST::{Expr, UntypedArgument};
use crate::_3_checking::typedAST::TypedExpr;
use crate::compileErr::CompileErr;
use crate::metadata::Metadata;
use crate::symbols::fnInfo::{FnIndex, FnInfo, FnKind, FnTable, ParamType};
use crate::{_3_checking::typeChecker::TypeChecker, symbols::builtins::BuiltinFn, vartype::VarType};

impl TypeChecker {
    pub fn insertBuiltinFn(&mut self) {
        // PRINT
        self.fnTable
            .insert(
                "builtin.printEmpty",
                FnInfo {
                    name: "builtin.printEmpty".to_string(),
                    typ: VarType::Void,
                    params: Vec::new(),
                    isThrowable: false,
                    panics: false,
                    kind: FnKind::Builtin(BuiltinFn::Print), // TODO: pas utilisé pour l'instant
                },
            )
            .unwrap();

        self.fnTable
            .insert(
                "builtin.printInt",
                FnInfo {
                    name: "builtin.printInt".to_string(),
                    typ: VarType::Void,
                    params: vec![ParamType {
                        name: String::from("content"),
                        typ: VarType::Int,
                    }],
                    panics: false,
                    isThrowable: false,
                    kind: FnKind::Builtin(BuiltinFn::Print),
                },
            )
            .unwrap();

        self.fnTable
            .insert(
                "builtin.printDec",
                FnInfo {
                    name: "builtin.printDec".to_string(),
                    typ: VarType::Void,
                    params: vec![ParamType {
                        name: String::from("content"),
                        typ: VarType::Dec,
                    }],
                    panics: false,
                    isThrowable: false,
                    kind: FnKind::Builtin(BuiltinFn::Print),
                },
            )
            .unwrap();

        self.fnTable
            .insert(
                "builtin.printBool",
                FnInfo {
                    name: "builtin.printBool".to_string(),
                    typ: VarType::Void,
                    params: vec![ParamType {
                        name: String::from("content"),
                        typ: VarType::Bool,
                    }],
                    panics: false,
                    isThrowable: false,
                    kind: FnKind::Builtin(BuiltinFn::Print),
                },
            )
            .unwrap();

        self.fnTable
            .insert(
                "builtin.printStr",
                FnInfo {
                    name: "builtin.printStr".to_string(),
                    typ: VarType::Void,
                    params: vec![
                        ParamType {
                            name: String::from("content"),
                            typ: VarType::Str,
                        },
                        ParamType {
                            name: String::from("len"),
                            typ: VarType::Int,
                        },
                    ],
                    panics: false,
                    isThrowable: false,
                    kind: FnKind::Builtin(BuiltinFn::Print),
                },
            )
            .unwrap();

        // builtin version needs the string length
        self.fnTable
            .insert(
                "builtin.panicWithMsg",
                FnInfo {
                    name: "builtin.panicWithMsg".to_string(),
                    typ: VarType::Void,
                    params: vec![
                        ParamType {
                            name: String::from("message"),
                            typ: VarType::Str,
                        },
                        ParamType {
                            name: String::from("len"),
                            typ: VarType::Int,
                        },
                    ],
                    panics: true,
                    isThrowable: false,
                    kind: FnKind::Builtin(BuiltinFn::Panic),
                },
            )
            .unwrap();

        self.fnTable
            .insert(
                "builtin.panicWithoutMsg",
                FnInfo {
                    name: "builtin.panicWithoutMsg".to_string(),
                    typ: VarType::Void,
                    params: vec![],
                    isThrowable: false,
                    panics: true,
                    kind: FnKind::Builtin(BuiltinFn::Panic),
                },
            )
            .unwrap();
    }

    pub fn callBuiltins(&mut self, fnName: &str, arguments: &mut Vec<UntypedArgument>, metadata: &Metadata) -> Result<Option<FnIndex>, ()> {
        let mut fnIndex = None;

        if fnName == "print" {
            let builtinName;
            if arguments.len() == 0 {
                builtinName = "builtin.printEmpty";
            } else {
                let typedArg = self.exprTyped(&arguments[0].data);
                let argType = typedArg.typ();
                builtinName = match argType {
                    VarType::Int => "builtin.printInt",
                    VarType::Dec => "builtin.printDec",
                    VarType::Bool => "builtin.printBool",
                    VarType::Str => {
                        self.addStrLenArg(typedArg, arguments, metadata)?;
                        "builtin.printStr"
                    }
                    _ => {
                        self.errors.push(CompileErr {
                            msg: format!("No print function for type {:?}", argType),
                            errType: "Invalid builtin call".to_string(),
                            metadata: metadata.clone(),
                        });
                        return Err(());
                    }
                };
            }

            fnIndex = Some(self.fnTable.getIndex(builtinName).unwrap());
        } else if fnName == "panic" {
            let mut hasError = false;
            if arguments.len() == 0 {
                fnIndex = Some(self.fnTable.getIndex("builtin.panicWithoutMsg").unwrap());
            } else if arguments.len() == 1 {
                let typedArg = self.exprTyped(&arguments[0].data);
                if typedArg.typ() != VarType::Str {
                    hasError = true
                } else {
                    self.addStrLenArg(typedArg, arguments, metadata)?;
                    fnIndex = Some(self.fnTable.getIndex("builtin.panicWithMsg").unwrap());
                }
            } else {
                hasError = true;
            }
            if hasError {
                self.errors.push(CompileErr {
                    msg: "panic() function takes an Str or nothing".to_string(),
                    errType: "Invalid builtin call".to_string(),
                    metadata: metadata.clone(),
                });
                return Err(());
            }
        }

        return Ok(fnIndex);
    }

    fn addStrLenArg(&mut self, strArg: TypedExpr, arguments: &mut Vec<UntypedArgument>, metadata: &Metadata) -> Result<(), ()> {
        let len: usize = match strArg {
            TypedExpr::Literal { value, metadata, typ } => match value {
                Literal::Str(s) => s.len(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        let lenAsInt = match i64::try_from(len) {
            Ok(v) => v,
            Err(_) => {
                self.errors.push(CompileErr {
                    msg: "Str too long".to_string(),
                    errType: "Invalid call to builtin function".to_string(),
                    metadata: metadata.clone(),
                });
                return Err(());
            }
        };

        arguments.push(UntypedArgument {
            data: Expr::Literal {
                value: Literal::Int(lenAsInt),
                metadata: Metadata::empty(),
            },
            metadata: Metadata::empty(),
            name: Some("len".to_string()),
        });

        return Ok(());
    }
}
