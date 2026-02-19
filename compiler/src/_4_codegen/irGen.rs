use crate::{
    compileErr::CompileErr,
    literal::Literal,
    metadata::{self, Metadata},
    symbolValues::Symbols,
    symbols::fnInfo::{FnIndex, FnTable},
    token::TokenType,
    typedAST::{TypedExpr, TypedStmt},
    untypedAST::Expr,
    vartype::VarType,
};
use inkwell::{
    AddressSpace,
    intrinsics::Intrinsic,
    types::{FunctionType, PointerType},
    values::{AnyValueEnum, CallSiteValue, FunctionValue},
};
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::{
    basic_block::BasicBlock,
    types::{BasicTypeEnum, FloatType, IntType, StructType},
    values::BasicMetadataValueEnum,
};
use inkwell::{builder::Builder, module::Module};
use inkwell::{context::Context, types::BasicMetadataTypeEnum};
use inkwell::{
    types::VoidType,
    values::{BasicValueEnum, IntValue, PointerValue},
};
use std::collections::HashMap;

// To keep track of start/end of loops for break/continue
#[derive(Debug, Clone)]
pub struct LoopContext<'ctx> {
    pub breakTarget: BasicBlock<'ctx>,
    pub continueTarget: BasicBlock<'ctx>,
}

pub struct IrGen<'ctx, 'ft> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    pub symbols: Symbols<'ctx>,
    llvmFns: Vec<FunctionValue<'ctx>>,
    fnTable: &'ft FnTable, // fn infos
    loopStack: Vec<LoopContext<'ctx>>,
    labelTable: HashMap<String, LoopContext<'ctx>>,
    currentFn: Option<FnIndex>,

    // Types for new variables
    pub i1: IntType<'ctx>,      // bool
    pub i8: IntType<'ctx>,      // byte
    pub i64: IntType<'ctx>,     // int
    pub f64: FloatType<'ctx>,   // dec
    pub void: VoidType<'ctx>,   // function returning nothing
    pub ptr: PointerType<'ctx>, // TODO: Keep? Ptr used ?
}

impl<'ctx, 'ft> IrGen<'ctx, 'ft> {
    pub fn new(context: &'ctx Context, moduleName: &str, fnTable: &'ft FnTable) -> IrGen<'ctx, 'ft> {
        return IrGen {
            context: context,
            module: context.create_module(moduleName),
            builder: context.create_builder(),
            symbols: Symbols::new(None),
            llvmFns: Vec::new(),
            fnTable: fnTable,
            loopStack: Vec::new(),
            labelTable: HashMap::new(),
            currentFn: None,

            // For variable types
            i1: context.bool_type(),
            i8: context.i8_type(),
            i64: context.i64_type(),
            f64: context.f64_type(),
            void: context.void_type(),
            ptr: context.ptr_type(AddressSpace::default()),
        };
    }

    pub fn declareFnSignatures(&mut self) -> Result<(), CompileErr> {
        for (index, fnInfo) in self.fnTable.functions.iter().enumerate() {
            let fnIndex = FnIndex(index);

            // Creating function signature LLVM
            let mut paramTypesIW: Vec<BasicMetadataTypeEnum> = Vec::new();
            for parameter in &fnInfo.params {
                match self.getIWType(&parameter.typ) {
                    Ok(iwType) => paramTypesIW.push(iwType.into()),
                    Err((msg, errType)) => {
                        return Err(CompileErr {
                            msg,
                            errType,
                            metadata: Metadata::empty(),
                        });
                    }
                }
            }

            let paramTypesIW = paramTypesIW.as_slice();

            let fnName = self.fnTable.getInfo(&fnIndex).name;

            let fnSignature: FunctionType;
            if fnName == "main" {
                fnSignature = self.context.i32_type().fn_type(&[], false);
            } else {
                fnSignature = match &fnInfo.typ {
                    VarType::Void => self.context.void_type().fn_type(paramTypesIW, false),
                    typ => {
                        // non void functions
                        let iwType = match self.getIWType(&typ) {
                            Ok(iwType) => iwType,
                            Err((msg, errType)) => {
                                return Err(CompileErr {
                                    msg,
                                    errType,
                                    metadata: Metadata::empty(),
                                });
                            }
                        };

                        match iwType {
                            BasicTypeEnum::IntType(t) => t.fn_type(paramTypesIW, false),
                            BasicTypeEnum::FloatType(t) => t.fn_type(paramTypesIW, false),
                            BasicTypeEnum::PointerType(t) => t.fn_type(paramTypesIW, false),
                            BasicTypeEnum::StructType(t) => t.fn_type(paramTypesIW, false),
                            BasicTypeEnum::VectorType(t) => t.fn_type(paramTypesIW, false),
                            _ => unreachable!(),
                        }
                    }
                };
            }

            // Adding function to module
            let function: FunctionValue = self.module.add_function(&fnName, fnSignature, None);
            self.llvmFns.push(function); // adding in the same order as the fnTable
        }
        return Ok(());
    }

    pub fn genStatement(&mut self, typedStmt: &TypedStmt) -> Result<(), CompileErr> {
        // Function already returned: ignore after
        if self.currentFn != None && self.hasTerminator() {
            // TODO: Make a real warning
            println!("Warning ! Code from there is ignored until end of function");
            typedStmt.metadata().print();
            return Ok(());
        }

        match &typedStmt {
            TypedStmt::ExprStmt { expr, metadata } => {
                self.genExpr(expr)?;
                return Ok(());
            }
            TypedStmt::VarDecl {
                name,
                expr,
                typ,
                isConstant,
                metadata,
            } => {
                let val = match expr {
                    Some(expr) => Some(self.genExpr(expr)?),
                    None => None,
                };

                self.storeInStack(name, typ.clone(), val, metadata.clone())?;

                return Ok(());
            }
            TypedStmt::VarAssign { name, expr, metadata } => {
                let val = self.genExpr(expr)?;
                let typ = expr.typ();
                self.modifyInStack(name, typ, val, metadata.clone())?;

                return Ok(());
            }
            TypedStmt::Block {
                statements: blockStmts,
                metadata,
            } => {
                // Enter a new scope
                let parent = Some(self.symbols.clone());
                self.symbols = Symbols::new(parent); // current symbols become parent

                for stmt in blockStmts {
                    self.genStatement(stmt)?;
                }

                // Exit scope: restore symbols
                // take() : take ownership
                match self.symbols.parent.take() {
                    Some(parent) => {
                        self.symbols = *parent;
                    }
                    None => {
                        return Err(CompileErr {
                            metadata: metadata.clone(),
                            msg: String::from("Cannot exit global scope"),
                            errType: String::from("Unexpected compiler error"),
                        });
                    }
                }

                return Ok(());
            }
            TypedStmt::If {
                condition,
                ifBlock,
                elseBranch,
                alwaysTerminates,
                metadata,
            } => {
                // TODO: handle the unwraps
                let currFn = self.builder.get_insert_block().unwrap().get_parent().unwrap();

                // Create blocks
                let ifBlockLLVM = self.context.append_basic_block(currFn, "if.true");
                let elseBlockLLVM = self.context.append_basic_block(currFn, "if.else");

                // branch check
                // Compute condition
                let conditionVal = self.genExpr(condition)?.into_int_value();
                let res = self.builder.build_conditional_branch(conditionVal, ifBlockLLVM, elseBlockLLVM);
                if res.is_err() {
                    return Err(CompileErr {
                        metadata: metadata.clone(),
                        msg: String::from("Unable to build conditionnal branch"),
                        errType: String::from("Unexpected compiler error"),
                    });
                }

                // ---- ifBlock: if == true
                self.builder.position_at_end(ifBlockLLVM);

                self.genStatement(ifBlock)?; // compute block instructions

                // if there is a "return", "break", "raise"...
                let ifHasTerminator = self.hasTerminator();

                // ---- elseBlock: if == false
                self.builder.position_at_end(elseBlockLLVM);

                match elseBranch {
                    // for If (else if) and Block (else)
                    Some(elseBranchVal) => self.genStatement(&**elseBranchVal)?,
                    // no else
                    None => {}
                }

                let elseHasTerminator = self.hasTerminator();

                // every if not every path returns / break (in loop) / raise
                if !alwaysTerminates {
                    let afterIfLLVM = self.context.append_basic_block(currFn, "if.after");

                    if !ifHasTerminator {
                        self.builder.position_at_end(ifBlockLLVM);
                        self.builder.build_unconditional_branch(afterIfLLVM).unwrap();
                    }
                    if !elseHasTerminator {
                        self.builder.position_at_end(elseBlockLLVM);
                        self.builder.build_unconditional_branch(afterIfLLVM).unwrap();
                    }

                    // ---- Move to end
                    self.builder.position_at_end(afterIfLLVM);
                }

                return Ok(());
            }
            TypedStmt::While {
                condition,
                body,
                label,
                metadata,
            } => {
                // TODO: store label in a labels

                // TODO: handle the unwraps
                let currFn = self.builder.get_insert_block().unwrap().get_parent().unwrap();

                // Create blocks
                let conditionBlocLlvm = self.context.append_basic_block(currFn, "while.condition");
                let whileBodyLlvm = self.context.append_basic_block(currFn, "while.body");
                let afterWhileLlvm = self.context.append_basic_block(currFn, "while.after");

                // Jump to Condition
                self.builder.build_unconditional_branch(conditionBlocLlvm).unwrap();

                // Condition bloc
                self.builder.position_at_end(conditionBlocLlvm); // Go to the end of the bloc
                let conditionVal = self.genExpr(condition)?.into_int_value(); // Add generated code for the condition
                self.builder
                    .build_conditional_branch(conditionVal, whileBodyLlvm, afterWhileLlvm)
                    .unwrap(); // If condition false go after whileBody

                // ---- while body
                self.builder.position_at_end(whileBodyLlvm);

                let loopCtx = LoopContext {
                    breakTarget: afterWhileLlvm,
                    continueTarget: conditionBlocLlvm,
                };

                self.loopStack.push(loopCtx.clone()); // for simple break/continue
                // for labelised break/continue
                if let Some(name) = label {
                    self.labelTable.insert(name.clone(), loopCtx);
                }

                // compute block instructions
                self.genStatement(body)?;

                self.loopStack.pop();
                if let Some(name) = label {
                    self.labelTable.remove(name);
                }

                // Reloop
                // if there is no "return", "break", "raise"...
                if !self.hasTerminator() {
                    // restart loop
                    self.builder.build_unconditional_branch(conditionBlocLlvm).unwrap();
                }

                // ---- after, move the builder at the end
                self.builder.position_at_end(afterWhileLlvm);

                return Ok(());
            }
            TypedStmt::Break { label, metadata } | TypedStmt::Continue { label, metadata } => {
                let targetJump;
                match label {
                    Some(label) => match self.labelTable.get(label) {
                        Some(labelisedLoop) => {
                            targetJump = match typedStmt {
                                TypedStmt::Break { label, metadata } => labelisedLoop.breakTarget,
                                TypedStmt::Continue { label, metadata } => labelisedLoop.continueTarget,
                                _ => unreachable!(),
                            }
                        }
                        None => {
                            let stmtLength = match typedStmt {
                                TypedStmt::Break { label, metadata } => 5,
                                TypedStmt::Continue { label, metadata } => 8,
                                _ => unreachable!(),
                            };

                            return Err(CompileErr {
                                metadata: Metadata {
                                    line: metadata.line,
                                    column: metadata.column + stmtLength + 1,
                                    start: metadata.start + stmtLength + 1,
                                    end: metadata.end,
                                },
                                msg: String::from("Undefined label"),
                                errType: String::from("Invalid loop label"),
                            });
                        }
                    },
                    None => match self.loopStack.last() {
                        Some(outerLoop) => {
                            targetJump = match typedStmt {
                                TypedStmt::Break { label, metadata } => outerLoop.breakTarget,
                                TypedStmt::Continue { label, metadata } => outerLoop.continueTarget,
                                _ => unreachable!(),
                            }
                        }
                        None => {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Cannot break outside a loop"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                    },
                }

                self.builder.build_unconditional_branch(targetJump).unwrap();
                Ok(())
            }
            TypedStmt::Return {
                value: returnValue,
                metadata,
            } => {
                let fnInfo = match self.currentFn {
                    Some(currFn) => self.fnTable.getInfo(&currFn),
                    None => {
                        return Err(CompileErr {
                            errType: "Unexpected compiler error".to_string(),
                            msg: "Return outside a function".to_string(),
                            metadata: metadata.clone(),
                        });
                    }
                };
                println!("BUILDING RETURN: {:?} in {}", returnValue, fnInfo.name);

                // llvm main returns 0
                if fnInfo.name == "main" {
                    // Replace all "return void" in main by "return 0"
                    let zero = self.context.i32_type().const_int(0, false);
                    self.builder.build_return(Some(&zero)).unwrap();
                    return Ok(());
                }

                match returnValue {
                    Some(expr) => {
                        let llvmAnyValue = self.genExpr(expr)?;
                        let llvmBasicValue = match self.anyToBasicEnum(llvmAnyValue) {
                            Ok(basicVal) => basicVal,
                            Err((msg, errType)) => {
                                return Err(CompileErr {
                                    metadata: metadata.clone(),
                                    msg,
                                    errType,
                                });
                            }
                        };

                        match self.builder.build_return(Some(&llvmBasicValue)) {
                            Ok(_) => {}
                            Err(_) => {
                                return Err(CompileErr {
                                    errType: "Unexpected compiler error".to_string(),
                                    msg: format!("Failed to build a return of type {}", expr.typ()),
                                    metadata: metadata.clone(),
                                });
                            }
                        }
                    }
                    None => match self.builder.build_return(None) {
                        Ok(_) => {}
                        Err(_) => {
                            return Err(CompileErr {
                                errType: "Unexpected compiler error".to_string(),
                                msg: "Failed to build a return void".to_string(),
                                metadata: metadata.clone(),
                            });
                        }
                    },
                }

                return Ok(());
            }

            TypedStmt::FnDecl {
                fnIndex,
                parameters,
                body,
                fnType,
                metadata,
            } => {
                // Enter a new scope
                let parent = Some(self.symbols.clone());
                self.symbols = Symbols::new(parent); // current symbols become parent

                // Function already added to table during declarationPass
                let function = self.llvmFns[fnIndex.0];

                // Create entry block
                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                // Add parameters to symbols (and allocate)
                for (i, parameter) in parameters.iter().enumerate() {
                    let paramVal = function.get_nth_param(i as u32).unwrap();
                    self.storeInStack(parameter.name.as_str(), parameter.typ.clone(), Some(paramVal.into()), metadata.clone())?;
                }

                self.currentFn = Some(fnIndex.clone());

                // Handle body (manually because of scope with arguments
                match **body {
                    TypedStmt::Block {
                        ref statements,
                        ref metadata,
                    } => {
                        for stmt in statements {
                            self.genStatement(&stmt)?;
                        }
                    }
                    _ => unreachable!(),
                }

                // Insert final return if missing
                if !self.hasTerminator() {
                    match fnType {
                        VarType::Void => {
                            // println!("Inserting another return void in {}", self.fnTable.getInfo(fnIndex).name);
                            if self.fnTable.getInfo(fnIndex).name == "main" {
                                // In AST main() is still a void fn
                                let _ = self.builder.build_return(Some(&self.context.i32_type().const_int(0, false))).unwrap();
                            } else {
                                let _ = self.builder.build_return(None).unwrap();
                            }
                        }
                        _ => {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Non-void function missing return on some path"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                    }
                }

                // Exit scope: restore symbols
                // take() : take ownership
                match self.symbols.parent.take() {
                    Some(parent) => {
                        self.symbols = *parent;
                    }
                    None => {
                        return Err(CompileErr {
                            metadata: metadata.clone(),
                            msg: String::from("Cannot exit global scope"),
                            errType: String::from("Unexpected compiler error"),
                        });
                    }
                }

                self.currentFn = None;

                return Ok(());
            }
            TypedStmt::StandaloneFnCall { expr, metadata } => match expr {
                TypedExpr::FnCall {
                    fnIndex,
                    metadata,
                    arguments,
                    typ,
                } => {
                    self.genFnCall(*fnIndex, arguments.clone(), metadata.clone())?;
                    return Ok(());
                }
                _ => unreachable!("StandaloneFnCall must be a FnCall"),
            },
            TypedStmt::CompileErr { .. } => unreachable!(),
        }
    }

    pub fn genExpr(&mut self, typedExpr: &TypedExpr) -> Result<AnyValueEnum<'ctx>, CompileErr> {
        match typedExpr {
            TypedExpr::Grouping { expr, metadata, typ } => self.genExpr(expr),
            TypedExpr::Literal { value, metadata, typ } => match value {
                Literal::Int(innerValue) => Ok(AnyValueEnum::IntValue(
                    self.i64.const_int(innerValue.clone() as u64, true), // true = is signed
                )),
                Literal::Byte(innerValue) => Ok(AnyValueEnum::IntValue(
                    self.i8.const_int(innerValue.clone() as u64, false), // false = is unsigned
                )),
                Literal::Dec(value) => Ok(AnyValueEnum::FloatValue(self.f64.const_float(value.clone()))),
                Literal::Bool(value) => {
                    let boolVal;
                    if *value == true {
                        boolVal = 1;
                    } else {
                        boolVal = 0;
                    }
                    return Ok(AnyValueEnum::IntValue(self.i1.const_int(boolVal, false)));
                }
                Literal::Str(value) => {
                    return Ok(AnyValueEnum::PointerValue(self.createStrLiteral(
                        value,
                        "literalStr",
                        metadata.clone(),
                    )?));
                }
                Literal::Null => {
                    unimplemented!();
                }
                Literal::Identifier(name) => return self.loadFromStack(name, metadata.clone()),
            },
            TypedExpr::UnaryExpr {
                operator,
                right,
                metadata,
                typ,
            } => {
                let exprValue = self.genExpr(right)?;

                match typ {
                    VarType::Int => {
                        let exprInt = match exprValue {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };
                        match operator.tokenType {
                            TokenType::Minus => {
                                // = 0 - val
                                let zero = self.i64.const_int(0, true);
                                let negativeInt = self.builder.build_int_sub(zero, exprInt, "negativeInt");
                                match negativeInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build negative int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            /*
                            TokenType::TwoPlus => {
                                let incrementedInt = self.builder.build_int_add(
                                    self.context.i64_type().const_int(1, true),
                                    exprInt,
                                    "incrementedInt",
                                );
                                match incrementedInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err((
                                            metadata.clone(),
                                            String::from("Unable to build incremented int"),
                                        ));
                                    }
                                }
                            }
                            */
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid unary operator"),
                                });
                            }
                        }
                    }
                    VarType::Byte => {
                        return Err(CompileErr {
                            metadata: operator.metadata.clone(),
                            msg: String::from("Unexpected token"),
                            errType: String::from("Invalid unary operator"),
                        });
                    }
                    VarType::Dec => {
                        let exprDec = match exprValue {
                            AnyValueEnum::FloatValue(v) => v,
                            _ => unreachable!(),
                        };
                        match operator.tokenType {
                            TokenType::Minus => {
                                // = 0.0 - val
                                let zero = self.f64.const_float(0.0);
                                let negativeDouble = self.builder.build_float_sub(exprDec, zero, "negativeDec");
                                match negativeDouble {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build negative dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid unary operator"),
                                });
                            }
                        }
                    }
                    VarType::Bool => {
                        let exprInt = match exprValue {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };

                        match operator.tokenType {
                            TokenType::Bang => {
                                let invertedBool = self.builder.build_not(exprInt, "invertedBool");
                                match invertedBool {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build negated bool"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid unary operator"),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(CompileErr {
                            metadata: operator.metadata.clone(),
                            msg: String::from("Unexpected token"),
                            errType: String::from("Invalid unary operator"),
                        });
                    }
                }
            }
            TypedExpr::BinaryExpr {
                left,
                operator,
                right,
                metadata,
                typ,
            } => {
                let leftExpr = self.genExpr(left)?;
                let rightExpr = self.genExpr(right)?;

                match typ {
                    VarType::Int => {
                        let leftInt = match leftExpr {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };
                        let rightInt = match rightExpr {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };

                        let currFn = self.builder.get_insert_block().unwrap().get_parent().unwrap();

                        match operator.tokenType {
                            TokenType::Plus => {
                                let sadd = Intrinsic::find("llvm.sadd.with.overflow").unwrap(); // LLVM Intrinsic function for panic on overflow
                                let addIntFn = sadd.get_declaration(&self.module, &[self.i64.into()]).unwrap();

                                let additionedInt = self
                                    .builder
                                    .build_call(addIntFn, &[leftInt.into(), rightInt.into()], "addIntFn")
                                    .unwrap()
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_struct_value();

                                let hasOverflow = self
                                    .builder
                                    .build_extract_value(additionedInt, 1, "addOverflow")
                                    .unwrap()
                                    .into_int_value();

                                let continueBlock = self.context.append_basic_block(currFn, "continueBlock");
                                let panicBlock = self.context.append_basic_block(currFn, "panicBlock");

                                self.builder.build_conditional_branch(hasOverflow, panicBlock, continueBlock).unwrap();

                                // bloc panic
                                self.builder.position_at_end(panicBlock);
                                self.callPanicOnError(&format!("Int overflow on line {}", metadata.line));

                                // bloc continue
                                self.builder.position_at_end(continueBlock);

                                match self.builder.build_extract_value(additionedInt, 0, "addValue") {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build addition of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::PlusPercent => {
                                // Wrap around by default in LLVM
                                let additionedWrappedInt =
                                    self.builder
                                        .build_int_add(leftExpr.into_int_value(), rightExpr.into_int_value(), "additionedWrappedInt");

                                match additionedWrappedInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build wrap arround addition of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Minus => {
                                let subtractionInt = self.builder.build_int_sub(leftInt, rightInt, "subtractionInt");
                                match subtractionInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build subtraction of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Asterisk => {
                                let mutliplicationInt = self.builder.build_int_mul(leftInt, rightInt, "mutiplicationInt");
                                match mutliplicationInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build multiplication of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Slash => {
                                // TODO:
                                // Verify division by 0
                                // let isZero = match self
                                //     .builder
                                //     .build_int_compare(IntPredicate::EQ, rightInt, self.i64.const_int(0, false), "isZero")
                                // {
                                //     Ok(isZ) => isZ,
                                //     Err(_) => {
                                //         return Err(CompileErr {
                                //             metadata: metadata.clone(),
                                //             msg: String::from("Unable to compare int to 0"),
                                //             errType: String::from("Unexpected compiler error"),
                                //         });
                                //     }
                                // };
                                //
                                // let currFn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                                // let ok_block = self.context.append_basic_block(currFn, "div_ok");
                                // let error_block = self.context.append_basic_block(currFn, "div_by_zero");
                                //
                                // self.builder.build_conditional_branch(isZero, error_block, ok_block);
                                // self.builder.position_at_end(ok_block);
                                let divisionInt = self.builder.build_int_signed_div(leftInt, rightInt, "divisionInt");

                                // builder.position_at_end(error_block);
                                // builder.build_call(panic_fn, &[], "panic");
                                // builder.build_unreachable();

                                match divisionInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build division of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Percent => {
                                let moduloInt = self.builder.build_int_signed_rem(leftInt, rightInt, "moduloInt");
                                match moduloInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build modulo of int"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid binary operator"),
                                });
                            }
                        }
                    }

                    VarType::Byte => {
                        let leftInt = match leftExpr {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };
                        let rightInt = match rightExpr {
                            AnyValueEnum::IntValue(v) => v,
                            _ => unreachable!(),
                        };

                        match operator.tokenType {
                            TokenType::Plus => {
                                let additionedInt = self
                                    .builder
                                    .build_int_add(leftExpr.into_int_value(), rightExpr.into_int_value(), "additionInt");
                                match additionedInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build addition of Byte"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Minus => {
                                let subtractionInt = self.builder.build_int_sub(leftInt, rightInt, "subtractionInt");
                                match subtractionInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build subtraction of Byte"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Asterisk => {
                                let mutliplicationInt = self.builder.build_int_mul(leftInt, rightInt, "mutiplicationInt");
                                match mutliplicationInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build multiplication of Byte"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Slash => {
                                // TODO:
                                let divisionInt = self.builder.build_int_unsigned_div(leftInt, rightInt, "divisionInt");
                                match divisionInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build division of Byte"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Percent => {
                                let moduloInt = self.builder.build_int_unsigned_rem(leftInt, rightInt, "moduloInt");
                                match moduloInt {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build modulo of Byte"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid binary operator"),
                                });
                            }
                        }
                    }
                    VarType::Dec => {
                        let leftDec = match leftExpr {
                            AnyValueEnum::FloatValue(v) => v,
                            _ => unreachable!(),
                        };
                        let rightDec = match rightExpr {
                            AnyValueEnum::FloatValue(v) => v,
                            _ => unreachable!(),
                        };

                        match operator.tokenType {
                            TokenType::Plus => {
                                let additionnedDec = self.builder.build_float_add(leftDec, rightDec, "additionDec");
                                match additionnedDec {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build addition of dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Minus => {
                                let subtractionDec = self.builder.build_float_sub(leftDec, rightDec, "subtractionDec");
                                match subtractionDec {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build subtraction of dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Asterisk => {
                                let multiplicationDec = self.builder.build_float_mul(leftDec, rightDec, "mutliplicationDec");
                                match multiplicationDec {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build multiplication of dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Slash => {
                                let divisionDec = self.builder.build_float_div(leftDec, rightDec, "divisionDec");
                                match divisionDec {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build division of dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            TokenType::Percent => {
                                let moduloDec = self.builder.build_float_rem(leftDec, rightDec, "moduloDec");
                                match moduloDec {
                                    Ok(val) => return Ok(val.into()),
                                    Err(_) => {
                                        return Err(CompileErr {
                                            metadata: metadata.clone(),
                                            msg: String::from("Unable to build modulo of dec"),
                                            errType: String::from("Unexpected compiler error"),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(CompileErr {
                                    metadata: operator.metadata.clone(),
                                    msg: String::from("Unexpected token"),
                                    errType: String::from("Invalid binary operator"),
                                });
                            }
                        }
                    }
                    VarType::Bool => match (*left).typ() {
                        // Bool expressions can contain many types (ex: int < int)
                        VarType::Int => {
                            let intPredicate = match operator.tokenType {
                                TokenType::TwoEquals => IntPredicate::EQ,
                                TokenType::BangEqual => IntPredicate::NE,
                                TokenType::Less => IntPredicate::SLT,
                                TokenType::LessEqual => IntPredicate::SLE,
                                TokenType::Greater => IntPredicate::SGT,
                                TokenType::GreaterEqual => IntPredicate::SGE,
                                _ => {
                                    return Err(CompileErr {
                                        metadata: operator.metadata.clone(),
                                        msg: String::from("Unexpected token"),
                                        errType: String::from("Invalid binary operator"),
                                    });
                                }
                            };
                            let intComparison =
                                self.builder
                                    .build_int_compare(intPredicate, leftExpr.into_int_value(), rightExpr.into_int_value(), "equalityInt");
                            match intComparison {
                                Ok(val) => return Ok(val.into()),
                                Err(_) => {
                                    return Err(CompileErr {
                                        metadata: metadata.clone(),
                                        msg: String::from("Unable to build int comparison"),
                                        errType: String::from("Unexpected compiler error"),
                                    });
                                }
                            }
                        }
                        VarType::Dec => {
                            let decPredicate = match operator.tokenType {
                                TokenType::TwoEquals => FloatPredicate::OEQ,
                                TokenType::BangEqual => FloatPredicate::ONE,
                                TokenType::Less => FloatPredicate::OLT,
                                TokenType::LessEqual => FloatPredicate::OLE,
                                TokenType::Greater => FloatPredicate::OGT,
                                TokenType::GreaterEqual => FloatPredicate::OGE,
                                _ => {
                                    return Err(CompileErr {
                                        metadata: operator.metadata.clone(),
                                        msg: String::from("Unexpected token"),
                                        errType: String::from("Invalid binary operator"),
                                    });
                                }
                            };
                            let decComparison = self.builder.build_float_compare(
                                decPredicate,
                                leftExpr.into_float_value(),
                                rightExpr.into_float_value(),
                                "equalityInt",
                            );
                            match decComparison {
                                Ok(val) => return Ok(val.into()),
                                Err(_) => {
                                    return Err(CompileErr {
                                        metadata: metadata.clone(),
                                        msg: String::from("Unable to build dec comparison"),
                                        errType: String::from("Unexpected compiler error"),
                                    });
                                }
                            }
                        }
                        VarType::Bool => {
                            let boolPredicate = match operator.tokenType {
                                TokenType::TwoEquals => IntPredicate::EQ,
                                TokenType::BangEqual => IntPredicate::NE,
                                _ => {
                                    return Err(CompileErr {
                                        metadata: operator.metadata.clone(),
                                        msg: String::from("Unexpected token"),
                                        errType: String::from("Invalid binary operator"),
                                    });
                                }
                            };
                            let boolComparison =
                                self.builder
                                    .build_int_compare(boolPredicate, leftExpr.into_int_value(), rightExpr.into_int_value(), "equalityInt");
                            match boolComparison {
                                Ok(val) => return Ok(val.into()),
                                Err(_) => {
                                    return Err(CompileErr {
                                        metadata: metadata.clone(),
                                        msg: String::from("Unable to build bool comparison"),
                                        errType: String::from("Unexpected compiler error"),
                                    });
                                }
                            }
                        }
                        _ => {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: format!("{} types cannot be compared", (*left).typ()),
                                errType: String::from("Invalid comparison type"),
                            });
                        }
                    },
                    _ => {
                        return Err(CompileErr {
                            metadata: operator.metadata.clone(),
                            msg: String::from("Unexpected token"),
                            errType: String::from("Invalid binary operator"),
                        });
                    }
                }
            }

            TypedExpr::LogicalExpr {
                left,
                operator,
                right,
                metadata,
                typ,
            } => {
                // LLVM replaces AND / OR with IF / ELSE

                let phi; // to store value after different block paths

                // Compute left
                let leftVal = self.genExpr(left)?.into_int_value();

                // Get current Fn
                let currFn = self.builder.get_insert_block().unwrap().get_parent().unwrap();

                match operator.tokenType {
                    TokenType::TwoAmpersands => {
                        // prepare blocks
                        let ifBlock = self.context.append_basic_block(currFn, "and.if");
                        let elseBlock = self.context.append_basic_block(currFn, "and.else");
                        let mergeBlock = self.context.append_basic_block(currFn, "and.merge");

                        // branch check leftVal
                        let res = self.builder.build_conditional_branch(leftVal, ifBlock, elseBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build conditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }

                        // ---- ifBlock: a == true
                        self.builder.position_at_end(ifBlock);
                        let rightVal = self.genExpr(right)?.into_int_value();
                        let res = self.builder.build_unconditional_branch(mergeBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build unconditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                        let ifBlockEnd = self.builder.get_insert_block().unwrap();

                        // ---- elseBlock: a == false
                        self.builder.position_at_end(elseBlock);
                        let falseVal = self.i1.const_int(0, false);
                        let res = self.builder.build_unconditional_branch(mergeBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build unconditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                        let elseBlockEnd = self.builder.get_insert_block().unwrap();

                        // ---- mergeBlock
                        self.builder.position_at_end(mergeBlock);
                        phi = self.builder.build_phi(self.i1, "and.result").unwrap();
                        phi.add_incoming(&[(&rightVal, ifBlockEnd), (&falseVal, elseBlockEnd)]);
                    }
                    TokenType::TwoPipes => {
                        // Create blocks
                        let ifBlock = self.context.append_basic_block(currFn, "or.if");
                        let elseBlock = self.context.append_basic_block(currFn, "or.else");
                        let mergeBlock = self.context.append_basic_block(currFn, "or.merge");

                        // branch check leftVal
                        let res = self.builder.build_conditional_branch(leftVal, ifBlock, elseBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build conditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }

                        // ---- ifBlock: a == true
                        self.builder.position_at_end(ifBlock);
                        let trueVal = self.i1.const_int(1, false);
                        let res = self.builder.build_unconditional_branch(mergeBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build unconditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                        let ifBlockEnd = self.builder.get_insert_block().unwrap();

                        // ---- elseBlock: a == false
                        self.builder.position_at_end(elseBlock);
                        let rightVal = self.genExpr(right)?.into_int_value();
                        let res = self.builder.build_unconditional_branch(mergeBlock);
                        if res.is_err() {
                            return Err(CompileErr {
                                metadata: metadata.clone(),
                                msg: String::from("Unable to build unconditionnal branch"),
                                errType: String::from("Unexpected compiler error"),
                            });
                        }
                        let elseBlockEnd = self.builder.get_insert_block().unwrap();

                        // ---- mergeBlock
                        self.builder.position_at_end(mergeBlock);
                        phi = self.builder.build_phi(self.i1, "or.result").unwrap();
                        phi.add_incoming(&[(&trueVal, ifBlockEnd), (&rightVal, elseBlockEnd)]);
                    }
                    _ => unreachable!(),
                }

                let resultInt = phi.as_basic_value().into_int_value();
                return Ok(resultInt.into());
            }

            TypedExpr::FnCall {
                fnIndex,
                arguments,
                metadata,
                typ,
            } => {
                let retValue = match self.genFnCall(fnIndex.clone(), arguments.clone(), metadata.clone()) {
                    Ok(val) => val,
                    Err(compileErr) => return Err(compileErr),
                };

                // fnCall as expression cannot be a void fn
                let result = match retValue.try_as_basic_value().left() {
                    Some(res) => res,
                    None => {
                        return Err(CompileErr {
                            errType: "Unexpected compile error".to_string(),
                            msg: "Unable to get returned value".to_string(),
                            metadata: metadata.clone(),
                        });
                    }
                };
                return Ok(result.into());
            }
            TypedExpr::CompileErr { .. } => unreachable!(),
        }
    }

    fn genFnCall(&mut self, fnIndex: FnIndex, arguments: Vec<TypedExpr>, metadata: Metadata) -> Result<CallSiteValue<'ctx>, CompileErr> {
        let mut llvmArgs: Vec<BasicMetadataValueEnum> = Vec::new();
        for arg in arguments {
            let llvmAnyValue: AnyValueEnum = self.genExpr(&arg)?;
            let llvmBasicValue = match self.anyToBasicEnum(llvmAnyValue) {
                Ok(basicVal) => basicVal,
                Err((msg, errType)) => {
                    return Err(CompileErr {
                        metadata: metadata.clone(),
                        msg,
                        errType,
                    });
                }
            };

            llvmArgs.push(llvmBasicValue.into());
        }

        let llvmFn = self.llvmFns[fnIndex.0];
        let fnName = self.fnTable.getInfo(&fnIndex).name;

        let retValue = match self.builder.build_call(llvmFn, &llvmArgs, format!("callTo{}", fnName).as_str()) {
            Ok(val) => val,
            Err(_) => {
                return Err(CompileErr {
                    errType: "Unexpected compile error".to_string(),
                    msg: "Unable to build conditionnal branch".to_string(),
                    metadata: metadata.clone(),
                });
            }
        };

        return Ok(retValue);
    }

    fn callPanicOnError(&self, msg: &str) {
        let msgStr = self.builder.build_global_string_ptr(msg, "panicMsg").unwrap().as_pointer_value();
        let lenInt = self.context.i64_type().const_int(msg.len() as u64, false);
        let panicFn = self.module.get_function("builtin.panicWithMsg").unwrap();
        self.builder.build_call(panicFn, &[msgStr.into(), lenInt.into()], "").unwrap();
        self.builder.build_unreachable().unwrap(); // optionnal if the panic function really ends
    }
}
