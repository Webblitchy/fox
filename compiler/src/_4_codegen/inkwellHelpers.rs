use crate::compileErr::CompileErr;
use crate::irGen::IrGen;
use crate::metadata::{self, Metadata};
use crate::symbolValues::{Symbol, Symbols};
use crate::vartype::{self, VarType};
use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, GlobalValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::{builder::Builder, module::Module, values::BasicValueEnum};

impl<'ctx, 'ft> IrGen<'ctx, 'ft> {
    pub fn createStrLiteral(&self, value: &str, name: &str, metadata: Metadata) -> Result<PointerValue<'ctx>, CompileErr> {
        // TODO: if necessary remove .as_pointer_value()
        let res = self.builder.build_global_string_ptr(value, name);
        match res {
            Ok(v) => Ok(v.as_pointer_value()),
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata,
                    msg: format!("Unable to build literal Str {}:{} : {}", name, value, e),
                    errType: String::from("Unexpected compiler error"),
                });
            }
        }
    }

    pub fn getIWType(&self, varType: &VarType) -> Result<BasicTypeEnum<'ctx>, (String, String)> {
        match varType {
            VarType::Int => Ok(self.context.i64_type().into()),
            VarType::Dec => Ok(self.context.f64_type().into()),
            VarType::Bool => Ok(self.context.bool_type().into()),
            VarType::Str => Ok(self.context.ptr_type(inkwell::AddressSpace::default()).into()),
            _ => {
                return Err((
                    format!("Unable to convert vartype {} to inkwellType", varType),
                    String::from("Unexpected compiler error"),
                ));
            }
        }
    }

    pub fn anyToBasicEnum(&self, anyValueEnum: AnyValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, (String, String)> {
        match anyValueEnum {
            AnyValueEnum::IntValue(v) => Ok(BasicValueEnum::IntValue(v)),
            AnyValueEnum::FloatValue(v) => Ok(BasicValueEnum::FloatValue(v)),
            AnyValueEnum::PointerValue(v) => Ok(BasicValueEnum::PointerValue(v)),
            AnyValueEnum::StructValue(v) => Ok(BasicValueEnum::StructValue(v)),
            AnyValueEnum::ArrayValue(v) => Ok(BasicValueEnum::ArrayValue(v)),
            AnyValueEnum::VectorValue(v) => Ok(BasicValueEnum::VectorValue(v)),
            _ => {
                return Err((
                    format!("Unable to convert AnyValueEnum to BasicValueEnum: {}", anyValueEnum),
                    String::from("Unexpected compiler error"),
                ));
            }
        }
    }

    pub fn storeInStack(
        &mut self,
        name: &str,
        varType: VarType,
        val: Option<AnyValueEnum<'ctx>>, // None for empty declaration and function params
        metadata: Metadata,
    ) -> Result<(), CompileErr> {
        let iwType = match self.getIWType(&varType) {
            Ok(typ) => typ,
            Err((msg, errType)) => {
                return Err(CompileErr {
                    errType,
                    msg,
                    metadata: metadata.clone(),
                });
            }
        };

        // Allocate space for variable
        let ptr: PointerValue = match self.builder.build_alloca(iwType, &name) {
            Ok(p) => p,
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg: format!("Unable to allocate \"{}\" of type \"{:?}\": {}", name, iwType, e),
                    errType: String::from("Unexpected compiler error"),
                });
            }
        };

        // let isEmpty;
        println!("val: {:?}", val);
        match val {
            Some(val) => {
                let basicVal = match self.anyToBasicEnum(val) {
                    Ok(basicVal) => basicVal,
                    Err((msg, errType)) => {
                        return Err(CompileErr {
                            metadata: metadata.clone(),
                            msg,
                            errType,
                        });
                    }
                };

                // Store value
                match self.builder.build_store(ptr, basicVal) {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(CompileErr {
                            metadata: metadata.clone(),
                            msg: format!("Unable to build store {}", name),
                            errType: String::from("Unexpected compiler error"),
                        });
                    }
                }
                //isEmpty = false;
            }
            None => {} //isEmpty = true,
        }

        // Insert address into runtime symbols
        self.symbols.insert(name.to_string(), varType, ptr);

        return Ok(());
    }

    pub fn loadFromStack(&self, name: &str, metadata: Metadata) -> Result<AnyValueEnum<'ctx>, CompileErr> {
        // Get symbol from runtime table
        let symbol: Symbol = match self.symbols.get(name) {
            Ok(s) => s,
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg: e,
                    errType: String::from("Name resolution error"),
                });
            }
        };

        // translate type:
        let iwType = match self.getIWType(&symbol.varType) {
            Ok(typ) => typ,
            Err((msg, errType)) => {
                return Err(CompileErr {
                    errType,
                    msg,
                    metadata: metadata.clone(),
                });
            }
        };

        // Get address from symbol
        let ptr = symbol.ptr.clone();

        // Return AnyValueEnum
        match self.builder.build_load(iwType, ptr, name) {
            Ok(v) => return Ok(v.as_any_value_enum()),
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg: format!("Unable to load variable {}", name),
                    errType: String::from("Unexpected compiler error"),
                });
            }
        }
    }

    pub fn modifyInStack(&mut self, name: &str, varType: VarType, val: AnyValueEnum<'ctx>, metadata: Metadata) -> Result<(), CompileErr> {
        // Get symbol from runtime table
        let symbol: Symbol = match self.symbols.get(name) {
            Ok(s) => s,
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg: e,
                    errType: String::from("Name resolution error"),
                });
            }
        };

        // translate type:
        let iwType = match self.getIWType(&varType) {
            Ok(typ) => typ,
            Err((msg, errType)) => {
                return Err(CompileErr {
                    errType,
                    msg,
                    metadata: metadata.clone(),
                });
            }
        };

        // Get address from symbol
        let ptr = symbol.ptr.clone();

        // Store new value
        let basicVal = match self.anyToBasicEnum(val) {
            Ok(basicVal) => basicVal,
            Err((msg, errType)) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg,
                    errType,
                });
            }
        };
        // Store value
        match self.builder.build_store(ptr, basicVal) {
            Ok(_) => return Ok(()),
            Err(e) => {
                return Err(CompileErr {
                    metadata: metadata.clone(),
                    msg: format!("Unable to build store {}", name),
                    errType: String::from("Unexpected compiler error"),
                });
            }
        }
    }

    pub fn hasTerminator(&self) -> bool {
        return self.builder.get_insert_block().unwrap().get_terminator().is_some();
    }
}
