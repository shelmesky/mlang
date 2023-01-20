use crate::types as runtime_types;
use inkwell::values as llvm_values;

#[derive(Clone)]
pub struct Value<'a> {
    pub compile_type: runtime_types::Type<'a>,
    pub llvm_value: llvm_values::BasicValueEnum<'a>,
    // TODO: use the same type that llvm_value have, to store FunctionValue
    pub llvm_func_value: Option<llvm_values::FunctionValue<'a>>,
    pub is_variable: bool,
    pub is_global: bool,
}

impl<'a> Value<'a> {
    pub fn from_compiled_value(
        compile_type: runtime_types::Type<'a>,
        llvm_value: llvm_values::BasicValueEnum<'a>,
        llvm_func_value: Option<llvm_values::FunctionValue<'a>>,
        is_variable: bool,
        is_global: bool,
    ) -> Self {
        Self {
            compile_type,
            llvm_value,
            llvm_func_value,
            is_variable,
            is_global,
        }
    }
}
