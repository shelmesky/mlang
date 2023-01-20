use crate::compiler::Compiler;
use std::ops::Deref;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values as llvm_values;
use inkwell::{types as llvm_types, AddressSpace};
use parser::ast;
use parser::ast::{ArrayType, Expression};

#[derive(Debug, Clone)]
pub enum Type<'a> {
    PointerTypeEnum(Box<CompilePointerType<'a>>),
    IntTypeEnum(CompileIntType<'a>),
    StringTypeEnum(CompileStringType<'a>),
    FunctionTypeEnum(Box<CompileFunctionType<'a>>),
    ArrayTypeEnum(Box<CompileArrayType<'a>>),
    SliceTypeEnum(Box<CompileSliceType<'a>>),
    VoidTypeEnum,
}

impl<'a> Type<'a> {
    pub fn llvm_basic_type_enum(&self) -> Option<BasicTypeEnum<'a>> {
        match self {
            Type::IntTypeEnum(int_type) => Some(int_type.llvm_type.as_basic_type_enum()),
            Type::FunctionTypeEnum(..) => None,
            Type::PointerTypeEnum(pointer_value) => {
                Some(pointer_value.llvm_type.as_basic_type_enum())
            }
            Type::StringTypeEnum(string_type) => Some(string_type.llvm_type.as_basic_type_enum()),
            Type::ArrayTypeEnum(array_type) => Some(array_type.llvm_type.as_basic_type_enum()),
            Type::SliceTypeEnum(slice_type) => Some(slice_type.llvm_type.as_basic_type_enum()),
            Type::VoidTypeEnum => None,
        }
    }

    pub fn to_compile_int_type(&self) -> Option<CompileIntType<'a>> {
        match self {
            Type::IntTypeEnum(int_type) => Some(int_type.clone()),
            _ => None,
        }
    }

    pub fn to_compile_function_type(&self) -> Option<CompileFunctionType<'a>> {
        match self {
            Type::FunctionTypeEnum(function_struct) => Some(function_struct.deref().clone()),
            _ => None,
        }
    }

    pub fn to_compile_pointer_type(&self) -> Option<CompilePointerType<'a>> {
        match self {
            Type::PointerTypeEnum(pointer_type) => Some(pointer_type.deref().clone()),
            _ => None,
        }
    }

    pub fn to_compile_string_type(&self) -> Option<CompileStringType<'a>> {
        match self {
            Type::StringTypeEnum(string_type) => Some(string_type.clone()),
            _ => None,
        }
    }

    pub fn to_compile_array_type(&self) -> Option<CompileArrayType<'a>> {
        match self {
            Type::ArrayTypeEnum(array_type) => Some(*array_type.clone()),
            _ => None,
        }
    }

    pub fn to_compile_slice_type(&self) -> Option<CompileSliceType<'a>> {
        match self {
            Type::SliceTypeEnum(slice_type) => Some(*slice_type.clone()),
            _ => None,
        }
    }

    pub fn from_string_type(string_type: CompileStringType<'a>) -> Self {
        Type::StringTypeEnum(string_type)
    }

    pub fn from_array_type(array_type: CompileArrayType<'a>) -> Self {
        Type::ArrayTypeEnum(Box::from(array_type))
    }

    pub fn from_slice_type(slice_type: CompileSliceType<'a>) -> Self {
        Type::SliceTypeEnum(Box::from(slice_type))
    }

    pub fn from_int_type(int_type: CompileIntType<'a>) -> Self {
        Type::IntTypeEnum(int_type)
    }

    pub fn from_pointer_type(pointer_type: Type<'a>) -> Self {
        Type::PointerTypeEnum(Box::new(CompilePointerType {
            point_to: Some(pointer_type.clone()),
            llvm_type: pointer_type
                .llvm_basic_type_enum()
                .unwrap()
                .ptr_type(AddressSpace::Generic),
        }))
    }

    pub fn from_function_type(func_type: CompileFunctionType<'a>) -> Self {
        Type::FunctionTypeEnum(Box::new(func_type))
    }

    pub fn from_void_type() -> Self {
        Type::VoidTypeEnum
    }

    pub fn size(&self) -> Option<u64> {
        match self {
            Type::PointerTypeEnum(pointer_type) => Some(0),
            Type::IntTypeEnum(int_type) => Some(int_type.clone().type_size),
            Type::StringTypeEnum(string_type) => Some(0),
            Type::FunctionTypeEnum(function_type) => Some(0),
            Type::ArrayTypeEnum(array_type) => Some(0),
            // 12 bytes is total length of fields slice len、offset、cap
            // 8 bytes is length of data pointer
            Type::SliceTypeEnum(slice_type) => Some(12 + 8),
            Type::VoidTypeEnum => Some(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilePointerType<'a> {
    pub point_to: Option<Type<'a>>,
    pub llvm_type: llvm_types::PointerType<'a>,
}

#[derive(Debug, Clone)]
pub struct CompileIntType<'a> {
    pub llvm_type: llvm_types::IntType<'a>,
    pub type_name: String,
    pub type_size: u64,
    pub signed: bool,
}

#[derive(Debug, Clone)]
pub struct CompileStringType<'a> {
    pub llvm_type: llvm_types::StructType<'a>,
    pub type_name: String,
    pub type_size: u64,
}

#[derive(Debug, Clone)]
pub struct CompileArrayType<'a> {
    pub llvm_type: llvm_types::ArrayType<'a>,
    pub item_llvm_type: llvm_types::BasicTypeEnum<'a>,
    pub item_compile_type: Type<'a>,
    pub type_name: String,
    pub type_size: u64,
}

#[derive(Debug, Clone)]
pub struct CompileSliceType<'a> {
    pub llvm_type: llvm_types::StructType<'a>,
    pub item_llvm_type: llvm_types::BasicTypeEnum<'a>,
    pub item_compile_type: Type<'a>,
    pub type_name: String,
    pub type_size: u64,
}

#[derive(Debug, Clone)]
pub struct CompileFunctionType<'a> {
    pub llvm_type: llvm_types::FunctionType<'a>,
    pub return_type: Type<'a>,
    pub is_variadic: bool,
    pub is_void: bool,
    pub argument_types: Vec<llvm_types::BasicTypeEnum<'a>>,
    pub jump_function: Option<llvm_values::FunctionValue<'a>>,
}

pub fn init_builtin_types(compiler: &mut Compiler) {
    // construct builtin types and insert them into the package

    // byte
    let byte = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i8_type(),
        type_name: "byte".to_string(),
        type_size: 8 / 8,
        signed: false,
    });
    compiler
        .current_package
        .define_pkg_type("byte".to_string(), byte);

    // int8
    let int8 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i8_type(),
        type_name: "int8".to_string(),
        type_size: 8 / 8,
        signed: true,
    });
    compiler
        .current_package
        .define_pkg_type("int8".to_string(), int8);

    // uint8
    let uint8 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i8_type(),
        type_name: "uint8".to_string(),
        type_size: 8 / 8,
        signed: false,
    });
    compiler
        .current_package
        .define_pkg_type("uint8".to_string(), uint8);

    // int16
    let int16 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i16_type(),
        type_name: "int16".to_string(),
        type_size: 16 / 8,
        signed: true,
    });
    compiler
        .current_package
        .define_pkg_type("int16".to_string(), int16);

    // uint16
    let uint16 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i16_type(),
        type_name: "uint16".to_string(),
        type_size: 16 / 8,
        signed: false,
    });
    compiler
        .current_package
        .define_pkg_type("uint16".to_string(), uint16);

    // int32
    let int32 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i32_type(),
        type_name: "int32".to_string(),
        type_size: 32 / 8,
        signed: true,
    });
    compiler
        .current_package
        .define_pkg_type("int32".to_string(), int32);

    // uint32
    let uint32 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i32_type(),
        type_name: "uint32".to_string(),
        type_size: 32 / 8,
        signed: false,
    });
    compiler
        .current_package
        .define_pkg_type("uint32".to_string(), uint32);

    // int64
    let uint64 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i64_type(),
        type_name: "int64".to_string(),
        type_size: 64 / 8,
        signed: true,
    });
    compiler
        .current_package
        .define_pkg_type("int64".to_string(), uint64);

    // uint64
    let uint64 = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i64_type(),
        type_name: "uint64".to_string(),
        type_size: 64 / 8,
        signed: false,
    });
    compiler
        .current_package
        .define_pkg_type("uint64".to_string(), uint64);

    // int
    let int = Type::IntTypeEnum(CompileIntType {
        llvm_type: compiler.context.i64_type(),
        type_name: "int".to_string(),
        type_size: 64 / 8,
        signed: true,
    });
    compiler
        .current_package
        .define_pkg_type("int".to_string(), int);

    // void
    let void = Type::VoidTypeEnum;
    compiler
        .current_package
        .define_pkg_type("void".to_string(), void);

    // string
    let string_type_fields = vec![
        compiler
            .context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum(),
        compiler.context.i32_type().as_basic_type_enum(),
    ];
    let string_struct_type = compiler
        .context
        .struct_type(string_type_fields.as_slice(), false);
    let compile_string_type = Type::StringTypeEnum(CompileStringType {
        llvm_type: string_struct_type,
        type_name: "string".to_string(),
        type_size: 128 / 8,
    });

    compiler
        .current_package
        .define_pkg_type("string".to_string(), compile_string_type);
}

pub fn ast_expression_to_type(expression: Expression) -> Option<ast::Type> {
    match expression {
        Expression::Type(t) => Some(t),
        _ => None,
    }
}
