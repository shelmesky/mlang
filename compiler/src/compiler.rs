use parser::Parser;
use std::io::Write;
use std::ops::Deref;

use std::path::Path;
use std::process::Command;

use crate::package;
use crate::symbol_table::{ReturnValues, SymbolTable};
use crate::types as compile_types;
use crate::value as compile_value;

use parser::ast;

use inkwell::builder::Builder;
use inkwell::context::Context;

use inkwell::module::Module;

use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};

use inkwell::types as llvm_types;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values as llvm_values;
use inkwell::values::{BasicValue, FunctionValue, GlobalValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use lazy_static::lazy_static;
use parser::ast::{BasicLit, Expression, Type};

use crate::types::Type::ArrayTypeEnum;
use inkwell::basic_block::BasicBlock;
use parser::token::LitKind;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct CompileOption {
    pub cpu_platform: String,
    pub operating_system: String,
    pub source_path: String,
    pub emit_llvm_ir: String,
    pub emit_asm: String,
    pub emit_object_file: String,
    pub run_binary: String,
    pub delete_binary: String,
}

pub struct Compiler<'itself, 'context> {
    pub context: &'context Context,
    pub builder: &'itself Builder<'context>,
    pub fpm: &'itself PassManager<FunctionValue<'context>>,
    pub current_package: &'itself mut package::Pkg<'context>,
    pub current_package_name: String,
    pub module: &'itself Module<'context>,
    pub symbol_table: SymbolTable<'context>,
    pub return_values: ReturnValues<'context>,
    pub current_function: Option<FunctionValue<'itself>>,
    pub context_condition_after: Vec<BasicBlock<'itself>>,
    pub context_loop_break: Vec<BasicBlock<'itself>>,
    pub context_loop_continue: Vec<BasicBlock<'itself>>,
}

lazy_static! {
    static ref LLVM_CONTEXT: Arc<Mutex<Context>> = Arc::new(Mutex::new(Context::create()));
    // static ref LLVM_MODULE_CONTEXT: Mutex<Option<ModuleContext<'static>>> =
    //    Mutex::new(Option::None);
}

pub fn compile_file(compile_option: CompileOption) {
    let filename = compile_option.clone().source_path;

    if !Path::new(filename.as_str()).exists() {
        panic!(
            "{}",
            format!(
                "{} is not exists or is not a regular mlang source file.",
                filename
            )
        )
    }

    let mut p = Parser::from_file(filename.clone()).unwrap();
    let ast_file = p.parse_file().unwrap();

    let context = Context::create();
    let global_module = context.create_module(filename.clone().as_str());
    let builder = context.create_builder();

    let fpm = PassManager::create(&global_module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let mut pkg = package::Pkg::new("main".to_string());
    let symbol_table = SymbolTable::new();
    let return_values = ReturnValues::new();

    Compiler::do_compile(
        compile_option.clone(),
        ast_file,
        &context,
        &builder,
        &fpm,
        &mut pkg,
        &global_module,
        symbol_table,
        return_values,
    );
}

fn add_global<'a>(context: &'a Context, module: &Module<'a>) -> GlobalValue<'a> {
    module.add_global(context.i64_type(), Some(AddressSpace::Global), "ddd")
}

fn add_function<'a>(
    name: String,
    fn_type: FunctionType<'a>,
    module: &Module<'a>,
) -> FunctionValue<'a> {
    module.add_function(name.as_str(), fn_type, None)
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn do_compile(
        compile_option: CompileOption,
        ast_file: ast::File,
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
        pkg: &'a mut package::Pkg<'ctx>,
        global_module: &'a Module<'ctx>,
        symbol_table: SymbolTable<'ctx>,
        return_values: ReturnValues<'ctx>,
    ) {
        // create Compiler for every package
        let mut compiler = Compiler {
            context,
            builder,
            fpm,
            current_package: pkg,
            current_package_name: "".to_string(),
            module: global_module,
            symbol_table,
            return_values,
            current_function: None,
            context_condition_after: Vec::new(),
            context_loop_break: Vec::new(),
            context_loop_continue: Vec::new(),
        };

        // initial top level symbol table for this package
        compiler.symbol_table.push_variables_stack();

        // initial all builtin types and them into the package
        compile_types::init_builtin_types(&mut compiler);

        // initial all builtin functions and variables
        compiler.init_package_vars();

        // compile global declarations: function, types, global variables, and etc
        for dec in ast_file.decl {
            compiler.compile_global_declaration(&dec);
        }

        // set target for object file
        Target::initialize_all(&InitializationConfig::default());

        let opt = OptimizationLevel::None; // OptimizationLevel::None for develop only
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;

        let mut target_platform = "".to_string();
        let mut target_triple = "".to_string();
        let mut object_file_suffix = "".to_string();

        if compile_option.cpu_platform == "amd64" && compile_option.operating_system == "linux" {
            target_platform = "x86-64".to_string();
            target_triple = "x86_64-pc-linux-gnu".to_string();
            object_file_suffix = ".o".to_string();
        } else if compile_option.cpu_platform == "amd64"
            && compile_option.operating_system == "windows"
        {
            target_platform = "x86-64".to_string();
            target_triple = "x86_64-pc-windows-gnu".to_string();
            object_file_suffix = ".obj".to_string();
        } else {
            panic!(
                "{}",
                format!(
                    "cpu platform {} and operating system {} was not supported.",
                    compile_option.cpu_platform, compile_option.operating_system
                )
            )
        }

        let target = Target::from_name(target_platform.as_str())
            .ok_or("target not found")
            .unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create(target_triple.as_str()),
                target_platform.as_str(),
                "+avx2",
                opt,
                reloc,
                model,
            )
            .unwrap();
        target_machine.set_asm_verbosity(true);

        let mut filename_no_ext = Path::new(ast_file.path.as_path())
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap();

        let mut temp_path = "".to_string();
        if compile_option.run_binary == "true" {
            temp_path = String::from("/tmp/") + &*filename_no_ext.to_string();
            filename_no_ext = temp_path.as_str();
        }

        let object_file = String::from(filename_no_ext) + &*String::from(object_file_suffix);
        let object_file_path = Path::new(object_file.as_str());

        let asm_file = String::from(filename_no_ext) + &*String::from(".s");
        let asm_file_path = Path::new(asm_file.as_str());

        // write target object file
        if compile_option.emit_object_file == "true" {
            target_machine
                .write_to_file(global_module, FileType::Object, object_file_path)
                .unwrap();
        }

        // write assembly file
        if compile_option.emit_asm == "true" {
            target_machine
                .write_to_file(global_module, FileType::Assembly, asm_file_path)
                .unwrap();
        }

        // write llvm IR file
        if compile_option.emit_llvm_ir == "true" {
            let ll_file = String::from(filename_no_ext) + &*String::from(".ll");
            let ll_file_path = ll_file.as_str();
            global_module
                .print_to_file(Path::new(ll_file_path))
                .expect("write module failed");
        }

        Command::new("gcc")
            .arg("-no-pie")
            .arg("-o")
            .arg(filename_no_ext)
            .arg(object_file)
            .output()
            .expect("compile source code failed.");

        // delete binary object file default
        Command::new("/bin/rm")
            .arg(String::from(filename_no_ext) + &*String::from(".o"))
            .arg("-rf")
            .output()
            .expect("remove object file failed.");

        if compile_option.run_binary == "true" {
            let output = Command::new("/bin/bash")
                .arg("-c")
                .arg(filename_no_ext)
                .output()
                .expect("run binary failed");
            if output.status.success() {
                std::io::stdout().write_all(&output.stdout).unwrap();
                std::io::stderr().write_all(&output.stderr).unwrap();
            }
        }

        if compile_option.delete_binary == "true" {
            Command::new("/bin/rm")
                .arg(filename_no_ext)
                .arg("-rf")
                .output()
                .expect("remove binary file failed.");
        }
    }

    fn compile_global_declaration(&mut self, declaration: &ast::Declaration) {
        match declaration {
            ast::Declaration::Function(func_decl) => {
                // into new scope: make new symbol table
                self.symbol_table.push_variables_stack();

                // convert all of the arguments and return types of ast type of function to compile type
                let mut ast_func_args_types = vec![];
                for arg in &func_decl.typ.params.list {
                    ast_func_args_types.push(arg.typ.clone());
                }

                let mut ast_fun_ret_types = vec![];
                for ret in &func_decl.typ.result.list {
                    ast_fun_ret_types.push(ret.typ.clone());
                }

                let mut compile_func_args_types = vec![];
                for arg_type in ast_func_args_types {
                    let compile_arg_type = Compiler::convert_type(
                        self,
                        self.current_package.clone(),
                        compile_types::ast_expression_to_type(arg_type).unwrap(),
                    );
                    compile_func_args_types.push(compile_arg_type);
                }

                let mut compile_func_ret_types = vec![];
                for ret_type in ast_fun_ret_types {
                    let compile_ret_type = Compiler::convert_type(
                        self,
                        self.current_package.clone(),
                        compile_types::ast_expression_to_type(ret_type).unwrap(),
                    );
                    compile_func_ret_types.push(compile_ret_type);
                }

                if compile_func_ret_types.is_empty() {
                    compile_func_ret_types.push(compile_types::Type::from_void_type());
                }

                // only support 1 return value, multiple return values packed into a struct
                if compile_func_ret_types.len() > 1 {
                    panic!(
                        "{}",
                        format!(
                            "the quantity of return values of function \
                    must be less than 1, but actually got: {}",
                            compile_func_ret_types.len()
                        )
                    )
                }

                // convert the type of args list from Vec<Type> to Vec<BasicMetadataTypeEnum>
                let args_type_enum_list = compile_func_args_types
                    .clone()
                    .iter()
                    .map(|arg_type| {
                        llvm_types::BasicMetadataTypeEnum::from(
                            arg_type.llvm_basic_type_enum().unwrap(),
                        )
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                // use function return value type to construct a llvm function type
                let arg_slice = args_type_enum_list.as_slice();
                let fn_compile_type = compile_func_ret_types.get(0).unwrap();
                let ret_basic_type_enum = fn_compile_type.llvm_basic_type_enum();

                // check if the function is has void return
                let mut void_return_function = false;

                // if the return type is Option::None, which means the function is void return type
                let compile_func_type = {
                    match ret_basic_type_enum {
                        None => {
                            void_return_function = true;

                            let void_type = self.context.void_type();
                            void_type.fn_type(arg_slice, false)
                        }
                        Some(fn_type) => fn_type.fn_type(arg_slice, false),
                    }
                };

                // allocate a llvm function value
                let func_name = func_decl.name.name.clone();
                let func_val = add_function(func_name, compile_func_type, self.module);

                // set current compiling function
                self.current_function = Some(func_val);

                // println!("func_decl: {:?}", func_decl);

                // create a basic block which named 'entry'
                let entry = self.context.append_basic_block(func_val, "entry");
                match entry.get_first_instruction() {
                    Some(first_instr) => self.builder.position_before(&first_instr),
                    None => self.builder.position_at_end(entry),
                }

                // compile function arguments
                for (idx, arg) in func_val.get_param_iter().enumerate() {
                    // get the compile time type for a argument
                    let arg_type_enum = compile_func_args_types.get(idx).unwrap();
                    let arg_basic_llvm_type = arg_type_enum.llvm_basic_type_enum().unwrap();

                    // (a,b,c int)
                    // construct three of llvm value for a and b and c, but only with one type: int
                    let name_list_with_one_type = &func_decl.typ.params.list[idx].name;
                    for arg_name in name_list_with_one_type {
                        // get every name of argument
                        let each_arg_name = &arg_name.name;

                        // build alloca instruction with same type used by these arguments
                        let pointer_value = self
                            .builder
                            .build_alloca(arg_basic_llvm_type, each_arg_name.as_str());
                        self.builder.build_store(pointer_value, arg);

                        // get compile type of an argument
                        let compile_type = compile_func_args_types.get(idx).unwrap().clone();
                        self.symbol_table.set_var(
                            each_arg_name.clone(),
                            // argument of function is a variable, must be loaded before use it
                            true,
                            compile_type.clone(),
                            pointer_value.as_basic_value_enum(),
                        );
                    }
                }

                let args_type_enum_list = compile_func_args_types
                    .clone()
                    .iter()
                    .map(|arg_type| arg_type.llvm_basic_type_enum().unwrap())
                    .collect::<Vec<BasicTypeEnum>>();

                // construct CompileFunctionType
                let compile_func_type = compile_types::CompileFunctionType {
                    llvm_type: compile_func_type,
                    return_type: fn_compile_type.clone(),
                    is_variadic: false,
                    is_void: true,
                    argument_types: args_type_enum_list,
                    jump_function: None,
                };
                // construct Type enum
                let compile_type = compile_types::Type::from_function_type(compile_func_type);

                // construct compile Value
                let compile_func_value = compile_value::Value {
                    compile_type,
                    llvm_value: self.context.i8_type().const_zero().as_basic_value_enum(),
                    llvm_func_value: Some(func_val),
                    is_variable: false,
                    is_global: false,
                };
                let func_name = func_decl.name.name.clone();

                // save function to package as package level vars
                self.current_package
                    .define_pkg_var(func_name.clone(), compile_func_value);

                // compile function body
                match &func_decl.body {
                    None => {}
                    Some(block_stmt) => {
                        for stmt in block_stmt.list.iter() {
                            // println!("statement {:?}", stmt);
                            self.compile_statement(stmt);
                        }
                    }
                }

                // there is no return statement in function body,
                // and function is a void function,
                // then build a void return instruction
                if self.return_values.length() == 0 && void_return_function {
                    self.builder.build_return(None);
                }

                // if there is a function and it's not have return statement
                // panic message
                if self.return_values.length() == 0 && !void_return_function {
                    panic!(
                        "{}",
                        format!("function {} need return statement", func_name)
                    )
                }

                // leave function: clear the return_values table
                self.return_values.clear();

                // leave function: release symbol table
                self.symbol_table.pop_variables_stack();

                // leave function: set current compiling to None
                self.current_function = None;
            }
            ast::Declaration::Variable(_var_spec) => {}
            ast::Declaration::Const(_const_spec) => {}
            ast::Declaration::Type(_type_spec) => {}
            _ => {}
        }
    }

    pub fn convert_type(
        compiler: &Compiler<'a, 'ctx>,
        pkg: package::Pkg<'ctx>, // use cloned package instead of reference
        ast_type: ast::Type,
    ) -> compile_types::Type<'ctx> {
        match ast_type {
            Type::Ident(type_name_ident) => {
                // type name ident, such as int32、int64、string etc
                let name = type_name_ident.name.name;
                pkg.get_pkg_type(name, false)
            }
            Type::Array(array_type) => {
                let item_size_expression = array_type.len.deref().clone();

                // got array size from BasicLit type
                let array_size = {
                    match item_size_expression {
                        Expression::BasicLit(size_basic_literal) => {
                            if let LitKind::Integer = size_basic_literal.kind {
                                let size = size_basic_literal
                                    .value
                                    .parse::<u32>()
                                    .expect("LitKind::Integer parse failed");
                                Some(size)
                            } else {
                                panic!("only support LitKind::Integer type for size type in Type::Array")
                            }
                        }
                        _ => {
                            panic!("only support Expression::BasicLit type for size type in Type::Array")
                        }
                    }
                };

                // construct a compile_types::CompileArrayType type object
                let item_type = array_type.typ.deref().clone();
                let compiled_item_type = Compiler::convert_type(compiler, pkg.clone(), item_type);
                let compile_array_type = compile_types::CompileArrayType {
                    llvm_type: compiled_item_type
                        .llvm_basic_type_enum()
                        .unwrap()
                        .array_type(array_size.unwrap()),
                    item_llvm_type: compiled_item_type.llvm_basic_type_enum().unwrap(),
                    item_compile_type: compiled_item_type.clone(),
                    type_name: "array".to_string(),
                    type_size: compiled_item_type.size().unwrap(),
                };
                ArrayTypeEnum(Box::new(compile_array_type))
            }
            Type::Slice(slice_ast_type) => {
                // construct a compile_types::CompileSliceType type object
                let item_type = slice_ast_type.typ.deref().clone();
                let compile_item_type = Compiler::convert_type(compiler, pkg.clone(), item_type);

                // use item type of slice to construct a llvm struct as llvm slice type
                let slice_type_struct_fields = vec![
                    compile_item_type
                        .llvm_basic_type_enum()
                        .unwrap()
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum(), // slice content pointer
                    compiler.context.i32_type().as_basic_type_enum(), // array offset
                    compiler.context.i32_type().as_basic_type_enum(), // len
                    compiler.context.i32_type().as_basic_type_enum(), // cap
                ];

                // use llvm struct as slice type
                let slice_type_struct = compiler
                    .context
                    .struct_type(slice_type_struct_fields.as_slice(), false);

                let compile_slice_type = compile_types::CompileSliceType {
                    llvm_type: slice_type_struct,
                    item_llvm_type: compile_item_type.llvm_basic_type_enum().unwrap(),
                    item_compile_type: compile_item_type.clone(),
                    type_name: "slice".to_string(),
                    type_size: compile_item_type.size().unwrap(),
                };

                compile_types::Type::SliceTypeEnum(Box::new(compile_slice_type))
            }
            Type::Pointer(pointer_ast_type) => {
                let pointer_type = Compiler::convert_type(
                    compiler,
                    pkg.clone(),
                    pointer_ast_type.typ.deref().clone(),
                );
                compile_types::Type::from_pointer_type(pointer_type.clone())
            }
            t => {
                println!("convert_type: unknown type {:?}", t);
                panic!("convert_type: unknown type");
            }
        }
    }

    pub(crate) fn get_basic_value_from_value_enum(
        value_enum: llvm_values::BasicValueEnum,
    ) -> Box<dyn BasicValue + '_> {
        match value_enum.get_type() {
            BasicTypeEnum::ArrayType(_) => {
                Box::new(value_enum.into_array_value()) as Box<dyn BasicValue>
            }
            BasicTypeEnum::FloatType(_) => {
                Box::new(value_enum.into_float_value()) as Box<dyn BasicValue>
            }
            BasicTypeEnum::IntType(_) => {
                Box::new(value_enum.into_int_value()) as Box<dyn BasicValue>
            }
            BasicTypeEnum::PointerType(_) => {
                Box::new(value_enum.into_pointer_value()) as Box<dyn BasicValue>
            }
            BasicTypeEnum::StructType(_) => {
                Box::new(value_enum.into_struct_value()) as Box<dyn BasicValue>
            }
            BasicTypeEnum::VectorType(_) => {
                Box::new(value_enum.into_vector_value()) as Box<dyn BasicValue>
            }
        }
    }

    fn init_package_vars(&mut self) {
        // printf function
        let is_variadic_args = true;
        let arg_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
        let func_type = self
            .context
            .i32_type()
            .fn_type(&[BasicMetadataTypeEnum::from(arg_type)], is_variadic_args);
        let printf_func = add_function("printf".to_string(), func_type, self.module);
        let pkg_cloned = self.current_package.clone();
        let func_type_struct = compile_types::CompileFunctionType {
            llvm_type: func_type,
            return_type: pkg_cloned.get_pkg_type("uint32".to_string(), false),
            is_variadic: is_variadic_args,
            is_void: false,
            argument_types: vec![arg_type.as_basic_type_enum()],
            jump_function: None,
        };
        let compile_func_type = compile_types::Type::FunctionTypeEnum(Box::new(func_type_struct));

        let compile_func_value = compile_value::Value {
            compile_type: compile_func_type,
            // Function Value use const_zero() as empty llvm_value
            llvm_value: self.context.i8_type().const_zero().as_basic_value_enum(),
            llvm_func_value: Some(printf_func),
            is_variable: false,
            is_global: false,
        };
        self.current_package
            .define_pkg_var("printf".to_string(), compile_func_value);

        // malloc function
        let is_variadic_args = false;
        let arg_type = self.context.i64_type();
        let func_type = self
            .context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .fn_type(&[BasicMetadataTypeEnum::from(arg_type)], is_variadic_args);
        let malloc_function = add_function("malloc".to_string(), func_type, self.module);
        let pkg_cloned = self.current_package.clone();
        let int8_type = pkg_cloned.get_pkg_type("int8".to_string(), false);
        let func_type_struct = compile_types::CompileFunctionType {
            llvm_type: func_type,
            return_type: compile_types::Type::PointerTypeEnum(Box::new(
                compile_types::CompilePointerType {
                    point_to: Some(int8_type.clone()),
                    llvm_type: int8_type
                        .llvm_basic_type_enum()
                        .unwrap()
                        .ptr_type(AddressSpace::Generic),
                },
            )),
            is_variadic: is_variadic_args,
            is_void: false,
            argument_types: vec![arg_type.as_basic_type_enum()],
            jump_function: None,
        };
        let compile_func_type = compile_types::Type::FunctionTypeEnum(Box::new(func_type_struct));

        let compile_func_value = compile_value::Value {
            compile_type: compile_func_type,
            // Function Value use const_zero() as empty llvm_value
            llvm_value: self.context.i8_type().const_zero().as_basic_value_enum(),
            llvm_func_value: Some(malloc_function),
            is_variable: false,
            is_global: false,
        };
        self.current_package
            .define_pkg_var("malloc".to_string(), compile_func_value);

        // exit function
        let is_variadic_args = false;
        let arg_type = self.context.i32_type();
        let func_type = self
            .context
            .void_type()
            .fn_type(&[BasicMetadataTypeEnum::from(arg_type)], is_variadic_args);
        let exit_func = add_function("exit".to_string(), func_type, self.module);
        let pkg_cloned = self.current_package.clone();
        let func_type_struct = compile_types::CompileFunctionType {
            llvm_type: func_type,
            return_type: pkg_cloned.get_pkg_type("void".to_string(), false),
            is_variadic: is_variadic_args,
            is_void: true,
            argument_types: vec![arg_type.as_basic_type_enum()],
            jump_function: None,
        };
        let compile_func_type = compile_types::Type::FunctionTypeEnum(Box::new(func_type_struct));

        let compile_func_value = compile_value::Value {
            compile_type: compile_func_type,
            // Function Value use const_zero() as empty llvm_value
            llvm_value: self.context.i8_type().const_zero().as_basic_value_enum(),
            llvm_func_value: Some(exit_func),
            is_variable: false,
            is_global: false,
        };
        self.current_package
            .define_pkg_var("exit".to_string(), compile_func_value);
    }

    // store value to pointer value, with different types do the different action
    pub fn store_value(&self, left_value: PointerValue, right_value: llvm_values::BasicValueEnum) {
        if right_value.is_pointer_value() {
            let loaded_right_value = self
                .builder
                .build_load(right_value.into_pointer_value(), "");
            self.builder.build_store(left_value, loaded_right_value);
        } else {
            self.builder.build_store(left_value, right_value);
        }
    }
}
