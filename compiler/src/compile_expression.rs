use anyhow;

use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicType, StringRadix};
use inkwell::{AddressSpace, IntPredicate};
use std::ops::Deref;

use crate::types as compile_types;
use crate::value as compile_value;

use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use parser::ast;
use unescape::unescape;

use crate::compiler::Compiler;
use parser::ast::{Element, Expression, Type};
use parser::token::{LitKind, Operator};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_composite_literal(
        &mut self,
        composite_lit: ast::CompositeLit,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        // println!("composite_lit {:?}", composite_lit);

        // convert ast type of composite literal to compile type
        let composite_lit_type = composite_lit.typ.deref().clone();
        let ast_type = compile_types::ast_expression_to_type(composite_lit_type).unwrap();
        let compiled_composite_type =
            Compiler::convert_type(self, self.current_package.clone(), ast_type.clone());

        // length of all values in composite literal
        let values_length = composite_lit.val.values.len();

        // array composite literal
        let mut compiled_items_values = Vec::new();
        if compiled_composite_type.to_compile_array_type().is_some() {
            // array type
            // compile every initial value in composite literal
            for keyed_element in composite_lit.val.values {
                let element = keyed_element.val;
                match element {
                    // array element is basic expression, just like: a, 123, func3(), "hello" etc
                    Element::Expr(expr) => {
                        compiled_items_values.push(self.compile_expression(expr).unwrap());
                    }

                    // array element is another composite literal, just like: {1,2}
                    // this means array is N-dimensional array
                    Element::LitValue(literal_value) => {
                        let cloned_ast_type = ast_type.clone();
                        // got inner array type of N-dimensional array
                        let inner_ast_array_type = {
                            match cloned_ast_type {
                                Type::Array(array_type) => Some(array_type.typ.deref().clone()),
                                _ => None,
                            }
                        }
                        .unwrap();

                        let inner_type = inner_ast_array_type.clone();
                        // construct a temporary Expression::CompositeLit to wrapper inner array
                        let wrapped_composite_lit = Expression::CompositeLit(ast::CompositeLit {
                            typ: Box::new(Expression::Type(inner_type)),
                            val: literal_value,
                        });
                        // and we compile the temporary Expression::CompositeLit
                        let compiled_wrapped_composite_lit =
                            self.compile_expression(wrapped_composite_lit);
                        compiled_items_values.push(compiled_wrapped_composite_lit.unwrap());
                    }
                }
            }

            // allocate llvm value and store initial values to it
            return self.compile_initialize_array_with_values(
                values_length,
                compiled_composite_type,
                compiled_items_values,
            );
        } else if compiled_composite_type.to_compile_slice_type().is_some() {
            // slice type
            // slice composite literal
            // compile every initial value in composite literal
            for keyed_element in composite_lit.val.values {
                let element = keyed_element.val;
                match element {
                    Element::Expr(expr) => {
                        compiled_items_values.push(self.compile_expression(expr).unwrap());
                    }

                    // slice element is another composite literal, just like: {1,2}
                    // this means slice is N-dimensional slice
                    Element::LitValue(literal_value) => {
                        let cloned_ast_type = ast_type.clone();
                        // got inner slice type of N-dimensional slice
                        let inner_ast_slice_type = {
                            match cloned_ast_type {
                                Type::Slice(slice_type) => Some(slice_type.typ.deref().clone()),
                                _ => None,
                            }
                        }
                        .unwrap();

                        let inner_type = inner_ast_slice_type.clone();
                        // construct a temporary Expression::CompositeLit to wrapper inner slice
                        let wrapped_composite_lit = Expression::CompositeLit(ast::CompositeLit {
                            typ: Box::new(Expression::Type(inner_type)),
                            val: literal_value,
                        });
                        // and we compile the temporary Expression::CompositeLit
                        let compiled_wrapped_composite_lit =
                            self.compile_expression(wrapped_composite_lit);
                        compiled_items_values.push(compiled_wrapped_composite_lit.unwrap());
                    }
                }
            }

            return self
                .compile_initial_slice_with_values(compiled_composite_type, compiled_items_values);
        } else {
            panic!("compiled_composite_type unknown type failed")
        }
    }

    pub fn compile_unary_expression(
        &mut self,
        unary_expression: ast::UnaryExpression,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        let operator = unary_expression.op;
        match operator {
            // x = &y
            Operator::And => {
                let get_reference_expr_value = self.compile_expression(*unary_expression.right);
                let expr_value = get_reference_expr_value.unwrap();
                let get_reference_pointer_type =
                    compile_types::Type::from_pointer_type(expr_value.clone().compile_type);

                let compile_reference_value = compile_value::Value {
                    compile_type: get_reference_pointer_type,
                    llvm_value: expr_value.clone().llvm_value,
                    llvm_func_value: None,
                    // result of '&x' expression evaluation is temporary value, not a variable
                    is_variable: false,
                    is_global: false,
                };
                Ok(compile_reference_value)
            }
            _ => Err(anyhow::Error::msg(
                "Expression::Unary unary operator unimplemented",
            )),
        }
    }

    pub fn compile_star_expression(
        &mut self,
        star_expression: ast::StarExpression,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        let star_expr_value = self.compile_expression(*star_expression.right);
        let expr_value = star_expr_value.unwrap();

        if expr_value.llvm_value.is_pointer_value() {
            let loaded_value = self
                .builder
                .build_load(expr_value.llvm_value.into_pointer_value(), "load-ptr");

            let star_expr_type =
                compile_types::Type::from_pointer_type(expr_value.clone().compile_type);

            let compile_star_value = compile_value::Value {
                compile_type: star_expr_type,
                llvm_value: loaded_value.as_basic_value_enum(),
                llvm_func_value: None,
                // *a is a temporary variable, will used in expression evaluation
                // that means it should be loaded before use
                is_variable: true,
                is_global: false,
            };

            return Ok(compile_star_value);
        }
        Ok(expr_value)
    }

    pub fn compile_basic_literal(
        &mut self,
        basic_literal: ast::BasicLit,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        match basic_literal.kind {
            LitKind::Ident => Err(anyhow::Error::msg("unimplemented")),
            LitKind::String => {
                let cached_global_string_value = self
                    .symbol_table
                    .string_constants_table
                    .get(&*basic_literal.value);

                // get global string variable from cache or create it
                let global_string_value = match cached_global_string_value {
                    None => unsafe {
                        let a = basic_literal.value.trim_matches('"').to_string();
                        let b = unescape(a.as_str()).expect("unescape failed");
                        self.builder.build_global_string_ptr(b.as_str(), "str")
                    },
                    Some(v) => *v,
                };

                // allocate a temporary string type on stack
                let cloned_package = self.current_package.clone();
                let string_type = cloned_package
                    .get_pkg_type("string".to_string(), false)
                    .to_compile_string_type()
                    .unwrap()
                    .llvm_type;
                let string_alloca = self.builder.build_alloca(string_type, "");

                // get pointer field which point to real data of string
                let ptr_field = self
                    .builder
                    .build_struct_gep(string_alloca, 0, "ptr")
                    .unwrap();
                // convert global string from [n x i8]* to i8*
                let i8_global_string = unsafe {
                    self.builder.build_gep(
                        global_string_value.as_pointer_value(),
                        &[self.context.i32_type().const_int(0, false)],
                        "ptr",
                    )
                };
                // store global constant string pointer to pointer field of string
                self.builder.build_store(ptr_field, i8_global_string);

                // get length field which indicator the length of real data of string
                let len_field = self
                    .builder
                    .build_struct_gep(string_alloca, 1, "len")
                    .unwrap();
                // store length of string
                self.builder.build_store(
                    len_field,
                    self.context
                        .i32_type()
                        .const_int(basic_literal.value.len() as u64, false),
                );

                // save the global const string to constant string cache
                self.symbol_table
                    .string_constants_table
                    .insert(basic_literal.value, global_string_value);

                let cloned_package = self.current_package.clone();
                let string_type = cloned_package.get_pkg_type("string".to_string(), false);
                let compile_type = compile_types::Type::from_string_type(
                    string_type.to_compile_string_type().unwrap(),
                );
                let compile_value = compile_value::Value::from_compiled_value(
                    compile_type,
                    string_alloca.as_basic_value_enum(),
                    None,
                    false,
                    false,
                );
                Ok(compile_value)
            }
            LitKind::Integer => {
                // integer literal
                let cloned_package = self.current_package.clone();
                let int64_type = cloned_package.get_pkg_type("int64".to_string(), false);
                let compile_value = compile_value::Value::from_compiled_value(
                    int64_type,
                    self.context
                        .i64_type()
                        .const_int_from_string(basic_literal.value.as_str(), StringRadix::Decimal)
                        .unwrap()
                        .as_basic_value_enum(),
                    None,
                    false,
                    false,
                );

                Ok(compile_value)
            }
            LitKind::Float => Err(anyhow::Error::msg(
                "Expression::BasicLit Float unimplemented",
            )),
            LitKind::Imag => Err(anyhow::Error::msg(
                "Expression::BasicLit Imag unimplemented",
            )),
            LitKind::Char => Err(anyhow::Error::msg(
                "Expression::BasicLit Char unimplemented",
            )),
        }
    }

    pub fn compile_ident(
        &mut self,
        ident: ast::TypeName,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        let var_name = ident.name.name;
        // get variable from symbol table
        let variable = self.symbol_table.get_var(var_name.clone());
        match variable {
            Ok(v) => Ok(v.clone()),
            Err(_) => {
                // if it is not exists in symbol table, then search in package level variable table
                let package_variable = self.current_package.get_pkg_var(var_name.clone(), false);
                match package_variable {
                    None => {
                        panic!(
                            "{}",
                            format!(
                                "Expression::Ident '{}' can not find ident name",
                                var_name.clone()
                            )
                        )
                    }
                    Some(v) => Ok(v.clone()),
                }
            }
        }

        // Ok(variable.llvm_value)
    }

    pub fn compile_list_index(
        &mut self,
        index_access: ast::Index,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        // println!("index_access {:?}", index_access);

        // array ast node
        let array_ast_node = index_access.left.deref().clone();

        // compile array name
        let array_llvm_value = self.compile_expression(array_ast_node.clone()).unwrap();
        let mut array_basic_value = array_llvm_value.llvm_value;

        // array index ast node
        let array_index_ast_node = index_access.index.deref().clone();

        // compile index expression of array index
        let array_index_llvm_value = self.compile_expression(array_index_ast_node).unwrap();

        // if array index value is pointer, load it before use
        let mut index_llvm_value = if array_index_llvm_value.llvm_value.is_pointer_value() {
            self.builder
                .build_load(
                    array_index_llvm_value
                        .llvm_value
                        .as_basic_value_enum()
                        .into_pointer_value(),
                    "loaded-index",
                )
                .into_int_value()
        } else {
            array_index_llvm_value
                .llvm_value
                .as_basic_value_enum()
                .into_int_value()
        };

        let mut runtime_array_length: BasicValueEnum =
            self.context.i32_type().const_zero().as_basic_value_enum();
        let mut compile_time_array_length: u32 = 0;

        // check if we know the length of array at compile time
        // 'array' get length at compile time
        // 'string', 'slice' get length at runtime
        let mut length_known_at_compile_time: bool = false;
        let mut length_known_at_runtime: bool = false;

        let mut is_llvm_based_array: bool = false;
        let mut ret_type: compile_types::Type = compile_types::Type::VoidTypeEnum;

        // if object is array type, get the length of array at compile time
        let compile_array_type = array_llvm_value.compile_type.to_compile_array_type();
        if compile_array_type.is_some() {
            // llvm array length
            compile_time_array_length = compile_array_type.clone().unwrap().llvm_type.len();
            // we know array length at compile time
            length_known_at_compile_time = true;
            // array is based on llvm array
            is_llvm_based_array = true;
            // return type
            ret_type = compile_array_type.unwrap().item_compile_type;
        }

        // if object is slice type, get the length of slice at runtime
        let compile_slice_type = array_llvm_value.compile_type.to_compile_slice_type();
        let mut is_slice: bool = false;
        if compile_slice_type.is_some() {
            is_slice = true;

            length_known_at_compile_time = false;
            length_known_at_runtime = true;

            // return type
            ret_type = compile_slice_type.unwrap().item_compile_type;

            // when the slice is slice window into an array
            let is_slice_of_array: bool;

            // load llvm array value before use it
            array_basic_value = self
                .builder
                .build_load(array_basic_value.into_pointer_value(), "");

            // convert integer type(i8、i16、i32、u32 etc) of index llvm value to i32 type
            index_llvm_value =
                self.builder
                    .build_int_truncate(index_llvm_value, self.context.i32_type(), "");

            // get 'offset' field of slice struct
            let slice_offset_field = self
                .builder
                .build_extract_value(array_basic_value.into_struct_value(), 1, "")
                .unwrap();

            // add index value and slice offset field
            // index = index + offset
            index_llvm_value = self.builder.build_int_add(
                index_llvm_value,
                slice_offset_field.into_int_value(),
                "",
            );

            // get 'len' field of slice struct
            let mut slice_len_field = self
                .builder
                .build_extract_value(array_basic_value.into_struct_value(), 2, "")
                .unwrap();

            // len = len + offset
            runtime_array_length = self
                .builder
                .build_int_add(
                    slice_len_field.into_int_value(),
                    slice_offset_field.into_int_value(),
                    "",
                )
                .as_basic_value_enum();

            // get data pinter field in slice struct
            array_basic_value = self
                .builder
                .build_extract_value(array_basic_value.into_struct_value(), 0, "")
                .unwrap();
        };

        // get array value and runtime length of string type
        // let str: string = "hello"
        // let b: i8 = str[0]
        if !length_known_at_compile_time {
            if array_llvm_value
                .compile_type
                .to_compile_string_type()
                .is_some()
            {
                array_basic_value = self
                    .builder
                    .build_extract_value(array_basic_value.into_struct_value(), 0, "")
                    .unwrap();

                runtime_array_length = self
                    .builder
                    .build_extract_value(array_basic_value.into_struct_value(), 0, "")
                    .unwrap();

                length_known_at_compile_time = true;

                let cloned_package = self.current_package.clone();
                ret_type = cloned_package.get_pkg_type("i8".to_string(), false);
                is_llvm_based_array = false;
            }
        }

        if !length_known_at_compile_time && !length_known_at_runtime {
            println!("array_ast_node {:?}", array_ast_node.clone());
            panic!("unable to load array element: cloud not calculate array length")
        }

        let mut is_checked_at_compile_time: bool = false;

        // check index size of array length
        // panic if index size out of range
        if length_known_at_compile_time {
            if compile_time_array_length < 0 {
                panic!("array index out of range")
            }

            // if array index value is constant integer value
            // which means we can check array index at compile
            if index_llvm_value.is_const() {
                let constant_index_value = index_llvm_value.get_sign_extended_constant().unwrap();

                if constant_index_value > compile_time_array_length as i64 {
                    panic!("array index out of range")
                }
            } else {
                // but if array index value is a variable
                // we must check array index size at compile time
                is_checked_at_compile_time = false;
            }
        }

        // check index size of array at runtime
        // but as we mentioned above, if index size of array is a variable
        // we must check the variable of index size at runtime
        if !is_checked_at_compile_time {
            // get current function
            let current_function = self.current_function.unwrap();
            // get current code block
            let current_block = current_function.get_last_basic_block().unwrap();

            let outside_of_length_block = self
                .context
                .append_basic_block(current_function, "array-index-out-of-range");

            // set panic message at runtime
            self.runtime_panic(outside_of_length_block, "index out of range".to_string());

            self.builder.position_at_end(outside_of_length_block);
            self.builder.build_unreachable();

            // restore code block from 'outside_of_length_block' to current block
            self.builder.position_at_end(current_block);

            // if array index checking has passwd, jump to this block
            let safe_block = self
                .context
                .append_basic_block(current_function, "after-array-index-check");

            // compare slice index value with runtime_array_length
            // (if array index is not constant - is a variable, compare it at runtime also)
            let runtime_or_compile_time_compare = if length_known_at_compile_time {
                self.builder.build_int_compare(
                    IntPredicate::UGE,
                    index_llvm_value,
                    self.context
                        .i32_type()
                        .const_int(compile_time_array_length as u64, false),
                    "",
                )
            } else {
                self.builder.build_int_compare(
                    IntPredicate::UGE,
                    index_llvm_value,
                    runtime_array_length.into_int_value(),
                    "",
                )
            };

            let index_lt_zero = self.builder.build_int_compare(
                IntPredicate::ULT,
                index_llvm_value,
                self.context.i32_type().const_zero(),
                "",
            );

            let out_of_range_cmp =
                self.builder
                    .build_or(index_lt_zero, runtime_or_compile_time_compare, "");

            // jump to out of range block or safety block
            self.builder.build_conditional_branch(
                out_of_range_cmp,
                outside_of_length_block,
                safe_block,
            );

            self.builder.position_at_end(safe_block);
        }

        let mut arg_vec = vec![];
        if is_slice {
            arg_vec.push(index_llvm_value);
        } else {
            arg_vec.push(self.context.i64_type().const_zero());
            arg_vec.push(index_llvm_value);
        };

        let index_accessed_value = unsafe {
            self.builder.build_gep(
                array_basic_value.into_pointer_value(),
                arg_vec.as_slice(),
                "array-gep",
            )
        };

        let compile_value = compile_value::Value {
            compile_type: ret_type,
            llvm_value: index_accessed_value.as_basic_value_enum(),
            llvm_func_value: None,
            // result of array/slice index expression is a temporary variable
            // it should be loaded before use
            is_variable: true,
            is_global: false,
        };

        Ok(compile_value)
    }

    pub fn compile_function_call(
        &mut self,
        function_call: ast::Call,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        // println!("Expression::Call {:?}", function_call);

        // compile function name of function call
        let function_value_result = self.compile_expression(function_call.left.deref().clone());
        let function_value = function_value_result.as_ref().unwrap();
        let function = function_value.llvm_func_value.unwrap();

        // compile arguments of function call
        let compiled_args_value = function_call
            .args
            .iter()
            .map(|arg_expression| self.compile_expression(arg_expression.clone()).unwrap())
            .collect::<Vec<compile_value::Value<'ctx>>>();

        // convert from compile Value to llvm BasicMetadataValueEnum
        let args_basic_value_enum = compiled_args_value
            .iter()
            .zip(function_call.args)
            .map(|(arg_value, arg_ast_expr)| {
                // if we call printf function
                // and the arguments is string type or slice type
                // use the data pointer instead the struct itself
                if function.get_name().to_str().unwrap() == "printf"
                    && (arg_value.compile_type.to_compile_string_type().is_some())
                {
                    // because string struct is always a pointer value:
                    // 1. constant string is global pointer value
                    // 2. string struct in stack is allocated by malloc function
                    let loaded_value = self
                        .builder
                        .build_load(arg_value.llvm_value.into_pointer_value(), "");
                    // extract data pointer field from string struct
                    let extracted_value = self
                        .builder
                        .build_extract_value(loaded_value.into_struct_value(), 0, "")
                        .unwrap();
                    BasicMetadataValueEnum::from(extracted_value)
                } else {
                    let mut arg_value_enum = arg_value.llvm_value.as_basic_value_enum();

                    // common function calling
                    // load the llvm value if it is a variable
                    if arg_value.is_variable {
                        let loaded_value = self
                            .builder
                            .build_load(arg_value_enum.into_pointer_value(), "");
                        BasicMetadataValueEnum::from(loaded_value)
                    } else {
                        // not pointer value, like IntValue
                        BasicMetadataValueEnum::from(arg_value_enum)
                    }
                }
            })
            .collect::<Vec<BasicMetadataValueEnum>>();

        // emit function calling instruction
        let expr_value = self
            .builder
            .build_call(function, args_basic_value_enum.as_slice(), "");

        let func_type = function_value.clone().compile_type;

        let option_expr_value = expr_value.try_as_basic_value().left();
        // if got nothing from function calling expression
        // just return a error
        match option_expr_value {
            None => Err(anyhow::Error::msg("call void function")),
            Some(_) => {
                // return function calling result
                let basic_enum_value = expr_value
                    .try_as_basic_value()
                    .expect_left("expect_left failed");
                let compile_value = compile_value::Value::from_compiled_value(
                    func_type,
                    basic_enum_value,
                    None,
                    // result of a function calling is not a temporary variable
                    // because function calling can return IntValue or PointerValue
                    // if return value is IntValue, it's not a temporary variable
                    false,
                    false,
                );
                Ok(compile_value)
            }
        }
    }
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_expression(
        &mut self,
        expression: Expression,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        match expression {
            Expression::Type(_) => Err(anyhow::Error::msg("Expression::Type unimplemented")),
            Expression::Call(function_call) => self.compile_function_call(function_call),
            // load element from array
            // in this scenario, one of 'array' and 'string' and 'slice' can called as 'array'
            // because we access them all use object[index] style
            Expression::Index(index_access) => self.compile_list_index(index_access),
            Expression::Slice(_) => Err(anyhow::Error::msg("Expression::Slice unimplemented")),
            Expression::Ident(ident) => self.compile_ident(ident),
            Expression::FuncLit(_) => Err(anyhow::Error::msg("Expression::FuncLit unimplemented")),
            Expression::Ellipsis(_) => {
                Err(anyhow::Error::msg("Expression::Ellipsis unimplemented"))
            }
            Expression::Selector(_) => {
                Err(anyhow::Error::msg("Expression::Selector unimplemented"))
            }
            Expression::BasicLit(basic_literal) => self.compile_basic_literal(basic_literal),
            Expression::Range(_) => Err(anyhow::Error::msg("Expression::Range unimplemented")),
            Expression::Star(star_expression) => self.compile_star_expression(star_expression),
            Expression::Paren(_) => Err(anyhow::Error::msg("Expression::Paren unimplemented")),
            Expression::Unary(unary_expression) => self.compile_unary_expression(unary_expression),
            Expression::Binary(binary_expr) => self.compile_binary(binary_expr),
            Expression::TypeAssert(_) => {
                Err(anyhow::Error::msg("Expression::TypeAssert unimplemented"))
            }
            Expression::CompositeLit(composite_lit) => {
                self.compile_composite_literal(composite_lit)
            }
        }
    }

    pub fn compile_initialize_array_with_values(
        &self,
        values_length: usize,
        compiled_composite_type: compile_types::Type<'ctx>,
        compiled_items_values: Vec<compile_value::Value>,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        // allocate a llvm array value
        let llvm_array_type = compiled_composite_type
            .to_compile_array_type()
            .unwrap()
            .llvm_type;
        let allocated_array_value = self.builder.build_array_alloca(
            llvm_array_type,
            self.context
                .i32_type()
                .const_int(values_length as u64, false),
            "arr-alloca",
        );

        // copy every initial value to allocated llvm array type
        for (idx, value) in compiled_items_values.iter().enumerate() {
            unsafe {
                let store_destination = self.builder.build_gep(
                    allocated_array_value,
                    &[
                        self.context.i32_type().const_zero(),
                        self.context.i32_type().const_int(idx as u64, false),
                    ],
                    "loaded-array-field",
                );

                store_destination.set_name("init_arr_value");

                // load value if needed
                // alloca(), getelementptr() and others is pointer value
                // basic value like int, float are not
                let loaded_init_value = if value.llvm_value.is_pointer_value() {
                    self.builder
                        .build_load(value.llvm_value.into_pointer_value(), "")
                } else {
                    value.llvm_value
                };

                self.builder
                    .build_store(store_destination, loaded_init_value);
            }
        }

        let compile_value = compile_value::Value {
            compile_type: compiled_composite_type,
            llvm_value: allocated_array_value.as_basic_value_enum(),
            llvm_func_value: None,
            // result of array/slice index expression is a temporary variable
            // it should be loaded before use
            is_variable: true,
            is_global: false,
        };

        Ok(compile_value)
    }

    pub fn compile_initial_slice_with_values(
        &self,
        compiled_composite_type: compile_types::Type<'ctx>,
        compiled_initial_items_values: Vec<compile_value::Value>,
    ) -> Result<compile_value::Value<'ctx>, anyhow::Error> {
        let llvm_slice_type = compiled_composite_type
            .to_compile_slice_type()
            .unwrap()
            .llvm_type;

        let llvm_slice_value = self.builder.build_alloca(llvm_slice_type, "slice-alloca");

        // alloca memory by use malloc function, and fill the memory address to slice struct
        // fill initial values of other fields - offset, len, cap to slice struct
        self.init_llvm_slice(
            compiled_composite_type.clone(),
            compiled_initial_items_values.len(),
            llvm_slice_value,
        );

        // if slice have initial values
        // compile them and store them to backing array pointer
        if compiled_initial_items_values.len() > 0 {
            let slice_ptr_field = unsafe {
                self.builder
                    .build_struct_gep(llvm_slice_value, 0, "slice-data-ptr")
            }
            .unwrap();

            // load slice data pointer is needed
            // because in slice struct, the type of data pinter is Type*, but Type itself is also Type*
            // so the type of slice data pointer is Type**
            let loaded_slice_ptr_field = self
                .builder
                .build_load(slice_ptr_field, "loaded-slice-data-ptr");

            // save every initial value to slice data pointer
            for (idx, value) in compiled_initial_items_values.iter().enumerate() {
                unsafe {
                    let element_store_position_ptr = self.builder.build_gep(
                        loaded_slice_ptr_field.into_pointer_value(),
                        &[self.context.i32_type().const_int(idx as u64, false)],
                        "slice-element-pos",
                    );

                    self.store_value(element_store_position_ptr, value.llvm_value);
                }
            }

            // update the length of slice
            let slice_len_field = unsafe {
                self.builder
                    .build_struct_gep(llvm_slice_value, 2, "slice-len")
            }
            .unwrap();
            self.builder.build_store(
                slice_len_field,
                self.context
                    .i32_type()
                    .const_int(compiled_initial_items_values.len() as u64, false),
            );
        }

        let compile_value = compile_value::Value {
            compile_type: compiled_composite_type.clone(),
            llvm_value: llvm_slice_value.as_basic_value_enum(),
            llvm_func_value: None,
            is_variable: true,
            is_global: false,
        };

        Ok(compile_value)
    }

    // set content pointer, len, cap, offset for llvm slice type
    fn init_llvm_slice(
        &self,
        compile_composite_literal_type: compile_types::Type<'ctx>,
        mut init_cap: usize,
        allocated_slice_struct: PointerValue,
    ) {
        // the cap must always be larger than 0
        // use 2 as the default value
        if init_cap < 2 {
            init_cap = 2;
        }

        // load pointer、offset、len、cap fields from allocated slice struct
        let content_pointer_filed =
            self.builder
                .build_struct_gep(allocated_slice_struct, 0, "slice-data");
        let content_offset_filed =
            self.builder
                .build_struct_gep(allocated_slice_struct, 1, "slice-offset");
        let content_len_filed =
            self.builder
                .build_struct_gep(allocated_slice_struct, 2, "slice-len");
        let content_cap_filed =
            self.builder
                .build_struct_gep(allocated_slice_struct, 3, "slice-cap");

        // got function from package variables
        let malloc_function = self
            .current_package
            .get_pkg_var("malloc".to_string(), false)
            .unwrap()
            .llvm_func_value
            .unwrap();

        /*
        // item size of slice
        let item_type_size = compile_composite_literal_type
            .to_compile_slice_type()
            .unwrap()
            .type_size;

        // calculate memory size we want to malloc

        let malloc_size = init_cap as u64 * item_type_size;
        let malloc_size_args = vec![BasicMetadataValueEnum::from(
            self.context
                .i64_type()
                .const_int(malloc_size, false)
                .as_basic_value_enum(),
        )];
         */

        // calculate the size of slice struct by llvm
        // and multiply the size value and init_cap
        let type_size_value = compile_composite_literal_type
            .to_compile_slice_type()
            .unwrap()
            .llvm_type
            .size_of()
            .unwrap()
            .const_mul(self.context.i64_type().const_int(init_cap as u64, false));

        // call malloc function at runtime
        let malloc_called = self.builder.build_call(
            malloc_function,
            &[BasicMetadataValueEnum::from(
                type_size_value.as_basic_value_enum(),
            )],
            "malloc-call",
        );

        // get value returning from malloc function
        let malloc_slice_space_raw_pointer = malloc_called
            .try_as_basic_value()
            .left()
            .expect("call malloc failed");

        // get element type of slice and convert it to it's pointer type
        let slice_item_llvm_pointer_type = compile_composite_literal_type
            .to_compile_slice_type()
            .unwrap()
            .item_llvm_type
            .ptr_type(AddressSpace::Generic);

        // convert memory type of the returning of malloc function to the element pointer type of slice
        // for example, the returning type of malloc function is i8*
        // but it must be converted to int* before use it
        let bit_cast_malloc_raw = self.builder.build_pointer_cast(
            malloc_slice_space_raw_pointer.into_pointer_value(),
            slice_item_llvm_pointer_type,
            "bitcast",
        );

        // store the value of field pointer in slice struct to slice value
        self.builder
            .build_store(content_pointer_filed.unwrap(), bit_cast_malloc_raw);

        // store filed offset
        self.builder.build_store(
            content_offset_filed.unwrap(),
            self.context.i32_type().const_int(0, false),
        );

        // store filed len
        self.builder.build_store(
            content_len_filed.unwrap(),
            self.context.i32_type().const_int(0, false),
        );

        // store field cap
        self.builder.build_store(
            content_cap_filed.unwrap(),
            self.context.i32_type().const_int(init_cap as u64, false),
        );
    }

    fn runtime_panic(&self, code_block: BasicBlock, message: String) {
        self.builder.position_at_end(code_block);
        let panic_string = "runtime panic:".to_string() + &*message + &*"\n".to_string();
        // let panic_string_value = self.context.const_string(panic_string.as_ref(), true);
        let global_message = self.builder.build_global_string_ptr(&*panic_string, "str");

        let print_function = self
            .current_package
            .get_pkg_var("printf".parse().unwrap(), false)
            .unwrap()
            .llvm_func_value
            .unwrap();

        let string_i8_ptr = unsafe {
            self.builder.build_gep(
                global_message.as_pointer_value(),
                &[
                    // self.context.i32_type().const_zero(),
                    self.context.i32_type().const_zero(),
                ],
                "",
            )
        };

        // call printf function to print panic message
        self.builder.build_call(
            print_function,
            &[BasicMetadataValueEnum::from(
                string_i8_ptr.as_basic_value_enum(),
            )],
            "",
        );

        // call 'exit' function with return value 0 to exit current function
        // can make the procession step easily
        let exit_function = self
            .current_package
            .get_pkg_var("exit".parse().unwrap(), false)
            .unwrap()
            .llvm_func_value
            .unwrap();
        self.builder.build_call(
            exit_function,
            &[BasicMetadataValueEnum::from(
                self.context
                    .i32_type()
                    // exit with `exit code 0` instead of `exit code 1` to make things easy
                    .const_int(0, false)
                    .as_basic_value_enum(),
            )],
            "",
        );
    }
}
