use anyhow;

use inkwell::{FloatPredicate, IntPredicate};

use crate::types as compile_types;

use crate::compiler;
use crate::value::Value;

use inkwell::values::{BasicValue, BasicValueEnum};

use parser::ast::BinaryExpression;
use parser::token::Operator;

fn generate_compile_value<'a: 'b, 'b>(
    left_value: Value<'a>,
    llvm_value: BasicValueEnum<'a>,
) -> Value<'b> {
    let pointer_element_type = left_value.clone().compile_type;
    let pointer_type = compile_types::Type::from_pointer_type(pointer_element_type);

    Value::from_compiled_value(pointer_type, llvm_value, None, false, false)
}

impl<'a, 'ctx> compiler::Compiler<'a, 'ctx> {
    pub fn compile_binary<'x, 'y: 'x>(
        &mut self,
        binary_expression: BinaryExpression,
    ) -> Result<Value<'ctx>, anyhow::Error> {
        let left_expr = *binary_expression.left;
        let left_value = self
            .compile_expression(left_expr)
            .expect("Expression::Binary left_expr compiled failed");

        let right_expr = *binary_expression.right;
        let right_value = self
            .compile_expression(right_expr)
            .expect("Expression::Binary right_expr compiled failed");

        let mut left_enum_value = left_value.llvm_value;
        let mut right_enum_value = right_value.llvm_value;

        if left_enum_value.is_pointer_value() {
            left_enum_value = self
                .builder
                .build_load(left_enum_value.into_pointer_value(), "");
        }

        if right_enum_value.is_pointer_value() {
            right_enum_value = self
                .builder
                .build_load(right_enum_value.into_pointer_value(), "");
        }

        match binary_expression.op {
            Operator::Add => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = self.builder.build_int_add(
                        left_enum_value.into_int_value(),
                        right_enum_value.into_int_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && left_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_add(
                        left_enum_value.into_float_value(),
                        left_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }

            Operator::Equal => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        left_enum_value.into_int_value(),
                        right_enum_value.into_int_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && right_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::OEQ,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            Operator::NotEqual => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = self.builder.build_int_compare(
                        IntPredicate::NE,
                        left_enum_value.into_int_value(),
                        right_enum_value.into_int_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && right_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            Operator::Greater => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = if !left_value
                        .compile_type
                        .to_compile_int_type()
                        .unwrap()
                        .signed
                    {
                        self.builder.build_int_compare(
                            IntPredicate::UGT,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    } else {
                        self.builder.build_int_compare(
                            IntPredicate::SGT,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    };

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && right_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::OGT,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            Operator::GreaterEqual => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = if !left_value
                        .compile_type
                        .to_compile_int_type()
                        .unwrap()
                        .signed
                    {
                        self.builder.build_int_compare(
                            IntPredicate::UGE,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    } else {
                        self.builder.build_int_compare(
                            IntPredicate::SGE,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    };

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && right_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::OGE,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            Operator::Less => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = if !left_value
                        .compile_type
                        .to_compile_int_type()
                        .unwrap()
                        .signed
                    {
                        self.builder.build_int_compare(
                            IntPredicate::ULT,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    } else {
                        self.builder.build_int_compare(
                            IntPredicate::SLT,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    };

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && right_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::OLT,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            Operator::LessEqual => {
                if left_enum_value.is_int_value() && right_enum_value.is_int_value() {
                    let expr_value = if !left_value
                        .compile_type
                        .to_compile_int_type()
                        .unwrap()
                        .signed
                    {
                        self.builder.build_int_compare(
                            IntPredicate::ULE,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    } else {
                        self.builder.build_int_compare(
                            IntPredicate::SLE,
                            left_enum_value.into_int_value(),
                            right_enum_value.into_int_value(),
                            "",
                        )
                    };

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else if left_enum_value.is_float_value() && left_enum_value.is_float_value() {
                    let expr_value = self.builder.build_float_compare(
                        FloatPredicate::OLE,
                        left_enum_value.into_float_value(),
                        right_enum_value.into_float_value(),
                        "",
                    );

                    Ok(generate_compile_value(
                        left_value.clone(),
                        expr_value.as_basic_value_enum(),
                    ))
                } else {
                    Err(anyhow::Error::msg(
                        "add operator only support int and float type",
                    ))
                }
            }
            _ => Err(anyhow::Error::msg(
                "Expression::Binary got unknown operator",
            )),
        }
    }
}
