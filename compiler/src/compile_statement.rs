use std::ops::Deref;

use crate::compiler::Compiler;
use parser::ast::{DeclStmt, Expression, Statement};
use parser::token::Keyword;

use inkwell::values::{BasicValue, BasicValueEnum};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_if_statement(&mut self, if_stmt: &parser::ast::IfStmt) {
        // into a [if] scope, make new symbol table
        self.symbol_table.push_variables_stack();

        // compile condition expression
        let cond_value = self.compile_expression(if_stmt.cond.clone()).unwrap();

        // create after block, it means code after if block
        let after_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if-after");

        // if condition is true
        let true_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if-true");
        // if condition is false, default set to false block
        let mut false_block = after_block;

        // save after block
        self.context_condition_after.push(after_block);

        // if has else branch
        // set false block to real false block, instead of after block
        if if_stmt.else_.is_some() {
            false_block = self
                .context
                .append_basic_block(self.current_function.unwrap(), "of-false")
        }

        // conditional branch:
        // if condition is true, branch to true_block
        // else branch to false_block
        self.builder.build_conditional_branch(
            cond_value.llvm_value.into_int_value(),
            true_block,
            false_block,
        );

        // compile true block
        self.builder.position_at_end(true_block);
        let if_stmt_body = if_stmt.body.list.clone();
        for stmt in if_stmt_body {
            self.compile_statement(&stmt)
        }

        // jump to after_block if no terminator has been set
        // (such as return statement)
        if true_block.get_terminator().is_none() {
            self.builder.build_unconditional_branch(after_block);
        }

        // leave [if] scope, release symbol table
        self.symbol_table.pop_variables_stack();

        // if code has else branch, compile it
        let if_stmt_else = if_stmt.else_.clone();
        if if_stmt_else.is_some() {
            // into a [else] scope, make new symbol table
            self.symbol_table.push_variables_stack();

            self.builder.position_at_end(false_block);
            let else_statement = if_stmt_else.unwrap().deref().clone();
            self.compile_statement(&else_statement);

            // jump to after_block if no terminator has been set
            // (such as return statement)
            if false_block.get_terminator().is_none() {
                self.builder.build_unconditional_branch(after_block);
            }

            // leave [else] scope, release symbol table
            self.symbol_table.pop_variables_stack();
        }

        self.builder.position_at_end(after_block);

        self.context_condition_after.pop();

        if !self.context_condition_after.is_empty() {
            self.builder
                .build_unconditional_branch(*self.context_condition_after.last().unwrap());
        }
    }

    pub fn compile_for_statement(&mut self, for_stmt: &parser::ast::ForStmt) {
        self.symbol_table.push_variables_stack();

        let current_function = self.current_function.expect("not in a function");

        // compile before loop statement
        let before_loop_stmt_option = for_stmt.clone().init;
        if before_loop_stmt_option.is_some() {
            self.compile_statement(before_loop_stmt_option.unwrap().deref())
        }

        // check condition block
        let check_cond_block = self
            .context
            .append_basic_block(current_function, "for-cond");

        // loop body block
        let loop_body_block = self
            .context
            .append_basic_block(current_function, "for-body");
        let loop_after_body_block = self
            .context
            .append_basic_block(current_function, "for-after-body");

        // after loop block
        let after_loop_block = self
            .context
            .append_basic_block(current_function, "for-after");

        // save after_look_block and loop_after_body_block
        // so 'break' and 'continue' can jump to correct destination
        self.context_loop_break.push(after_loop_block);
        self.context_loop_continue.push(loop_after_body_block);

        // jump from beforeLoop to checkCond
        self.builder.build_unconditional_branch(check_cond_block);

        // compile condition block
        self.builder.position_at_end(check_cond_block);
        let cond_statement = for_stmt.clone().cond.unwrap().deref().clone();
        if let Statement::Expr(expr_stmt) = cond_statement {
            let cond_value = self.compile_expression(expr_stmt.expr);
            self.builder.build_conditional_branch(
                cond_value.unwrap().llvm_value.into_int_value(),
                loop_body_block,
                after_loop_block,
            );
        }

        // compile loop body
        self.builder.position_at_end(loop_body_block);
        let cloned_loop_body = for_stmt.clone().body.list;
        for stmt in cloned_loop_body {
            self.compile_statement(&stmt);
        }
        // jump to after body
        self.builder
            .build_unconditional_branch(loop_after_body_block);

        // after body block
        self.builder.position_at_end(loop_after_body_block);
        let post_statement = for_stmt.clone().post.unwrap().deref().clone();
        self.compile_statement(&post_statement);
        self.builder.build_unconditional_branch(check_cond_block);

        self.builder.position_at_end(after_loop_block);

        // pop break and continue
        self.context_loop_break.pop();
        self.context_loop_continue.pop();

        self.symbol_table.pop_variables_stack();
    }

    pub fn compile_assign_statement(&mut self, assign_stmt: &parser::ast::AssignStmt) {
        if assign_stmt.left.len() != assign_stmt.right.len() {
            panic!("Statement::Assign length of two sides are not equal")
        }

        for (idx, left_expr) in assign_stmt.left.iter().enumerate() {
            let right_expr = assign_stmt.right.get(idx).unwrap();

            let left_compile_value = self.compile_expression(left_expr.clone()).unwrap();
            let right_compile_value = self.compile_expression(right_expr.clone()).unwrap();

            if !left_compile_value.llvm_value.is_pointer_value() {
                panic!("Statement::Assign left is not pointer value")
            }

            let mut right_enum_value = right_compile_value.llvm_value.as_basic_value_enum();

            // load the pointer of right value is a variable
            if right_compile_value.is_variable {
                self.builder.build_store(
                    left_compile_value.llvm_value.into_pointer_value(),
                    self.builder
                        .build_load(right_enum_value.into_pointer_value(), ""),
                );
            } else {
                self.builder.build_store(
                    left_compile_value.llvm_value.into_pointer_value(),
                    right_enum_value.as_basic_value_enum(),
                );
            }
        }
    }

    pub fn compile_return_statement(&mut self, return_stmt: &parser::ast::ReturnStmt) {
        // if return
        if !return_stmt.ret.is_empty() {
            let return_expression = return_stmt.clone().ret.get(0).unwrap().clone();
            let compiled_return_value = self
                .compile_expression(return_expression)
                .expect("compile_expression failed");

            let basic_value =
                Compiler::get_basic_value_from_value_enum(compiled_return_value.llvm_value);

            self.builder.build_return(Some(basic_value.deref()));

            // save return values
            // when function compilation was finished,
            // use return values to generate return instruction
            self.return_values
                .push_value(Some(compiled_return_value.llvm_value));
        } else {
            self.builder.build_return(None);
            self.return_values.push_value(None);
        }
    }

    pub fn compile_branch_statement(&mut self, branch_stmt: &parser::ast::BranchStmt) {
        let keyword = branch_stmt.key;
        match keyword {
            Keyword::Break => {
                let loop_break_block = self.context_loop_break.last().unwrap();
                self.builder.build_unconditional_branch(*loop_break_block);
            }
            Keyword::Continue => {
                let loop_continue_block = self.context_loop_continue.last().unwrap();
                self.builder
                    .build_unconditional_branch(*loop_continue_block);
            }
            _ => {}
        }
    }

    pub fn compile_local_variable_declaration(
        &mut self,
        declaration_list: &parser::ast::Decl<parser::ast::VarSpec>,
    ) {
        for variables in declaration_list.specs.iter() {
            // check if these variables have type
            let variables_type_option = variables.typ.clone();
            let variables_type = match variables_type_option {
                Some(top_variables_type) => top_variables_type,
                None => {
                    panic!("variable must have type")
                }
            };

            // convert type of variable from ast type to compile type
            let compile_variables_type =
                Compiler::convert_type(self, self.current_package.clone(), variables_type);

            // if variables declaration statement have initial value, build it to llvm const
            // but we must check if the length of initial values list is equal to the length of variables list
            // or if the length of initial values list is 1, also it's ok
            // if both of those are not, should report compile error
            if variables.values.len() > 1 && variables.values.len() != variables.name.len() {
                panic!("size of initial values list is not correct")
            }

            if variables.values.len() == 1 {
                // only one initial value

                let initial_value = variables.values.first().unwrap().clone();

                // compile initial value
                let compile_initial_value = { self.compile_expression(initial_value).unwrap() };

                for variable in variables.name.iter() {
                    // get variable name
                    let variable_name = variable.name.clone();

                    // allocate a llvm value for variable
                    let llvm_value = self.builder.build_alloca(
                        compile_variables_type.llvm_basic_type_enum().unwrap(),
                        &*variable_name,
                    );
                    self.store_value(llvm_value, compile_initial_value.llvm_value);

                    self.symbol_table.set_var(
                        variable_name,
                        true, // insert variable into symbol table
                        compile_variables_type.clone(),
                        llvm_value.as_basic_value_enum(),
                    );
                }
            } else {
                // empty or more than one initial values
                // just alloca a variable, because we dont have initial value
                for (idx, variable) in variables.name.iter().enumerate() {
                    // get variable name
                    let variable_name = variable.name.clone();

                    // allocate a llvm value for variable
                    let llvm_left_value = self.builder.build_alloca(
                        compile_variables_type.llvm_basic_type_enum().unwrap(),
                        &*variable_name,
                    );

                    // if variable declaration has initial value
                    let variable_values_option = variables.values.get(idx);
                    if variable_values_option.is_some() {
                        // get ast initial value
                        let ast_right_value = variable_values_option.unwrap().clone();
                        // compile ast initial value
                        let compile_right_value = self.compile_expression(ast_right_value).unwrap();

                        // store value to variable
                        self.store_value(llvm_left_value, compile_right_value.llvm_value);
                    }

                    self.symbol_table.set_var(
                        variable_name,
                        true, // insert variable into symbol table
                        compile_variables_type.clone(),
                        llvm_left_value.as_basic_value_enum(),
                    );
                }
            }

            // println!("variable: {:?}", variables);
        }
    }
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Go(_) => {}
            Statement::If(if_stmt) => self.compile_if_statement(if_stmt),
            Statement::For(for_stmt) => self.compile_for_statement(for_stmt),
            Statement::Send(_) => {}
            Statement::Expr(expr) => {
                self.compile_expression(expr.clone().expr);
            }
            Statement::Block(block_stmt) => {
                let block_stmt_list = block_stmt.list.clone();
                for stmt in block_stmt_list {
                    self.compile_statement(&stmt)
                }
            }
            Statement::Range(_) => {}
            Statement::Empty(_) => {}
            Statement::Label(_) => {}
            Statement::IncDec(_) => {}
            Statement::Assign(assign_stmt) => self.compile_assign_statement(assign_stmt),
            Statement::Return(return_stmt) => self.compile_return_statement(return_stmt),
            Statement::Branch(branch_stmt) => self.compile_branch_statement(branch_stmt),
            Statement::Switch(_) => {}
            Statement::TypeSwitch(_) => {}
            Statement::Declaration(declaration) => match declaration {
                DeclStmt::Type(_) => {}
                DeclStmt::Const(_) => {}
                DeclStmt::Variable(declaration_list) => {
                    self.compile_local_variable_declaration(declaration_list)
                }
            },
        }
    }
}
