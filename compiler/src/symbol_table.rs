use crate::types as compile_type;
use crate::value as compile_value;
use anyhow::Result;
use std::borrow::BorrowMut;
use std::cell::RefCell;

use inkwell::values as llvm_values;
use std::collections::BTreeMap;

#[derive(Clone)]
pub struct SymbolTable<'a> {
    table: Vec<BTreeMap<String, compile_value::Value<'a>>>,
    pub string_constants_table: BTreeMap<String, llvm_values::GlobalValue<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self {
            table: Default::default(),
            string_constants_table: Default::default(),
        }
    }

    pub fn get_var(&self, name: String) -> Result<&compile_value::Value<'a>, anyhow::Error> {
        let table_length = self.table.len();
        let mut i = table_length - 1;
        // search in nested symbol table by loop
        loop {
            if i == 0 {
                break;
            };

            let current_table_option = self.table.get(i);

            i -= 1;

            match current_table_option {
                None => continue, // skip empty table
                Some(current_table) => {
                    let value = current_table.get(name.as_str());
                    // found symbol
                    if value.is_some() {
                        return Ok(value.unwrap());
                    }
                }
            }
        }

        Err(anyhow::Error::msg(format!("can not find symbol {}", name)))
    }

    // insert variable into symbol table
    // @name: variable name
    // @is_variable: if symbol is a variable
    // @compile_type: compile time type of symbol
    // @llvm_value_enum: llvm value of symbol
    pub fn set_var<'x: 'a>(
        &mut self,
        name: String,
        is_variable: bool,
        compile_type: compile_type::Type<'x>,
        llvm_value_enum: llvm_values::BasicValueEnum<'x>,
    ) {
        let compile_value = compile_value::Value {
            compile_type,
            llvm_value: llvm_value_enum,
            llvm_func_value: None,
            is_variable,
            is_global: false,
        };
        self.do_set_var(name, compile_value);
    }

    fn do_set_var(&mut self, name: String, val: compile_value::Value<'a>) {
        let table_length = self.table.len();
        let current_table = self.table.get_mut(table_length - 1);
        match current_table {
            Some(table) => {
                table.insert(name, val);
            }
            None => {
                panic!("symbol table is empty")
            }
        }
    }

    pub fn push_variables_stack(&mut self) {
        let new_table = BTreeMap::new();
        self.table.push(new_table);
    }

    pub fn pop_variables_stack(&mut self) {
        self.table.pop();
    }

    pub fn length(&self) -> usize {
        let table_length = self.table.len();
        let current_table = self.table.get(table_length - 1).unwrap();
        current_table.len()
    }
}

#[derive(Clone)]
pub struct ReturnValues<'a> {
    list: RefCell<Vec<Option<llvm_values::BasicValueEnum<'a>>>>,
}

impl<'a> ReturnValues<'a> {
    pub fn new() -> Self {
        Self {
            list: Default::default(),
        }
    }

    pub fn push_value(&mut self, value: Option<llvm_values::BasicValueEnum<'a>>) {
        self.list.borrow_mut().push(value)
    }

    pub fn pop_value(&mut self) -> Option<llvm_values::BasicValueEnum<'a>> {
        self.list
            .borrow_mut()
            .pop()
            .ok_or("ReturnValues pop_value() failed")
            .unwrap()
    }

    pub fn length(&self) -> usize {
        self.list.borrow().len()
    }

    pub fn clear(&mut self) {
        self.list.borrow_mut().clear();
    }
}
