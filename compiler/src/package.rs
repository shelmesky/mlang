use crate::types as compile_types;

use crate::value as compile_value;
use std::collections::BTreeMap;

#[derive(Clone)]
pub struct Pkg<'a> {
    name: String,
    vars: BTreeMap<String, compile_value::Value<'a>>,
    types: BTreeMap<String, compile_types::Type<'a>>,
}

impl<'a> Pkg<'a> {
    pub fn new(name: String) -> Self {
        Pkg {
            name,
            vars: Default::default(),
            types: Default::default(),
        }
    }

    pub fn define_pkg_var(&mut self, name: String, val: compile_value::Value<'a>) {
        let mut is_function: bool = false;
        if val.compile_type.to_compile_function_type().is_some() {
            is_function = true;
        }

        if self.get_pkg_var(name.clone(), false).is_some() {
            if is_function {
                panic!(
                    "{}",
                    format!(
                        "there is a function with same name '{}' already in symbol table",
                        name.clone()
                    )
                )
            } else {
                panic!(
                    "{}",
                    format!(
                        "there is a variable with same name '{}' already in symbol table",
                        name.clone()
                    )
                )
            }
        }
        self.vars.insert(name, val);
    }

    pub fn get_pkg_var(
        &self,
        name: String,
        _is_same_package: bool,
    ) -> Option<&compile_value::Value<'a>> {
        self.vars.get(&name)
    }

    pub fn define_pkg_type(&mut self, name: String, ty: compile_types::Type<'a>) {
        self.types.insert(name, ty);
    }

    // use moved Self
    pub fn get_pkg_type(self, name: String, _is_same_package: bool) -> compile_types::Type<'a> {
        let pkg_type = self.types.get(&name);
        match pkg_type {
            None => {
                panic!("{}", format!("get_pkg_type can not find type {}", name))
            }
            Some(v) => v.clone(),
        }
    }
}
