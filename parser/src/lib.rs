//#![feature(exclusive_range_pattern)]
//#![feature(assert_matches)]
//#![feature(bool_to_option)]
//#![feature(trait_alias)]
//#![feature(custom_inner_attributes)]
//#![feature(stmt_expr_attributes)]

use std::collections::HashMap;
use std::ffi::OsStr;

mod error;
mod parser;
mod scanner;

pub mod ast;
pub mod token;

pub use crate::parser::Parser;

/// represent the offset of a `Token` relative to the beginning of source code
pub type Pos = usize;

/// a tuple of `(Pos, Token)`
pub type PosTok = (Pos, token::Token);

pub use error::Error;

/// standard parser result
pub type Result<T> = core::result::Result<T, Error>;

/// parse source code to `ast::File`
pub fn parse_source<S: AsRef<str>>(source: S) -> Result<ast::File> {
    parser::Parser::from(source).parse_file()
}

/// parse source code from given path to  `ast::File`
pub fn parse_file<P: AsRef<std::path::Path>>(path: P) -> Result<ast::File> {
    parser::Parser::from_file(path)?.parse_file()
}

/// parse a directory to packages
pub fn parse_dir<P: AsRef<std::path::Path>>(dir_path: P) -> Result<HashMap<String, ast::Package>> {
    let go = &OsStr::new("go");
    let mut result = HashMap::new();
    for file in std::fs::read_dir(&dir_path)? {
        let path = file?.path();
        if path.extension() == Some(go) {
            let file = parse_file(&path)?;
            result
                .entry(String::from(&file.pkg_name.name))
                .or_insert(ast::Package {
                    // FIXME: here will be executed in every loop
                    path: dir_path.as_ref().into(),
                    files: HashMap::new(),
                })
                .files
                .insert(path, file);
        }
    }

    Ok(result)
}
