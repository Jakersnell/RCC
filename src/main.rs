#![allow(unused)]

use crate::analysis::Analyzer;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod analysis;
mod codegen;
mod data;
mod lexer;
mod parser;
mod util;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI frontend and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't have a compiler.

static DISPLAY_AST: bool = false;
static OUTPUT_GRAPH: bool = false;
static DISPLAY_MLIR: bool = false;
static PRETTY_PRINT_AST: bool = false;

fn main() {
    let source = std::fs::read_to_string("_c_test_files/should_succeed/basic_blocks.c").unwrap();
    let lexer = Lexer::new(source.into());
    let parser = Parser::new(lexer);
    let analyzer = Analyzer::new(parser.parse_all().unwrap());
    let mlir = analyzer.validate().unwrap();
    for (func_name, func) in mlir.functions.iter() {
        println!("\n-- function '{}' --", func_name);
        let basic_blocks = crate::codegen::pre_construct_blocks(&func.body);
        for block in basic_blocks {
            println!("{}", crate::util::display_utils::indent_string(format!("{}", block), 0, 4));
        }
        println!("--");
    }
}

/// Integration tests
#[cfg(test)]
mod tests {
    use std::panic::catch_unwind;
    use std::path::PathBuf;

    use crate::{analysis, lexer, parser};
    use crate::analysis::SharedReporter;
    use crate::data::ast::{Expression, InitDeclaration};
    use crate::util::error::CompilerError;

    pub(crate) fn get_file_paths(path: &PathBuf) -> std::io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();
        let res_paths = std::fs::read_dir(path)?.map(|res| res.map(|entry| entry.path()));
        for path in res_paths {
            paths.push(path?);
        }
        Ok(paths)
    }

    #[derive(Debug)]
    enum FailReason {
        Parser(Vec<CompilerError>),
        Analyzer(SharedReporter),
    }

    fn run_test_on_file(path: &PathBuf) -> Result<(), FailReason> {
        let source = std::fs::read_to_string(path).expect("Could not read file.");
        let lexer = lexer::Lexer::new(source.into());
        let parser = parser::Parser::new(lexer);
        let result = parser.parse_all().map_err(FailReason::Parser)?;
        let global_validator = analysis::Analyzer::new(result);
        global_validator
            .validate()
            .map(|_| ())
            .map_err(FailReason::Analyzer)
    }

    macro_rules! file_test_assert {
        ($filename:expr, $assertion:expr, $result:expr) => {
            if !$assertion {
                panic!(
                    "Assertion error in '{:#?}': `{:#?}`\n{:#?}",
                    $filename,
                    stringify!($assertion),
                    $result
                )
            }
        };
    }

    #[test]
    fn test_should_succeed_files() {
        get_file_paths(&"_c_test_files/should_succeed".into())
            .unwrap()
            .iter()
            .for_each(|test| {
                match catch_unwind(|| run_test_on_file(test)) {
                    Ok(result) => {
                        file_test_assert!(test, result.is_ok(), result);
                    }
                    Err(err) => {
                        panic!("Panic in thread from file '{:#?}'.", test);
                    }
                };
            });
    }

    #[test]
    fn test_should_fail_files() {
        get_file_paths(&"_c_test_files/should_fail".into())
            .unwrap()
            .iter()
            .for_each(|test| {
                let result = run_test_on_file(test);
                file_test_assert!(test, result.is_err(), result);
            })
    }

    #[test]
    fn test_subscripting_order() {
        let src = "int y = &x[3];";
        let lexer = lexer::Lexer::new(src.into());
        let parser = parser::Parser::new(lexer);
        let result = parser.parse_all().unwrap();
        let var_dec = result.first().expect("Expected non-empty result.");

        let variable = match var_dec {
            InitDeclaration::Declaration(variable) => variable,
            _ => panic!("First element should be a variable declaration!"),
        };

        let init = &**variable
            .initializer
            .as_ref()
            .expect("Expected initializer in variable declaration.");

        match init {
            Expression::Unary(crate::data::ast::UnaryOp::AddressOf, _) => (),
            _ => panic!("Incorrect Parse Tree: Expected unary operator AddressOf '&'"),
        };
    }
}
