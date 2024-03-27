#![allow(unused)]

use crate::analysis::mlir::MlirTypeKind;
use crate::parser::ast::AbstractSyntaxTree;
use crate::util::error::{CompilerError, CompilerWarning};
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

mod analysis;
mod lexer;
mod parser;
mod util;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI frontend and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't have a compiler.

fn main() {}

/// Contains integration tests for the components and their cohesion
#[cfg(test)]
mod tests {

    use crate::analysis::SharedReporter;
    use crate::parser::ast::{AbstractSyntaxTree, Expression, InitDeclaration};
    use crate::parser::ParseResult;
    use crate::util::error::{CompilerError, CompilerWarning};
    use crate::util::{CompilerResult, Locatable};
    use crate::{analysis, lexer, parser};
    use std::cell::RefCell;
    use std::env::var;
    use std::panic::catch_unwind;
    use std::path::PathBuf;
    use std::rc::Rc;

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
        let global_validator = analysis::GlobalValidator::new(result);
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
            Expression::Unary(crate::parser::ast::UnaryOp::AddressOf, _) => (),
            _ => panic!("Incorrect Parse Tree: Expected unary operator AddressOf '&'"),
        };
    }
}
