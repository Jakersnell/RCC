#![allow(unused)]

use crate::analysis::hlir::HlirTypeKind;
use crate::parser::ast::AbstractSyntaxTree;
use crate::util::error::{CompilerError, CompilerWarning};
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

mod analysis;
mod lexer;
mod parser;
mod util;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't have a compiler.

fn main() {
    let src = "

    int main() {
        int x = 1;

        return 0;
    }
    ";
    let lexer = lexer::Lexer::new(src.into());
    let parser = parser::Parser::new(lexer);
    let result = parser.parse_all().unwrap();
    let global_validator = analysis::GlobalValidator::new(AbstractSyntaxTree::new(
        result.into_iter().map(|dec| dec.value).collect(),
    ));
    let hlir = global_validator.validate();
    println!("{:#?}", hlir);
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expression, InitDeclaration};
    use crate::parser::ParseResult;
    use crate::util::error::{CompilerError, CompilerWarning};
    use crate::util::{CompilerResult, Locatable};
    use crate::{lexer, parser};
    use std::cell::RefCell;
    use std::env::var;
    use std::path::PathBuf;
    use std::rc::Rc;

    fn get_file_paths(path: &PathBuf) -> std::io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();
        let res_paths = std::fs::read_dir(path)?.map(|res| res.map(|entry| entry.path()));
        for path in res_paths {
            paths.push(path?);
        }
        Ok(paths)
    }

    fn run_test_on_file(
        path: &PathBuf,
    ) -> Result<Vec<Locatable<InitDeclaration>>, Vec<CompilerError>> {
        let source = std::fs::read_to_string(path).expect("Could not read file.");
        let lexer = lexer::Lexer::new(source.into());
        let parser = parser::Parser::new(lexer);
        parser.parse_all()
    }

    #[test]
    fn test_should_succeed_files() {
        let tests = get_file_paths(&"_c_test_files/should_succeed".into()).unwrap();

        for test in tests {
            let result = run_test_on_file(&test);
            match result {
                Ok(body) => {
                    assert!(!body.is_empty(), "No body in file: {:#?}", test);
                }
                Err(errors) => panic!("File {:#?} errors:\n{:#?}", test, errors),
            }
        }
    }

    #[test]
    fn test_should_fail_files() {
        let tests = get_file_paths(&"_c_test_files/should_fail".into()).unwrap();

        for test in tests {
            let result = run_test_on_file(&test);
            match result {
                Err(errors) => {
                    assert!(!errors.is_empty(), "No errors in file: {:#?}", test);
                }
                Ok(body) => panic!("File {:#?} succeeded", test),
            }
        }
    }

    #[test]
    fn test_subscripting_order() {
        let src = "int y = &x[3];";
        let lexer = lexer::Lexer::new(src.into());
        let parser = parser::Parser::new(lexer);
        let result = parser.parse_all().unwrap();
        let var_dec = &**result.first().expect("Expected non-empty result.");

        let variable = match var_dec {
            InitDeclaration::Declaration(variable) => &**variable,
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
