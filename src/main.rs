#![allow(unused)]

use crate::util::error::{CompilerError, CompilerWarning, ErrorReporter};
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

const PRETTY: bool = false;

fn main() {}

#[cfg(test)]
mod tests {
    use crate::parser::ast::InitDeclaration;
    use crate::parser::ParseResult;
    use crate::util::error::{CompilerError, CompilerWarning, ErrorReporter};
    use crate::util::{CompilerResult, Locatable};
    use crate::{lexer, parser};
    use std::cell::RefCell;
    use std::rc::Rc;
    struct MockReporter {
        errors: Vec<CompilerError>,
        warnings: Vec<CompilerWarning>,
        status: Result<(), ()>,
    }

    impl MockReporter {
        fn new() -> Self {
            Self {
                errors: Vec::new(),
                warnings: Vec::new(),
                status: Ok(()),
            }
        }
    }

    impl ErrorReporter for MockReporter {
        fn get_status(&self) -> Result<(), ()> {
            self.status
        }

        fn report_error(&mut self, error: CompilerError) {
            self.errors.push(error);
            self.status = Err(());
        }

        fn report_warning(&mut self, warning: CompilerWarning) {
            self.warnings.push(warning);
        }

        fn get_errors(&self) -> &Vec<CompilerError> {
            &self.errors
        }

        fn get_warnings(&self) -> &Vec<CompilerWarning> {
            &self.warnings
        }
    }

    #[test]
    fn remove_this() {
        let source = "
        int main() {
            int x = 0;
            return 0;
        }
        ";
        let reporter = Rc::new(RefCell::new(MockReporter::new()));
        let lexer = lexer::Lexer::new(reporter.clone(), source.into());
        let tokens = lexer.collect::<Vec<_>>();
        println!("{:#?}", tokens);
        // let mut parser = parser::Parser::new(reporter.clone(), Box::new(lexer));
        // let results = parser
        //     .parse_all()
        //     .map_err(|_| std::mem::take(&mut reporter.borrow_mut().errors));
        // println!("{:#?}", results);
    }

    fn run_test_on_file(path: &str) -> Result<Vec<Locatable<InitDeclaration>>, Vec<CompilerError>> {
        let source = std::fs::read_to_string(path).expect("Could not read file.");
        let reporter = Rc::new(RefCell::new(MockReporter::new()));
        let lexer = lexer::Lexer::new(reporter.clone(), source.into());
        let parser = parser::Parser::new(reporter.clone(), Box::new(lexer));
        parser
            .parse_all()
            .map_err(|_| std::mem::take(&mut reporter.borrow_mut().errors))
    }

    #[test]
    fn test_should_succeed_files() {
        let tests = [
            "structs.c",
            "comments_and_functions.c",
            "sizeof.c",
            "const_ptr.c",
            "member_access.c",
            "fuzz.c",
            "control_flow_analysis.c",
        ];

        for test in tests {
            let result = run_test_on_file(&format!("_c_test_files/should_succeed/{}", test));
            match result {
                Ok(body) => {
                    assert!(!body.is_empty(), "No body in file: {}", test);
                }
                Err(errors) => panic!("File {} errors:\n{:#?}", test, errors),
            }
        }
    }

    #[test]
    fn test_should_fail_files() {
        let tests = [
            "declaration_in_expression.c",
            "function_in_struct.c",
            "if_in_condition.c",
        ];

        for test in tests {
            let result = run_test_on_file(&format!("_c_test_files/should_fail/{}", test));
            match result {
                Err(errors) => {
                    assert!(!errors.is_empty(), "No errors in file: {}", test);
                }
                Ok(body) => panic!("File {} succeeded:\n{:#?}", test, body),
            }
        }
    }
}
