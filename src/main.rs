#![allow(unused)]
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

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::ast::InitDeclaration;
//     use crate::util::Compiler;
//     use crate::util::CompilerResult;
//
//     fn run_test_on_file(path: &str) -> CompilerResult<Vec<InitDeclaration>> {
//         let source = std::fs::read_to_string(path).expect("Could not read file.");
//         let lexer = lex::Lexer::new(source);
//         let program = util::Compiler::new(path);
//         let parser = parse::Parser::from_lexer(program, lexer);
//         parser.parse().body.unwrap()
//     }
//
//     #[test]
//     fn test_should_succeed_files() {
//         let tests = [
//             "structs.c",
//             "comments_and_functions.c",
//             "sizeof.c",
//             "const_ptr.c",
//             "member_access.c",
//             "fuzz.c",
//             "control_flow_analysis.c",
//         ];
//
//         for test in tests {
//             let result = run_test_on_file(&format!("_c_test_files/should_succeed/{}", test));
//             match result {
//                 Ok(body) => {
//                     assert!(!body.is_empty(), "No body in file: {}", test);
//                 }
//                 Err(errors) => panic!("File {} errors:\n{:#?}", test, errors),
//             }
//         }
//     }
//
//     #[test]
//     fn test_should_fail_files() {
//         let tests = [
//             "declaration_in_expression.c",
//             "function_in_struct.c",
//             "if_in_condition.c",
//         ];
//
//         for test in tests {
//             let result = run_test_on_file(&format!("_c_test_files/should_fail/{}", test));
//             match result {
//                 Err(errors) => {
//                     assert!(!errors.is_empty(), "No errors in file: {}", test);
//                 }
//                 Ok(body) => panic!("File {} succeeded:\n{:#?}", test, body),
//             }
//         }
//     }
// }
