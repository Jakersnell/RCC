#![allow(unused)]

mod ast;
mod ast_pretty_print;
mod codegen;
mod error;
mod hlir;
mod lex;
mod lowering;
mod mlir;
mod parse;
mod str_intern;
mod tokens;
mod util;
mod validation;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't a compiler.

const DEBUG: bool = false;

fn main() {
    let source = std::fs::read_to_string("_c_test_files/sizeof.c").unwrap();
    let lexer = lex::Lexer::new(source);
    let program = util::Program::new("sizeof.c".to_string());
    let parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let body = program.body.unwrap().unwrap();
    for decl in body {
        println!("{}", decl);
    }
}

#[test]
fn check_test_files_are_ok() {
    let tests = [
        "structs.c",
        "comments_and_functions.c",
        "sizeof.c",
        "const_ptr.c",
    ];

    for test in tests {
        let source = std::fs::read_to_string(format!("_c_test_files/{}", test)).unwrap();
        let lexer = lex::Lexer::new(source);
        let program = util::Program::new(test.to_string());
        let parser = parse::Parser::from_lexer(program, lexer);
        let program = parser.parse();
        let body = program.body.unwrap();
        assert!(body.is_ok(), "Error in file: {}", test);
        let body = body.unwrap();
        assert!(!body.is_empty(), "No body in file: {}", test);
    }
}
