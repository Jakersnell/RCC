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

fn main() {}

#[test]
fn test_comments_and_functions_dot_c() {
    let source = std::fs::read_to_string("tests/comments_and_functions.c").unwrap();
    let lexer = lex::Lexer::new(source);
    let program = util::Program::new("comments_and_functions.c".to_string());
    let parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let body = program.body.unwrap().unwrap();
    if cfg!(DEBUG) {
        println!("{:#?}", body);
    } else {
        for node in body {
            println!("{}", node);
        }
    }
}
