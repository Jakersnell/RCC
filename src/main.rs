#![allow(unused)]

use std::sync::Arc;

mod ast;
mod ast_pretty_print;
mod codegen;
mod error;
mod lex;
mod lowering;
mod parse;
mod str_intern;
mod tokens;
mod util;
mod validation;

/// The main entry point for the program.
/// This will be the last to be completed as all the other components of the program
/// need to function for the main to be usable.
/// Cant compile a program if you don't have a lexer or parser.
fn main() {
    let source = "
int x = 1 + 4 / 2 + y * 20;
double y = 2.3434;

double take_and_return_double(double d) {
    return d;
}
"; // test string
    let lexer = lex::Lexer::new(source.to_string());
    let program = util::Program::new("test".to_string());
    let parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let body = program.body.unwrap().unwrap();
    for node in body {
        println!("{}", node);
    }
}
