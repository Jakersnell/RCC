#![allow(unused)]

use std::sync::Arc;

mod ast;
mod ast_pretty_print;
mod codegen;
mod error;
mod hir;
mod lex;
mod lowering;
mod parse;
mod str_intern;
mod tokens;
mod util;
mod validation;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't a compiler.

static DEBUG: bool = false;

fn main() {
    let source = "
// int x = 1 + 4 / 2 + y * 20; this is in a comment
/*
double y = 2.3434; this is also in a comment, it will be ignored by the lexer
*/

double take_and_return_double(double d) {
    int new_double = d * 2 + 1;
    return d;
}

int x = 4;
double y;

int main() {
    y = y + x;
    return 0;
}
"; // test string
    let lexer = lex::Lexer::new(source.to_string());
    let program = util::Program::new("test".to_string());
    let parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let body = program.body.unwrap().unwrap();
    if DEBUG {
        println!("{:#?}", body);
    } else {
        for node in body {
            println!("{}", node);
        }
    }
}
