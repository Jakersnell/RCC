#![allow(unused)]

mod ast;
mod error;
mod lex;
mod parse;
mod str_intern;
mod tokens;
mod util;

/// The main entry point for the program.
/// This will be the last to be completed as all the other components of the program
/// need to function for the main to be usable.
/// Cant compile a program if you don't have a lexer or parser.
fn main() {
    let program = util::Program::new("main.c".to_string());
    let source = "
2 + 4 * 5 - 6 / 3;
8+3;
"
    .to_string();
    let lexer = lex::Lexer::new(source);
    let mut parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let ast = program.body.unwrap().unwrap();
    for node in ast {
        print!("{}", node);
    }
}
