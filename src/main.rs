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
fn main() {}
