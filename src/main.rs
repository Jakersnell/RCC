#![allow(unused)]

use std::arch::aarch64::int16x4_t;

use arcstr::ArcStr;
use lex::Lexer;
use tokens::{TokenKind, TokenProblem};
mod lex;
mod tokens;

fn main() {
    let test = "0xffffffffffffffffffffffffff";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.next();
    println!("{:?}", token);
}

