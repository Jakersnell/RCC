#![allow(unused)]

use std::arch::aarch64::int16x4_t;

use arcstr::ArcStr;
use tokens::{TokenKind, TokenProblem};
mod lex;
mod tokens;

fn main() {
    
}

struct TestLexer<'a> {
    chars: std::str::Chars<'a>,
    current: Option<char>,
    next: Option<char>,
    problems: Vec<TokenProblem>,
}
impl<'a> TestLexer<'a> {
    fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        let problems = Vec::new();
        Self {
            chars,
            current,
            next,
            problems,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current = self.next;
        self.next = self.chars.next();
        self.current
    }

    fn eat_number(&mut self) -> Option<TokenKind> {
        if !self.current.is_some_and(|c| c.is_digit(16)) {
            return None;
        }
        enum State {
            Start,
            Zero,
            Decimal,
            Hex,
            Binary,
            Octal,
            Float,
        }
        let mut state = State::Start;
        let mut number = String::new();
        while let Some(current) = self.current {
            match state {
                State::Start => match current {
                    '0' => {
                        number.push(current);
                        state = State::Zero;
                    }
                    '1'..='9' => {
                        number.push(current);
                        state = State::Decimal;
                    }
                    _ => unreachable!(),
                },
                State::Zero => match current {
                    'x' | 'X' => {
                        number.push(current);
                        state = State::Hex;
                    }
                    'b' | 'B' => {
                        number.push(current);
                        state = State::Binary;
                    }
                    '0'..='7' => {
                        number.push(current);
                        state = State::Octal;
                    }
                    '.' => {
                        number.push(current);
                        state = State::Float;
                    }
                    _ => break,
                },
                State::Decimal => match current {
                    '0'..='9' => number.push(current),
                    '.' => {
                        number.push(current);
                        state = State::Float;
                    }
                    _ => break,
                },
                State::Hex => match current {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => number.push(current), // include a-z to catch invalid hex lits
                    _ => break,
                },
                State::Binary => match current {
                    '0' | '1' => number.push(current),
                    _ => break,
                },
                State::Octal => match current {
                    '0'..='7' => number.push(current),
                    '.' => {
                        number.push(current);
                        state = State::Float;
                    }
                    _ => break,
                },
                State::Float => match current {
                    '0'..='9' => number.push(current),
                    _ => break,
                },
            }
            self.next_char();
        }

        let base = match state {
            State::Zero => 10,
            State::Decimal => 10,
            State::Hex => 16,
            State::Binary => 2,
            State::Octal => 8,
            State::Float => 10,
            _ => unreachable!(),
        };

        let number = match state {
            State::Hex | State::Binary | State::Octal => number[2..].to_owned(),
            _ => number
        };

        match state {
            State::Zero | State::Decimal | State::Hex | State::Binary | State::Octal => Some(
                TokenKind::Integer(u64::from_str_radix(&number, base).unwrap_or_else(|error| {
                    self.problems.push(TokenProblem::from(error));
                    0
                })),
            ),
            State::Float => Some(TokenKind::FloatLiteral(number.parse().unwrap_or_else(
                |error| {
                    self.problems.push(TokenProblem::from(error));
                    0.0
                },
            ))),
            _ => unreachable!(),
        }
    }

}

#[test]
fn test_eat_number_for_float_number() {
    let test = "3.14";
    let mut lexer = TestLexer::new(test);
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::FloatLiteral(3.14)));
}

#[test]
fn test_eat_number_leading_zeros_are_still_float() {
    let test = "003.44";
    let mut lexer = TestLexer::new(test);
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::FloatLiteral(3.44)));
}

#[test]
fn test_eat_number_decimal_number() {
    let test = "123";
    let mut lexer = TestLexer::new(test);
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Integer(123)));
}

#[test]
fn test_eat_number_for_hex_number() {
    let test = "0x1A";
    let mut lexer = TestLexer::new(test);
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Integer(0x1A)));
}

// current goal, develope algorithm for processing numbers from float, decimal, hex, binary, and octal
