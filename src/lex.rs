use crate::{
    lex,
    tokens::{LexToken, Span, TokenKind, TokenProblem},
};
use arcstr::{ArcStr, Substr};
use std::{
    fs::File,
    io::{self, BufReader, Read},
    mem,
    str::Chars,
    sync::Arc,
};
use thiserror::Error;

pub struct Lexer {
    position: usize,
    source: ArcStr,
    problems: Vec<TokenProblem>,
    current: Option<char>,
    next: Option<char>,
}

impl Lexer {
    #[inline(always)]
    pub fn new(source: String) -> Self {
        let source = ArcStr::from(source);
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        let problems = Vec::new();
        let position = 0;
        let chars = chars;
        Self {
            position,
            source,
            problems,
            current,
            next,
        }
    }

    #[inline(always)]
    fn next_char(&mut self) -> Option<char> {
        self.position += 1;
        self.current = self.next;
        self.next = self.source.chars().nth(self.position + 1);
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
                    '0'..='9' | 'a'..='f' | 'A'..='F' => number.push(current),
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
            _ => number,
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

    fn eat_symbol(&mut self) -> Option<TokenKind> {
        let mut symbol = String::new();
        let character = self
            .current
            .expect("Method eat_symbol called on an empty input.");
        symbol.push(character);

        macro_rules! push_if_matches {
            ($($pattern:literal)|+) => {
                {
                    let matches = matches!(self.next, Some(x) if $(x == $pattern)||+);
                    if  matches {
                        symbol.push(self.next_char().unwrap());
                    }
                    matches
                }
            };
        }

        // use cargo expand to see the expanded code
        match character {
            '-' => {
                push_if_matches!('-' | '=' | '>');
            }
            '+' => {
                push_if_matches!('+' | '=');
            }
            '/' | '*' | '%' | '=' | '!' | '^' => {
                push_if_matches!('=');
            }
            '<' => {
                push_if_matches!('=');
                if push_if_matches!('<') {
                    push_if_matches!('=');
                }
            }
            '>' => {
                push_if_matches!('=');
                if push_if_matches!('>') {
                    push_if_matches!('=');
                }
            }
            '&' => {
                push_if_matches!('&' | '=');
            }
            '|' => {
                push_if_matches!('|' | '=');
            }
            _ => (),
        };

        TokenKind::match_symbol(&symbol)
    }
}

impl Iterator for Lexer {
    type Item = LexToken;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current.is_some_and(|c| c.is_whitespace()) {
            self.next_char();
        }
        self.current.map(|c| {
            let start = self.position;
            let kind = match c {
                '\'' => {
                    panic!("CHAR"); // this is a placeholder
                }

                '"' => {
                    panic!("STRING"); // this is a placeholder
                }

                '0'..='9' => self.eat_number(),

                '_' | 'a'..='z' | 'A'..='Z' => {
                    panic!("IDENTIFIER"); // this is a placeholder
                }

                c => self.eat_symbol(),
            }
            .unwrap_or(TokenKind::BadToken);
            let end = self.position;
            let location = Span::new(start, end);
            LexToken::new(kind, location, mem::take(&mut self.problems))
        })
    }
}

#[test]
fn test_current_is_first_char() {
    let lexer = Lexer::new("abc".to_string());
    assert_eq!(lexer.current, Some('a'));
    assert_eq!(lexer.next, Some('b'));
}

#[test]
fn test_next_char_iterates_correctly() {
    let mut lexer = Lexer::new("abc".to_string());
    assert_eq!(lexer.current, Some('a'));
    assert_eq!(lexer.next_char(), Some('b'));
    assert_eq!(lexer.next_char(), Some('c'));
    assert_eq!(lexer.next_char(), None);
}

#[test]
fn test_symbols_lex_correctly() {
    use TokenKind::*;
    let symbols = [
        ("+", Plus),
        ("-", Minus),
        ("*", Star),
        ("/", Slash),
        ("%", Modulo),
        ("==", EqualEqual),
        ("!=", BangEqual),
        (">", GreaterThan),
        (">=", GreaterThanEqual),
        ("<", LessThan),
        ("<=", LessThanEqual),
        ("!", Bang),
        ("&&", DoubleAmpersand),
        ("||", DoublePipe),
        ("&", Ampersand),
        ("|", Pipe),
        ("^", Caret),
        ("~", Tilde),
        ("<<", LeftShift),
        (">>", RightShift),
        ("=", Equal),
        ("+=", PlusEqual),
        ("-=", MinusEqual),
        ("*=", StarEqual),
        ("/=", SlashEqual),
        ("%=", ModuloEqual),
        ("&=", AmpersandEqual),
        ("|=", PipeEqual),
        ("^=", CaretEqual),
        ("<<=", LeftShiftEqual),
        (">>=", RightShiftEqual),
        ("++", Increment),
        ("--", Decrement),
        ("?", QuestionMark),
        (":", Colon),
        (",", Comma),
        (".", Dot),
        ("->", Arrow),
        ("[", OpenSquare),
        ("]", CloseSquare),
        ("{", OpenCurly),
        ("}", CloseCurly),
        ("(", OpenParen),
        (")", CloseParen),
        (";", Semicolon),
        ("?", QuestionMark),
    ];

    for (symbol, kind) in symbols.iter() {
        let mut lexer = Lexer::new(symbol.to_string());
        let returned_kind = lexer.eat_symbol().unwrap();
        if *kind != returned_kind {
            panic!(
                "Expected {:?}, got {:?}, input symbol: {}",
                kind, returned_kind, symbol
            );
        }
    }
}

#[test]
fn test_parse_number_works_for_valid_int() {
    let test = "344";
    let mut lexer = Lexer::new(test.to_string());
    let kind = lexer.eat_number().unwrap();
    assert_eq!(kind, TokenKind::Integer(344));
}

#[test]
fn test_eat_number_for_float_number() {
    let test = "3.14";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::FloatLiteral(3.14)));
}

#[test]
fn test_eat_number_leading_zeros_are_still_float() {
    let test = "003.44";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::FloatLiteral(3.44)));
}

#[test]
fn test_eat_number_decimal_number() {
    let test = "123";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Integer(123)));
}

#[test]
fn test_eat_number_for_hex_number() {
    let test = "0x1A";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Integer(0x1A)));
}