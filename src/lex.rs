use crate::{
    lex,
    tokens::{Keyword, LexToken, Literal, Span, Symbol, TokenKind, TokenProblem},
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

    /// This will consume the next identifier like token
    /// Such tokens include Identifiers, Keywords, and Special Symbols like "sizeof"
    fn eat_ident_like(&mut self) -> Option<TokenKind> {
        if !self.current.is_some_and(|c| c.is_alphabetic() || c == '_') {
            return None;
        }
        let mut ident = String::new();
        while let Some(current) = self.current {
            if current.is_alphanumeric() || current == '_' {
                ident.push(current);
            } else {
                break;
            }
            self.next_char();
        }
        Some(match ident.as_str() {
            "int" => TokenKind::Keyword(Keyword::Int),
            "double" => TokenKind::Keyword(Keyword::Double),
            "return" => TokenKind::Keyword(Keyword::Return),
            "sizeof" => TokenKind::Symbol(Symbol::Sizeof),
            _ => TokenKind::Identifier(ident),
        })
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
                    /// Include a-z to catch erroneous hex numbers
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => number.push(current),
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

        let literal = match state {
            State::Zero | State::Decimal | State::Hex | State::Binary | State::Octal => {
                let result = u64::from_str_radix(&number, base);
                let value = result.unwrap_or_else(|error| {
                    /// I would like to improve this error message
                    self.problems.push(TokenProblem::from(error));
                    0
                });
                Literal::Integer(value)
            }
            State::Float => {
                let result = number.parse();
                let value = result.unwrap_or_else(|error| {
                    self.problems.push(TokenProblem::from(error));
                    0.0
                });
                Literal::Float(value)
            }
            _ => unreachable!(),
        };

        Some(TokenKind::Literal(literal))
    }

    fn eat_symbol(&mut self) -> Option<TokenKind> {
        /// I hate this and im sorry
        self.current
            .map(|current| {
                use Symbol::*;
                macro_rules! single {
                    ($kind:ident) => {
                        Some({
                            self.next_char();
                            $kind
                        })
                    };
                }
                match current {
                    '+' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                PlusEqual
                            }
                            Some('+') => {
                                self.next_char();
                                Increment
                            }
                            _ => Plus,
                        }
                    }),

                    '-' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                MinusEqual
                            }
                            Some('-') => {
                                self.next_char();
                                Decrement
                            }
                            Some('>') => {
                                self.next_char();
                                Arrow
                            }
                            _ => Minus,
                        }
                    }),

                    '*' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                StarEqual
                            }
                            _ => Star,
                        }
                    }),

                    '/' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                SlashEqual
                            }
                            Some('*' | '/') if cfg!(debug_assertions) => {
                                panic!("Unhandled comment in input position: {}", self.position);
                            }
                            _ => Slash,
                        }
                    }),
                    '%' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                ModuloEqual
                            }
                            _ => Modulo,
                        }
                    }),

                    '=' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                EqualEqual
                            }
                            _ => Equal,
                        }
                    }),

                    '!' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                BangEqual
                            }
                            _ => Bang,
                        }
                    }),

                    '|' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                PipeEqual
                            }
                            Some('|') => {
                                self.next_char();
                                DoublePipe
                            }
                            _ => Pipe,
                        }
                    }),

                    '&' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                AmpersandEqual
                            }
                            Some('&') => {
                                self.next_char();
                                DoubleAmpersand
                            }
                            _ => Ampersand,
                        }
                    }),

                    '^' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                CaretEqual
                            }
                            _ => Caret,
                        }
                    }),

                    '<' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                LessThanEqual
                            }
                            Some('<') => {
                                self.next_char();
                                match self.current {
                                    Some('=') => {
                                        self.next_char();
                                        LeftShiftEqual
                                    }
                                    _ => LeftShift,
                                }
                            }
                            _ => LessThan,
                        }
                    }),

                    '>' => Some({
                        self.next_char();
                        match self.current {
                            Some('=') => {
                                self.next_char();
                                GreaterThanEqual
                            }
                            Some('>') => {
                                self.next_char();
                                match self.current {
                                    Some('=') => {
                                        self.next_char();
                                        RightShiftEqual
                                    }
                                    _ => RightShift,
                                }
                            }
                            _ => GreaterThan,
                        }
                    }),

                    '.' => single!(Dot),
                    '?' => single!(QuestionMark),
                    ':' => single!(Colon),
                    '~' => single!(Tilde),
                    ',' => single!(Comma),
                    ';' => single!(Semicolon),
                    '(' => single!(OpenParen),
                    ')' => single!(CloseParen),
                    '[' => single!(OpenSquare),
                    ']' => single!(CloseSquare),
                    '{' => single!(OpenCurly),
                    '}' => single!(CloseCurly),

                    _ => None,
                }
                .map(|symbol| TokenKind::Symbol(symbol))
            })
            .flatten()
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
    let symbols = [
        ("+", TokenKind::Symbol(Symbol::Plus)),
        ("-", TokenKind::Symbol(Symbol::Minus)),
        ("*", TokenKind::Symbol(Symbol::Star)),
        ("/", TokenKind::Symbol(Symbol::Slash)),
        ("%", TokenKind::Symbol(Symbol::Modulo)),
        ("==", TokenKind::Symbol(Symbol::EqualEqual)),
        ("!=", TokenKind::Symbol(Symbol::BangEqual)),
        (">", TokenKind::Symbol(Symbol::GreaterThan)),
        (">=", TokenKind::Symbol(Symbol::GreaterThanEqual)),
        ("<", TokenKind::Symbol(Symbol::LessThan)),
        ("<=", TokenKind::Symbol(Symbol::LessThanEqual)),
        ("!", TokenKind::Symbol(Symbol::Bang)),
        ("&&", TokenKind::Symbol(Symbol::DoubleAmpersand)),
        ("||", TokenKind::Symbol(Symbol::DoublePipe)),
        ("&", TokenKind::Symbol(Symbol::Ampersand)),
        ("|", TokenKind::Symbol(Symbol::Pipe)),
        ("^", TokenKind::Symbol(Symbol::Caret)),
        ("~", TokenKind::Symbol(Symbol::Tilde)),
        ("<<", TokenKind::Symbol(Symbol::LeftShift)),
        (">>", TokenKind::Symbol(Symbol::RightShift)),
        ("=", TokenKind::Symbol(Symbol::Equal)),
        ("+=", TokenKind::Symbol(Symbol::PlusEqual)),
        ("-=", TokenKind::Symbol(Symbol::MinusEqual)),
        ("*=", TokenKind::Symbol(Symbol::StarEqual)),
        ("/=", TokenKind::Symbol(Symbol::SlashEqual)),
        ("%=", TokenKind::Symbol(Symbol::ModuloEqual)),
        ("&=", TokenKind::Symbol(Symbol::AmpersandEqual)),
        ("|=", TokenKind::Symbol(Symbol::PipeEqual)),
        ("^=", TokenKind::Symbol(Symbol::CaretEqual)),
        ("<<=", TokenKind::Symbol(Symbol::LeftShiftEqual)),
        (">>=", TokenKind::Symbol(Symbol::RightShiftEqual)),
        ("++", TokenKind::Symbol(Symbol::Increment)),
        ("--", TokenKind::Symbol(Symbol::Decrement)),
        ("?", TokenKind::Symbol(Symbol::QuestionMark)),
        (":", TokenKind::Symbol(Symbol::Colon)),
        (",", TokenKind::Symbol(Symbol::Comma)),
        (".", TokenKind::Symbol(Symbol::Dot)),
        ("->", TokenKind::Symbol(Symbol::Arrow)),
        ("?", TokenKind::Symbol(Symbol::QuestionMark)),
        ("[", TokenKind::Symbol(Symbol::OpenSquare)),
        ("]", TokenKind::Symbol(Symbol::CloseSquare)),
        ("{", TokenKind::Symbol(Symbol::OpenCurly)),
        ("}", TokenKind::Symbol(Symbol::CloseCurly)),
        ("(", TokenKind::Symbol(Symbol::OpenParen)),
        (")", TokenKind::Symbol(Symbol::CloseParen)),
        (";", TokenKind::Symbol(Symbol::Semicolon)),
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
    let kind = lexer.eat_number();
    assert_eq!(kind, Some(TokenKind::Literal(Literal::Integer(344))));
}

#[test]
fn test_eat_number_for_float_number() {
    let test = "3.14";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Literal(Literal::Float(3.14))));
}

#[test]
fn test_eat_number_leading_zeros_are_still_float() {
    let test = "003.44";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Literal(Literal::Float(3.44))));
}

#[test]
fn test_eat_number_decimal_number() {
    let test = "123";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Literal(Literal::Integer(123))));
}

#[test]
fn test_eat_number_for_hex_number() {
    let test = "0x1A";
    let mut lexer = Lexer::new(test.to_string());
    let token = lexer.eat_number();
    assert_eq!(token, Some(TokenKind::Literal(Literal::Integer(0x1A))));
}
