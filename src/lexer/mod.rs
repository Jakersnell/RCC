use std::io::Read;

use arcstr::ArcStr;

use crate::data::tokens::Token;
use crate::util::*;
use crate::util::{Locatable, Span};
use crate::util::error::CompilerError;

mod literals;
mod symbols;
mod trivial;

pub type LexResult = Result<Locatable<Token>, Vec<CompilerError>>;

pub struct Lexer {
    pub(in crate::lexer) source: ArcStr,
    pub(in crate::lexer) errors: Vec<CompilerError>,
    pub(in crate::lexer) position: usize,
    pub(in crate::lexer) line: usize,
    pub(in crate::lexer) col: usize,
    pub(in crate::lexer) current: Option<char>,
    pub(in crate::lexer) next: Option<char>,
}
impl From<(ArcStr)> for Lexer {
    fn from(value: (ArcStr)) -> Self {
        Lexer::new(value)
    }
}

impl Lexer {
    pub fn new(source: ArcStr) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        Self {
            source,
            errors: Vec::new(),
            position: 0,
            col: 1,
            line: 1,
            current,
            next,
        }
    }

    #[inline(always)]
    pub(super) fn next_char(&mut self) -> Option<char> {
        if self.current.is_some_and(|c| c == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.position += 1;
        self.current = self.next;
        self.next = self.source.chars().nth(self.position + 1);
        self.current
    }

    #[inline(always)]
    pub(super) fn report_error(&mut self, err: CompilerError) {
        self.errors.push(err)
    }

    #[inline(always)]
    pub(super) fn start_span(&mut self) -> Span {
        Span {
            start: self.position,
            end: self.position + 1, // not very important but just to place something here
            line: self.line,
            col: self.col,
        }
    }

    #[inline(always)]
    pub(super) fn end_span(&mut self, span: Span) -> Span {
        let mut span = span;
        span.end = self.position;
        span
    }

    pub(super) fn consume_alphanumeric_text(&mut self) -> Option<String> {
        if !self.current.is_some_and(|c| c.is_alphabetic() || c == '_') {
            return None;
        }
        let mut text = String::new();
        while let Some(current) = self.current {
            if current.is_alphanumeric() || current == '_' {
                text.push(current);
            } else {
                break;
            }
            self.next_char();
        }
        if text.is_empty() {
            None
        } else {
            Some(text)
        }
    }
}

impl Iterator for Lexer {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.remove_trivial();
        self.current.map(|c| {
            let span = self.start_span();
            let kind = match c {
                '\'' => self.eat_char(),

                '"' => self.eat_string(),

                '0'..='9' => self.eat_number(),

                '_' | 'a'..='z' | 'A'..='Z' => self.eat_ident_or_keyword(),

                c => {
                    let result = self.eat_symbol();
                    if result.is_none() {
                        self.next_char();
                    }
                    result
                }
            };
            let span = self.end_span(span);
            if !self.errors.is_empty() {
                Err(std::mem::take(&mut self.errors))
            } else if let Some(kind) = kind {
                Ok(Locatable::new(span, kind))
            } else {
                Err(std::mem::take(&mut self.errors))
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::data::tokens::{Literal, Symbol, Token};

    use super::Lexer;

    #[test]
    fn test_current_is_first_char() {
        let lexer = Lexer::new("abc".into());
        assert_eq!(lexer.current, Some('a'));
        assert_eq!(lexer.next, Some('b'));
    }

    #[test]
    fn test_next_char_iterates_correctly() {
        let mut lexer = Lexer::new("abc".into());
        assert_eq!(lexer.current, Some('a'));
        assert_eq!(lexer.next_char(), Some('b'));
        assert_eq!(lexer.next_char(), Some('c'));
        assert_eq!(lexer.next_char(), None);
    }

    #[test]
    fn test_symbols_lex_correctly() {
        let symbols = [
            ("+", Token::Symbol(Symbol::Plus)),
            ("-", Token::Symbol(Symbol::Minus)),
            ("*", Token::Symbol(Symbol::Star)),
            ("/", Token::Symbol(Symbol::Slash)),
            ("%", Token::Symbol(Symbol::Modulo)),
            ("==", Token::Symbol(Symbol::EqualEqual)),
            ("!=", Token::Symbol(Symbol::BangEqual)),
            (">", Token::Symbol(Symbol::GreaterThan)),
            (">=", Token::Symbol(Symbol::GreaterThanEqual)),
            ("<", Token::Symbol(Symbol::LessThan)),
            ("<=", Token::Symbol(Symbol::LessThanEqual)),
            ("!", Token::Symbol(Symbol::Bang)),
            ("&&", Token::Symbol(Symbol::DoubleAmpersand)),
            ("||", Token::Symbol(Symbol::DoublePipe)),
            ("&", Token::Symbol(Symbol::Ampersand)),
            ("|", Token::Symbol(Symbol::Pipe)),
            ("^", Token::Symbol(Symbol::Caret)),
            ("~", Token::Symbol(Symbol::Tilde)),
            ("<<", Token::Symbol(Symbol::LeftShift)),
            (">>", Token::Symbol(Symbol::RightShift)),
            ("=", Token::Symbol(Symbol::Equal)),
            ("+=", Token::Symbol(Symbol::PlusEqual)),
            ("-=", Token::Symbol(Symbol::MinusEqual)),
            ("*=", Token::Symbol(Symbol::StarEqual)),
            ("/=", Token::Symbol(Symbol::SlashEqual)),
            ("%=", Token::Symbol(Symbol::ModuloEqual)),
            ("&=", Token::Symbol(Symbol::AmpersandEqual)),
            ("|=", Token::Symbol(Symbol::PipeEqual)),
            ("^=", Token::Symbol(Symbol::CaretEqual)),
            ("<<=", Token::Symbol(Symbol::LeftShiftEqual)),
            (">>=", Token::Symbol(Symbol::RightShiftEqual)),
            ("++", Token::Symbol(Symbol::Increment)),
            ("--", Token::Symbol(Symbol::Decrement)),
            ("?", Token::Symbol(Symbol::QuestionMark)),
            (":", Token::Symbol(Symbol::Colon)),
            (",", Token::Symbol(Symbol::Comma)),
            (".", Token::Symbol(Symbol::Dot)),
            ("->", Token::Symbol(Symbol::Arrow)),
            ("?", Token::Symbol(Symbol::QuestionMark)),
            ("[", Token::Symbol(Symbol::OpenSquare)),
            ("]", Token::Symbol(Symbol::CloseSquare)),
            ("{", Token::Symbol(Symbol::OpenCurly)),
            ("}", Token::Symbol(Symbol::CloseCurly)),
            ("(", Token::Symbol(Symbol::OpenParen)),
            (")", Token::Symbol(Symbol::CloseParen)),
            (";", Token::Symbol(Symbol::Semicolon)),
        ];

        for (symbol, kind) in symbols.iter() {
            #[allow(suspicious_double_ref_op)]
            let mut lexer = Lexer::new(symbol.clone().into());
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
        let mut lexer = Lexer::new("344".into());
        let kind = lexer.eat_number();
        assert_eq!(
            kind,
            Some(Token::Literal(Literal::Integer {
                value: 344,
                suffix: None,
            }))
        );
    }

    #[test]
    fn test_eat_number_for_float_number() {
        let test = "3.16";
        let mut lexer = Lexer::new(test.into());
        let token = lexer.eat_number();
        assert_eq!(
            token,
            Some(Token::Literal(Literal::Float {
                value: 3.16,
                suffix: None,
            }))
        );
    }

    #[test]
    fn test_eat_number_leading_zeros_are_still_float() {
        let test = "003.44";
        let mut lexer = Lexer::new(test.into());
        let token = lexer.eat_number();
        assert_eq!(
            token,
            Some(Token::Literal(Literal::Float {
                value: 3.44,
                suffix: None,
            }))
        );
    }

    #[test]
    fn test_eat_number_decimal_number() {
        let test = "123";
        let mut lexer = Lexer::new(test.into());
        let token = lexer.eat_number();
        assert_eq!(
            token,
            Some(Token::Literal(Literal::Integer {
                value: 123,
                suffix: None,
            }))
        );
    }

    #[test]
    fn test_eat_number_for_hex_number() {
        let test = "0x1A";
        let mut lexer = Lexer::new(test.into());
        let token = lexer.eat_number();
        assert_eq!(
            token,
            Some(Token::Literal(Literal::Integer {
                value: 26,
                suffix: None,
            }))
        );
    }

    #[test]
    fn test_eat_number_parses_integer_suffixes_properly() {
        let tests = [
            ("123u", "u"),
            ("123U", "u"),
            ("123l", "l"),
            ("123L", "l"),
            ("123ul", "ul"),
            ("123UL", "ul"),
            ("123lu", "lu"),
            ("123LU", "lu"),
            ("123LU", "lu"),
            ("123llu", "llu"),
            ("123LLu", "llu"),
            ("123llU", "llu"),
            ("123LLu", "llu"),
            ("123ll", "ll"),
            ("123LL", "ll"),
        ];

        for (test, control) in tests {
            let mut lexer = Lexer::new(test.into());
            let token = lexer.eat_number().expect("Expected token");
            assert_eq!(
                token,
                Token::Literal(Literal::Integer {
                    value: 123,
                    suffix: Some(control.to_string()),
                })
            );
        }
    }

    #[test]
    fn test_eat_number_properly_catches_problematic_integer_suffix() {
        let tests = [
            "123z", "123Z", "123h", "123H", "123m", "123M", "123t", "123T", "123v", "123V",
        ];
        for test in tests {
            let mut lexer = Lexer::new(test.into());
            let token = lexer.eat_number();
            assert!(!lexer.errors.is_empty());
        }
    }

    #[test]
    fn test_eat_number_properly_consumes_float_suffix() {
        let tests = [
            ("123.4f", "f"),
            ("123.4F", "f"),
            ("123.4l", "l"),
            ("123.4L", "l"),
        ];
        for (test, control) in tests {
            let mut lexer = Lexer::new(test.into());
            let token = lexer.eat_number().expect("Expected token");
            assert_eq!(
                token,
                Token::Literal(Literal::Float {
                    value: 123.4,
                    suffix: Some(control.to_string()),
                })
            );
        }
    }

    #[test]
    fn test_eat_string_values_match() {
        let tests = [r#"sgasf"#, r#"1234"#, r#"!@#$\\\"%^&*()_+"#];
        for test in tests {
            let mut lexer = Lexer::new(format!("\"{}\"", test).into());
            let token = lexer.eat_string().expect("Expected token");
            match token {
                Token::Literal(Literal::String { value }) => {
                    assert_eq!(value.to_string(), test);
                }
                _ => panic!("Expected string literal, got {:#?}", token),
            }
        }
    }

    macro_rules! advancement_test {
        ($test:ident, $item:ident, $evaluation:expr) => {{
            let mut lexer = Lexer::new($test.into());
            for (i, token) in lexer.enumerate() {
                #[allow(clippy::redundant_closure_call)]
                match token {
                    Ok(token) => {
                        assert_eq!(token.location.$item, $evaluation(i));
                    }
                    Err(_) => panic!("Error in lexer"),
                }
            }
        }};
    }

    #[test]
    fn test_line_number_advances_correctly() {
        let test = "
            break
            while
            continue 
            return 
        ";
        advancement_test!(test, line, |i| { i + 2 })
    }

    #[test]
    fn test_column_advances_correctly() {
        let test = "123 123 123 123 123";
        advancement_test!(test, col, |i| { i * 4 + 1 })
    }

    #[test]
    fn test_column_and_line_numbers_advance_correctly() {
        let test = "123 123 
123 123
123 123
123 123";
        let mut lexer = Lexer::new(test.into());
        for (i, token) in lexer.enumerate() {
            match token {
                Ok(token) => {
                    assert_eq!(token.location.col, i % 2 * 4 + 1);
                    assert_eq!(token.location.line, i / 2 + 1)
                }
                Err(_) => panic!("Error in lexer"),
            }
        }
    }
}
