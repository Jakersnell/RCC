mod literals;
mod symbols;
pub mod tokens;
mod trivial;

use crate::lexer::tokens::Token;
pub(super) use crate::util::error::{CompilerError, CompilerWarning, ErrorReporter};
pub(super) use crate::util::*;
pub(super) use crate::util::{Locatable, Span};
pub(super) use arcstr::ArcStr;
use std::cell::RefCell;
pub(super) use std::io::Read;
use std::rc::Rc;

pub type LexResult = Result<Locatable<Token>, ()>;

pub struct Lexer {
    source: ArcStr,
    reporter: Rc<RefCell<dyn ErrorReporter>>,
    position: usize,
    line: usize,
    col: usize,
    current: Option<char>,
    next: Option<char>,
}
impl From<(Rc<RefCell<dyn ErrorReporter>>, ArcStr)> for Lexer {
    fn from(value: (Rc<RefCell<dyn ErrorReporter>>, ArcStr)) -> Self {
        Lexer::new(value.0, value.1)
    }
}

impl Lexer {
    pub fn new(reporter: Rc<RefCell<dyn ErrorReporter>>, source: ArcStr) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        Self {
            source,
            reporter,
            position: 0,
            col: 0,
            line: 0,
            current,
            next,
        }
    }

    #[inline(always)]
    pub(super) fn next_char(&mut self) -> Option<char> {
        if self.current.is_some_and(|c| c == '\n') {
            self.line += 1;
            self.col = 0;
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
        self.reporter.report_error(err)
    }

    #[inline(always)]
    pub(super) fn report_warning(&mut self, warn: CompilerWarning) {
        self.reporter.report_warning(warn)
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
            if self.reporter.get_status().is_err() {
                return Err(());
            }
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
            if self.reporter.get_status().is_err() {
                return Err(());
            }
            if let Some(kind) = kind {
                Ok(Locatable::new(span, kind))
            } else {
                Err(())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::lexer::tokens::{Literal, Symbol, Token};
    use crate::util::error::{CompilerError, CompilerWarning, ErrorReporter};
    use std::cell::RefCell;
    use std::rc::Rc;

    struct ErrorReporterMock {
        status: Result<(), ()>,
        errors: Vec<CompilerError>,
        warnings: Vec<CompilerWarning>,
    }

    impl Default for ErrorReporterMock {
        fn default() -> Self {
            Self {
                status: Ok(()),
                errors: Vec::new(),
                warnings: Vec::new(),
            }
        }
    }

    impl ErrorReporter for ErrorReporterMock {
        fn get_status(&self) -> Result<(), ()> {
            self.status
        }

        fn report_error(&mut self, error: CompilerError) {
            self.status = Err(());
            self.errors.push(error);
        }

        fn report_warning(&mut self, warning: CompilerWarning) {
            self.warnings.push(warning);
        }

        fn get_errors(&self) -> &Vec<CompilerError> {
            &self.errors
        }

        fn get_warnings(&self) -> &Vec<CompilerWarning> {
            &self.warnings
        }
    }

    #[test]
    fn test_current_is_first_char() {
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let lexer = Lexer::new(report, "abc".into());
        assert_eq!(lexer.current, Some('a'));
        assert_eq!(lexer.next, Some('b'));
    }

    #[test]
    fn test_next_char_iterates_correctly() {
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, "abc".into());
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
            let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
            #[allow(suspicious_double_ref_op)]
            let mut lexer = Lexer::new(report, symbol.clone().into());
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
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, "344".into());
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
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, test.into());
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
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, test.into());
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
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, test.into());
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
        let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
        let mut lexer = Lexer::new(report, test.into());
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
            let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
            let mut lexer = Lexer::new(report, test.into());
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
            let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
            let mut lexer = Lexer::new(report, test.into());
            let token = lexer.eat_number();
            assert!(report.get_status().is_err());
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
            let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
            let mut lexer = Lexer::new(report, test.into());
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
            let mut report = Rc::new(RefCell::new(ErrorReporterMock::default()));
            let mut lexer = Lexer::new(report, format!("\"{}\"", test).into());
            let token = lexer.eat_string().expect("Expected token");
            match token {
                Token::Literal(Literal::String { value }) => {
                    assert_eq!(value.to_string(), test);
                }
                _ => panic!("Expected string literal, got {:#?}", token),
            }
        }
    }
}
