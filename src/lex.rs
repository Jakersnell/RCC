use crate::error::{CompilerError, CompilerWarning, ErrorReporter};
use crate::util::{CompilerResult, Locatable, Span};
use crate::{
    lex,
    tokens::{Keyword, Literal, Symbol, Token},
};
use arcstr::{ArcStr, Substr};
use std::path::PathBuf;
use std::{
    fs::File,
    io::{self, BufReader, Read},
    mem,
    str::Chars,
    sync::Arc,
};
use thiserror::Error;

pub type LexResult = Result<Locatable<Token>, ()>;

pub struct Lexer<'a, E: ErrorReporter> {
    reporter: &'a mut E,
    source: ArcStr,
    position: usize,
    line: usize,
    col: usize,
    current: Option<char>,
    next: Option<char>,
}
impl<'a, E> From<(&'a mut E, ArcStr)> for Lexer<'a, E>
where
    E: ErrorReporter,
{
    fn from(value: (&'a mut E, ArcStr)) -> Self {
        Lexer::new(value.0, value.1)
    }
}

impl<'a, E> Lexer<'a, E>
where
    E: ErrorReporter,
{
    pub fn new(reporter: &'a mut E, source: ArcStr) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        Self {
            reporter,
            source,
            position: 0,
            col: 0,
            line: 0,
            current,
            next,
        }
    }

    #[inline(always)]
    fn next_char(&mut self) -> Option<char> {
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
    fn report_error(&mut self, err: CompilerError) {
        self.reporter.report_error(err)
    }

    #[inline(always)]
    fn report_warning(&mut self, warn: CompilerWarning) {
        self.reporter.report_warning(warn)
    }

    #[inline(always)]
    fn start_span(&mut self) -> Span {
        Span {
            start: self.position,
            end: self.position + 1, // not very important but just to place something here
            line: self.line,
            col: self.col,
        }
    }

    #[inline(always)]
    fn end_span(&mut self, span: Span) -> Span {
        let mut span = span;
        span.end = self.position;
        span
    }

    fn consume_alphanumeric_text(&mut self) -> Option<String> {
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

    fn eat_ident_or_keyword(&mut self) -> Option<Token> {
        self.consume_alphanumeric_text()
            .map(|text| match text.as_str() {
                "int" => Token::Keyword(Keyword::Int),
                "double" => Token::Keyword(Keyword::Double),
                "char" => Token::Keyword(Keyword::Char),
                "long" => Token::Keyword(Keyword::Long),
                "void" => Token::Keyword(Keyword::Void),
                "signed" => Token::Keyword(Keyword::Signed),
                "unsigned" => Token::Keyword(Keyword::Unsigned),
                "struct" => Token::Keyword(Keyword::Struct),
                "if" => Token::Keyword(Keyword::If),
                "else" => Token::Keyword(Keyword::Else),
                "while" => Token::Keyword(Keyword::While),
                "for" => Token::Keyword(Keyword::For),
                "break" => Token::Keyword(Keyword::Break),
                "continue" => Token::Keyword(Keyword::Continue),
                "static" => Token::Keyword(Keyword::Static),
                "const" => Token::Keyword(Keyword::Const),
                "return" => Token::Keyword(Keyword::Return),

                // not a keyword, but a special symbol
                "sizeof" => Token::Symbol(Symbol::Sizeof),

                _ => Token::Identifier(crate::str_intern::intern(&text)),
            })
    }

    fn eat_number(&mut self) -> Option<Token> {
        if !self.current.is_some_and(|c| c.is_ascii_digit()) {
            return None;
        }

        macro_rules! consume_suffix {
            ($($pattern:literal)|+, $error_type:expr) => {
                {
                    let span = self.start_span();
                    let mut invalid_text = None;
                    let suffix = self.consume_alphanumeric_text().map(|text| {
                        let lowercased = text.to_lowercase();
                        match lowercased.as_str() {
                            $($pattern)|+ => Some(lowercased),
                            _ => {
                                invalid_text = Some(text);
                                None
                            }
                        }
                    }).flatten();
                    if let Some(text) = invalid_text {
                        self.report_error($error_type(text, span));
                    }
                    suffix
                }
            };
        }

        let span = self.start_span();

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
                    '0'..='9'
                    | 'a'..='e' // skip f, l & u because they are suffixes
                    | 'h'..='k' // ex: 0xff3ul for unsigned long
                    | 'm'..='t'
                    | 'v'..='z'
                    | 'A'..='E'
                    | 'H'..='K'
                    | 'M'..='T'
                    | 'V'..='Z' => number.push(current),
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

        let span = self.end_span(span);

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
                let result = u128::from_str_radix(&number, base);
                let value = result.unwrap_or_else(|error| {
                    self.report_error(CompilerError::ParseIntError(span));
                    0
                });
                let suffix = consume_suffix!(
                    "u" | "l" | "ul" | "lu" | "llu" | "ll",
                    CompilerError::InvalidIntegerSuffix
                );
                Literal::Integer { value, suffix }
            }

            State::Float => {
                let result = number.parse();
                let value = result.unwrap_or_else(|error| {
                    self.report_error(CompilerError::ParseFloatError(span));
                    0.0
                });
                let suffix = consume_suffix!("f" | "l", CompilerError::InvalidFloatSuffix);
                Literal::Float { value, suffix }
            }
            _ => unreachable!(),
        };

        Some(Token::Literal(literal))
    }

    fn eat_symbol(&mut self) -> Option<Token> {
        use Symbol::*;
        macro_rules! single {
            ($kind:ident) => {
                Some({
                    self.next_char();
                    $kind
                })
            };
        }

        // I want to clean this up soon.
        // But for now it's ok.
        match self.current {
            Some('+') => Some({
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

            Some('-') => Some({
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

            Some('*') => Some({
                self.next_char();
                match self.current {
                    Some('=') => {
                        self.next_char();
                        StarEqual
                    }
                    _ => Star,
                }
            }),

            Some('/') => Some({
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
            Some('%') => Some({
                self.next_char();
                match self.current {
                    Some('=') => {
                        self.next_char();
                        ModuloEqual
                    }
                    _ => Modulo,
                }
            }),

            Some('=') => Some({
                self.next_char();
                match self.current {
                    Some('=') => {
                        self.next_char();
                        EqualEqual
                    }
                    _ => Equal,
                }
            }),

            Some('!') => Some({
                self.next_char();
                match self.current {
                    Some('=') => {
                        self.next_char();
                        BangEqual
                    }
                    _ => Bang,
                }
            }),

            Some('|') => Some({
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

            Some('&') => Some({
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

            Some('^') => Some({
                self.next_char();
                match self.current {
                    Some('=') => {
                        self.next_char();
                        CaretEqual
                    }
                    _ => Caret,
                }
            }),

            Some('<') => Some({
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

            Some('>') => Some({
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

            Some('.') => single!(Dot),
            Some('?') => single!(QuestionMark),
            Some(':') => single!(Colon),
            Some('~') => single!(Tilde),
            Some(',') => single!(Comma),
            Some(';') => single!(Semicolon),
            Some('(') => single!(OpenParen),
            Some(')') => single!(CloseParen),
            Some('[') => single!(OpenSquare),
            Some(']') => single!(CloseSquare),
            Some('{') => single!(OpenCurly),
            Some('}') => single!(CloseCurly),

            _ => None,
        }
        .map(Token::Symbol)
    }

    fn eat_char(&mut self) -> Option<Token> {
        if self.current != Some('\'') {
            return None;
        }
        self.next_char();
        let span = self.start_span();
        let value = match self.current {
            Some('\\') => self.eat_escape_char(),
            Some(current) => {
                self.next_char();
                Some(current)
            }
            None => {
                let span = self.end_span(span);
                self.reporter
                    .report_error(CompilerError::UnclosedCharLiteral(span));
                None
            }
        }
        .unwrap_or('\0');
        if self.current != Some('\'') {
            let span = self.end_span(span);
            self.reporter
                .report_error(CompilerError::UnclosedCharLiteral(span));
        } else {
            self.next_char();
        }
        Some(Token::Literal(Literal::Char { value }))
    }

    fn eat_string(&mut self) -> Option<Token> {
        if self.current != Some('"') {
            return None;
        }
        self.next_char();
        let span = self.start_span();
        while let Some(current) = self.current {
            match current {
                '"' => {
                    self.next_char();
                    break;
                }
                '\\' => {
                    self.eat_escape_char();
                }
                '\n' | '\r' | '\0' => {
                    let span = self.end_span(span);
                    self.report_error(CompilerError::UnclosedStringLiteral(span));
                    break;
                }
                _ => {
                    self.next_char();
                }
            }
        }
        let span = self.end_span(span);
        let value = self.source.substr(span.start..span.end - 1);
        Some(Token::Literal(Literal::String { value }))
    }

    fn eat_escape_char(&mut self) -> Option<char> {
        let span = self.start_span();
        debug_assert!(self.current == Some('\\'));
        self.next_char();
        self.next_char().map(|current| match current {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            _ => {
                let span = self.end_span(span);
                self.report_error(CompilerError::InvalidEscapeSequence(span));
                '\0'
            }
        })
    }

    /// Removes whitespace and comments from incoming source
    fn remove_trivial(&mut self) {
        let mut state = TriviaState::Start;
        loop {
            if state != TriviaState::Start {
                self.next_char();
            }
            if state == TriviaState::BlockCommentEnd {
                self.next_char();
            }
            state = self.get_current_trivia(state);
            if state == TriviaState::End {
                break;
            }
        }
    }

    fn get_current_trivia(&self, state: TriviaState) -> TriviaState {
        use TriviaState::*;
        match (state, self.current, self.next) {
            (WhiteSpace | BlockCommentEnd, Some(current), _) => Start,

            (Start, Some(current), _) if current.is_whitespace() => WhiteSpace,

            (BlockComment, Some('*'), Some('/')) => BlockCommentEnd,

            (Start, Some('/'), Some('*')) | (BlockComment, Some(_), _) => BlockComment,

            (InlineComment, Some('\n'), _) => Start,

            (Start, Some('/'), Some('/')) | (InlineComment, Some(_), _) => InlineComment,

            _ => End,
        }
    }
}

#[derive(Eq, PartialEq)]
enum TriviaState {
    Start,
    InlineComment,
    WhiteSpace,
    BlockComment,
    BlockCommentEnd,
    End,
}

impl<'a, E> Iterator for Lexer<'a, E>
where
    E: ErrorReporter,
{
    /// the only reason this is a Vec<CompilerError> is for strings with multiple invalid escapes.
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
    use crate::error::{CompilerError, CompilerWarning, ErrorReporter};
    use crate::lex::Lexer;
    use crate::tokens::{Literal, Symbol, Token};

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
        let mut report = ErrorReporterMock::default();
        let lexer = Lexer::new(&mut report, "abc".into());
        assert_eq!(lexer.current, Some('a'));
        assert_eq!(lexer.next, Some('b'));
    }

    #[test]
    fn test_next_char_iterates_correctly() {
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, "abc".into());
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
            let mut report = ErrorReporterMock::default();
            #[allow(suspicious_double_ref_op)]
            let mut lexer = Lexer::new(&mut report, symbol.clone().into());
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
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, "344".into());
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
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, test.into());
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
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, test.into());
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
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, test.into());
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
        let mut report = ErrorReporterMock::default();
        let mut lexer = Lexer::new(&mut report, test.into());
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
            let mut report = ErrorReporterMock::default();
            let mut lexer = Lexer::new(&mut report, test.into());
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
            let mut report = ErrorReporterMock::default();
            let mut lexer = Lexer::new(&mut report, test.into());
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
            let mut report = ErrorReporterMock::default();
            let mut lexer = Lexer::new(&mut report, test.into());
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
            let mut report = ErrorReporterMock::default();
            let mut lexer = Lexer::new(&mut report, format!("\"{}\"", test).into());
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
