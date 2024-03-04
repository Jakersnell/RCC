use super::*;
use crate::lexer::tokens::{Keyword, Symbol};

impl Lexer {
    pub(super) fn eat_ident_or_keyword(&mut self) -> Option<Token> {
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

                _ => Token::Identifier(str_intern::intern(&text)),
            })
    }

    pub(super) fn eat_symbol(&mut self) -> Option<Token> {
        use crate::lexer::tokens::Symbol::*;
        macro_rules! single {
            ($kind:ident) => {
                Some({
                    self.next_char();
                    $kind
                })
            };
        }

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
}
