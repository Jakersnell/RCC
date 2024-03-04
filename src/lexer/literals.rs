use super::*;
use crate::lexer::tokens::Literal;

impl Lexer {
    pub(super) fn eat_number(&mut self) -> Option<Token> {
        if !self.current.is_some_and(|c| c.is_ascii_digit()) {
            return None;
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

    pub(super) fn eat_char(&mut self) -> Option<Token> {
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

    pub(super) fn eat_string(&mut self) -> Option<Token> {
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

    pub(super) fn eat_escape_char(&mut self) -> Option<char> {
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
}
