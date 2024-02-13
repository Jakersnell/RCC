use crate::tokens::{LexToken, Location, SymbolKind, SyntaxTokenKind, TokenProblem};
use arcstr::ArcStr;
use std::{
    fs::File,
    io::{self, BufReader, Read},
    mem,
    sync::Arc,
};
use thiserror::Error;

pub type LexResult<T> = Result<T, LexError>;

#[derive(Error, Debug)]
pub enum LexError {
    #[error("The character '{0}' is not recognized.")]
    InvalidSymbol(String),

    #[error("{0}")]
    Io(#[from] io::Error),

    #[error("End of file reached.")]
    EndOfFile,
}

pub struct Lexer {
    filepath: Arc<String>,
    source: ArcStr,
    position: usize,
    problems: Vec<TokenProblem>,
    current: Option<char>,
    next: Option<char>,
}

impl Lexer {
    #[inline(always)]
    pub fn new(filepath: String) -> Result<Self, LexError> {
        let file = File::open(&filepath)?;
        let mut reader = BufReader::new(&file);
        let mut buffer = String::new();
        let result = reader.read_to_string(&mut buffer);
        let source = ArcStr::from(buffer);
        let filepath = Arc::new(filepath);
        let mut chars = source.chars();
        let current = chars.next();
        let next = chars.next();
        let problems = Vec::new();
        let position = 0;
        match result {
            Ok(_) => Ok(Self {
                filepath,
                source,
                position,
                problems,
                current,
                next,
            }),
            Err(err) => Err(LexError::Io(err)),
        }
    }

    #[inline(always)]
    fn next_char(&mut self) -> Option<char> {
        self.current = self.next;
        self.next = self.source.chars().next();
        self.position += 1;
        self.current
    }

    fn eat_symbol(&mut self) -> LexResult<SyntaxTokenKind> {
        let mut symbol = String::new();
        let character = self
            .next_char()
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

        match SymbolKind::match_symbol(&symbol) {
            Some(kind) => Ok(SyntaxTokenKind::Symbol(kind)),
            None => Err(LexError::InvalidSymbol(symbol)),
        }
    }
}

impl Iterator for Lexer {
    type Item = Result<LexToken, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|c| {
            let start = self.position;
            let kind = match c {
                '\'' => {
                    panic!("CHAR"); // this is a placeholder
                }

                '"' => {
                    panic!("STRING"); // this is a placeholder
                }

                '0'..='9' => {
                    panic!("DIGIT"); // this is a placeholder
                }

                '_' | 'a'..='z' | 'A'..='Z' => {
                    panic!("IDENTIFIER"); // this is a placeholder
                }

                c => self.eat_symbol(),
            }?;
            let end = self.position;
            let location = Location::new(self.filepath.clone(), start, end);
            Ok(LexToken::new(kind, location, mem::take(&mut self.problems)))
        })
    }
}
