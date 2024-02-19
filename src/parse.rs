use crate::lex::{LexResult, Lexer};
use crate::tokens::Token;
use crate::{error::CompilerError, file::Locatable, tokens::Token as LexToken};
use std::collections::btree_map::IterMut;
use std::io;
use std::path::PathBuf;

pub struct Parser<L>
where
    L: Iterator<Item = LexResult>,
{
    lexer: L,
    current: Option<Locatable<Token>>,
    next: Option<Locatable<Token>>,
    errors: Vec<CompilerError>,
}

impl Parser<Lexer> {
    pub fn from_file(path: PathBuf) -> io::Result<Self> {
        let lexer = Lexer::from_file(path)?;
        Ok(Self::new(lexer))
    }
}

impl<L> Parser<L>
where
    L: Iterator<Item = LexResult>,
{
    pub fn new(lexer: L) -> Self {
        Self {
            lexer,
            current: None,
            next: None,
            errors: Vec::new(),
        }
    }
}
