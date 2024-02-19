use crate::ast::{ASTNode, Program};
use crate::file::Span;
use crate::lex::{LexResult, Lexer};
use crate::{error::CompilerError, file::Locatable, tokens::Token as LexToken};
use std::collections::btree_map::IterMut;
use std::io;
use std::path::PathBuf;

type Token = Locatable<LexToken>;
pub type ParseResult<T> = Result<T, Vec<Locatable<CompilerError>>>;

pub struct Parser<L>
where
    L: Iterator<Item = LexResult> + Default,
{
    lexer: L,
    global: Vec<ASTNode>,
    current: Option<Token>,
    next: Option<Token>,
    span: Span,
}

impl<L> Parser<L>
where
    L: Iterator<Item = LexResult> + Default,
{
    pub fn parse(lexer: L) -> ParseResult<Program> {
        let mut parser = Self {
            lexer,
            global: Vec::new(),
            current: None,
            next: None,
            span: Span::new(0, 0),
        };
        parser.advance()?;
        parser.advance()?;

        while parser.current.is_some() {
            let global_declaration = parser.parse_function()?;
            parser.global.push(global_declaration);
        }
        todo!()
    }

    fn advance(&mut self) -> ParseResult<()> {
        match self.lexer.next() {
            Some(Ok(token)) => {
                self.current = self.next.take();
                self.next = Some(token);
                Ok(())
            }
            Some(Err(errors)) => {
                let all_results = std::mem::take(&mut self.lexer).collect::<Vec<_>>();
                let all_errors = all_results
                    .into_iter()
                    .filter_map(|r| if let Err(e) = r { Some(e) } else { None })
                    .flatten();
                let mut errors = errors;
                errors.extend(all_errors);
                Err(errors)
            }
            None => Ok(()),
        }
    }

    fn parse_function(&mut self) -> ParseResult<ASTNode> {
        todo!()
    }
}

// goal: tokens are lexed only as needed, so that validations can be done on the fly
/* issue facing:
   A None type given by the lexer indicates that the lexer has reached the end of the file.
   The parser should not request more tokens from the lexer if the lexer has reached the end of the file.
   A few positions during parsing allow for EOF expectedly, otherwise an UnexpectedEOF error should be raised.

   if the option is a Some(T) type there are two cases from there:
       Some(token): the token is valid and can be used for parsing
       Err(Vec<locatable error>): described in the next paragraph

   Unrecoverable errors.
       If the lexer encounters an Err(Vec<locatable error>) type, no further parsing should ensue.
       An error of this kind means the file is unrecoverable. however, the rest of the text should be lexed
       to find any more lexical errors and report them.
*/
