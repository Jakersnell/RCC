use crate::ast::ASTNode;
use crate::error::CompilerError;
use crate::lex::LexResult;
use crate::tokens::Token;
use crate::util::{CompilerResult, Locatable, LocatableToken, Node, Program, Span};
use std::io;
use std::path::PathBuf;

pub struct Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    lexer: L,
    program: Program,
    global: Vec<Node>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    span: Span,
}

macro_rules! is {
    ($invoker:ident, $current_or_next:ident,  $pattern:pat $(if $guard:expr)? $(,)?) => {
        $invoker.$current_or_next.as_ref().is_some_and(|locatable| matches!(&locatable.value,$pattern $(if $guard)?))
    };
}

impl<L> Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    pub fn new(program: Program) -> io::Result<Self> {
        let lexer = L::try_from(PathBuf::from(&program.file_name))?;
        Ok(Self {
            lexer,
            program,
            global: Vec::new(),
            current: None,
            next: None,
            span: Span::new(0, 0),
        })
    }

    pub fn parse(mut self) -> Program {
        let body = self.get_body();
        let mut program = self.program;
        program.body = Some(body);
        program
    }

    fn get_body(&mut self) -> CompilerResult<Vec<Node>> {
        self.advance()?;
        self.advance()?;

        while self.current.is_some() {
            let global_declaration = self.parse_global()?;
            self.global.push(global_declaration);
        }
        Ok(std::mem::take(&mut self.global))
    }

    fn advance(&mut self) -> CompilerResult<()> {
        if self.current.is_none() {
            let mut errors = Vec::new();
            self.get_all_errors(&mut errors);
            return Err(errors);
        }
        match self.lexer.next() {
            Some(Ok(token)) => {
                self.current = self.next.take();
                self.next = Some(token);
                Ok(())
            }
            Some(Err(errors)) => {
                let mut errors = errors;
                self.get_all_errors(&mut errors);
                Err(errors)
            }
            None => Ok(()),
        }
    }

    fn get_all_errors(&mut self, errors: &mut Vec<Locatable<CompilerError>>) {
        let all_results = std::mem::take(&mut self.lexer).collect::<Vec<_>>();
        let all_errors = all_results
            .into_iter()
            .filter_map(|r| if let Err(e) = r { Some(e) } else { None })
            .flatten();
        let mut errors = errors;
        errors.extend(all_errors);
    }

    fn parse_global(&mut self) -> CompilerResult<Node> {
        todo!()
    }

    fn parse_declaration(&mut self) -> CompilerResult<Node> {
        if is!(self, current, Token::Keyword(x) if x.is_type()) {
            self.advance()?;

            if is!(self, next, Token::Identifier(_)) {
                self.advance()?;
                // could be function or variable declaration
            } else {
                // this is cause for an error.
            }
        } else {
            // this is cause for an error.
        }

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
