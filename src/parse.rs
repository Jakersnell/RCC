use crate::ast::{ASTNode, Program};
use crate::lex::LexResult;
use crate::util::{CompilerResult, Span, Token};
use std::io;
use std::path::PathBuf;

pub struct Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    lexer: L,
    program: Program,
    global: Vec<ASTNode>,
    current: Option<Token>,
    next: Option<Token>,
    span: Span,
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

    fn get_body(&mut self) -> CompilerResult<Vec<ASTNode>> {
        self.advance()?;
        self.advance()?;

        while self.current.is_some() {
            let global_declaration = self.parse_global()?;
            self.global.push(global_declaration);
        }
        Ok(std::mem::take(&mut self.global))
    }

    fn advance(&mut self) -> CompilerResult<()> {
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

    fn parse_global(&mut self) -> CompilerResult<ASTNode> {
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
