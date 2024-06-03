use crate::data::ast::*;
use crate::data::tokens::*;
use crate::parser::{Parser, ParseResult};
use crate::util::error::CompilerError;
use crate::util::Locatable;

use super::macros::*;

impl<L> Parser<L>
where
    L: Iterator<Item = Locatable<Token>>,
{
    pub(super) fn parse_init_declaration(&mut self) -> ParseResult<Locatable<InitDeclaration>> {
        let location = self.current_span()?;
        let dec = self.parse_declaration()?;
        let init_dec = if is!(
            self,
            current,
            Token::Symbol(Symbol::Semicolon) | Token::Symbol(Symbol::Comma)
        ) || is!(self, current, token if token.is_assign_op() )
        {
            let variable_declaration = self.parse_variable_declaration(dec)?;
            confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";")?;
            Ok(InitDeclaration::Declaration(variable_declaration))
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen)) {
            let function = self.parse_function_declaration(dec)?;
            Ok(InitDeclaration::Function(function))
        } else if is!(self, current, Token::Symbol(Symbol::OpenCurly)) {
            let _struct = self.parse_struct_declaration(dec)?;
            Ok(InitDeclaration::Struct(_struct))
        } else {
            self.report_error(CompilerError::ExpectedButFound(
                "function or variable declaration".to_string(),
                format!("{:#?}", self.current.as_ref().unwrap().value),
                location.merge(self.current_span),
            ));
            Err(())
        };
        init_dec.map(|init_dec| Locatable::new(location, init_dec))
    }

    pub(super) fn parse_struct_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<StructDeclaration>> {
        confirm!(self, consume, Token::Symbol(Symbol::OpenCurly), "{")?;
        let mut members = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseCurly)) {
            let member = self.parse_declaration()?;
            members.push(member);
            confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";");
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseCurly), "}")?;
        confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";")?;
        let location = declaration.location.merge(self.last_span);
        Ok(Locatable::new(
            location,
            StructDeclaration {
                declaration,
                members,
            },
        ))
    }

    pub(super) fn parse_declaration(&mut self) -> ParseResult<Locatable<Declaration>> {
        let location = self.current_span()?;
        let specifier = self.parse_declaration_specifier()?;
        let mut ident = match_token!(self, current, Token::Identifier(ident) => ident.clone());
        if ident.is_some() {
            self.advance()?;
        }
        let location = ident
            .as_ref()
            .map_or(location, |locatable| location.merge(locatable.location));
        Ok(Locatable {
            location,
            value: Declaration { specifier, ident },
        })
    }

    pub(super) fn parse_declaration_specifier(
        &mut self,
    ) -> ParseResult<Locatable<DeclarationSpecifier>> {
        let span = self.current_span()?;
        let mut storage_specifiers = Vec::new();
        while let Some(storage_specifier) =
            match_token!(self, current, |x|{StorageSpecifier::try_from(x)}, Ok(x) => x)
        {
            storage_specifiers.push(storage_specifier.value);
            self.advance()?;
        }
        let mut type_qualifiers = Vec::new();
        while let Some(type_qualifier) =
            match_token!(self, current, |x|{TypeQualifier::try_from(x)}, Ok(x) => x)
        {
            type_qualifiers.push(type_qualifier.value);
            self.advance()?;
        }
        let mut type_specifiers = Vec::new();
        loop {
            if let Some(type_specifier) =
                match_token!(self, current, |x|{TypeSpecifier::try_from(x)}, Ok(x) => x)
            {
                type_specifiers.push(type_specifier.value);
                self.advance()?;
            } else if is!(self, current, Token::Keyword(Keyword::Struct)) {
                self.advance()?;
                let ident = self.confirm_identifier()?;
                type_specifiers.push(TypeSpecifier::Struct(ident.value));
            } else {
                break;
            }
        }
        let mut pointer = false;
        while is!(self, current, Token::Symbol(Symbol::Star)) {
            if pointer {
                self.report_error(CompilerError::CustomError(
                    "Pointer depth greater than one is not currently supported!".to_string(),
                    span,
                ));
                return Err(());
            }
            self.advance()?;
            pointer = true;
        }
        let span = span.extend(self.current_span()?);
        Ok(Locatable {
            location: span,
            value: DeclarationSpecifier {
                specifiers: storage_specifiers,
                qualifiers: type_qualifiers,
                ty: type_specifiers,
                pointer,
            },
        })
    }

    pub(super) fn parse_function_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<FunctionDeclaration>> {
        confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
        let mut parameters = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseParen)) {
            let param = self.parse_declaration()?;
            parameters.push(param);
            if is!(self, current, Token::Symbol(Symbol::Comma)) {
                self.advance()?;
            } else {
                break;
            }
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
        let body = self.parse_compound_statement()?;
        let location = declaration.location.merge(body.location);
        Ok(Locatable::new(
            location,
            FunctionDeclaration {
                declaration,
                parameters,
                body,
            },
        ))
    }

    pub(super) fn parse_variable_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<VariableDeclaration>> {
        let is_array = is!(self, current, Token::Symbol(Symbol::OpenSquare));
        let array_size = if is_array {
            self.advance()?;
            let size = if let Some(Locatable {
                location,
                value: (integer, suffix),
            }) = match_token!(self, current, Token::Literal(Literal::Integer {value, suffix}) => (*value, suffix.clone()))
            {
                if suffix.is_some() {
                    self.report_error(CompilerError::CustomError(
                        "Suffixes in array sizes are not currently supported.".to_string(),
                        location,
                    ));
                    return Err(());
                }
                self.advance()?;
                Some(integer as usize)
            } else {
                None
            };
            confirm!(self, consume, Token::Symbol(Symbol::CloseSquare) => (), "]")?;
            size
        } else {
            None
        };
        if is!(self, current, Token::Symbol(Symbol::OpenSquare)) {
            self.report_error(CompilerError::CustomError(
                "Nested arrays are not currently supported!".to_string(),
                declaration.location,
            ));
            return Err(());
        }
        let initializer = if is!(self, current, Token::Symbol(Symbol::Equal)) {
            self.advance()?;
            Some(self.parse_initializer()?)
        } else {
            None
        };
        let location = declaration.location.merge(self.last_span);
        Ok(Locatable::new(
            location,
            VariableDeclaration {
                is_array,
                array_size,
                declaration,
                initializer,
            },
        ))
    }
}
