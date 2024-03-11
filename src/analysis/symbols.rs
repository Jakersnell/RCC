use crate::analysis::hlir::{HlirType, HlirTypeDecl, HlirTypeKind, HlirVariable};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::{str_intern, Locatable, Span};
use std::collections::HashMap;

pub type SymbolResult = Result<(), CompilerError>;
#[derive(Debug, Clone)]
pub struct BuiltinFunctionSymbol {
    ty: &'static str,
    location: &'static str,
    ident: &'static str,
    params: Vec<HlirType>,
    varargs: bool,
    return_ty: HlirType,
}
thread_local! {
    static BUILTINS: [BuiltinFunctionSymbol; 3] = [
        BuiltinFunctionSymbol {
            ty: "C",
            location: "stdio.h",
            ident: "printf",
            params: vec![HlirType {
                kind: HlirTypeKind::Char(true),
                decl: HlirTypeDecl::Pointer(false),
            }],
            varargs: true,
            return_ty: HlirType {
                kind: HlirTypeKind::Void,
                decl: HlirTypeDecl::Basic,
            },
        },
        BuiltinFunctionSymbol {
            ty: "C",
            location: "stdlib.h",
            ident: "malloc",
            params: vec![HlirType {
                kind: HlirTypeKind::Int(true),
                decl: HlirTypeDecl::Basic,
            }],
            varargs: false,
            return_ty: HlirType {
                kind: HlirTypeKind::Void,
                decl: HlirTypeDecl::Basic,
            },
        },
        BuiltinFunctionSymbol {
            ty: "C",
            location: "stdlib.h",
            ident: "free",
            params: vec![HlirType {
                kind: HlirTypeKind::Void,
                decl: HlirTypeDecl::Pointer(false),
            }],
            varargs: false,
            return_ty: HlirType {
                kind: HlirTypeKind::Void,
                decl: HlirTypeDecl::Basic,
            },
        },
    ];
}

pub enum SymbolKind {
    BuiltinFunction(BuiltinFunctionSymbol),
    UserFunction(UserFunctionSymbol),
    Struct(StructSymbol),
    Variable(VariableSymbol),
}

pub struct StructSymbol {
    size: usize,
    fields: HashMap<InternedStr, HlirType>,
}

pub struct UserFunctionSymbol {
    return_ty: HlirType,
    parameters: Vec<HlirType>,
}

pub struct VariableSymbol {
    ty: HlirType,
    is_const: bool,
    is_initialized: bool,
}

pub struct SymbolResolver<'a> {
    symbols: HashMap<InternedStr, SymbolKind>,
    parent: Option<&'a SymbolResolver<'a>>,
}

impl<'a> SymbolResolver<'a> {
    pub fn create_root() -> Self {
        let mut root = Self {
            symbols: HashMap::new(),
            parent: None,
        };
        root.init_builtins();
        root
    }

    fn init_builtins(&mut self) {
        debug_assert!(self.parent.is_none());
        BUILTINS.with(|builtins| {
            for builtin in builtins.iter() {
                self.symbols.insert(
                    str_intern::intern(builtin.ident),
                    SymbolKind::BuiltinFunction(builtin.clone()),
                );
            }
        });
    }
    pub fn new(parent: Option<&'a SymbolResolver<'_>>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent,
        }
    }

    #[inline]
    pub fn add_function(
        &mut self,
        ident: &InternedStr,
        return_ty: HlirType,
        parameters: Vec<HlirType>,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::UserFunction(UserFunctionSymbol {
            return_ty,
            parameters,
        });
        self.add_symbol(ident, symbol, span)
    }

    #[inline]
    pub fn add_variable(
        &mut self,
        ident: &InternedStr,
        ty: &HlirType,
        is_const: bool,
        is_initialized: bool,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::Variable(VariableSymbol {
            ty: ty.clone(),
            is_const,
            is_initialized,
        });
        self.add_symbol(ident, symbol, span)
    }

    #[inline]
    fn add_symbol(&mut self, ident: &InternedStr, kind: SymbolKind, span: Span) -> SymbolResult {
        if self.symbols.contains_key(ident) {
            Err(CompilerError::IdentifierExists(span))
        } else {
            self.symbols.insert(ident.clone(), kind);
            Ok(())
        }
    }

    pub fn get_function_call(
        &self,
        ident: &InternedStr,
        args: &[HlirType],
        span: Span,
    ) -> Result<HlirType, CompilerError> {
        let kind = self.retrieve(ident, span)?;
        match kind {
            SymbolKind::BuiltinFunction(builtin) => {
                Self::validate_function_params(builtin.varargs, &builtin.params, args, span)?;
                Ok(builtin.return_ty.clone())
            }
            SymbolKind::UserFunction(user) => {
                Self::validate_function_params(false, &user.parameters, args, span)?;
                Ok(user.return_ty.clone())
            }
            _ => Err(CompilerError::NotAFunction(span)),
        }
    }

    fn validate_function_params(
        varargs: bool,
        params: &[HlirType],
        args: &[HlirType],
        span: Span,
    ) -> SymbolResult {
        if varargs {
            return Ok(());
        }
        if params.len() != args.len() {
            return Err(CompilerError::FunctionTypeMismatch(span));
        }
        for (param, arg) in params.iter().zip(args.iter()) {
            if param != arg {
                return Err(CompilerError::FunctionTypeMismatch(span));
            }
        }
        Ok(())
    }

    pub fn get_variable_type(
        &self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<HlirType, CompilerError> {
        match self.retrieve(ident, span)? {
            SymbolKind::Variable(var_ty) => Ok(var_ty.ty.clone()),
            _ => Err(CompilerError::NotAVariable(span)),
        }
    }

    pub fn check_valid_assignment(
        &self,
        ident: &InternedStr,
        ty: &HlirType,
        span: Span,
    ) -> SymbolResult {
        let var_ty = self.get_variable_type(ident, span)?;
        if var_ty != *ty {
            Err(CompilerError::VariableTypeMismatch(
                span,
                ty.to_string(),
                var_ty.to_string(),
            ))
        } else {
            Ok(())
        }
    }

    fn retrieve(&self, ident: &InternedStr, span: Span) -> Result<&SymbolKind, CompilerError> {
        let symbol = self.symbols.get(ident);
        if let Some(symbol) = symbol {
            Ok(symbol)
        } else if let Some(parent) = self.parent {
            parent.retrieve(ident, span)
        } else {
            Err(CompilerError::IdentNotFound(span))
        }
    }
}
