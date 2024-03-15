use crate::analysis::hlir::{HlirStruct, HlirType, HlirTypeDecl, HlirTypeKind, HlirVariable};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::{str_intern, Locatable, Span};
use std::collections::HashMap;
use std::sync::Arc;

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
    static BUILTINS: [(&'static str, FunctionSymbol); 3] = [
        (
            "printf",
            FunctionSymbol {
                location: Some("stdio.h"),
                params: vec![HlirType {
                    kind: HlirTypeKind::Char(true),
                    decl: HlirTypeDecl::Pointer(false),
                }],
                varargs: true,
                return_ty: HlirType {
                    kind: HlirTypeKind::Void,
                    decl: HlirTypeDecl::Basic,
                },
            }
        ),
        (
            "malloc",
            FunctionSymbol {
                location: Some("stdlib.h"),
                  params: vec![HlirType {
                    kind: HlirTypeKind::Int(true),
                    decl: HlirTypeDecl::Basic,
                }],
                varargs: false,
                return_ty: HlirType {
                    kind: HlirTypeKind::Void,
                    decl: HlirTypeDecl::Basic,
                },
            }
        ),
        (
            "free",
            FunctionSymbol {
                location: Some("stdlib.h"),
                params: vec![HlirType {
                    kind: HlirTypeKind::Void,
                    decl: HlirTypeDecl::Pointer(false),
                }],
                varargs: false,
                return_ty: HlirType {
                    kind: HlirTypeKind::Void,
                    decl: HlirTypeDecl::Basic,
                },
            }
        ),
    ];
}

pub(super) enum SymbolKind {
    Function(FunctionSymbol),
    Struct(StructSymbol),
    Variable(VariableSymbol),
}

pub(super) struct StructSymbol {
    pub(super) size: u64,
    pub(super) body: SymbolResolver,
}

#[derive(Clone)]
pub(super) struct FunctionSymbol {
    pub(super) location: Option<&'static str>,
    pub(super) return_ty: HlirType,
    pub(super) varargs: bool,
    pub(super) params: Vec<HlirType>,
}

pub(super) struct VariableSymbol {
    pub(super) ty: HlirType,
    pub(super) is_const: bool,
    pub(super) is_initialized: bool,
    pub(super) array_size: Option<u64>,
}

pub struct SymbolResolver {
    symbols: HashMap<InternedStr, SymbolKind>,
    parent: Option<Box<SymbolResolver>>,
}

impl SymbolResolver {
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
                    str_intern::intern(builtin.0),
                    SymbolKind::Function(builtin.1.clone()),
                );
            }
        });
    }
    pub fn new(parent: Option<Box<SymbolResolver>>) -> Self {
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
        params: Vec<HlirType>,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::Function(FunctionSymbol {
            location: None,
            return_ty,
            varargs: false,
            params,
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
        array_size: Option<u64>,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::Variable(VariableSymbol {
            ty: ty.clone(),
            is_const,
            is_initialized,
            array_size,
        });
        self.add_symbol(ident, symbol, span)
    }

    #[inline]
    fn add_symbol(&mut self, ident: &InternedStr, kind: SymbolKind, span: Span) -> SymbolResult {
        if self.retrieve(ident, span).is_ok() {
            Err(CompilerError::IdentifierExists(span))
        } else {
            self.symbols.insert(ident.clone(), kind);
            Ok(())
        }
    }

    pub fn validate_function_call(
        &self,
        ident: &InternedStr,
        args: &[HlirType],
        span: Span,
    ) -> Result<HlirType, CompilerError> {
        let kind = self.retrieve(ident, span)?;
        match kind {
            SymbolKind::Function(builtin) => {
                Self::validate_function_params(builtin.varargs, &builtin.params, args, span)?;
                Ok(builtin.return_ty.clone())
            }
            SymbolKind::Function(user) => {
                Self::validate_function_params(false, &user.params, args, span)?;
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

    pub fn check_valid_assignment(
        &self,
        ident: &InternedStr,
        ty: &HlirType,
        span: Span,
    ) -> SymbolResult {
        let var = match self.retrieve(ident, span)? {
            SymbolKind::Variable(var_ty) => Ok(var_ty),
            _ => Err(CompilerError::LeftHandNotLVal(span)),
        }?;
        self.verify_for_variable_symbol_assignment(ty, var, span)
    }

    pub fn get_variable_type(
        &self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<HlirType, CompilerError> {
        match self.retrieve(ident, span)? {
            SymbolKind::Variable(var) => Ok(var.ty.clone()),
            _ => Err(CompilerError::NotAVariable(span)),
        }
    }

    fn verify_for_variable_symbol_assignment(
        &self,
        ty: &HlirType,
        var: &VariableSymbol,
        span: Span,
    ) -> Result<(), CompilerError> {
        if &var.ty != ty {
            Err(CompilerError::VariableTypeMismatch(
                span,
                ty.to_string(),
                var.ty.to_string(),
            ))
        } else if var.is_const {
            Err(CompilerError::ConstAssignment(span))
        } else {
            Ok(())
        }
    }

    fn retrieve(&self, ident: &InternedStr, span: Span) -> Result<&SymbolKind, CompilerError> {
        let symbol = self.symbols.get(ident);
        if let Some(symbol) = symbol {
            Ok(symbol)
        } else if let Some(parent) = self.parent.as_ref() {
            parent.retrieve(ident, span)
        } else {
            Err(CompilerError::IdentNotFound(span))
        }
    }

    pub fn add_struct(&mut self, _struct: &HlirStruct, span: Span) -> SymbolResult {
        let ident = _struct.ident.clone();
        let mut symbol = StructSymbol {
            size: _struct.size,
            body: SymbolResolver::new(None),
        };
        for field in &_struct.fields {
            let array_size = if let HlirTypeDecl::Array(size) = &field.ty.decl {
                Some(*size)
            } else {
                None
            };
            symbol.body.add_variable(
                &ident,
                &field.ty,
                field.is_const,
                field.initializer.is_some(),
                array_size,
                span,
            )?;
        }
        let symbol = SymbolKind::Struct(symbol);
        self.add_symbol(&ident, symbol, span)
    }

    pub fn get_struct(
        &self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<&StructSymbol, CompilerError> {
        match self.retrieve(ident, span)? {
            SymbolKind::Struct(s) => Ok(s),
            _ => Err(CompilerError::NotAStruct(span)),
        }
    }

    pub fn get_struct_size(&self, ident: &InternedStr, span: Span) -> Result<u64, CompilerError> {
        Ok(self.get_struct(ident, span)?.size)
    }
}
