use crate::analysis::hlir::{HlirType, HlirTypeKind};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::{str_intern, Locatable, Span};
use std::collections::HashMap;

pub type SymbolResult<T> = Result<T, CompilerError>;
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
                pointer: false,
                is_const: true,
                kind: HlirTypeKind::Char(true),
            }],
            varargs: true,
            return_ty: HlirType {
                pointer: false,
                is_const: false,
                kind: HlirTypeKind::Void,
            },
        },
        BuiltinFunctionSymbol {
            ty: "C",
            location: "stdlib.h",
            ident: "malloc",
            params: vec![HlirType {
                pointer: false,
                is_const: false,
                kind: HlirTypeKind::Int(true),
            }],
            varargs: false,
            return_ty: HlirType {
                pointer: true,
                is_const: false,
                kind: HlirTypeKind::Void,
            },
        },
        BuiltinFunctionSymbol {
            ty: "C",
            location: "stdlib.h",
            ident: "free",
            params: vec![HlirType {
                pointer: true,
                is_const: false,
                kind: HlirTypeKind::Void,
            }],
            varargs: false,
            return_ty: HlirType {
                pointer: false,
                is_const: false,
                kind: HlirTypeKind::Void,
            },
        },
    ];
}

pub enum SymbolKind {
    BuiltinFunction(BuiltinFunctionSymbol),
    UserFunction(UserFunctionSymbol),
    Variable(HlirType),
}

pub struct UserFunctionSymbol {
    return_ty: HlirType,
    parameters: Vec<HlirType>,
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
        ident: &Locatable<InternedStr>,
        return_ty: HlirType,
        parameters: Vec<HlirType>,
    ) -> SymbolResult<()> {
        self.add_symbol(
            ident,
            SymbolKind::UserFunction(UserFunctionSymbol {
                return_ty,
                parameters,
            }),
        )
    }

    #[inline]
    pub fn add_variable(
        &mut self,
        ident: &Locatable<InternedStr>,
        ty: HlirType,
    ) -> SymbolResult<()> {
        self.add_symbol(ident, SymbolKind::Variable(ty))
    }

    #[inline]
    fn add_symbol(&mut self, ident: &Locatable<InternedStr>, kind: SymbolKind) -> SymbolResult<()> {
        if self.symbols.contains_key(&ident.value) {
            Err(CompilerError::IdentifierExists(ident.location))
        } else {
            self.symbols.insert(ident.value.clone(), kind);
            Ok(())
        }
    }

    pub fn validate_function_call(
        &self,
        ident: &InternedStr,
        args: &[HlirType],
        span: Span,
    ) -> SymbolResult<()> {
        let kind = self.retrieve(ident, span)?;
        fn validate(
            varargs: bool,
            params: &[HlirType],
            args: &[HlirType],
            span: Span,
        ) -> SymbolResult<()> {
            if varargs {
                Ok(())
            } else if params.len() == args.len() {
                for (param, arg) in params.iter().zip(args.iter()) {
                    if param != arg {
                        return Err(CompilerError::FunctionTypeMismatch(span));
                    }
                }
                Ok(())
            } else {
                Err(CompilerError::FunctionTypeMismatch(span))
            }
        }
        match kind {
            SymbolKind::BuiltinFunction(builtin) => {
                validate(builtin.varargs, &builtin.params, args, span)
            }
            SymbolKind::UserFunction(user) => validate(false, &user.parameters, args, span),
            SymbolKind::Variable(_) => Err(CompilerError::NotAFunction(span)),
        }
    }

    fn validate_variable_type(
        &self,
        ident: &InternedStr,
        ty: &HlirType,
        span: Span,
    ) -> SymbolResult<()> {
        let kind = self.retrieve(ident, span)?;
        match kind {
            SymbolKind::Variable(var_ty) => {
                if var_ty != ty {
                    Err(CompilerError::VariableTypeMismatch(
                        span,
                        ty.clone(),
                        var_ty.clone(),
                    ))
                } else {
                    Ok(())
                }
            }
            SymbolKind::BuiltinFunction(_) | SymbolKind::UserFunction(_) => {
                Err(CompilerError::NotAVariable(span))
            }
        }
    }

    fn retrieve(&self, ident: &InternedStr, span: Span) -> SymbolResult<&SymbolKind> {
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
