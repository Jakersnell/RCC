use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::data::mlir::{MlirType, MlirTypeDecl, MlirTypeKind};
use crate::util::str_intern;
use crate::util::str_intern::InternedStr;

#[derive(Debug, Clone)]
pub(crate) struct StructSymbol {
    pub(crate) size: u64,
    pub(crate) as_type: MlirType,
    pub(crate) body: HashMap<InternedStr, VariableSymbol>,
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionSymbol {
    pub(crate) ident: InternedStr,
    pub(crate) location: Option<&'static str>,
    pub(crate) return_ty: MlirType,
    pub(crate) varargs: bool,
    pub(crate) params: Vec<MlirType>,
}

#[derive(Clone, Debug)]
pub(crate) struct VariableSymbol {
    pub(crate) uid: usize,
    pub(crate) ty: MlirType,
    pub(crate) is_const: bool,
    pub(crate) is_initialized: bool,
    pub(crate) array_size: Option<u64>,
}

lazy_static! {
    pub(crate) static ref BUILTINS: [(&'static str, FunctionSymbol); 3] = [
        (
            "printf",
            FunctionSymbol {
                ident: str_intern::intern("printf"),
                location: Some("stdio.h"),
                params: vec![MlirType {
                    kind: MlirTypeKind::Char(true),
                    decl: MlirTypeDecl::Pointer,
                }],
                varargs: true,
                return_ty: MlirType {
                    kind: MlirTypeKind::Int(false),
                    decl: MlirTypeDecl::Basic,
                },
            }
        ),
        (
            "malloc",
            FunctionSymbol {
                ident: str_intern::intern("malloc"),
                location: Some("stdlib.h"),
                params: vec![MlirType {
                    kind: MlirTypeKind::Long(true),
                    decl: MlirTypeDecl::Basic,
                }],
                varargs: false,
                return_ty: MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Pointer,
                },
            }
        ),
        (
            "free",
            FunctionSymbol {
                ident: str_intern::intern("free"),
                location: Some("stdlib.h"),
                params: vec![MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Pointer,
                }],
                varargs: false,
                return_ty: MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Basic,
                },
            }
        ),
    ];
}
