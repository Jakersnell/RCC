use crate::data::mlir::*;
use crate::util::str_intern::get;
use crate::util::Span;
use std::cmp::Ordering;
use std::mem::discriminant;

macro_rules! cast {
    ($hlir:expr, $to:expr, $cast_type:expr, $span:expr, $lval:literal) => {{
        MlirExpr {
            span: $span,
            kind: Box::new(MlirExprKind::Cast($cast_type, $hlir)),
            ty: $to,
            is_lval: $lval,
        }
    }};
}

pub(in crate::analysis) fn explicit_cast(
    cast_ty: MlirType,
    expr: MlirExpr,
    span: Span,
) -> Result<MlirExpr, ()> {
    if cast_ty == expr.ty {
        return Ok(expr);
    }
    let expr_ty = &expr.ty;
    match (&expr_ty.kind, &expr_ty.decl, &cast_ty.kind, &cast_ty.decl) {
        (_, MlirTypeDecl::Array(_), to_kind, MlirTypeDecl::Pointer) => Ok(cast!(
            expr,
            MlirType {
                kind: to_kind.clone(),
                decl: MlirTypeDecl::Pointer
            },
            CastType::ArrayToPointer,
            span,
            true
        )),
        (kind, MlirTypeDecl::Pointer, to_kind, MlirTypeDecl::Pointer) => Ok(cast!(
            expr,
            MlirType {
                kind: to_kind.clone(),
                decl: MlirTypeDecl::Basic
            },
            CastType::PointerToPointer,
            span,
            true
        )),
        (kind, MlirTypeDecl::Pointer, MlirTypeKind::Long(signed), MlirTypeDecl::Basic) => {
            Ok(cast!(
                expr,
                MlirType {
                    kind: MlirTypeKind::Long(true),
                    decl: MlirTypeDecl::Basic
                },
                CastType::PointerToLong,
                span,
                true
            ))
        }
        (kind, MlirTypeDecl::Basic, cast_kind, MlirTypeDecl::Basic)
            if expr.ty.is_numeric() && cast_ty.is_numeric() =>
        {
            Ok(cast_numeric_to_numeric(cast_ty, expr, span))
        }
        _ => Err(()),
    }
}

pub(in crate::analysis) fn implicit_cast(
    cast_to: MlirType,
    expr: MlirExpr,
    span: Span,
) -> Result<MlirExpr, ()> {
    if expr.ty == cast_to {
        return Ok(expr);
    }
    match (&cast_to.kind, &cast_to.decl, &expr.ty.kind, &expr.ty.decl) {
        (_, MlirTypeDecl::Pointer, MlirTypeKind::Void, MlirTypeDecl::Pointer) => Ok(cast!(
            expr,
            MlirType {
                kind: MlirTypeKind::Void,
                decl: MlirTypeDecl::Pointer,
            },
            CastType::PointerToPointer,
            span,
            false
        )),
        (kind, MlirTypeDecl::Array(_), cast_kind, MlirTypeDecl::Pointer) if kind == cast_kind => {
            let kind = cast_kind.clone();
            Ok(cast!(
                expr,
                MlirType {
                    kind,
                    decl: MlirTypeDecl::Pointer,
                },
                CastType::ArrayToPointer,
                span,
                false
            ))
        }
        (casting_kind, MlirTypeDecl::Basic, cast_kind, MlirTypeDecl::Basic) => {
            Ok(cast_numeric_to_numeric(cast_to, expr, span))
        }
        _ => Err(()),
    }
}

pub(in crate::analysis) fn cast_numeric_to_numeric(
    cast_to: MlirType,
    expr: MlirExpr,
    span: Span,
) -> MlirExpr {
    debug_assert!(cast_to.decl == MlirTypeDecl::Basic);
    debug_assert!(expr.ty.decl == MlirTypeDecl::Basic);
    if expr.ty.kind == cast_to.kind {
        expr
    } else if discriminant(&expr.ty.kind) == discriminant(&cast_to.kind) {
        let ty = expr.ty.clone();
        if get_numeric_signed(&cast_to.kind) {
            cast!(expr, ty, CastType::UnsignedToSigned, span, false)
        } else {
            cast!(expr, ty, CastType::SignedToUnsigned, span, false)
        }
    } else if matches!(
        (&expr.ty.kind, &cast_to.kind),
        (MlirTypeKind::Double, MlirTypeKind::Float)
    ) {
        // double -> float && int -> float are handled separately
        // as they would cause a clash in promote/demote logic
        cast!(
            expr,
            MlirType {
                kind: MlirTypeKind::Float,
                decl: MlirTypeDecl::Basic,
            },
            CastType::DoubleToFloat,
            span,
            false
        )
    } else if matches!(
        (&expr.ty.kind, &cast_to.kind),
        | (MlirTypeKind::Int(_), MlirTypeKind::Float))
    {
        cast!(
            expr,
            MlirType {
                kind: MlirTypeKind::Float,
                decl: MlirTypeDecl::Basic,
            },
            CastType::IntToFloat,
            span,
            false
        )
    } else {
        let expr_level = get_numeric_cast_hierarchy(&expr.ty.kind);
        let cast_level = get_numeric_cast_hierarchy(&cast_to.kind);
        let kind = match expr_level.cmp(&cast_level) {
            Ordering::Less => Some(demote_numeric(&cast_to.kind)),
            Ordering::Greater => Some(promote_numeric(&cast_to.kind)),
            Ordering::Equal => None,
        };
        if let Some((cast_type, kind)) = kind {
            let ty = MlirType {
                decl: MlirTypeDecl::Basic,
                kind,
            };
            let casted_lower = cast_numeric_to_numeric(ty, expr, span);

            MlirExpr {
                span,
                kind: Box::new(MlirExprKind::Cast(cast_type, casted_lower)),
                ty: cast_to,
                is_lval: false,
            }
        } else {
            unreachable!()
        }
    }
}
fn give_numeric_sign(ty: MlirTypeKind, signed: bool) -> MlirTypeKind {
    match ty {
        MlirTypeKind::Long(_) => MlirTypeKind::Long(signed),
        MlirTypeKind::Int(_) => MlirTypeKind::Int(signed),
        MlirTypeKind::Char(_) => MlirTypeKind::Char(signed),
        _ => ty,
    }
}

fn get_numeric_signed(ty: &MlirTypeKind) -> bool {
    match ty {
        MlirTypeKind::Long(signed) => *signed,
        MlirTypeKind::Int(signed) => *signed,
        MlirTypeKind::Char(signed) => *signed,
        _ => false,
    }
}

fn demote_numeric(ty: &MlirTypeKind) -> (CastType, MlirTypeKind) {
    // does not include double -> float, as this needs to be handled directly
    match ty {
        MlirTypeKind::Double => (CastType::LongToDouble, MlirTypeKind::Long(false)),
        MlirTypeKind::Float => (CastType::IntToFloat, MlirTypeKind::Int(false)),
        MlirTypeKind::Long(signed) => (CastType::IntToLong, MlirTypeKind::Int(*signed)),
        MlirTypeKind::Int(signed) => (CastType::CharToInt, MlirTypeKind::Char(*signed)),
        _ => panic!("Unexpected type: {:#?}", ty),
    }
}

fn promote_numeric(ty: &MlirTypeKind) -> (CastType, MlirTypeKind) {
    // does not include int -> float
    match ty {
        MlirTypeKind::Float => (CastType::DoubleToFloat, MlirTypeKind::Double),
        MlirTypeKind::Long(signed) => (CastType::DoubleToLong, MlirTypeKind::Double),
        MlirTypeKind::Int(signed) => (CastType::LongToInt, MlirTypeKind::Long(*signed)),
        MlirTypeKind::Char(signed) => (CastType::IntToChar, MlirTypeKind::Int(*signed)),
        _ => panic!("Unexpected type: {:#?}", ty),
    }
}

fn get_numeric_cast_hierarchy(ty: &MlirTypeKind) -> u8 {
    match ty {
        MlirTypeKind::Double => 4,
        MlirTypeKind::Long(signed) => 3,
        MlirTypeKind::Int(signed) => 2,
        MlirTypeKind::Char(signed) => 1,
        _ => panic!("Unexpected type: {:#?}", ty),
    }
}

#[cfg(test)]
fn test_cast_structure(expr: MlirExpr, cast_to: MlirType, order: &[MlirTypeKind]) {
    let cast_structure = cast_numeric_to_numeric(cast_to, expr, Span::default());
    let mut given = cast_structure;

    for kind in order {
        let given_ty = given.ty.clone();
        assert_eq!(given_ty.kind, *kind);
        given = match *given.kind {
            MlirExprKind::Cast(ty, expr) => {
                assert_eq!(given.ty.kind, *kind);
                expr
            }
            _ => panic!("Unexpected expr type, expected HlirExprKind::Cast."),
        }
    }
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_upcast() {
    let expr = MlirExpr {
        span: Span::default(),
        kind: Box::new(MlirExprKind::Literal(MlirLiteral::Char(1))),
        ty: MlirType {
            kind: MlirTypeKind::Char(false),
            decl: MlirTypeDecl::Basic,
        },
        is_lval: false,
    };
    let cast_to = MlirType {
        kind: MlirTypeKind::Double,
        decl: MlirTypeDecl::Basic,
    };

    let expected = [
        MlirTypeKind::Double,
        MlirTypeKind::Long(false),
        MlirTypeKind::Int(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_downcast() {
    let expr = MlirExpr {
        span: Span::default(),
        kind: Box::new(MlirExprKind::Literal(MlirLiteral::Float(1.0))),
        ty: MlirType {
            kind: MlirTypeKind::Double,
            decl: MlirTypeDecl::Basic,
        },
        is_lval: false,
    };

    let cast_to = MlirType {
        kind: MlirTypeKind::Char(false),
        decl: MlirTypeDecl::Basic,
    };

    let expected = [
        MlirTypeKind::Char(false),
        MlirTypeKind::Int(false),
        MlirTypeKind::Long(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}

#[test]
fn test_cast_type_to_itself_returns_initial_expression_for_struct_pointer() {
    let ty = MlirType {
        kind: MlirTypeKind::Struct(crate::util::str_intern::intern("test")),
        decl: MlirTypeDecl::Pointer,
    };
    let expr = MlirExpr {
        span: Span::default(),
        kind: Box::new(MlirExprKind::Literal(MlirLiteral::Int(0))), // kind can be anything here, it is not used
        ty: MlirType {
            kind: MlirTypeKind::Struct(crate::util::str_intern::intern("test")), // separate interning is important
            decl: MlirTypeDecl::Pointer,
        },
        is_lval: false,
    };
    let result = implicit_cast(ty, expr, Span::default());
    assert!(result.is_ok());
}
