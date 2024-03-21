use crate::analysis::hlir::*;
use std::cmp::Ordering;

macro_rules! cast {
    ($hlir:expr, $to:expr) => {{
        HlirExpr {
            kind: Box::new(HlirExprKind::Cast($to.clone(), $hlir)),
            ty: $to,
            is_lval: false,
        }
    }};
}

pub(in crate::analysis) fn explicit_cast(
    cast_ty: HlirType,
    expr: HlirExpr,
) -> Result<HlirExpr, ()> {
    if cast_ty == expr.ty {
        return Ok(expr);
    }
    let expr_ty = &expr.ty;
    match (&expr_ty.kind, &expr_ty.decl, &cast_ty.kind, &cast_ty.decl) {
        (_, HlirTypeDecl::Array(_), to_kind, HlirTypeDecl::Pointer) => Ok(cast!(
            expr,
            HlirType {
                kind: to_kind.clone(),
                decl: HlirTypeDecl::Pointer
            }
        )),
        (kind, HlirTypeDecl::Pointer, to_kind, HlirTypeDecl::Pointer) => Ok(cast!(
            expr,
            HlirType {
                kind: to_kind.clone(),
                decl: HlirTypeDecl::Basic
            }
        )),
        (kind, HlirTypeDecl::Pointer, HlirTypeKind::Long(signed), HlirTypeDecl::Basic) => {
            Ok(cast!(
                expr,
                HlirType {
                    kind: HlirTypeKind::Long(true),
                    decl: HlirTypeDecl::Basic
                }
            ))
        }
        (kind, HlirTypeDecl::Basic, cast_kind, HlirTypeDecl::Basic)
            if kind.is_numeric() && cast_kind.is_numeric() =>
        {
            Ok(cast_numeric_to_numeric(cast_ty, expr))
        }
        _ => Err(()),
    }
}

pub(in crate::analysis) fn implicit_cast(
    cast_to: HlirType,
    expr: HlirExpr,
) -> Result<HlirExpr, ()> {
    match (&cast_to.kind, &cast_to.decl, &expr.ty.kind, &expr.ty.decl) {
        (_, HlirTypeDecl::Pointer, HlirTypeKind::Void, HlirTypeDecl::Pointer) => {
            let ty = HlirType {
                kind: HlirTypeKind::Void,
                decl: HlirTypeDecl::Pointer,
            };
            Ok(HlirExpr {
                kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
                is_lval: false,
                ty,
            })
        }
        (kind, HlirTypeDecl::Array(_), cast_kind, HlirTypeDecl::Pointer) if kind == cast_kind => {
            let ty = HlirType {
                kind: cast_kind.clone(),
                decl: HlirTypeDecl::Pointer,
            };
            Ok(HlirExpr {
                kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
                is_lval: false,
                ty,
            })
        }
        (casting_kind, HlirTypeDecl::Basic, cast_kind, HlirTypeDecl::Basic) => {
            Ok(cast_numeric_to_numeric(cast_to, expr))
        }
        _ => Err(()),
    }
}

pub(in crate::analysis) fn cast_numeric_to_numeric(cast_to: HlirType, expr: HlirExpr) -> HlirExpr {
    debug_assert!(cast_to.decl == HlirTypeDecl::Basic);
    debug_assert!(expr.ty.decl == HlirTypeDecl::Basic);
    if matches!(
        (&expr.ty.kind, &cast_to.kind),
        (HlirTypeKind::Double, HlirTypeKind::Float) | (HlirTypeKind::Int(_), HlirTypeKind::Float)
    ) {
        let ty = HlirType {
            kind: HlirTypeKind::Float,
            decl: HlirTypeDecl::Basic,
        };
        HlirExpr {
            kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
            is_lval: false,
            ty,
        }
    } else {
        let expr_level = get_numeric_cast_hierarchy(&expr.ty.kind);
        let cast_level = get_numeric_cast_hierarchy(&cast_to.kind);
        let kind = match expr_level.cmp(&cast_level) {
            Ordering::Less => Some(demote_numeric(&cast_to.kind)),
            Ordering::Greater => Some(promote_numeric(&cast_to.kind)),
            Ordering::Equal => None,
        };
        if let Some(kind) = kind {
            let ty = HlirType {
                decl: HlirTypeDecl::Basic,
                kind,
            };
            let casted_lower = cast_numeric_to_numeric(ty, expr);
            HlirExpr {
                kind: Box::new(HlirExprKind::Cast(cast_to.clone(), casted_lower)),
                ty: cast_to,
                is_lval: false,
            }
        } else {
            HlirExpr {
                kind: Box::new(HlirExprKind::Cast(cast_to.clone(), expr)),
                is_lval: false,
                ty: cast_to,
            }
        }
    }
}

fn give_numeric_sign(ty: HlirTypeKind, signed: bool) -> HlirTypeKind {
    match ty {
        HlirTypeKind::Long(_) => HlirTypeKind::Long(signed),
        HlirTypeKind::Int(_) => HlirTypeKind::Int(signed),
        HlirTypeKind::Char(_) => HlirTypeKind::Char(signed),
        _ => ty,
    }
}

fn get_numeric_signed(ty: &HlirTypeKind) -> bool {
    match ty {
        HlirTypeKind::Long(signed) => *signed,
        HlirTypeKind::Int(signed) => *signed,
        HlirTypeKind::Char(signed) => *signed,
        _ => false,
    }
}

fn demote_numeric(ty: &HlirTypeKind) -> HlirTypeKind {
    // does not include double -> float
    match ty {
        HlirTypeKind::Double => HlirTypeKind::Long(false),
        HlirTypeKind::Float => HlirTypeKind::Int(false),
        HlirTypeKind::Long(_) => HlirTypeKind::Int(false),
        HlirTypeKind::Int(_) => HlirTypeKind::Char(false),
        _ => unreachable!(),
    }
}

fn promote_numeric(ty: &HlirTypeKind) -> HlirTypeKind {
    // does not include int -> float
    match ty {
        HlirTypeKind::Float => HlirTypeKind::Double,
        HlirTypeKind::Long(_) => HlirTypeKind::Double,
        HlirTypeKind::Int(_) => HlirTypeKind::Long(false),
        HlirTypeKind::Char(_) => HlirTypeKind::Int(false),
        _ => unreachable!(),
    }
}

fn get_numeric_cast_hierarchy(ty: &HlirTypeKind) -> u8 {
    match ty {
        HlirTypeKind::Double => 4,
        HlirTypeKind::Long(_) => 3,
        HlirTypeKind::Int(_) => 2,
        HlirTypeKind::Char(_) => 1,
        _ => unreachable!(),
    }
}

#[cfg(test)]
fn test_cast_structure(expr: HlirExpr, cast_to: HlirType, order: &[HlirTypeKind]) {
    let cast_structure = cast_numeric_to_numeric(cast_to, expr);

    let mut given = cast_structure;

    for kind in order {
        let given_ty = given.ty.clone();
        assert_eq!(given_ty.kind, *kind);
        given = match *given.kind {
            HlirExprKind::Cast(ty, expr) => {
                assert_eq!(ty.kind, *kind);
                expr
            }
            _ => panic!("Unexpected expr type, expected HlirExprKind::Cast."),
        }
    }
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_upcast() {
    let expr = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Char(1))),
        ty: HlirType {
            kind: HlirTypeKind::Char(false),
            decl: HlirTypeDecl::Basic,
        },
        is_lval: false,
    };
    let cast_to = HlirType {
        kind: HlirTypeKind::Double,
        decl: HlirTypeDecl::Basic,
    };

    let expected = [
        HlirTypeKind::Double,
        HlirTypeKind::Long(false),
        HlirTypeKind::Int(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_downcast() {
    let expr = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Float(1.0))),
        ty: HlirType {
            kind: HlirTypeKind::Double,
            decl: HlirTypeDecl::Basic,
        },
        is_lval: false,
    };

    let cast_to = HlirType {
        kind: HlirTypeKind::Char(false),
        decl: HlirTypeDecl::Basic,
    };

    let expected = [
        HlirTypeKind::Char(false),
        HlirTypeKind::Int(false),
        HlirTypeKind::Long(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}
