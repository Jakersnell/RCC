use std::cmp::Ordering;

use crate::analysis::GlobalValidator;
use crate::data::ast::AbstractSyntaxTree;
use crate::data::mlir::*;
use crate::util::error::CompilerError;
use crate::util::Span;

macro_rules! cast_basic {
    ($expr:expr, $cast_ty:expr, $cast_to:expr) => {{
        let expr = $expr;
        let span = expr.span;
        MlirExpr {
            kind: Box::new(MlirExprKind::Cast($cast_ty, expr)),
            is_lval: false,
            ty: basic_ty!($cast_to),
            span,
        }
    }};
}

impl GlobalValidator {
    pub(super) fn explicit_cast(
        &mut self,
        expr: MlirExpr,
        cast_to: MlirType,
        span: Span,
    ) -> MlirExpr {
        macro_rules! cast {
            ($cast_ty:expr, $is_lval: literal) => {
                MlirExpr {
                    kind: Box::new(MlirExprKind::Cast($cast_ty, expr)),
                    is_lval: $is_lval,
                    ty: cast_to,
                    span,
                }
            };
        }
        macro_rules! cast_to_pointer {
            ($cast_ty:expr) => {
                cast!($cast_ty, true)
            };
        }

        macro_rules! cast_to_basic {
            ($cast_ty:expr) => {
                cast!($cast_ty, false)
            };
        }

        match (&expr.ty, &cast_to) {
            (
                // number <-> number
                MlirType {
                    kind: left,
                    decl: MlirTypeDecl::Basic,
                },
                MlirType {
                    kind: right,
                    decl: MlirTypeDecl::Basic,
                },
            ) if left.is_numeric() && right.is_numeric() => numeric_cast(expr, cast_to, span),

            (
                // any* <-> any*
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
            ) => {
                cast_to_pointer!(CastType::PointerToPointer)
            }

            (
                // long <-> any*
                MlirType {
                    decl: MlirTypeDecl::Basic,
                    kind: MlirTypeKind::Long(_),
                },
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
            ) => {
                cast_to_pointer!(CastType::LongToPointer)
            }

            (
                // any* -> long
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
                MlirType {
                    decl: MlirTypeDecl::Basic,
                    kind: MlirTypeKind::Long(_),
                },
            ) => {
                cast_to_basic!(CastType::PointerToLong)
            }

            (
                // array<_> -> pointer
                MlirType {
                    decl: MlirTypeDecl::Array(_),
                    ..
                },
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
            ) => {
                cast_to_basic!(CastType::ArrayToPointer)
            }

            (_, _) => {
                if expr.ty != cast_to {
                    self.report_error(CompilerError::CannotImplicitCast(
                        expr.ty.to_string(),
                        cast_to.to_string(),
                        span,
                    ));
                }
                expr
            }
        }
    }

    pub(super) fn implicit_cast(
        &mut self,
        expr: MlirExpr,
        cast_to: MlirType,
        span: Span,
    ) -> MlirExpr {
        // 'cast_to_pointer' is defined twice as it needs to be defined in a scope that contains
        // expr, cast_to, and span, so that they are usable in the macro call
        // without passing them as parameters
        macro_rules! cast_to_pointer {
            ($cast_ty:expr) => {
                MlirExpr {
                    kind: Box::new(MlirExprKind::Cast($cast_ty, expr)),
                    is_lval: true,
                    ty: cast_to,
                    span,
                }
            };
        }
        match (&expr.ty, &cast_to) {
            (
                // number <-> number
                MlirType {
                    kind: left,
                    decl: MlirTypeDecl::Basic,
                },
                MlirType {
                    kind: right,
                    decl: MlirTypeDecl::Basic,
                },
            ) if left.is_numeric() && right.is_numeric() => numeric_cast(expr, cast_to, span),

            (
                //  array<_> -> void*
                MlirType {
                    kind: left,
                    decl: MlirTypeDecl::Array(_),
                },
                MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Pointer,
                },
            ) => cast_to_pointer!(CastType::ArrayToPointer),

            (
                //  array<T> -> T*
                MlirType {
                    kind: left,
                    decl: MlirTypeDecl::Array(_),
                },
                MlirType {
                    kind: right,
                    decl: MlirTypeDecl::Pointer,
                },
            ) if left == right => cast_to_pointer!(CastType::ArrayToPointer),

            (
                // any* -> void*
                MlirType {
                    kind,
                    decl: MlirTypeDecl::Pointer,
                },
                MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Pointer,
                },
            ) => cast_to_pointer!(CastType::PointerToPointer),

            (_, _) => {
                if expr.ty != cast_to {
                    self.report_error(CompilerError::CannotImplicitCast(
                        expr.ty.to_string(),
                        cast_to.to_string(),
                        span,
                    ));
                }
                expr
            }
        }
    }
}

pub(super) fn numeric_cast(expr: MlirExpr, to: MlirType, span: Span) -> MlirExpr {
    debug_assert!(expr.ty.is_basic(), "Cast from type must be basic.");
    debug_assert!(to.is_basic(), "Cast to type must be basic.");
    debug_assert!(expr.ty.is_numeric(), "Cast from type must be numeric.");
    debug_assert!(to.is_numeric(), "Cast to type must be numeric.");

    match (&expr.ty.kind, &to.kind) {
        (from_kind, to_kind) if from_kind == to_kind => expr,

        (MlirTypeKind::Int(unsigned), MlirTypeKind::Float) => {
            if *unsigned {
                cast_basic!(
                    cast_basic!(expr, CastType::UnsignedToSigned, MlirTypeKind::Int(false)),
                    CastType::IntToFloat,
                    MlirTypeKind::Float
                )
            } else {
                cast_basic!(expr, CastType::IntToFloat, MlirTypeKind::Float)
            }
        }

        (MlirTypeKind::Double, MlirTypeKind::Float) => {
            cast_basic!(expr, CastType::DoubleToFloat, MlirTypeKind::Float)
        }

        _ => promote_or_demote_cast(expr, to, span),
    }
}

fn promote_or_demote_cast(expr: MlirExpr, to: MlirType, span: Span) -> MlirExpr {
    let (left_pv, right_pv) = (expr.ty.get_promotion_value(), to.get_promotion_value());
    match left_pv.cmp(&right_pv) {
        // char -> int -> long -> double
        Ordering::Less => {
            let (ty, cast_type) = expr.ty.promote();
            numeric_cast(cast_basic!(expr, cast_type, ty.kind), to, span)
        }

        // char <- int <- long <- double
        Ordering::Greater => {
            let (ty, cast_type) = expr.ty.demote();
            numeric_cast(cast_basic!(expr, cast_type, ty.kind), to, span)
        }

        // sign is different
        Ordering::Equal => match (&expr.ty.kind, &to.kind) {
            (MlirTypeKind::Char(true), MlirTypeKind::Char(false))
            | (MlirTypeKind::Int(true), MlirTypeKind::Int(false))
            | (MlirTypeKind::Long(true), MlirTypeKind::Long(false)) => {
                cast_basic!(expr, CastType::UnsignedToSigned, to.kind.clone())
            }

            (MlirTypeKind::Char(false), MlirTypeKind::Char(true))
            | (MlirTypeKind::Int(false), MlirTypeKind::Int(true))
            | (MlirTypeKind::Long(false), MlirTypeKind::Long(true)) => {
                cast_basic!(expr, CastType::SignedToUnsigned, to.kind.clone())
            }

            _ => unreachable!(),
        },
    }
}

impl MlirType {
    fn get_promotion_value(&self) -> u8 {
        match &self.kind {
            MlirTypeKind::Double => 5,
            MlirTypeKind::Float => 4,
            MlirTypeKind::Long(_) => 3,
            MlirTypeKind::Int(_) => 2,
            MlirTypeKind::Char(_) => 1,
            non_promotable => panic!("'{:?}' is not a promotable type!", non_promotable),
        }
    }

    fn promote(&self) -> (MlirType, CastType) {
        debug_assert!(self.is_basic(), "Cannot promote type '{:?}'", self);
        let (kind, conversion) = match &self.kind {
            MlirTypeKind::Float => (MlirTypeKind::Double, CastType::DoubleToFloat),
            MlirTypeKind::Long(_) => (MlirTypeKind::Double, CastType::LongToDouble),
            MlirTypeKind::Int(unsigned) => (MlirTypeKind::Long(*unsigned), CastType::IntToLong),
            MlirTypeKind::Char(unsigned) => (MlirTypeKind::Int(*unsigned), CastType::CharToInt),
            cannot_promote => panic!("Type '{:?}' is not able to be promoted!", cannot_promote),
        };
        (
            MlirType {
                decl: MlirTypeDecl::Basic,
                kind,
            },
            conversion,
        )
    }

    fn demote(&self) -> (MlirType, CastType) {
        debug_assert!(self.is_basic(), "Cannot demote type '{:?}'", self);
        let (kind, conversion) = match &self.kind {
            MlirTypeKind::Double => (MlirTypeKind::Long(false), CastType::DoubleToLong),
            MlirTypeKind::Float => (MlirTypeKind::Int(false), CastType::FloatToInt),
            MlirTypeKind::Long(unsigned) => (MlirTypeKind::Int(*unsigned), CastType::LongToInt),
            MlirTypeKind::Int(unsigned) => (MlirTypeKind::Char(*unsigned), CastType::IntToChar),
            cannot_demote => panic!("Type '{:?}' is not able to be demoted!", cannot_demote),
        };
        (
            MlirType {
                decl: MlirTypeDecl::Basic,
                kind,
            },
            conversion,
        )
    }
}

#[cfg(test)]
fn test_implicit_cast(cast_from: MlirExpr, cast_to: MlirType, is_ok: bool) -> MlirExpr {
    let mut analyzer = GlobalValidator::new(AbstractSyntaxTree::default());
    let cast = analyzer.implicit_cast(cast_from, cast_to, Span::default());
    assert_eq!(analyzer.reporter.borrow().errors.is_empty(), is_ok);
    cast
}

#[test]
fn test_all_possible_cast_permutations() {
    static TYPES: [MlirTypeKind; 8] = [
        MlirTypeKind::Double,
        MlirTypeKind::Float,
        MlirTypeKind::Long(true),
        MlirTypeKind::Long(false),
        MlirTypeKind::Int(true),
        MlirTypeKind::Int(false),
        MlirTypeKind::Char(true),
        MlirTypeKind::Char(false),
    ];

    for cast_from in &TYPES {
        for cast_to in TYPES.iter().rev() {
            let cast_from = MlirExpr {
                kind: Box::new(MlirExprKind::Literal(MlirLiteral::Int(0))),
                ty: basic_ty!(cast_from.clone()),
                span: Default::default(),
                is_lval: false,
            };

            let cast_to = basic_ty!(cast_to.clone());
            test_implicit_cast(cast_from, cast_to, true);
        }
    }
}

#[cfg(test)]
macro_rules! create_literal {
    ($ty:expr, $kind:expr) => {
        MlirExpr {
            span: Default::default(),
            kind: Box::new(MlirExprKind::Literal($ty)),
            ty: basic_ty!($kind),
            is_lval: false,
        }
    };
}

#[cfg(test)]
fn test_cast_type_structure(expected: &[CastType], mut cast: MlirExpr) {
    for expected_cast_type in expected.iter().rev() {
        let cast_type = match *cast.kind {
            MlirExprKind::Cast(cast_type, expr) => {
                cast = expr;
                cast_type
            }
            kind => panic!("Expected MlirExprKind::Cast, found '{:?}'", kind),
        };
        assert_eq!(expected_cast_type, &cast_type);
    }
}

#[test]
fn test_cast_double_to_unsigned_char() {
    let from = create_literal!(MlirLiteral::Double(1.0), MlirTypeKind::Double);
    let to = basic_ty!(MlirTypeKind::Char(true));
    use CastType::*;
    static EXPECTED: [CastType; 4] = [DoubleToLong, LongToInt, IntToChar, SignedToUnsigned];
    let mut cast = test_implicit_cast(from, to, true);
    test_cast_type_structure(&EXPECTED, cast);
}

#[test]
fn test_cast_char_to_unsigned_long() {
    let from = create_literal!(MlirLiteral::Char(1), MlirTypeKind::Char(false));
    let to = basic_ty!(MlirTypeKind::Long(true));
    use CastType::*;
    static EXPECTED: [CastType; 3] = [CharToInt, IntToLong, SignedToUnsigned];
    let cast = test_implicit_cast(from, to, true);
    test_cast_type_structure(&EXPECTED, cast);
}
