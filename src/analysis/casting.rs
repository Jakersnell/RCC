use std::cmp::Ordering;

use crate::analysis::Analyzer;
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

impl Analyzer {
    pub(super) fn explicit_cast(
        &mut self,
        expr: MlirExpr,
        cast_to: MlirType,
        span: Span,
    ) -> MlirExpr {
        if expr.ty == cast_to {
            return expr;
        }
        let cast_type = match (&expr.ty, &cast_to) {
            (_, _) if expr.ty.is_numeric() && cast_to.is_numeric() => {
                self.numeric_cast(&expr, &cast_to, span)
            }

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
            ) => CastType::PointerToPointer,

            (
                // int -> *ptr
                _,
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
            ) if expr.ty.is_integer() => CastType::IntToPointer,

            (
                // any* -> int
                MlirType {
                    decl: MlirTypeDecl::Pointer,
                    ..
                },
                _,
            ) if cast_to.is_integer() => CastType::PointerToInt,

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
            ) => CastType::ArrayToPointer,

            (_, _) => {
                if expr.ty != cast_to {
                    self.report_error(CompilerError::CannotImplicitCast(
                        expr.ty.to_string(),
                        cast_to.to_string(),
                        span,
                    ));
                }
                CastType::InvalidCast
            }
        };
        MlirExpr {
            span,
            ty: cast_to.clone(),
            is_lval: false,
            kind: Box::new(MlirExprKind::Cast(cast_to, cast_type, expr)),
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
        if expr.ty == cast_to {
            return expr;
        }
        let cast_type = match (&expr.ty, &cast_to) {
            (_, _) if expr.ty.is_numeric() && cast_to.is_numeric() => {
                self.numeric_cast(&expr, &cast_to, span)
            }

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
            ) => CastType::ArrayToPointer,

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
            ) if left == right => CastType::ArrayToPointer,

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
            ) => CastType::PointerToPointer,

            (_, _) => {
                if expr.ty != cast_to {
                    self.report_error(CompilerError::CannotImplicitCast(
                        expr.ty.to_string(),
                        cast_to.to_string(),
                        span,
                    ));
                }
                CastType::InvalidCast
            }
        };

        MlirExpr {
            span,
            ty: cast_to.clone(),
            is_lval: false,
            kind: Box::new(MlirExprKind::Cast(cast_to, cast_type, expr)),
        }
    }

    fn numeric_cast(&mut self, expr: &MlirExpr, cast_to: &MlirType, span: Span) -> CastType {
        debug_assert!(expr.ty.is_numeric());
        debug_assert!(cast_to.is_numeric());
        match (&expr.ty, &cast_to) {
            (
                // int <-> int
                _,
                _,
            ) if expr.ty.is_integer() && cast_to.is_integer() => CastType::IntToInt,

            (
                // int -> float
                _,
                _,
            ) if expr.ty.is_integer() && cast_to.is_float() => CastType::IntToFloat,

            (
                // int -> float
                MlirType {
                    kind: left,
                    decl: MlirTypeDecl::Basic,
                },
                MlirType {
                    kind: right,
                    decl: MlirTypeDecl::Basic,
                },
            ) if expr.ty.is_float() && cast_to.is_integer() => CastType::FloatToInt,

            (
                // int -> float
                _,
                _,
            ) if expr.ty.is_float() && cast_to.is_float() => CastType::FloatToFloat,

            (_, _) => {
                if expr.ty != *cast_to {
                    self.report_error(CompilerError::CannotImplicitCast(
                        expr.ty.to_string(),
                        cast_to.to_string(),
                        span,
                    ));
                }
                CastType::InvalidCast
            }
        }
    }

    pub(super) fn binary_numeric_cast(
        &mut self,
        left: MlirExpr,
        right: MlirExpr,
    ) -> (MlirExpr, MlirExpr) {
        debug_assert!(left.ty.is_basic());
        debug_assert!(left.ty.is_numeric());
        debug_assert!(right.ty.is_basic());
        debug_assert!(right.ty.is_numeric());
        let cast_to_ty_kind = get_implicit_cast_together_type(&left.ty.kind, &right.ty.kind);
        let cast_to_ty = MlirType {
            kind: cast_to_ty_kind,
            decl: MlirTypeDecl::Basic,
        };
        let left_span = left.span;
        let right_span = right.span;
        let left = self.implicit_cast(left, cast_to_ty.clone(), left_span);
        let right = self.implicit_cast(right, cast_to_ty, right_span);
        (left, right)
    }
}

pub(in crate::analysis) fn get_implicit_cast_together_type(
    left: &MlirTypeKind,
    right: &MlirTypeKind,
) -> MlirTypeKind {
    match (left, right) {
        (left, right) if left == right => left.clone(),
        (MlirTypeKind::Long(unsigned_left), MlirTypeKind::Long(unsigned_right))
            if unsigned_left != unsigned_right =>
        {
            MlirTypeKind::Long(true)
        }
        (MlirTypeKind::Int(unsigned_left), MlirTypeKind::Int(unsigned_right))
            if unsigned_left != unsigned_right =>
        {
            MlirTypeKind::Int(true)
        }
        (MlirTypeKind::Char(unsigned_left), MlirTypeKind::Char(unsigned_right))
            if unsigned_left != unsigned_right =>
        {
            MlirTypeKind::Char(true)
        }
        _ => {
            let promotion_ordering = left.get_promotion_value().cmp(&right.get_promotion_value());
            match promotion_ordering {
                Ordering::Less => right.clone(),
                Ordering::Greater => left.clone(),
                Ordering::Equal => unreachable!(),
            }
        }
    }
}

impl MlirTypeKind {
    fn get_promotion_value(&self) -> u8 {
        match &self {
            MlirTypeKind::Double => 5,
            MlirTypeKind::Float => 4,
            MlirTypeKind::Long(_) => 3,
            MlirTypeKind::Int(_) => 2,
            MlirTypeKind::Char(_) => 1,
            non_promotable => panic!("'{:?}' is not a promotable type!", non_promotable),
        }
    }
}
