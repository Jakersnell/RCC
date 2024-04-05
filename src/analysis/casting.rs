use crate::analysis::GlobalValidator;
use crate::data::ast::AbstractSyntaxTree;
use crate::data::mlir::MlirExprKind::Cast;
use crate::data::mlir::*;
use crate::data::tokens::Literal;
use crate::util::error::CompilerError;
use crate::util::str_intern::get;
use crate::util::{Locatable, Span};
use std::cmp::Ordering;
use std::mem::discriminant;

macro_rules! cast {
    ($hlir:expr, $to:expr, $span:expr, $lval:expr) => {{
        MlirExpr {
            span: $span,
            kind: Box::new(MlirExprKind::Cast($to.clone(), $hlir)),
            ty: $to,
            is_lval: $lval,
        }
    }};
}

macro_rules! cast_basic {
    ($hlir:expr, $to:expr, $span:expr) => {{
        cast!(
            $hlir,
            MlirType {
                kind: $to,
                decl: MlirTypeDecl::Basic
            },
            $span,
            false
        )
    }};
}

fn get_numeric_type_hierarchy(ty: &MlirType) -> u8 {
    assert!(ty.is_basic(), "Type must only be basic numeric type.");
    match ty.kind {
        MlirTypeKind::Double => 4,
        MlirTypeKind::Long(_) => 3,
        MlirTypeKind::Float => 2,
        MlirTypeKind::Int(_) => 1,
        MlirTypeKind::Char(_) => 0,
        _ => panic!("Fatal Error: '{:?}' is not numeric.", ty),
    }
}

impl GlobalValidator {
    pub(super) fn binary_cast_together(
        left: Locatable<MlirExpr>,
        right: Locatable<MlirExpr>,
    ) -> (MlirExpr, MlirExpr) {
        let (left_lvl, right_lvl) = (
            get_numeric_type_hierarchy(&left.ty),
            get_numeric_type_hierarchy(&right.ty),
        );
        match left_lvl.cmp(&right_lvl) {
            Ordering::Less => (
                cast!(left.value, right.ty.clone(), left.location, false),
                right.value,
            ),
            Ordering::Greater => {
                let left_ty = left.ty.clone();
                (
                    left.value,
                    cast!(right.value, left_ty, right.location, false),
                )
            }
            Ordering::Equal => (left.value, right.value),
        }
    }

    pub(super) fn explicit_cast(
        &mut self,
        expr: MlirExpr,
        cast_to: MlirType,
        span: Span,
    ) -> MlirExpr {
        // conversions:
        //
        // - all implicit casts +
        // - any* <-> any*
        // - integer <-> any*
        // - array -> any*

        if expr.ty == cast_to {
            expr
        } else if (expr.ty.is_numeric() && cast_to.is_numeric())
            || (expr.ty.is_pointer() && cast_to.is_pointer())
            || (expr.ty.is_pointer() && cast_to.is_array())
            || ((expr.ty.is_integer() && expr.ty.is_basic() && cast_to.is_pointer())
                || (cast_to.is_integer() && cast_to.is_basic() && expr.ty.is_pointer()))
        {
            let is_lval = cast_to.is_pointer();
            cast!(expr, cast_to, span, is_lval)
        } else {
            self.report_error(CompilerError::CannotExplicitCast(
                expr.ty.to_string(),
                cast_to.to_string(),
                span,
            ));
            expr
        }
    }
    pub(super) fn implicit_cast(
        &mut self,
        expr: MlirExpr,
        cast_to: MlirType,
        span: Span,
    ) -> MlirExpr {
        // conversions:
        //
        // - number <-> number
        // - array<T> -> T*
        // - void* <-> any*

        if expr.ty == cast_to {
            expr
        } else if (expr.ty.is_numeric() && cast_to.is_numeric())
            || (expr.is_array()
                && cast_to.is_pointer()
                && (discriminant(&expr.ty.kind) == discriminant(&cast_to.kind)))
            || (expr.ty.is_pointer()
                && cast_to.is_pointer()
                && (cast_to.kind == MlirTypeKind::Void || expr.ty.kind == MlirTypeKind::Void))
            || (expr.ty.is_pointer()
                && cast_to.is_pointer()
                && matches!(cast_to.kind, MlirTypeKind::Char(_)))
        {
            let is_lval = cast_to.is_pointer();
            cast!(expr, cast_to, span, is_lval)
        } else {
            self.report_error(CompilerError::CannotImplicitCast(
                expr.ty.to_string(),
                cast_to.to_string(),
                span,
            ));
            expr
        }
    }
}
pub mod numeric_casts {
    use crate::data::mlir::*;
    use crate::util::Span;
    use std::cmp::Ordering;

    pub(super) fn cast(expr: MlirExpr, to: MlirType, span: Span) -> MlirExpr {
        debug_assert!(expr.ty.is_basic(), "Cast from type must be basic.");
        debug_assert!(to.is_basic(), "Cast to type must be basic.");
        debug_assert!(expr.ty.is_numeric(), "Cast from type must be numeric.");
        debug_assert!(to.is_numeric(), "Cast to type must be numeric.");
        match (&expr.ty.kind, &to.kind) {
            (MlirTypeKind::Int(signed), MlirTypeKind::Float) => {
                if *signed {
                    cast_basic!(
                        cast_basic!(expr, MlirTypeKind::Int(false), span),
                        MlirTypeKind::Float,
                        span
                    )
                } else {
                    cast_basic!(expr, MlirTypeKind::Float, span)
                }
            }
            (MlirTypeKind::Double, MlirTypeKind::Float) => {
                cast_basic!(expr, MlirTypeKind::Float, span)
            }
            _ => {
                let (left_pv, right_pv) = (expr.ty.get_promotion_value(), to.get_promotion_value());
                match left_pv.cmp(&right_pv) {
                    Ordering::Less => {
                        // char -> int -> long -> double
                        cast_basic!(cast(expr, to.demote(), span), to.kind.clone(), span)
                    }
                    Ordering::Greater => {
                        // char <- int <- long <- double
                        cast_basic!(cast(expr, to.promote(), span), to.kind.clone(), span)
                    }
                    Ordering::Equal => expr,
                }
            }
        }
    }

    impl MlirType {
        fn get_promotion_value(&self) -> u8 {
            match &self.kind {
                MlirTypeKind::Double => 8,
                MlirTypeKind::Float => 7,
                MlirTypeKind::Long(true) => 6,
                MlirTypeKind::Long(false) => 5,
                MlirTypeKind::Int(true) => 4,
                MlirTypeKind::Int(false) => 3,
                MlirTypeKind::Char(true) => 2,
                MlirTypeKind::Char(false) => 1,
                non_promotable => panic!("'{:?}' is not a promotable type!", non_promotable),
            }
        }

        pub(in crate::analysis) fn promote(&self) -> MlirType {
            debug_assert!(self.is_basic(), "Promotion type must be basic.");
            let kind = match &self.kind {
                MlirTypeKind::Long(false) => MlirTypeKind::Double,
                MlirTypeKind::Long(true) => MlirTypeKind::Long(false),
                MlirTypeKind::Float => MlirTypeKind::Long(false),
                MlirTypeKind::Int(true) => MlirTypeKind::Long(false),
                MlirTypeKind::Int(false) => MlirTypeKind::Int(false),
                MlirTypeKind::Char(false) => MlirTypeKind::Int(false),
                MlirTypeKind::Char(true) => MlirTypeKind::Char(false),
                cannot_promote => panic!("Type '{:?}' is not able to be promoted!", cannot_promote),
            };
            MlirType {
                decl: MlirTypeDecl::Basic,
                kind,
            }
        }

        pub(in crate::analysis) fn demote(&self) -> MlirType {
            debug_assert!(self.is_basic(), "Demotion type must be basic.");
            let kind = match &self.kind {
                MlirTypeKind::Double => MlirTypeKind::Long(false),
                MlirTypeKind::Long(true) => MlirTypeKind::Long(false),
                MlirTypeKind::Long(false) => MlirTypeKind::Int(false),
                MlirTypeKind::Float => MlirTypeKind::Int(false),
                MlirTypeKind::Int(true) => MlirTypeKind::Int(false),
                MlirTypeKind::Int(false) => MlirTypeKind::Char(false),
                MlirTypeKind::Char(true) => MlirTypeKind::Char(false),
                cannot_demote => panic!("Type '{:?}' is not able to be demoted!", cannot_demote),
            };
            MlirType {
                decl: MlirTypeDecl::Basic,
                kind,
            }
        }
    }
}

#[test]
fn test_cast_double_to_unsigned_long() {
    let unsigned_long_ty = MlirType {
        kind: MlirTypeKind::Long(true),
        decl: MlirTypeDecl::Basic,
    };

    let double_literal = MlirExpr {
        span: Default::default(),
        kind: Box::new(MlirExprKind::Literal(MlirLiteral::Double(1.0))),
        ty: MlirType {
            kind: MlirTypeKind::Double,
            decl: MlirTypeDecl::Basic,
        },
        is_lval: false,
    };

    let mut analyzer = GlobalValidator::new(AbstractSyntaxTree::default());
    analyzer.implicit_cast(double_literal, unsigned_long_ty, Span::default());
    assert!(analyzer.reporter.borrow().errors.is_empty());
}
