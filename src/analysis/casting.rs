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
