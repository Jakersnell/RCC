use crate::analysis::GlobalValidator;
use crate::data::ast::AbstractSyntaxTree;
use crate::data::mlir::*;
use crate::data::tokens::Literal;
use crate::util::error::CompilerError;
use crate::util::str_intern::get;
use crate::util::{Locatable, Span};
use std::cmp::Ordering;
use std::mem::discriminant;

// macro_rules! cast {
//     ($expr:expr, $cast_ty:expr, $cast_to:expr, $is_lval: literal, $span: expr) => {
//         MlirExpr {
//             kind: Box::new(MlirExprKind::Cast($cast_ty, $expr)),
//             is_lval: $is_lval,
//             ty: $cast_to,
//             $span,
//         }
//     };
// }

impl GlobalValidator {
    // pub(super) fn binary_cast_together(
    //     left: Locatable<MlirExpr>,
    //     right: Locatable<MlirExpr>,
    // ) -> (MlirExpr, MlirExpr) {
    //     let (left_lvl, right_lvl) = (
    //         get_numeric_type_hierarchy(&left.ty),
    //         get_numeric_type_hierarchy(&right.ty),
    //     );
    //     match left_lvl.cmp(&right_lvl) {
    //         Ordering::Less => (
    //             cast!(left.value, right.ty.clone(), left.location, false),
    //             right.value,
    //         ),
    //         Ordering::Greater => {
    //             let left_ty = left.ty.clone();
    //             (
    //                 left.value,
    //                 cast!(right.value, left_ty, right.location, false),
    //             )
    //         }
    //         Ordering::Equal => (left.value, right.value),
    //     }
    // }

    // pub(super) fn explicit_cast(
    //     &mut self,
    //     expr: MlirExpr,
    //     cast_to: MlirType,
    //     span: Span,
    // ) -> MlirExpr {
    //     // conversions:
    //     //
    //     // - all implicit casts +
    //     // - any* <-> any*
    //     // - integer -> any*
    //     // - any* -> long
    //     // - array -> any*
    //
    //     if expr.ty == cast_to {
    //         expr
    //     } else if (expr.ty.is_numeric() && cast_to.is_numeric())
    //         || (expr.ty.is_pointer() && cast_to.is_pointer())
    //         || (expr.ty.is_pointer() && cast_to.is_array())
    //         || ((expr.ty.is_integer() && expr.ty.is_basic() && cast_to.is_pointer())
    //             || (cast_to.is_integer() && cast_to.is_basic() && expr.ty.is_pointer()))
    //     {
    //         let is_lval = cast_to.is_pointer();
    //         cast!(expr, cast_to, span, is_lval)
    //     } else {
    //         self.report_error(CompilerError::CannotExplicitCast(
    //             expr.ty.to_string(),
    //             cast_to.to_string(),
    //             span,
    //         ));
    //         expr
    //     }
    // }

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
            ($cast_ty:expr ) => {
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
        };
        todo!()
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

    macro_rules! cast_basic {
        ($expr:expr, $cast_ty:expr, $cast_to:expr) => {
            MlirExpr {
                kind: Box::new(MlirExprKind::Cast($cast_ty, $expr)),
                is_lval: false,
                ty: MlirType {
                    kind: $cast_to,
                    decl: MlirTypeDecl::Basic,
                },
                span,
            }
        };
    }

    match (&expr.ty.kind, &to.kind) {
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
        _ => {
            let (left_pv, right_pv) = (expr.ty.get_promotion_value(), to.get_promotion_value());
            match left_pv.cmp(&right_pv) {
                Ordering::Less => {
                    // char -> int -> long -> double
                    cast_basic!(
                        numeric_cast(expr, to.demote(), span),
                        CastType::IntToFloat,
                        to.kind.clone()
                    )
                }
                Ordering::Greater => {
                    // char <- int <- long <- double
                    cast_basic!(
                        numeric_cast(expr, to.promote(), span),
                        CastType::IntToFloat,
                        to.kind.clone()
                    )
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
