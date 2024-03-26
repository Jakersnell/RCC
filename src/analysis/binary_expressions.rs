use crate::analysis::casting::*;
use crate::analysis::hlir::*;
use crate::analysis::*;
use crate::parser::ast::{AbstractSyntaxTree, AssignOp, BinaryOp};
use crate::util::error::CompilerError;
use crate::util::Span;

impl GlobalValidator {
    pub(super) fn validate_binary_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.validate_arithmetic_binary_op(op, left, right, span)
            }

            BinaryOp::Assign(assign_op) => self.validate_assign_op(assign_op, left, right, span),

            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanEqual => {
                self.validate_binary_equivalence_expression(op, left, right, span)
            }

            BinaryOp::LogicalAnd
            | BinaryOp::LogicalOr
            | BinaryOp::BitwiseAnd
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => {
                self.validate_binary_bitwise_expression(op, left, right, span)
            }
        }
    }

    pub(super) fn validate_arithmetic_binary_op(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        let ty = left.ty.clone();
        let kind = if left.ty.is_pointer() && right.ty.is_pointer() {
            // pointers can add and subtract from each other, resulting in a long.
            self.report_error(CompilerError::CustomError(
                "Pointer arithmetic not supported".into(),
                span,
            ));
            return Ok(left);
        } else if left.ty.is_pointer() && right.ty.is_numeric() {
            // only addition/subtraction
            let left = implicit_cast(
                HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                left,
            )
            .unwrap();
            let right = implicit_cast(
                HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                right,
            )
            .unwrap();
            match op {
                BinaryOp::Add => HlirExprKind::Add(left, right),
                BinaryOp::Sub => HlirExprKind::Sub(left, right),
                _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
            }
        } else if left.ty.is_numeric() && right.ty.is_numeric() {
            let left = implicit_cast(
                HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                left,
            )
            .unwrap();
            let right = implicit_cast(
                HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                right,
            )
            .unwrap();
            match op {
                BinaryOp::Add => HlirExprKind::Add(left, right),
                BinaryOp::Sub => HlirExprKind::Sub(left, right),
                BinaryOp::Mul => HlirExprKind::Mul(left, right),
                BinaryOp::Div => HlirExprKind::Div(left, right),
                BinaryOp::Mod => HlirExprKind::Mod(left, right),
                _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
            }
        } else {
            self.report_error(CompilerError::InvalidBinaryOperation(
                op.to_string(),
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            ));
            return Ok(left);
        };
        Ok(HlirExpr {
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }
    pub(super) fn validate_assign_op(
        &mut self,
        op: &AssignOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if !left.is_lval {
            let err = CompilerError::LeftHandNotLVal(span);
            self.report_error(err);
            return Ok(left);
        }
        let right_ty = right.ty.clone();
        let right = implicit_cast(left.ty.clone(), right);
        if right.is_err() {
            let err = CompilerError::CannotAssign(left.ty.to_string(), right_ty.to_string(), span);
            self.report_error(err);
            return Ok(left);
        }
        let right = right.unwrap();
        let op = match op {
            AssignOp::Assign => None,
            AssignOp::Plus => Some(BinaryOp::Add),
            AssignOp::Minus => Some(BinaryOp::Sub),
            AssignOp::Multiply => Some(BinaryOp::Mul),
            AssignOp::Divide => Some(BinaryOp::Div),
            AssignOp::Modulo => Some(BinaryOp::Mod),
            AssignOp::BitwiseAnd => Some(BinaryOp::BitwiseAnd),
            AssignOp::BitwiseOr => Some(BinaryOp::BitwiseOr),
            AssignOp::BitwiseXor => Some(BinaryOp::BitwiseXor),
            AssignOp::LeftShift => Some(BinaryOp::LeftShift),
            AssignOp::RightShift => Some(BinaryOp::RightShift),
        };
        let ty = left.ty.clone();
        let kind = if let Some(op) = op {
            let expr = self.validate_binary_expression(&op, left.clone(), right, span)?;
            HlirExprKind::Assign(left, expr)
        } else {
            HlirExprKind::Assign(left, right)
        };
        Ok(HlirExpr {
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn validate_binary_equivalence_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        debug_assert!(!left.ty.is_array());
        debug_assert!(!right.ty.is_array());
        if (left.ty.is_pointer() && !right.ty.is_pointer())
            || (!left.ty.is_pointer() && right.ty.is_pointer())
        {
            let err = CompilerError::InvalidBinaryOperation(
                op.to_string(),
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
            self.report_error(err);
            return Ok(left);
        }

        let left_ty = left.ty.clone();
        let left = implicit_cast(
            HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
            left,
        );

        let right_ty = right.ty.clone();
        let right = implicit_cast(
            HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
            right,
        );

        if left.is_err() || right.is_err() {
            self.report_error(CompilerError::CannotEq(
                left_ty.to_string(),
                right_ty.to_string(),
                span,
            ));
            return Err(());
        }

        let (left, right) = (left.unwrap(), right.unwrap());

        let kind = match op {
            BinaryOp::Equal => HlirExprKind::Equal(left, right),
            BinaryOp::NotEqual => HlirExprKind::NotEqual(left, right),
            BinaryOp::GreaterThan => HlirExprKind::GreaterThan(left, right),
            BinaryOp::GreaterThanEqual => HlirExprKind::GreaterThanEqual(left, right),
            BinaryOp::LessThan => HlirExprKind::LessThan(left, right),
            BinaryOp::LessThanEqual => HlirExprKind::LessThanEqual(left, right),
            _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
        };
        Ok(HlirExpr {
            kind: Box::new(kind),
            ty: HlirType::new(HlirTypeKind::Int(false), HlirTypeDecl::Basic),
            is_lval: false,
        })
    }

    pub(super) fn validate_binary_bitwise_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        let ty = HlirType {
            kind: HlirTypeKind::Long(true),
            decl: HlirTypeDecl::Basic,
        };
        let (left, right) = if left.is_pointer() && right.is_pointer() {
            (
                explicit_cast(ty.clone(), left).unwrap(),
                explicit_cast(ty.clone(), right).unwrap(),
            )
        } else if !left.is_pointer() && !right.is_pointer() {
            (
                implicit_cast(ty.clone(), left).unwrap(),
                implicit_cast(ty.clone(), right).unwrap(),
            )
        } else {
            let err = CompilerError::InvalidBinaryOperation(
                op.to_string(),
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
            self.report_error(err);
            return Err(());
        };
        let kind = match op {
            BinaryOp::BitwiseAnd => HlirExprKind::BitwiseAnd(left, right),
            BinaryOp::BitwiseOr => HlirExprKind::BitwiseOr(left, right),
            BinaryOp::BitwiseXor => HlirExprKind::BitwiseXor(left, right),
            BinaryOp::LeftShift => HlirExprKind::LeftShift(left, right),
            BinaryOp::RightShift => HlirExprKind::RightShift(left, right),
            _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
        };
        Ok(HlirExpr {
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }
}

#[test]
fn test_validate_binary_bitwise_expression_is_ok_for_valid_expressions() {
    let test_cases = [
        (
            HlirTypeKind::Int(false), // left
            HlirTypeKind::Long(true), // right
        ),
        (HlirTypeKind::Int(false), HlirTypeKind::Char(false)),
        (HlirTypeKind::Long(false), HlirTypeKind::Int(false)),
        (HlirTypeKind::Long(false), HlirTypeKind::Long(false)),
    ];
    use super::hlir::*;
    macro_rules! make_expr {
        ($kind:expr) => {
            HlirExpr {
                kind: Box::new(HlirExprKind::Literal(HlirLiteral::Int(1))),
                ty: HlirType::new($kind, HlirTypeDecl::Basic),
                is_lval: false,
            }
        };
    }
    for (left, right) in test_cases {
        let left = make_expr!(left);
        let right = make_expr!(right);
        let span = Span::default();
        let result = GlobalValidator::new(AbstractSyntaxTree::default())
            .validate_binary_bitwise_expression(&BinaryOp::BitwiseAnd, left, right, span);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().ty.kind, HlirTypeKind::Long(true),);
    }
}
