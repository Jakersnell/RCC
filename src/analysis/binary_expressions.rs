use crate::analysis::casting::*;
use crate::analysis::*;
use crate::data::ast::{AbstractSyntaxTree, AssignOp, BinaryOp};
use crate::data::mlir::*;
use crate::util::error::CompilerError;
use crate::util::Span;

impl GlobalValidator {
    pub(super) fn validate_binary_expression(
        &mut self,
        op: &BinaryOp,
        left: MlirExpr,
        right: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
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
        left: MlirExpr,
        right: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
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
                MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
                left,
                Span::default(),
            )
            .unwrap();
            let right = implicit_cast(
                MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
                right,
                span,
            )
            .unwrap();
            match op {
                BinaryOp::Add => MlirExprKind::Add(left, right),
                BinaryOp::Sub => MlirExprKind::Sub(left, right),
                _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
            }
        } else if left.ty.is_numeric() && right.ty.is_numeric() {
            let left = implicit_cast(
                MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
                left,
                span,
            )
            .unwrap();
            let right = implicit_cast(
                MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
                right,
                span,
            )
            .unwrap();
            match op {
                BinaryOp::Add => MlirExprKind::Add(left, right),
                BinaryOp::Sub => MlirExprKind::Sub(left, right),
                BinaryOp::Mul => MlirExprKind::Mul(left, right),
                BinaryOp::Div => MlirExprKind::Div(left, right),
                BinaryOp::Mod => MlirExprKind::Mod(left, right),
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
        Ok(MlirExpr {
            span,
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }
    pub(super) fn validate_assign_op(
        &mut self,
        op: &AssignOp,
        left: MlirExpr,
        right: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
        if !left.is_lval {
            let err = CompilerError::LeftHandNotLVal(span);
            self.report_error(err);
            return Ok(left);
        }
        let right_ty = right.ty.clone();
        let right = implicit_cast(left.ty.clone(), right, span);
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
            MlirExprKind::Assign(left, expr)
        } else {
            MlirExprKind::Assign(left, right)
        };
        Ok(MlirExpr {
            span,
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn validate_binary_equivalence_expression(
        &mut self,
        op: &BinaryOp,
        left: MlirExpr,
        right: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
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
            MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
            left,
            span,
        );

        let right_ty = right.ty.clone();
        let right = implicit_cast(
            MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
            right,
            span,
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
            BinaryOp::Equal => MlirExprKind::Equal(left, right),
            BinaryOp::NotEqual => MlirExprKind::NotEqual(left, right),
            BinaryOp::GreaterThan => MlirExprKind::GreaterThan(left, right),
            BinaryOp::GreaterThanEqual => MlirExprKind::GreaterThanEqual(left, right),
            BinaryOp::LessThan => MlirExprKind::LessThan(left, right),
            BinaryOp::LessThanEqual => MlirExprKind::LessThanEqual(left, right),
            _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
        };
        Ok(MlirExpr {
            span,
            kind: Box::new(kind),
            ty: MlirType::new(MlirTypeKind::Int(false), MlirTypeDecl::Basic),
            is_lval: false,
        })
    }

    pub(super) fn validate_binary_bitwise_expression(
        &mut self,
        op: &BinaryOp,
        left: MlirExpr,
        right: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
        let ty = MlirType {
            kind: MlirTypeKind::Long(true),
            decl: MlirTypeDecl::Basic,
        };

        if !(left.is_integer() && right.is_integer()) {
            let err = CompilerError::InvalidBinaryOperation(
                op.to_string(),
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
            self.report_error(err);
            return Err(());
        };

        let (left, right) = (
            implicit_cast(ty.clone(), left, span).unwrap(),
            implicit_cast(ty.clone(), right, span).unwrap(),
        );

        let kind = match op {
            BinaryOp::BitwiseAnd => MlirExprKind::BitwiseAnd(left, right),
            BinaryOp::BitwiseOr => MlirExprKind::BitwiseOr(left, right),
            BinaryOp::BitwiseXor => MlirExprKind::BitwiseXor(left, right),
            BinaryOp::LeftShift => MlirExprKind::LeftShift(left, right),
            BinaryOp::RightShift => MlirExprKind::RightShift(left, right),
            _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
        };
        Ok(MlirExpr {
            span,
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
            MlirTypeKind::Int(false), // left
            MlirTypeKind::Long(true), // right
        ),
        (MlirTypeKind::Int(false), MlirTypeKind::Char(false)),
        (MlirTypeKind::Long(false), MlirTypeKind::Int(false)),
        (MlirTypeKind::Long(false), MlirTypeKind::Long(false)),
    ];
    use crate::data::mlir::*;
    macro_rules! make_expr {
        ($kind:expr) => {
            MlirExpr {
                span: Span::default(),
                kind: Box::new(MlirExprKind::Literal(MlirLiteral::Int(1))),
                ty: MlirType::new($kind, MlirTypeDecl::Basic),
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
        assert_eq!(result.unwrap().ty.kind, MlirTypeKind::Long(true),);
    }
}
