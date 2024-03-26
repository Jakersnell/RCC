use crate::analysis::casting::{explicit_cast, implicit_cast};
use crate::analysis::hlir::{
    HlirExpr, HlirExprKind, HlirLiteral, HlirType, HlirTypeDecl, HlirTypeKind,
};
use crate::analysis::symbols::SymbolResult;
use crate::analysis::GlobalValidator;
use crate::lexer::tokens::Literal;
use crate::parser::ast::{
    AssignOp, BinaryOp, Declaration, Expression, PostfixOp, TypeOrExpression, UnaryOp,
};
use crate::util::error::{CompilerError, CompilerWarning};
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, Span};
use std::env::var;

impl GlobalValidator {
    pub(super) fn validate_expression(&mut self, expr: &Expression) -> Result<HlirExpr, ()> {
        match expr {
            Expression::Literal(literal) => self.validate_literal(literal, literal.location),
            Expression::Variable(variable) => self.validate_variable_call(variable),
            Expression::Sizeof(ty_or_expr) => self.validate_sizeof(ty_or_expr),
            Expression::Parenthesized(expr) => self.validate_expression(expr),
            Expression::PostFix(op, expr) => self.validate_post_inc_or_dec(op, expr, expr.location),
            Expression::Unary(op, expr) => self.route_unary(op, expr),
            Expression::Binary(op, left, right) => self.route_binary(op, left, right),
            Expression::FunctionCall(ident, args) => self.route_function_call(ident, args),
            Expression::Index(left, index) => self.route_index(left, index),
            Expression::Member(body, member) => self.route_member(body, member),
            Expression::PointerMember(body, member) => self.route_pointer_member(body, member),
            Expression::Cast(dec, expr) => self.route_cast_expression(dec, expr),
            _ => unreachable!(),
        }
    }

    fn validate_variable_call(
        &mut self,
        variable: &Locatable<InternedStr>,
    ) -> Result<HlirExpr, ()> {
        let result = self
            .scope
            .borrow_mut()
            .get_variable_type(&variable.value, variable.location)
            .map(|ty| HlirExpr {
                span: variable.location,
                kind: Box::new(HlirExprKind::Variable(variable.value.clone())),
                ty,
                is_lval: true,
            });
        if let Err(err) = result {
            self.report_error(err);
            Err(())
        } else {
            Ok(result.unwrap())
        }
    }

    fn validate_sizeof(
        &mut self,
        ty_or_expr: &Locatable<TypeOrExpression>,
    ) -> Result<HlirExpr, ()> {
        let size = match &ty_or_expr.value {
            TypeOrExpression::Type(ty) => {
                let ty = self.validate_type(&ty.specifier, ty_or_expr.location, false, false)?;
                self.sizeof(&ty, ty_or_expr.location)
            }
            TypeOrExpression::Expr(expr) => {
                let expr = self.validate_expression(expr)?;
                if !matches!(
                    &*expr.kind,
                    HlirExprKind::Variable(_) | HlirExprKind::Literal(_)
                ) {
                    let warning = CompilerWarning::ExprNoEffect(ty_or_expr.location);
                    self.report_warning(warning);
                }
                self.sizeof(&expr.ty, ty_or_expr.location)
            }
        };
        let ty = HlirType::new(HlirTypeKind::Int(false), HlirTypeDecl::Basic);
        Ok(HlirExpr {
            span: ty_or_expr.location,
            kind: Box::new(HlirExprKind::Literal(HlirLiteral::Int(size as i64))),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn sizeof(&mut self, ty: &HlirType, span: Span) -> u64 {
        use crate::util::arch::*;
        if ty.is_pointer() {
            return POINTER_SIZE;
        }

        let size = match &ty.kind {
            HlirTypeKind::Char(_) => CHAR_SIZE,
            HlirTypeKind::Int(_) => INT_SIZE,
            HlirTypeKind::Long(_) => LONG_SIZE,
            HlirTypeKind::Double => DOUBLE_SIZE,
            HlirTypeKind::Void => 0,
            HlirTypeKind::Float => FLOAT_SIZE,
            HlirTypeKind::Struct(ident) => {
                let result = self.scope.borrow_mut().get_struct_size(ident, span);
                if let Err(err) = result {
                    self.report_error(err);
                    0
                } else {
                    result.unwrap()
                }
            }
        };

        if let HlirTypeDecl::Array(array_size) = &ty.decl {
            size * array_size
        } else {
            size
        }
    }

    pub(super) fn validate_post_inc_or_dec(
        &mut self,
        op: &PostfixOp,
        expr: &Expression,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        let expr = self.validate_expression(expr)?;

        if self.not_incremental(&expr, span) {
            return Ok(expr);
        }
        let ty = expr.ty.clone();
        let kind = Box::new(match op {
            PostfixOp::Increment => HlirExprKind::PostIncrement(expr),
            PostfixOp::Decrement => HlirExprKind::PostDecrement(expr),
        });
        Ok(HlirExpr {
            span,
            is_lval: false,
            kind,
            ty,
        })
    }

    fn route_unary(
        &mut self,
        op: &UnaryOp,
        expr: &Locatable<Box<Expression>>,
    ) -> Result<HlirExpr, ()> {
        let span = expr.location;
        let expr = self.validate_expression(expr)?;
        self.validate_unary_expression(op, expr, span)
    }

    fn route_binary(
        &mut self,
        op: &BinaryOp,
        left: &Locatable<Box<Expression>>,
        right: &Locatable<Box<Expression>>,
    ) -> Result<HlirExpr, ()> {
        let span = left.location.merge(right.location);
        let left = self.validate_expression(left)?;
        let right = self.validate_expression(right)?;
        self.validate_binary_expression(op, left, right, span)
    }

    fn route_function_call(
        &mut self,
        ident: &Locatable<InternedStr>,
        args: &Vec<Locatable<Expression>>,
    ) -> Result<HlirExpr, ()> {
        let span = ident.location;
        let ident = ident.value.clone();
        let mut resolver = self.scope.borrow_mut();
        let func = resolver.validate_function_call(ident.clone(), span);
        if func.is_err() {
            let func = func.unwrap_err();
            return Err(());
        }
        let func = func.unwrap();
        let varargs = func.varargs;
        let params = func.params.clone();
        let location = func.location;
        let return_ty = func.return_ty.clone();
        drop(resolver);
        let mut hlir_args = Vec::new();
        let mut last_arg_span = span;
        for loc_expr in args {
            last_arg_span = loc_expr.location;
            hlir_args.push((
                self.validate_expression(&loc_expr.value)?,
                loc_expr.location,
            ));
        }
        let hlir_args = self.try_cast_function_args(hlir_args, &params)?;

        self.validate_function_params(varargs, &params, &hlir_args, span)?;
        let kind = HlirExprKind::FunctionCall {
            location,
            ident,
            args: hlir_args.into_iter().map(|arg| arg.0).collect(),
        };
        Ok(HlirExpr {
            span: span.merge(last_arg_span),
            kind: Box::new(kind),
            ty: return_ty,
            is_lval: false,
        })
    }
    fn try_cast_function_args(
        &mut self,
        args: Vec<(HlirExpr, Span)>,
        param_types: &[HlirType],
    ) -> Result<Vec<(HlirExpr, Span)>, ()> {
        let mut args = args;
        let var_args = args.split_off(param_types.len());
        let mut processed_args = Vec::new();
        for (arg, param_ty) in args.into_iter().zip(param_types.iter()) {
            let arg_ty = arg.0.ty.clone();
            let span = arg.1;
            if let Ok(arg) = implicit_cast(param_ty.clone(), arg.0, span) {
                processed_args.push((arg, span));
            } else {
                self.report_error(CompilerError::ArgumentTypeMismatch(
                    arg_ty.to_string(),
                    param_ty.to_string(),
                    span,
                ));
                return Err(());
            }
        }
        processed_args.extend(var_args);
        Ok(processed_args)
    }

    fn validate_function_params(
        &mut self,
        varargs: bool,
        params: &[HlirType],
        args: &[(HlirExpr, Span)],
        span: Span,
    ) -> Result<(), ()> {
        for (param, arg) in params.iter().zip(args.iter()) {
            if param != &arg.0.ty {
                self.report_error(CompilerError::FunctionTypeMismatch(arg.1));
                return Err(());
            }
        }
        if varargs {
            return Ok(());
        }
        if params.len() != args.len() {
            self.report_error(CompilerError::FunctionTypeMismatch(span));
            return Err(());
        }
        Ok(())
    }

    fn route_index(
        &mut self,
        left: &Locatable<Box<Expression>>,
        index: &Locatable<Box<Expression>>,
    ) -> Result<HlirExpr, ()> {
        let (left_span, left) = (left.location, self.validate_expression(left)?);
        let (index_span, index) = (index.location, self.validate_expression(index)?);
        self.validate_index_access(left, index, left_span, index_span)
    }

    fn route_member(
        &mut self,
        body: &Locatable<Box<Expression>>,
        member: &Locatable<InternedStr>,
    ) -> Result<HlirExpr, ()> {
        let (body_span, body) = (body.location, self.validate_expression(body)?);
        let (member_span, member) = (member.location, (**member).clone());
        self.validate_member_access(body, member, body_span, member_span)
    }

    fn route_pointer_member(
        &mut self,
        body: &Locatable<Box<Expression>>,
        member: &Locatable<InternedStr>,
    ) -> Result<HlirExpr, ()> {
        let (body_span, body) = (body.location, self.validate_expression(body)?);
        let (member_span, member) = (member.location, (**member).clone());
        self.validate_pointer_member_access(body, member, body_span, member_span)
    }

    fn route_cast_expression(
        &mut self,
        dec: &Locatable<Declaration>,
        expr: &Locatable<Box<Expression>>,
    ) -> Result<HlirExpr, ()> {
        let (expr_location, expr) = (expr.location, self.validate_expression(expr)?);
        debug_assert!(dec.ident.is_none());
        let cast_to_ty = self.validate_type(&dec.value.specifier, dec.location, false, false)?;
        let casting_ty_string = cast_to_ty.to_string();
        let expr_ty_string = expr.ty.to_string();
        explicit_cast(cast_to_ty.clone(), expr, dec.location.merge(expr_location)).map_err(|_| {
            let err = CompilerError::CannotCast(
                expr_ty_string,
                casting_ty_string,
                dec.location.merge(expr_location),
            );
            self.report_error(err);
        })
    }

    pub(super) fn validate_literal(
        &mut self,
        literal: &Literal,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        let (literal, ty) = match literal {
            Literal::Integer { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                if *value <= i64::MAX as isize {
                    (
                        HlirLiteral::Int(*value as i64),
                        HlirType::new(HlirTypeKind::Int(false), HlirTypeDecl::Basic),
                    )
                } else if *value <= u64::MAX as isize {
                    (
                        HlirLiteral::UInt(*value as u64),
                        HlirType::new(HlirTypeKind::Int(true), HlirTypeDecl::Basic),
                    )
                } else {
                    let err = CompilerError::NumberTooLarge(span);
                    self.report_error(err);
                    (
                        HlirLiteral::Int(0),
                        HlirType::new(HlirTypeKind::Int(false), HlirTypeDecl::Basic),
                    )
                }
            }
            Literal::Float { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                (
                    HlirLiteral::Float(*value),
                    HlirType::new(HlirTypeKind::Double, HlirTypeDecl::Basic),
                )
            }
            Literal::Char { value } => (
                HlirLiteral::Char(*value as u8),
                HlirType::new(HlirTypeKind::Char(false), HlirTypeDecl::Basic),
            ),
            Literal::String { value } => {
                let value = value.as_str().bytes().collect();
                (
                    HlirLiteral::String(value),
                    HlirType::new(HlirTypeKind::Char(false), HlirTypeDecl::Pointer),
                )
            }
        };
        Ok(HlirExpr {
            span,
            kind: Box::new(HlirExprKind::Literal(literal)),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn validate_index_access(
        &mut self,
        left: HlirExpr,
        index: HlirExpr,
        left_span: Span,
        index_span: Span,
    ) -> Result<HlirExpr, ()> {
        if !left.is_pointer() && !left.is_array() {
            self.report_error(CompilerError::InvalidLeftOfSubScript(
                left.ty.to_string(),
                left_span,
            ));
        }

        if !index.is_integer() {
            self.report_error(CompilerError::CannotIndexWith(
                index.ty.to_string(),
                index_span,
            ));
        }

        let ty = left.ty.clone();
        Ok(HlirExpr {
            span: left_span.merge(index_span),
            kind: Box::new(HlirExprKind::Index(left, index)),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn validate_pointer_member_access(
        &mut self,
        body: HlirExpr,
        member: InternedStr,
        body_span: Span,
        member_span: Span,
    ) -> Result<HlirExpr, ()> {
        if !body.is_pointer() {
            self.report_error(CompilerError::ArrowOnNonPointer(body_span));
        }
        // dereference to underlying type,
        let mut ty = body.ty.clone();
        ty.decl = HlirTypeDecl::Basic;
        let expr = HlirExpr {
            span: body_span.merge(member_span),
            kind: Box::new(HlirExprKind::Deref(body)),
            ty,
            is_lval: true,
        };
        self.validate_member_access(expr, member, body_span, member_span)
    }

    pub(super) fn validate_member_access(
        &mut self,
        body: HlirExpr,
        member: InternedStr,
        body_span: Span,
        member_span: Span,
    ) -> Result<HlirExpr, ()> {
        if !matches!(&body.ty.kind, HlirTypeKind::Struct(_)) || body.is_array() {
            self.report_error(CompilerError::CannotMemberAccessOnType(
                body.ty.to_string(),
                body_span,
            ));
        }

        if body.is_pointer() {
            self.report_error(CompilerError::DotOperatorOnPointer(body_span));
        }

        let body = body_span.into_locatable(body);
        let member = member_span.into_locatable(member);

        let result = self
            .scope
            .borrow_mut()
            .validate_struct_member_access(body, member);
        if let Err(err) = result {
            self.report_error(err);
            Err(())
        } else {
            Ok(result.unwrap())
        }
    }

    pub(super) fn validate_unary_expression(
        &mut self,
        op: &UnaryOp,
        expr: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        match op {
            UnaryOp::Increment | UnaryOp::Decrement => self.validate_pre_inc_or_dec(op, expr, span),
            UnaryOp::Plus => Ok(expr),
            UnaryOp::Negate => {
                if !expr.is_numeric() {
                    self.report_error(CompilerError::NonNumericNegation(span));
                    Ok(expr)
                } else {
                    let ty = expr.ty.clone();
                    Ok(HlirExpr {
                        span,
                        kind: Box::new(HlirExprKind::Negate(expr)),
                        is_lval: false,
                        ty,
                    })
                }
            }
            UnaryOp::LogicalNot | UnaryOp::BitwiseNot => {
                if !expr.is_integer() {
                    self.report_error(CompilerError::NotLogicalType(expr.ty.to_string(), span));
                    Ok(expr)
                } else {
                    let expr = implicit_cast(
                        HlirType {
                            decl: HlirTypeDecl::Basic,
                            kind: HlirTypeKind::Int(false),
                        },
                        expr,
                        span,
                    )?;
                    let ty = expr.ty.clone();
                    let expr = match op {
                        UnaryOp::LogicalNot => HlirExprKind::LogicalNot(expr),
                        UnaryOp::BitwiseNot => HlirExprKind::BitwiseNot(expr),
                        _ => unreachable!(),
                    };
                    Ok(HlirExpr {
                        span,
                        kind: Box::new(expr),
                        is_lval: false,
                        ty,
                    })
                }
            }
            UnaryOp::Deref => {
                if !expr.is_pointer() {
                    let err = CompilerError::DerefOnNonPointer(expr.ty.to_string(), span);
                    self.report_error(err);
                    return Ok(expr);
                }
                let ty_kind = expr.ty.kind.clone();

                Ok(HlirExpr {
                    span,
                    kind: Box::new(HlirExprKind::Deref(expr)),
                    ty: HlirType {
                        decl: HlirTypeDecl::Basic,
                        kind: ty_kind,
                    },
                    is_lval: true,
                })
            }
            UnaryOp::AddressOf => {
                if expr.is_pointer() {
                    let err = CompilerError::AttemptedAddressOfPointer(span);
                    self.report_error(err);
                    return Ok(expr);
                }
                let ty_kind = expr.ty.kind.clone();
                Ok(HlirExpr {
                    span,
                    kind: Box::new(HlirExprKind::AddressOf(expr)),
                    ty: HlirType {
                        kind: ty_kind,
                        decl: HlirTypeDecl::Pointer,
                    },
                    is_lval: false,
                })
            }
        }
    }

    pub(super) fn not_incremental(&mut self, expr: &HlirExpr, span: Span) -> bool {
        if !expr.is_lval {
            let err = CompilerError::LeftHandNotLVal(span);
            self.report_error(err);
            true
        } else if !(expr.is_numeric() || expr.is_pointer()) {
            self.report_error(CompilerError::CannotIncrementType(
                expr.ty.to_string(),
                span,
            ));
            true
        } else {
            false
        }
    }

    pub(super) fn validate_pre_inc_or_dec(
        &mut self,
        op: &UnaryOp,
        expr: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if self.not_incremental(&expr, span) {
            return Ok(expr);
        }

        let literal_one = HlirExpr {
            span,
            kind: Box::new(HlirExprKind::Literal(HlirLiteral::Char(1))),
            ty: HlirType {
                decl: HlirTypeDecl::Basic,
                kind: HlirTypeKind::Char(false),
            },
            is_lval: false,
        };

        let op = match op {
            UnaryOp::Increment => AssignOp::Minus,
            UnaryOp::Decrement => AssignOp::Plus,
            _ => panic!("Called `validate_post_inc_or_dec` with non incremental type!"),
        };

        self.validate_assign_op(&op, expr, literal_one, span)
    }
}
