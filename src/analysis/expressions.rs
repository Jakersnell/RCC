use crate::analysis::{err, Analyzer};
use crate::data::ast::{
    AssignOp, BinaryOp, Declaration, Expression, PostfixOp, TypeOrExpression, UnaryOp,
};
use crate::data::error::{CompilerError, CompilerWarning};
use crate::data::mlir::{
    MlirExpr, MlirExprKind, MlirLiteral, MlirType, MlirTypeDecl, MlirTypeKind, VOID_PTR,
};
use crate::data::tokens::Literal;
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, Span};

impl Analyzer {
    pub(super) fn validate_expression(&mut self, expr: &Expression) -> Result<MlirExpr, ()> {
        match expr {
            Expression::Literal(literal) => self.validate_literal(literal, literal.location),
            Expression::Variable(variable) => self.validate_variable_access(variable),
            Expression::Sizeof(ty_or_expr) => self.validate_sizeof(ty_or_expr),
            Expression::Parenthesized(expr) => self.validate_expression(expr),
            Expression::PostFix(op, expr) => self.validate_post_inc_or_dec(op, expr, expr.location),
            Expression::Unary(op, expr) => self.validate_unary(op, expr),
            Expression::Binary(op, left, right) => self.validate_binary(op, left, right),
            Expression::FunctionCall(ident, args) => self.validate_function_call(ident, args),
            Expression::Index(left, index) => self.validate_index(left, index),
            Expression::Member(body, member) => self.validate_member(body, member),
            Expression::PointerMember(body, member) => self.validate_pointer_member(body, member),
            Expression::Cast(dec, expr) => self.validate_cast_expression(dec, expr),
            _ => unreachable!(),
        }
        // .map(|expr| expr.fold())
    }

    fn validate_variable_access(
        &mut self,
        variable: &Locatable<InternedStr>,
    ) -> Result<MlirExpr, ()> {
        let result = self
            .scope
            .borrow_mut()
            .get_variable_type_and_id(&variable.value, variable.location)
            .map(|ty| MlirExpr {
                span: variable.location,
                kind: Box::new(MlirExprKind::Variable(variable.value.clone())),
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
    ) -> Result<MlirExpr, ()> {
        let size = match &ty_or_expr.value {
            TypeOrExpression::Type(ty) => {
                let ty = self.validate_type(&ty.specifier, ty_or_expr.location, false, false)?;
                self.sizeof(&ty, ty_or_expr.location)
            }
            TypeOrExpression::Expr(expr) => {
                let expr = self.validate_expression(expr)?;
                if !matches!(
                    &*expr.kind,
                    MlirExprKind::Variable(_) | MlirExprKind::Literal(_)
                ) {
                    let warning = CompilerWarning::ExprNoEffect(ty_or_expr.location);
                    self.report_warning(warning);
                }
                self.sizeof(&expr.ty, ty_or_expr.location)
            }
        };
        let ty = MlirType::new(MlirTypeKind::Int(true), MlirTypeDecl::Basic);
        Ok(MlirExpr {
            span: ty_or_expr.location,
            kind: Box::new(MlirExprKind::Literal(MlirLiteral::UInt(size as u32))),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn sizeof(&mut self, ty: &MlirType, span: Span) -> u64 {
        use crate::data::arch::*;
        if ty.is_pointer() {
            return POINTER_SIZE;
        }

        let size = match &ty.kind {
            MlirTypeKind::Char(_) => CHAR_SIZE,
            MlirTypeKind::Int(_) => INT_SIZE,
            MlirTypeKind::Long(_) => LONG_SIZE,
            MlirTypeKind::Double => DOUBLE_SIZE,
            MlirTypeKind::Void => 0,
            MlirTypeKind::Float => FLOAT_SIZE,
            MlirTypeKind::Struct(ident) => {
                let result = self.scope.borrow_mut().get_struct_size(ident, span);
                if let Err(err) = result {
                    self.report_error(err);
                    0
                } else {
                    result.unwrap()
                }
            }
        };

        if let MlirTypeDecl::Array(array_size) = &ty.decl {
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
    ) -> Result<MlirExpr, ()> {
        let expr = self.validate_expression(expr)?;

        if self.not_incremental(&expr, span) {
            self.report_error(CompilerError::CannotIncrementType(
                expr.ty.to_string(),
                span,
            ));
            return Ok(expr);
        }

        let ty = expr.ty.clone();
        let kind = Box::new(match op {
            PostfixOp::Increment => MlirExprKind::PostIncrement(expr),
            PostfixOp::Decrement => MlirExprKind::PostDecrement(expr),
        });

        Ok(MlirExpr {
            span,
            is_lval: false,
            kind,
            ty,
        })
    }

    fn validate_unary(
        &mut self,
        op: &UnaryOp,
        expr: &Locatable<Box<Expression>>,
    ) -> Result<MlirExpr, ()> {
        let span = expr.location;
        let expr = self.validate_expression(expr)?;
        self.validate_unary_expression(op, expr, span)
    }

    fn validate_binary(
        &mut self,
        op: &BinaryOp,
        left: &Locatable<Box<Expression>>,
        right: &Locatable<Box<Expression>>,
    ) -> Result<MlirExpr, ()> {
        let span = left.location.merge(right.location);
        let left = self.validate_expression(left)?;
        let right = self.validate_expression(right)?;
        self.validate_binary_expression(op, left, right, span)
    }

    fn validate_function_call(
        &mut self,
        ident: &Locatable<InternedStr>,
        args: &Vec<Locatable<Expression>>,
    ) -> Result<MlirExpr, ()> {
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
        let kind = MlirExprKind::FunctionCall {
            location,
            ident,
            args: hlir_args.into_iter().map(|arg| arg.0).collect(),
        };
        Ok(MlirExpr {
            span: span.merge(last_arg_span),
            kind: Box::new(kind),
            ty: return_ty,
            is_lval: false,
        })
    }
    fn try_cast_function_args(
        &mut self,
        args: Vec<(MlirExpr, Span)>,
        param_types: &[MlirType],
    ) -> Result<Vec<(MlirExpr, Span)>, ()> {
        let mut args = args;
        let var_args = args.split_off(param_types.len());
        let mut processed_args = Vec::new();
        for (arg, param_ty) in args.into_iter().zip(param_types.iter()) {
            let arg_ty = arg.0.ty.clone();
            let span = arg.1;
            let arg = self.implicit_cast(arg.0, param_ty.clone(), span);
            processed_args.push((arg, span));
        }
        processed_args.extend(var_args);
        Ok(processed_args)
    }

    fn validate_function_params(
        &mut self,
        varargs: bool,
        params: &[MlirType],
        args: &[(MlirExpr, Span)],
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

    fn validate_index(
        &mut self,
        left: &Locatable<Box<Expression>>,
        index: &Locatable<Box<Expression>>,
    ) -> Result<MlirExpr, ()> {
        let (left_span, left) = (left.location, self.validate_expression(left)?);
        let (index_span, index) = (index.location, self.validate_expression(index)?);
        self.validate_index_access(left, index, left_span, index_span)
    }

    fn validate_member(
        &mut self,
        body: &Locatable<Box<Expression>>,
        member: &Locatable<InternedStr>,
    ) -> Result<MlirExpr, ()> {
        let (body_span, body) = (body.location, self.validate_expression(body)?);
        let (member_span, member) = (member.location, (**member).clone());
        self.validate_member_access(body, member, body_span, member_span)
    }

    fn validate_pointer_member(
        &mut self,
        body: &Locatable<Box<Expression>>,
        member: &Locatable<InternedStr>,
    ) -> Result<MlirExpr, ()> {
        let (body_span, body) = (body.location, self.validate_expression(body)?);
        let (member_span, member) = (member.location, (**member).clone());
        self.validate_pointer_member_access(body, member, body_span, member_span)
    }

    fn validate_cast_expression(
        &mut self,
        dec: &Locatable<Declaration>,
        expr: &Locatable<Box<Expression>>,
    ) -> Result<MlirExpr, ()> {
        let (expr_location, expr) = (expr.location, self.validate_expression(expr)?);
        debug_assert!(dec.ident.is_none());
        let cast_to_ty = self.validate_type(&dec.value.specifier, dec.location, false, false)?;
        let casting_ty_string = cast_to_ty.to_string();
        let expr_ty_string = expr.ty.to_string();
        let casted_expr =
            self.explicit_cast(expr, cast_to_ty.clone(), dec.location.merge(expr_location));
        Ok(casted_expr)
    }

    pub(super) fn validate_literal(
        &mut self,
        literal: &Literal,
        span: Span,
    ) -> Result<MlirExpr, ()> {
        let (literal, ty) = match literal {
            Literal::Integer { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                if *value <= i64::MAX as isize {
                    (
                        MlirLiteral::Long(*value as i64),
                        MlirType::new(MlirTypeKind::Long(false), MlirTypeDecl::Basic),
                    )
                } else if *value <= u64::MAX as isize {
                    (
                        MlirLiteral::ULong(*value as u64),
                        MlirType::new(MlirTypeKind::Long(true), MlirTypeDecl::Basic),
                    )
                } else {
                    let err = CompilerError::NumberTooLarge(span);
                    self.report_error(err);
                    (
                        MlirLiteral::Long(0),
                        MlirType::new(MlirTypeKind::Long(false), MlirTypeDecl::Basic),
                    )
                }
            }
            Literal::Float { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                (
                    MlirLiteral::Double(*value),
                    MlirType::new(MlirTypeKind::Double, MlirTypeDecl::Basic),
                )
            }
            Literal::Char { value } => (
                MlirLiteral::UChar(*value as u8),
                MlirType::new(MlirTypeKind::Char(true), MlirTypeDecl::Basic),
            ),
            Literal::String { value } => {
                let mut value = value.bytes().collect::<Vec<_>>();
                (
                    MlirLiteral::String(value),
                    MlirType::new(MlirTypeKind::Char(true), MlirTypeDecl::Pointer),
                )
            }
        };
        Ok(MlirExpr {
            span,
            kind: Box::new(MlirExprKind::Literal(literal)),
            ty,
            is_lval: false,
        })
    }

    pub(super) fn validate_index_access(
        &mut self,
        left: MlirExpr,
        index: MlirExpr,
        left_span: Span,
        index_span: Span,
    ) -> Result<MlirExpr, ()> {
        if left.ty == VOID_PTR {
            err!(self, IncompleteSubscript, left.ty.to_string(), left_span);
        }

        if !left.is_pointer() && !left.is_array() {
            err!(self, InvalidLeftOfSubScript, left.ty.to_string(), left_span);
        }

        if !index.is_integer() {
            err!(self, CannotIndexWith, index.ty.to_string(), index_span);
        }

        let ty = left.ty.clone().as_basic();
        Ok(MlirExpr {
            kind: Box::new(MlirExprKind::Index(left, index)),
            span: left_span.merge(index_span),
            is_lval: true,
            ty,
        })
    }

    pub(super) fn validate_pointer_member_access(
        &mut self,
        body: MlirExpr,
        member: InternedStr,
        body_span: Span,
        member_span: Span,
    ) -> Result<MlirExpr, ()> {
        if !body.is_pointer() {
            err!(self, ArrowOnNonPointer, body_span);
        }
        // dereference to underlying type,
        let mut ty = body.ty.clone();
        ty.decl = MlirTypeDecl::Basic;
        let expr = MlirExpr {
            span: body_span.merge(member_span),
            kind: Box::new(MlirExprKind::Deref(body)),
            ty,
            is_lval: true,
        };
        self.validate_member_access(expr, member, body_span, member_span)
    }

    pub(super) fn validate_member_access(
        &mut self,
        body: MlirExpr,
        member: InternedStr,
        body_span: Span,
        member_span: Span,
    ) -> Result<MlirExpr, ()> {
        if !matches!(&body.ty.kind, MlirTypeKind::Struct(_)) || body.is_array() {
            err!(
                self,
                CannotMemberAccessOnType,
                body.ty.to_string(),
                body_span
            );
        }

        if body.is_pointer() {
            err!(self, DotOperatorOnPointer, body_span);
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
        expr: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
        match op {
            UnaryOp::Increment | UnaryOp::Decrement => self.validate_pre_inc_or_dec(op, expr, span),
            UnaryOp::Plus => Ok(expr),
            UnaryOp::Negate => {
                if !expr.is_numeric() || !expr.ty.is_basic() {
                    self.report_error(CompilerError::NonNumericNegation(span));
                    Ok(expr)
                } else {
                    let expr_cast_target = match &expr.ty.kind {
                        MlirTypeKind::Char(true) => Some(MlirType {
                            kind: MlirTypeKind::Char(false),
                            decl: MlirTypeDecl::Basic,
                        }),
                        MlirTypeKind::Int(true) => Some(MlirType {
                            kind: MlirTypeKind::Int(false),
                            decl: MlirTypeDecl::Basic,
                        }),
                        MlirTypeKind::Long(true) => Some(MlirType {
                            kind: MlirTypeKind::Long(false),
                            decl: MlirTypeDecl::Basic,
                        }),

                        _ => None,
                    };

                    let expr = if let Some(cast_target) = expr_cast_target {
                        self.implicit_cast(expr, cast_target, span)
                    } else {
                        expr
                    };

                    let ty = expr.ty.clone();
                    Ok(MlirExpr {
                        span,
                        kind: Box::new(MlirExprKind::Negate(expr)),
                        is_lval: false,
                        ty,
                    })
                }
            }
            UnaryOp::LogicalNot => {
                if !expr.is_numeric() {
                    self.report_error(CompilerError::NotLogicalType(expr.ty.to_string(), span));
                    Ok(expr)
                } else {
                    Ok(MlirExpr {
                        ty: MlirType {
                            decl: MlirTypeDecl::Basic,
                            kind: MlirTypeKind::Int(false),
                        },
                        kind: Box::new(MlirExprKind::LogicalNot(expr)),
                        is_lval: false,
                        span,
                    })
                }
            }
            UnaryOp::BitwiseNot => {
                if !expr.is_integer() {
                    self.report_error(CompilerError::CannotBitwise(expr.ty.to_string(), span));
                    Ok(expr)
                } else {
                    Ok(MlirExpr {
                        ty: MlirType {
                            decl: MlirTypeDecl::Basic,
                            kind: MlirTypeKind::Int(false),
                        },
                        kind: Box::new(MlirExprKind::BitwiseNot(expr)),
                        is_lval: false,
                        span,
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

                Ok(MlirExpr {
                    span,
                    kind: Box::new(MlirExprKind::Deref(expr)),
                    ty: MlirType {
                        decl: MlirTypeDecl::Basic,
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
                if !expr.is_lval {
                    let err = CompilerError::CannotAddressNonLVal(span);
                    self.report_error(err);
                    return Ok(expr);
                }
                let ty_kind = expr.ty.kind.clone();
                Ok(MlirExpr {
                    span,
                    kind: Box::new(MlirExprKind::AddressOf(expr)),
                    ty: MlirType {
                        kind: ty_kind,
                        decl: MlirTypeDecl::Pointer,
                    },
                    is_lval: false,
                })
            }
        }
    }

    pub(super) fn not_incremental(&mut self, expr: &MlirExpr, span: Span) -> bool {
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
        expr: MlirExpr,
        span: Span,
    ) -> Result<MlirExpr, ()> {
        if self.not_incremental(&expr, span) {
            return Ok(expr);
        }

        let literal_one = MlirExpr {
            span,
            kind: Box::new(MlirExprKind::Literal(MlirLiteral::UChar(1))),
            ty: MlirType {
                decl: MlirTypeDecl::Basic,
                kind: MlirTypeKind::Char(false),
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
