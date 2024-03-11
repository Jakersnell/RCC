mod flow;
pub mod hlir;
mod symbols;

use crate::analysis::hlir::{
    HighLevelIR, HlirExpr, HlirExprKind, HlirLiteral, HlirType, HlirTypeDecl, HlirTypeKind,
    HlirVarInit, HlirVariable,
};
use crate::analysis::symbols::SymbolResolver;
use crate::lexer::tokens::Literal;
use crate::parser::ast::{
    ASTRoot, AbstractSyntaxTree, AssignOp, BinaryOp, Block, DeclarationSpecifier, Expression,
    InitDeclaration, TypeQualifier, TypeSpecifier, VariableDeclaration,
};
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, Span};
use derive_new::new;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SharedReporter = Rc<RefCell<Reporter>>;

macro_rules! cast {
    ($hlir:expr, $to:expr) => {{
        let ty = $hlir.ty.clone();
        HlirExpr {
            kind: Box::new(HlirExprKind::Cast($to, $hlir)),
            ty,
            is_lval: false,
        }
    }};
}

pub struct GlobalValidator<'a> {
    ast: AbstractSyntaxTree,
    scope: SymbolResolver<'a>,
    function_scope: Option<SymbolResolver<'a>>,
    reporter: SharedReporter,
}

impl<'a> GlobalValidator<'a> {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast,
            scope: SymbolResolver::create_root(),
            function_scope: None,
            reporter: Rc::new(RefCell::new(Reporter::default())),
        }
    }

    fn report_error(&mut self, error: CompilerError) -> Result<(), ()> {
        self.reporter.borrow_mut().report_error(error);
        Err(())
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.borrow_mut().report_warning(warning);
    }

    pub fn validate(mut self) -> Result<HighLevelIR, SharedReporter> {
        let hlir = HighLevelIR::default();
        for node in &*self.ast {
            use crate::parser::ast::InitDeclaration::*;
            match node {
                Declaration(locatable_variable) => {
                    todo!("validation")
                }
                Function(locatable_function) => {
                    todo!("validation")
                }
                Struct(locatable_struct) => {
                    todo!("validation")
                }
            }
        }
        if self.reporter.borrow().status().is_err() {
            Err(self.reporter)
        } else {
            Ok(hlir)
        }
    }

    fn validate_variable(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<HlirVariable, ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        let declaration = &var.declaration;
        if declaration.value.ident.is_none() {
            let err = CompilerError::DeclarationMissingIdentifier(declaration.location);
            self.report_error(err);
            return Err(());
        }

        let ident = declaration
            .value
            .ident
            .as_ref()
            .expect("Fatal compiler error: Identifier not set");
        let ident_span = ident.location;
        let ident = ident.value.clone();

        let ty = self.validate_variable_specifier(&declaration.value.specifier, span)?;
        if var.is_array && var.array_size.is_none() && var.initializer.is_none() {
            let err = CompilerError::ArraySizeNotSpecified(span);
            self.report_error(err);
            return Err(());
        }

        let initializer: Option<HlirVarInit> = if let Some(init) = &var.initializer {
            todo!("Validate variable initializer here!");
        } else {
            None
        };

        todo!("get size of array and create variable");
    }

    fn validate_variable_specifier(
        &mut self,
        specifier: &DeclarationSpecifier,
        span: Span,
    ) -> Result<HlirType, ()> {
        for storage_spec in &specifier.specifiers {
            // need to change this span to the specific storage spec location
            self.report_warning(CompilerWarning::UnsupportedStorageSpecifier(
                storage_spec.to_string(),
                span,
            ))
        }

        let mut is_const = false;
        for ty_qual in &specifier.qualifiers {
            // this is set up for expansion
            match ty_qual {
                TypeQualifier::Const => {
                    if is_const {
                        let warning = CompilerWarning::RedundantUsage(ty_qual.to_string(), span);
                        self.report_warning(warning);
                    } else {
                        is_const = true;
                    }
                }
            }
        }

        let ty_kind = self.validate_type(&specifier.ty, span)?;
        let ty_dec = if specifier.pointer {
            HlirTypeDecl::Pointer(false)
        } else {
            HlirTypeDecl::Basic
        };
        Ok(HlirType {
            kind: ty_kind,
            decl: ty_dec,
        })
    }

    fn validate_type(
        &mut self,
        specifiers: &[TypeSpecifier],
        location: Span,
    ) -> Result<HlirTypeKind, ()> {
        #[derive(Debug, PartialEq)]
        enum State {
            Start,
            SeenUnsigned,
            SeenUnsignedLong,
            SeenSigned,
            SeenSignedLong,
            End,
        }
        let mut hlir_type: Option<HlirTypeKind> = None;
        let mut state = State::Start;
        let mut iter = specifiers.iter();
        macro_rules! seen_signed_or_unsigned {
            ($ty_spec:ident, $unsigned:literal, $long:expr) => {
                match $ty_spec {
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(HlirTypeKind::Char($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Int($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        state = $long;
                    }
                    Some(ty) => {
                        let err =
                            CompilerError::TypeCannotBeSignedOrUnsigned(ty.to_string(), location);
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        let err = CompilerError::ExpectedTypeSpecifier(location);
                        self.report_error(err);
                        return Err(());
                    }
                }
            };
        }
        macro_rules! seen_signed_or_unsigned_long {
            ($ty_spec:ident, $unsigned:literal) => {{
                match $ty_spec {
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(HlirTypeKind::LLong($unsigned));
                        state = State::End;
                    }
                    Some(ty) => {
                        let err =
                            CompilerError::InvalidTypeSpecifierOrder(ty.to_string(), location);
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
                        state = State::End;
                    }
                }
            }};
        }
        loop {
            let mut ty_spec = if state != State::End {
                iter.next()
            } else {
                None
            };

            match state {
                State::Start => match ty_spec {
                    Some(TypeSpecifier::Void) => {
                        hlir_type = Some(HlirTypeKind::Void);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(HlirTypeKind::Char(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Int(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        state = State::SeenSignedLong;
                    }
                    Some(TypeSpecifier::Double) => {
                        hlir_type = Some(HlirTypeKind::Double);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Struct(ident)) => {
                        hlir_type = Some(HlirTypeKind::Struct(ident.clone()));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Signed) => {
                        state = State::SeenSigned;
                    }
                    Some(TypeSpecifier::Unsigned) => {
                        state = State::SeenUnsigned;
                    }
                    None => panic!("Span: {:?}", location),
                },
                State::SeenUnsigned => {
                    seen_signed_or_unsigned!(ty_spec, true, State::SeenUnsignedLong)
                }
                State::SeenSigned => {
                    seen_signed_or_unsigned!(ty_spec, false, State::SeenSignedLong)
                }
                State::SeenUnsignedLong => seen_signed_or_unsigned_long!(ty_spec, true),
                State::SeenSignedLong => seen_signed_or_unsigned_long!(ty_spec, false),
                State::End => break,
            }
        }
        debug_assert!(hlir_type.is_some());
        if iter.next().is_some() {
            let err = CompilerError::InvalidTypeSpecifier(location);
            self.report_error(err);
            return Err(());
        }
        let ty = hlir_type.expect("Fatal compiler error: Type specifier not set");
        Ok(ty)
    }

    // Expression Validation
    fn validate_expression(&mut self, expr: &Expression) -> Result<HlirExpr, ()> {
        match expr {
            Expression::Literal(literal) => match &literal.value {
                Literal::Integer { .. } => {}
                Literal::Float { .. } => {}
                Literal::Char { .. } => {}
                Literal::String { .. } => {}
            },
            Expression::Variable(variable) => {
                // validate variable exists, return HlirExpr with the type of the variable and is_lval
            }
            Expression::Sizeof(ty_or_expr) => {
                // validate type or expression and return HlirExpr as an Int
            }
            Expression::Parenthesized(expr) => {
                // just remove the parentheses and validate the expression
            }
            Expression::PostFix(op, expr) => {
                // validate op can be performed on expr, return valid op
            }
            Expression::Unary(op, expr) => {
                // validate op can be performed on expr, return valid op
            }
            Expression::Binary(op, left, right) => {
                let left_span = left.location;
                let right_span = right.location;
                let left = self.validate_expression(left)?;
                let right = self.validate_expression(right)?;
                self.validate_binary_expression(op, left, right, left_span);
            }
            Expression::FunctionCall(ident, args) => {
                // validate function exists and validate args
            }
            Expression::Index(left, index) => {
                // validate left can be indexed into and validate index is integer type
            }
            Expression::Member(body, member) => {
                // validate body is a struct and member exists
            }
            Expression::PointerMember(body, member) => {
                // validate body is a struct pointer and member exists
            }
            Expression::Cast(dec, expr) => {
                // validate cast is valid and expr is valid
            }
            Expression::ArrayInitializer(arr) => {
                // validate all elements are of the same type
            }
        }
        todo!()
    }

    fn try_cast_together(
        &mut self,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<(HlirExpr, HlirExpr), ()> {
        let mut left = left;
        let mut right = right;
        if left.ty != right.ty {
            if let Some(cast) = left.ty.try_implicit_cast(&right.ty) {
                let casted = HlirExprKind::Cast(cast, left);
                left = HlirExpr {
                    kind: Box::new(casted),
                    ty: right.ty.clone(),
                    is_lval: false,
                };
            } else if let Some(cast) = right.ty.try_implicit_cast(&left.ty) {
                let casted = HlirExprKind::Cast(cast, right);
                right = HlirExpr {
                    kind: Box::new(casted),
                    ty: left.ty.clone(),
                    is_lval: false,
                };
            } else {
                let err = CompilerError::InvalidBinaryOperation(
                    left.ty.to_string(),
                    right.ty.to_string(),
                    span,
                );
                self.report_error(err);
                return Err(());
            };
        }
        Ok((left, right))
    }

    fn validate_binary_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if left.ty.is_array() || right.ty.is_array() {
            let err = CompilerError::InvalidArrayOperation(span);
            self.report_error(err);
            return Err(());
        }
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

    fn validate_arithmetic_binary_op(
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
            return Err(());
        } else if left.ty.is_pointer() && right.ty.is_numeric() {
            // only addition/subtraction
            let left = cast!(
                left,
                HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic)
            );
            match op {
                BinaryOp::Add => HlirExprKind::Add(left, right),
                BinaryOp::Sub => HlirExprKind::Sub(left, right),
                _ => panic!("Fatal compiler error: Invalid binary op past initial check."),
            }
        } else if left.ty.is_numeric() && right.ty.is_numeric() {
            let (left, right) = self.try_cast_together(left, right, span)?;
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
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            ));
            return Err(());
        };
        Ok(HlirExpr {
            kind: Box::new(kind),
            ty,
            is_lval: false,
        })
    }
    fn validate_assign_op(
        &mut self,
        op: &AssignOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if !left.is_lval {
            let err = CompilerError::LeftHandNotLVal(span);
            self.report_error(err);
            return Err(());
        }
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

    fn validate_binary_equivalence_expression(
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
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
            self.report_error(err);
            return Err(());
        }
        let (left, right) = if left.ty != right.ty {
            self.try_cast_together(left, right, span)?
        } else {
            (left, right)
        };
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

    fn validate_binary_bitwise_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if (!left.is_pointer() && right.is_pointer()) || (left.is_pointer() && !right.is_pointer())
        {
            let err = CompilerError::InvalidBinaryOperation(
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
            self.report_error(err);
            return Err(());
        }
        let (left, right) = if left.is_pointer() && right.is_pointer() {
            (
                cast!(
                    left,
                    HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic)
                ),
                cast!(
                    right,
                    HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic)
                ),
            )
        } else {
            self.try_cast_together(left, right, span)?
        };
        let ty = left.ty.clone();
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
fn test_try_cast_together_results_in_correct_higher_type_for_valid_types() {
    let test_left = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Integer(1))),
        ty: HlirType::new(HlirTypeKind::Int(false), HlirTypeDecl::Basic),
        is_lval: false,
    };
    let test_right = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Integer(1))),
        ty: HlirType::new(HlirTypeKind::Long(false), HlirTypeDecl::Basic),
        is_lval: false,
    };
    let span = Span::default();
    let (result_left, result_right) = GlobalValidator::new(AbstractSyntaxTree::default())
        .try_cast_together(test_left, test_right, span)
        .expect("Result should be ok");
    assert_eq!(
        result_left.ty,
        HlirType::new(HlirTypeKind::Long(false), HlirTypeDecl::Basic)
    );
    assert_eq!(
        result_right.ty,
        HlirType::new(HlirTypeKind::Long(false), HlirTypeDecl::Basic)
    );
}

#[test]
fn test_validate_type_returns_error_for_invalid_type_orientations() {
    use TypeSpecifier::*;
    let type_tests = [
        vec![Int, Int, Int],
        vec![Long, Int, Int],
        vec![Int, Long, Int],
        vec![Unsigned, Double],
        vec![Unsigned, Signed, Int],
        vec![Signed, Unsigned, Long],
        vec![Signed, Double],
    ];
    for types in type_tests {
        let mut validator = GlobalValidator::new(AbstractSyntaxTree::default());
        let result = validator.validate_type(&types, Span::default());
        if result.is_ok() {
            panic!("Expected error, got ok, test: {:?}", types);
        }
    }
}

#[test]
fn test_validate_type_returns_ok_for_valid_type_orientations() {
    use TypeSpecifier::*;
    let type_tests = [
        (vec![Int], HlirTypeKind::Int(false)),
        (vec![Long], HlirTypeKind::Long(false)),
        (vec![Unsigned, Int], HlirTypeKind::Int(true)),
        (vec![Unsigned, Long], HlirTypeKind::Long(true)),
        (vec![Signed, Int], HlirTypeKind::Int(false)),
        (vec![Signed, Long], HlirTypeKind::Long(false)),
        (vec![Double], HlirTypeKind::Double),
        (vec![Void], HlirTypeKind::Void),
    ];
    for (types, expected) in type_tests {
        let mut validator = GlobalValidator::new(AbstractSyntaxTree::default());
        let result = validator.validate_type(&types, Span::default());
        assert_eq!(result, Ok(expected));
    }
}

#[test]
fn test_validate_binary_bitwise_expression_is_ok_for_valid_expressions() {
    let test_cases = [
        (
            HlirTypeKind::Int(false),  // left
            HlirTypeKind::Long(false), // right
            HlirTypeKind::Long(false), // expected back
        ),
        (
            HlirTypeKind::Int(false),
            HlirTypeKind::Char(false),
            HlirTypeKind::Int(false),
        ),
        (
            HlirTypeKind::Long(false),
            HlirTypeKind::Int(false),
            HlirTypeKind::Long(false),
        ),
        (
            HlirTypeKind::Long(false),
            HlirTypeKind::Long(false),
            HlirTypeKind::Long(false),
        ),
    ];
    macro_rules! make_expr {
        ($kind:expr) => {
            HlirExpr {
                kind: Box::new(HlirExprKind::Literal(HlirLiteral::Integer(1))),
                ty: HlirType::new($kind, HlirTypeDecl::Basic),
                is_lval: false,
            }
        };
    }
    for (left, right, expected) in test_cases {
        let left = make_expr!(left);
        let right = make_expr!(right);
        let span = Span::default();
        let result = GlobalValidator::new(AbstractSyntaxTree::default())
            .validate_binary_bitwise_expression(&BinaryOp::BitwiseAnd, left, right, span);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().ty.kind, expected);
    }
}
