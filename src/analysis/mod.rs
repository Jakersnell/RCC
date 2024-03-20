pub mod hlir;
mod symbols;

use crate::analysis::hlir::*;
use crate::analysis::symbols::SymbolResolver;
use crate::lexer::tokens::Literal;
use crate::parser::ast::*;
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, Span};
use derive_new::new;
use log::debug;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

pub type SharedReporter = Rc<RefCell<Reporter>>;

macro_rules! cast {
    ($hlir:expr, $to:expr) => {{
        HlirExpr {
            kind: Box::new(HlirExprKind::Cast($to.clone(), $hlir)),
            ty: $to,
            is_lval: false,
        }
    }};
}

macro_rules! fabricate_expr {
    ($ty:expr) => {
        HlirExpr {
            HlirExpr {
                ki
            }
        }
    };
}

#[derive(PartialEq)]
enum DecType {
    Variable,
    Parameter,
}

pub struct GlobalValidator {
    ast: AbstractSyntaxTree,
    scope: SymbolResolver,
    reporter: SharedReporter,
}

impl GlobalValidator {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast,
            scope: SymbolResolver::create_root(),
            reporter: Rc::new(RefCell::new(Reporter::default())),
        }
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

    fn report_error(&mut self, error: CompilerError) -> Result<(), ()> {
        self.reporter.borrow_mut().report_error(error);
        Err(())
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.borrow_mut().report_warning(warning);
    }

    fn push_scope(&mut self) {
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        std::mem::swap(&mut resolver, &mut self.scope);
        resolver = SymbolResolver::new(Some(Box::new(resolver)));
        std::mem::swap(&mut resolver, &mut self.scope);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn pop_scope(&mut self) {
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        std::mem::swap(&mut resolver, &mut self.scope);
        resolver = *resolver
            .remove_self()
            .expect("Popped scope on global scope.");
        std::mem::swap(&mut resolver, &mut self.scope);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn validate_function(
        &mut self,
        func: &Locatable<FunctionDeclaration>,
    ) -> Result<HlirFunction, ()> {
        let (func_span, func) = (func.location, &func.value);
        let (dec_span, dec) = (func.declaration.location, &func.declaration.value);
        if !dec.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::FunctionStorageSpecifiers(dec_span));
            return Err(());
        }

        let ty = self.validate_type(&dec.specifier, dec_span)?;
        let ident = &dec.ident;
        if ident.is_none() {
            self.report_error(CompilerError::FunctionRequiresIdentifier(dec_span));
            return Err(());
        }

        let ident = ident.as_ref().unwrap().value.clone();
        let raw_params = &func.parameters;
        let mut parameters = Vec::new();
        for parameter in raw_params {
            parameters.push(self.validate_function_param(parameter)?);
        }

        let param_types = parameters
            .iter()
            .map(|var| var.ty.clone())
            .collect::<Vec<_>>();
        self.scope
            .add_function(&ident, ty.clone(), param_types, func_span);

        self.push_scope();

        for parameter in &parameters {
            let array_size = match &parameter.ty.decl {
                HlirTypeDecl::Array(size) => Some(*size),
                _ => None,
            };
            self.scope
                .add_variable(
                    &parameter.ident,
                    &parameter.ty,
                    parameter.is_const,
                    parameter.initializer.is_some(),
                    array_size,
                    func_span,
                )
                .map_err(|err| {
                    self.report_error(err);
                })?;
        }

        let body = self.validate_block(&func.body)?;

        Ok(HlirFunction {
            ty,
            ident,
            parameters,
            body,
        })
    }

    fn validate_block(&mut self, block: &Locatable<Block>) -> Result<HlirBlock, ()> {
        let mut statements = Vec::new();
        for raw_stmt in &block.0 {
            statements.push(self.validate_statement(raw_stmt)?);
        }
        Ok(HlirBlock(statements))
    }

    fn validate_statement(&mut self, stmt: &Locatable<Statement>) -> Result<HlirStmt, ()> {
        todo!()
    }

    fn validate_function_param(
        &mut self,
        param: &Locatable<Declaration>,
    ) -> Result<HlirVariable, ()> {
        if !param.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::ParamStorageSpecifiers(param.location));
            return Err(());
        }

        if param.ident.is_none() {
            self.report_error(CompilerError::ParamRequiresIdent(param.location));
            return Err(());
        }

        let hlir_var = self.process_dec_to_hlir_variable(&param.value, param.location)?;

        Ok(hlir_var)
    }

    fn validate_variable(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<HlirVariable, ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        let declaration = &var.declaration;

        let mut variable =
            self.process_dec_to_hlir_variable(&declaration.value, declaration.location)?;

        let ty = self.validate_type(&declaration.specifier, span)?;
        if var.is_array && var.array_size.is_none() && var.initializer.is_none() {
            let err = CompilerError::ArraySizeNotSpecified(span);
            self.report_error(err);
            return Err(());
        }

        let initializer: Option<HlirVarInit> = if let Some(init) = &var.initializer {
            Some(self.validate_initializer(init, init.location)?)
        } else {
            None
        };

        let array_size = var.array_size.map(|size| size as u64).or_else(|| {
            initializer.as_ref().and_then(|init| match init {
                HlirVarInit::Array(arr) => Some(arr.len() as u64),
                HlirVarInit::Expr(_) => None,
            })
        });

        let ty = if let Some(array_size) = array_size {
            let mut ty = ty;
            ty.decl = HlirTypeDecl::Array(array_size);
            ty
        } else {
            ty
        };

        variable.initializer = initializer;
        variable.ty = ty;
        Ok(variable)
    }

    fn process_dec_to_hlir_variable(
        &mut self,
        dec: &Declaration,
        span: Span,
    ) -> Result<HlirVariable, ()> {
        let ident = dec.ident.as_ref().unwrap();
        let ident_span = ident.location;
        let ident = ident.value.clone();

        let mut is_const = false;
        for ty_qual in &dec.specifier.qualifiers {
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
        for storage_spec in &dec.specifier.specifiers {
            // need to change this span to the specific storage spec location
            self.report_warning(CompilerWarning::UnsupportedStorageSpecifier(
                storage_spec.to_string(),
                span,
            ))
        }
        let ty = self.validate_type(&dec.specifier, span)?;

        Ok(HlirVariable {
            ty,
            ident,
            is_const,
            initializer: None,
        })
    }

    fn validate_initializer(&mut self, expr: &Expression, span: Span) -> Result<HlirVarInit, ()> {
        if let Expression::ArrayInitializer(arr) = expr {
            let mut inits = Vec::with_capacity(arr.len());
            for init in arr {
                let init = self.validate_expression(expr)?;
                inits.push(init);
            }
            Ok(HlirVarInit::Array(inits))
        } else {
            let expr = self.validate_expression(expr)?;
            Ok(HlirVarInit::Expr(expr))
        }
    }

    fn validate_type(
        &mut self,
        declaration: &DeclarationSpecifier,
        location: Span,
    ) -> Result<HlirType, ()> {
        #[derive(Debug, PartialEq)]
        enum State {
            Start,
            SeenUnsigned,
            SeenSigned,
            End,
        }
        let mut hlir_type: Option<HlirTypeKind> = None;
        let mut state = State::Start;
        let mut iter = declaration.ty.iter();
        macro_rules! seen_signed_or_unsigned {
            ($ty_spec:ident, $unsigned:literal) => {
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
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
                        state = State::End;
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
                        hlir_type = Some(HlirTypeKind::Long(false));
                        state = State::End;
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
                State::SeenUnsigned => seen_signed_or_unsigned!(ty_spec, true),
                State::SeenSigned => seen_signed_or_unsigned!(ty_spec, false),
                State::End => break,
            }
        }
        debug_assert!(hlir_type.is_some());
        if iter.next().is_some() {
            let err = CompilerError::InvalidTypeSpecifier(location);
            self.report_error(err);
            return Err(());
        }
        let ty_kind = hlir_type.unwrap();

        let ty_dec = if declaration.pointer {
            HlirTypeDecl::Pointer
        } else {
            HlirTypeDecl::Basic
        };

        if matches!(ty_kind, HlirTypeKind::Void) && !matches!(ty_dec, HlirTypeDecl::Pointer) {
            self.report_error(CompilerError::IncompleteType(location));
        }

        Ok(HlirType {
            kind: ty_kind,
            decl: ty_dec,
        })
    }

    // Expression Validation
    fn validate_expression(&mut self, expr: &Expression) -> Result<HlirExpr, ()> {
        match expr {
            Expression::Literal(literal) => self.validate_literal(literal, literal.location),
            Expression::Variable(variable) => self
                .scope
                .get_variable_type(&variable.value, variable.location)
                .map(|ty| HlirExpr {
                    kind: Box::new(HlirExprKind::Variable(variable.value.clone())),
                    ty,
                    is_lval: true,
                })
                .map_err(|err| {
                    self.report_error(err);
                }),
            Expression::Sizeof(ty_or_expr) => {
                let size = match &ty_or_expr.value {
                    TypeOrExpression::Type(ty) => {
                        let ty = self.validate_type(&ty.specifier, ty_or_expr.location)?;
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
                    kind: Box::new(HlirExprKind::Literal(HlirLiteral::Int(size as i64))),
                    ty,
                    is_lval: false,
                })
            }
            Expression::Parenthesized(expr) => self.validate_expression(expr),
            Expression::PostFix(op, expr) => self.validate_post_inc_or_dec(op, expr, expr.location),
            Expression::Unary(op, expr) => {
                let span = expr.location;
                let expr = self.validate_expression(expr)?;
                self.validate_unary_expression(op, expr, span)
            }
            Expression::Binary(op, left, right) => {
                let span = left.location.merge(right.location);
                let left = self.validate_expression(left)?;
                let right = self.validate_expression(right)?;
                self.validate_binary_expression(op, left, right, span)
            }
            Expression::FunctionCall(ident, args) => {
                let span = ident.location;
                let ident = ident.value.clone();
                let mut hlir_args = Vec::new();
                for loc_expr in args {
                    hlir_args.push((
                        self.validate_expression(&loc_expr.value)?,
                        loc_expr.location,
                    ));
                }
                self.scope
                    .validate_function_call(ident, hlir_args, span)
                    .map_err(|err| {
                        self.report_error(err);
                    })
            }
            Expression::Index(left, index) => {
                let (left_span, left) = (left.location, self.validate_expression(left)?);
                let (index_span, index) = (index.location, self.validate_expression(index)?);
                self.validate_index_access(left, index, left_span, index_span)
            }
            Expression::Member(body, member) => {
                let (body_span, body) = (body.location, self.validate_expression(body)?);
                let (member_span, member) = (member.location, (**member).clone());
                self.validate_member_access(body, member, body_span, member_span)
            }
            Expression::PointerMember(body, member) => {
                let (body_span, body) = (body.location, self.validate_expression(body)?);
                let (member_span, member) = (member.location, (**member).clone());
                self.validate_pointer_member_access(body, member, body_span, member_span)
            }
            Expression::Cast(dec, expr) => {
                let (expr_location, expr) = (expr.location, self.validate_expression(expr)?);
                debug_assert!(dec.ident.is_none());
                let dec_loc = dec.location;
                let cast_to_ty = self.validate_type(&dec.value.specifier, dec.location)?;
                self.explicit_cast(cast_to_ty, expr)
            }
            ty => panic!("Fatal compiler error: Unexpected expression type: {:?}", ty),
        }
    }

    fn validate_literal(&mut self, literal: &Literal, span: Span) -> Result<HlirExpr, ()> {
        let (literal, kind) = match literal {
            Literal::Integer { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                if *value <= i64::MAX as isize {
                    (HlirLiteral::Int(*value as i64), HlirTypeKind::Int(false))
                } else if *value <= u64::MAX as isize {
                    (HlirLiteral::UInt(*value as u64), HlirTypeKind::Int(true))
                } else {
                    let err = CompilerError::NumberTooLarge(span);
                    self.report_error(err);
                    (HlirLiteral::Int(0), HlirTypeKind::Int(false))
                }
            }
            Literal::Float { value, suffix } => {
                if suffix.is_some() {
                    let warning = CompilerWarning::SuffixIgnored(span);
                    self.report_warning(warning);
                }
                (HlirLiteral::Float(*value), HlirTypeKind::Double)
            }
            Literal::Char { value } => (HlirLiteral::Char(*value as u8), HlirTypeKind::Char(false)),
            Literal::String { value } => {
                let value = value.as_str().bytes().collect();
                (HlirLiteral::String(value), HlirTypeKind::Char(false))
            }
        };
        let ty = HlirType::new(kind, HlirTypeDecl::Basic);
        Ok(HlirExpr {
            kind: Box::new(HlirExprKind::Literal(literal)),
            ty,
            is_lval: false,
        })
    }

    fn explicit_cast(&mut self, cast_ty: HlirType, expr: HlirExpr) -> Result<HlirExpr, ()> {
        if cast_ty == expr.ty {
            return Ok(expr);
        }
        let expr_ty = &expr.ty;
        match (&expr_ty.kind, &expr_ty.decl, &cast_ty.kind, &cast_ty.decl) {
            (_, HlirTypeDecl::Array(_), to_kind, HlirTypeDecl::Pointer) => Ok(cast!(
                expr,
                HlirType {
                    kind: to_kind.clone(),
                    decl: HlirTypeDecl::Pointer
                }
            )),
            (kind, HlirTypeDecl::Pointer, to_kind, HlirTypeDecl::Pointer) => Ok(cast!(
                expr,
                HlirType {
                    kind: to_kind.clone(),
                    decl: HlirTypeDecl::Basic
                }
            )),
            (kind, HlirTypeDecl::Pointer, HlirTypeKind::Long(signed), HlirTypeDecl::Basic) => {
                Ok(cast!(
                    expr,
                    HlirType {
                        kind: HlirTypeKind::Long(true),
                        decl: HlirTypeDecl::Basic
                    }
                ))
            }
            (kind, HlirTypeDecl::Basic, cast_kind, HlirTypeDecl::Basic)
                if kind.is_numeric() && cast_kind.is_numeric() =>
            {
                Ok(self.cast_numeric_to_numeric(cast_ty, expr))
            }
            _ => Err(()),
        }
    }

    fn implicit_cast(&mut self, cast_to: HlirType, expr: HlirExpr) -> Result<HlirExpr, ()> {
        match (&cast_to.kind, &cast_to.decl, &expr.ty.kind, &expr.ty.decl) {
            (_, HlirTypeDecl::Pointer, HlirTypeKind::Void, HlirTypeDecl::Pointer) => {
                let ty = HlirType {
                    kind: HlirTypeKind::Void,
                    decl: HlirTypeDecl::Pointer,
                };
                Ok(HlirExpr {
                    kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
                    is_lval: false,
                    ty,
                })
            }
            (kind, HlirTypeDecl::Array(_), cast_kind, HlirTypeDecl::Pointer)
                if kind == cast_kind =>
            {
                let ty = HlirType {
                    kind: cast_kind.clone(),
                    decl: HlirTypeDecl::Pointer,
                };
                Ok(HlirExpr {
                    kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
                    is_lval: false,
                    ty,
                })
            }
            (casting_kind, HlirTypeDecl::Basic, cast_kind, HlirTypeDecl::Basic) => {
                Ok(self.cast_numeric_to_numeric(cast_to, expr))
            }
            _ => Err(()),
        }
    }

    fn cast_numeric_to_numeric(&mut self, cast_to: HlirType, expr: HlirExpr) -> HlirExpr {
        debug_assert!(cast_to.decl == HlirTypeDecl::Basic);
        debug_assert!(expr.ty.decl == HlirTypeDecl::Basic);
        if matches!(
            (&expr.ty.kind, &cast_to.kind),
            (HlirTypeKind::Double, HlirTypeKind::Float)
                | (HlirTypeKind::Int(_), HlirTypeKind::Float)
        ) {
            let ty = HlirType {
                kind: HlirTypeKind::Float,
                decl: HlirTypeDecl::Basic,
            };
            HlirExpr {
                kind: Box::new(HlirExprKind::Cast(ty.clone(), expr)),
                is_lval: false,
                ty,
            }
        } else {
            let expr_level = self.get_numeric_cast_hierarchy(&expr.ty.kind);
            let cast_level = self.get_numeric_cast_hierarchy(&cast_to.kind);
            let kind = match expr_level.cmp(&cast_level) {
                Ordering::Less => Some(self.demote_numeric(&cast_to.kind)),
                Ordering::Greater => Some(self.promote_numeric(&cast_to.kind)),
                Ordering::Equal => None,
            };
            if let Some(kind) = kind {
                let ty = HlirType {
                    decl: HlirTypeDecl::Basic,
                    kind,
                };
                let casted_lower = self.cast_numeric_to_numeric(ty, expr);
                HlirExpr {
                    kind: Box::new(HlirExprKind::Cast(cast_to.clone(), casted_lower)),
                    ty: cast_to,
                    is_lval: false,
                }
            } else {
                HlirExpr {
                    kind: Box::new(HlirExprKind::Cast(cast_to.clone(), expr)),
                    is_lval: false,
                    ty: cast_to,
                }
            }
        }
    }

    fn give_numeric_sign(&self, ty: HlirTypeKind, signed: bool) -> HlirTypeKind {
        match ty {
            HlirTypeKind::Long(_) => HlirTypeKind::Long(signed),
            HlirTypeKind::Int(_) => HlirTypeKind::Int(signed),
            HlirTypeKind::Char(_) => HlirTypeKind::Char(signed),
            _ => ty,
        }
    }

    fn get_numeric_signed(&self, ty: &HlirTypeKind) -> bool {
        match ty {
            HlirTypeKind::Long(signed) => *signed,
            HlirTypeKind::Int(signed) => *signed,
            HlirTypeKind::Char(signed) => *signed,
            _ => false,
        }
    }

    fn demote_numeric(&self, ty: &HlirTypeKind) -> HlirTypeKind {
        // does not include double -> float
        match ty {
            HlirTypeKind::Double => HlirTypeKind::Long(false),
            HlirTypeKind::Float => HlirTypeKind::Int(false),
            HlirTypeKind::Long(_) => HlirTypeKind::Int(false),
            HlirTypeKind::Int(_) => HlirTypeKind::Char(false),
            _ => panic!(), // panics as this should be guarded against before calling
        }
    }

    fn promote_numeric(&self, ty: &HlirTypeKind) -> HlirTypeKind {
        // does not include int -> float
        match ty {
            HlirTypeKind::Float => HlirTypeKind::Double,
            HlirTypeKind::Long(_) => HlirTypeKind::Double,
            HlirTypeKind::Int(_) => HlirTypeKind::Long(false),
            HlirTypeKind::Char(_) => HlirTypeKind::Int(false),
            _ => panic!(), // panics as this should be guarded against before calling
        }
    }

    fn get_numeric_cast_hierarchy(&self, ty: &HlirTypeKind) -> u8 {
        match ty {
            HlirTypeKind::Double => 4,
            HlirTypeKind::Long(_) => 3,
            HlirTypeKind::Int(_) => 2,
            HlirTypeKind::Char(_) => 1,
            _ => panic!(),
        }
    }

    fn validate_index_access(
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
            kind: Box::new(HlirExprKind::Index(left, index)),
            ty,
            is_lval: false,
        })
    }

    fn validate_pointer_member_access(
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

        if !body.is_pointer() {
            self.report_error(CompilerError::ArrowOnNonPointer(body_span));
        }

        // dereference to underlying type,
        let mut ty = body.ty.clone();
        ty.decl = HlirTypeDecl::Basic;

        Ok(HlirExpr {
            kind: Box::new(HlirExprKind::Deref(body)),
            ty,
            is_lval: true,
        })
    }

    fn validate_member_access(
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

        self.scope
            .validate_struct_member_access(body, member)
            .map_err(|err| {
                self.report_error(err);
            })
    }

    fn validate_unary_expression(
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
                    let expr = cast!(
                        expr,
                        HlirType {
                            decl: HlirTypeDecl::Basic,
                            kind: HlirTypeKind::Int(false)
                        }
                    );
                    let ty = expr.ty.clone();
                    let expr = match op {
                        UnaryOp::LogicalNot => HlirExprKind::LogicalNot(expr),
                        UnaryOp::BitwiseNot => HlirExprKind::BitwiseNot(expr),
                        _ => unreachable!(),
                    };
                    Ok(HlirExpr {
                        kind: Box::new(expr),
                        is_lval: false,
                        ty,
                    })
                }
            }
            UnaryOp::Deref => todo!(),
            UnaryOp::AddressOf => todo!(),
        }
    }

    fn not_incremental(&mut self, expr: &HlirExpr, span: Span) -> bool {
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

    fn validate_pre_inc_or_dec(
        &mut self,
        op: &UnaryOp,
        expr: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if self.not_incremental(&expr, span) {
            return Ok(expr);
        }

        let literal_one = HlirExpr {
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

    fn validate_post_inc_or_dec(
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
            is_lval: false,
            kind,
            ty,
        })
    }

    fn sizeof(&mut self, ty: &HlirType, span: Span) -> u64 {
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
                self.scope
                    .get_struct_size(ident, span)
                    .unwrap_or_else(|err| {
                        self.report_error(err);
                        0
                    })
            }
        };

        if let HlirTypeDecl::Array(array_size) = &ty.decl {
            size * array_size
        } else {
            size
        }
    }

    fn validate_binary_expression(
        &mut self,
        op: &BinaryOp,
        left: HlirExpr,
        right: HlirExpr,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        if left.is_array() || right.is_array() || left.is_string() || right.is_string() {
            let err = CompilerError::InvalidBinaryOperation(
                left.ty.to_string(),
                right.ty.to_string(),
                span,
            );
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
            return Ok(left);
        } else if left.ty.is_pointer() && right.ty.is_numeric() {
            // only addition/subtraction
            let left = self
                .implicit_cast(
                    HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                    left,
                )
                .unwrap();
            let right = self
                .implicit_cast(
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
            let left = self
                .implicit_cast(
                    HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
                    left,
                )
                .unwrap();
            let right = self
                .implicit_cast(
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
            return Ok(left);
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
            return Ok(left);
        }

        let left_ty = left.ty.clone();
        let left = self.implicit_cast(
            HlirType::new(HlirTypeKind::Long(true), HlirTypeDecl::Basic),
            left,
        );

        let right_ty = right.ty.clone();
        let right = self.implicit_cast(
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
        let ty = HlirType {
            kind: HlirTypeKind::Long(true),
            decl: HlirTypeDecl::Basic,
        };
        let (left, right) = (
            self.explicit_cast(ty.clone(), left).unwrap(),
            self.explicit_cast(ty.clone(), right).unwrap(),
        );
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

#[cfg(test)]
macro_rules! make_dec_specifier {
    ($types:expr) => {
        DeclarationSpecifier {
            specifiers: vec![],
            qualifiers: vec![],
            ty: $types,
            pointer: false,
        }
    };
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
        let types = make_dec_specifier!(types);
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
        let dec_spec = make_dec_specifier!(types);
        let expected = HlirType {
            decl: HlirTypeDecl::Basic,
            kind: expected,
        };
        let result = validator.validate_type(&dec_spec, Span::default());
        assert_eq!(result, Ok(expected));
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

#[cfg(test)]
fn test_cast_structure(expr: HlirExpr, cast_to: HlirType, order: &[HlirTypeKind]) {
    let mut validator = GlobalValidator::new(AbstractSyntaxTree::default());
    let cast_structure = validator.cast_numeric_to_numeric(cast_to, expr);

    let mut given = cast_structure;

    for kind in order {
        let given_ty = given.ty.clone();
        assert_eq!(given_ty.kind, *kind);
        given = match *given.kind {
            HlirExprKind::Cast(ty, expr) => {
                assert_eq!(ty.kind, *kind);
                expr
            }
            _ => panic!("Unexpected expr type, expected HlirExprKind::Cast."),
        }
    }
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_upcast() {
    let expr = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Char(1))),
        ty: HlirType {
            kind: HlirTypeKind::Char(false),
            decl: HlirTypeDecl::Basic,
        },
        is_lval: false,
    };
    let cast_to = HlirType {
        kind: HlirTypeKind::Double,
        decl: HlirTypeDecl::Basic,
    };

    let expected = [
        HlirTypeKind::Double,
        HlirTypeKind::Long(false),
        HlirTypeKind::Int(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}

#[test]
fn test_cast_numeric_to_numeric_creates_proper_cast_structure_for_downcast() {
    let expr = HlirExpr {
        kind: Box::new(HlirExprKind::Literal(HlirLiteral::Float(1.0))),
        ty: HlirType {
            kind: HlirTypeKind::Double,
            decl: HlirTypeDecl::Basic,
        },
        is_lval: false,
    };

    let cast_to = HlirType {
        kind: HlirTypeKind::Char(false),
        decl: HlirTypeDecl::Basic,
    };

    let expected = [
        HlirTypeKind::Char(false),
        HlirTypeKind::Int(false),
        HlirTypeKind::Long(false),
    ];

    test_cast_structure(expr, cast_to, &expected);
}
