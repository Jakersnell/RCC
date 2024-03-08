use std::fmt::Display;
use std::ops::Deref;

use crate::lexer::tokens::{Keyword, Literal, Symbol, Token};
use crate::util::str_intern::InternedStr;
use crate::util::Locatable;

pub type ASTRoot = Vec<InitDeclaration>;

pub struct AbstractSyntaxTree(Vec<InitDeclaration>);

impl Deref for AbstractSyntaxTree {
    type Target = Vec<InitDeclaration>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct Block(pub Vec<Locatable<Statement>>);

#[derive(Debug)]
pub enum InitDeclaration {
    Declaration(Locatable<VariableDeclaration>), // (declaration,  initializer)
    Function(Locatable<FunctionDeclaration>),
    Struct(Locatable<StructDeclaration>),
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub declaration: Locatable<Declaration>,
    pub members: Vec<Locatable<Declaration>>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub declaration: Locatable<Declaration>,
    pub parameters: Vec<Locatable<Declaration>>,
    pub body: Locatable<Block>, // don't support function prototypes
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub declaration: Locatable<Declaration>,
    pub is_array: bool,
    pub array_size: Option<usize>,
    pub initializer: Option<Locatable<Expression>>,
}

#[derive(Debug)]
pub struct Declaration {
    pub specifier: Locatable<DeclarationSpecifier>,
    pub ident: Option<Locatable<InternedStr>>,
}

#[derive(Debug)]
pub struct DeclarationSpecifier {
    pub specifiers: Vec<StorageSpecifier>,
    pub qualifiers: Vec<TypeQualifier>,
    pub ty: Vec<TypeSpecifier>,
    pub pointer: bool, // only supporting one pointer depth
}

#[derive(Debug)]
pub enum TypeSpecifier {
    Void,
    Char,
    Int,
    Long,
    Double,
    Signed,
    Unsigned,
    Struct(InternedStr),
}

impl TryFrom<&Token> for TypeSpecifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use TypeSpecifier::*;
        match value {
            Token::Keyword(Keyword::Int) => Ok(Int),
            Token::Keyword(Keyword::Double) => Ok(Double),
            Token::Keyword(Keyword::Void) => Ok(Void),
            Token::Keyword(Keyword::Char) => Ok(Char),
            Token::Keyword(Keyword::Long) => Ok(Long),
            Token::Keyword(Keyword::Signed) => Ok(Signed),
            Token::Keyword(Keyword::Unsigned) => Ok(Unsigned),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum StorageSpecifier {
    Static,
}

impl TryFrom<&Token> for StorageSpecifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use StorageSpecifier::*;
        match value {
            Token::Keyword(Keyword::Static) => Ok(Static),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
}

impl TryFrom<&Token> for TypeQualifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use TypeQualifier::*;
        match value {
            Token::Keyword(Keyword::Const) => Ok(Const),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Expression(Locatable<Expression>),
    Declaration(Locatable<VariableDeclaration>),
    If(
        Locatable<Expression>,
        Box<Locatable<Statement>>,
        Option<Box<Locatable<Statement>>>,
    ),
    While(Locatable<Expression>, Box<Locatable<Statement>>),
    For(
        Option<Locatable<VariableDeclaration>>,
        Option<Locatable<Expression>>,
        Option<Locatable<Expression>>,
        Box<Locatable<Statement>>,
    ),
    Break,
    Continue,
    Return(Option<Locatable<Expression>>),
    Block(Locatable<Block>),
    Empty, // this is for a semicolon by itself,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Locatable<Literal>),
    Variable(Locatable<InternedStr>),
    Sizeof(Locatable<TypeOrExpression>),
    Parenthesized(Locatable<Box<Expression>>),
    // this doesn't include all postfix operations, just inc and dec
    PostFix(PostfixOp, Locatable<Box<Expression>>),
    Unary(UnaryOp, Locatable<Box<Expression>>),
    Binary(
        BinaryOp,
        Locatable<Box<Expression>>,
        Locatable<Box<Expression>>,
    ),
    FunctionCall(Locatable<InternedStr>, Vec<Locatable<Expression>>),
    Index(Locatable<Box<Expression>>, Locatable<Box<Expression>>),
    Member(Locatable<Box<Expression>>, Locatable<InternedStr>),
    PointerMember(Locatable<Box<Expression>>, Locatable<InternedStr>),
    Cast(Locatable<Declaration>, Locatable<Box<Expression>>),
    ArrayInitializer(Vec<Locatable<Expression>>),
}

#[derive(Debug)]
pub enum TypeOrExpression {
    Type(Declaration),
    Expr(Box<Expression>),
}

#[derive(Debug)]
pub enum PostfixOp {
    Increment,
    Decrement,
}

impl TryFrom<&Token> for PostfixOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use PostfixOp::*;
        match value {
            Token::Symbol(Symbol::Increment) => Ok(Increment),
            Token::Symbol(Symbol::Decrement) => Ok(Decrement),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Increment,
    Decrement,
    Plus,
    Negate,
    LogicalNot,
    BitwiseNot,
    Deref,
    AddressOf,
}

impl TryFrom<&Token> for UnaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use UnaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Plus),
            Token::Symbol(Symbol::Minus) => Ok(Negate),
            Token::Symbol(Symbol::Bang) => Ok(LogicalNot),
            Token::Symbol(Symbol::Tilde) => Ok(BitwiseNot),
            Token::Symbol(Symbol::Increment) => Ok(Increment),
            Token::Symbol(Symbol::Decrement) => Ok(Decrement),
            Token::Symbol(Symbol::Star) => Ok(Deref),
            Token::Symbol(Symbol::Ampersand) => Ok(AddressOf),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    LeftShift,
    RightShift,

    Assign(AssignOp),
}

impl BinaryOp {
    pub fn precedence(&self) -> u8 {
        use BinaryOp::*;
        match self {
            Multiply | Divide | Modulo => 11,
            Add | Subtract => 10,
            LeftShift | RightShift => 9,
            GreaterThan | GreaterThanEqual | LessThan | LessThanEqual => 8,
            Equal | NotEqual => 7,
            BitwiseAnd => 6,
            BitwiseXor => 5,
            BitwiseOr => 4,
            LogicalAnd => 3,
            LogicalOr => 2,
            Assign(_) => 1,
        }
    }
}

impl TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use BinaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Add),
            Token::Symbol(Symbol::Minus) => Ok(Subtract),
            Token::Symbol(Symbol::Star) => Ok(Multiply),
            Token::Symbol(Symbol::Slash) => Ok(Divide),
            Token::Symbol(Symbol::Modulo) => Ok(Modulo),

            Token::Symbol(Symbol::EqualEqual) => Ok(Equal),
            Token::Symbol(Symbol::BangEqual) => Ok(NotEqual),
            Token::Symbol(Symbol::GreaterThan) => Ok(GreaterThan),
            Token::Symbol(Symbol::GreaterThanEqual) => Ok(GreaterThanEqual),
            Token::Symbol(Symbol::LessThan) => Ok(LessThan),
            Token::Symbol(Symbol::LessThanEqual) => Ok(LessThanEqual),

            Token::Symbol(Symbol::Ampersand) => Ok(BitwiseAnd),
            Token::Symbol(Symbol::Pipe) => Ok(BitwiseOr),
            Token::Symbol(Symbol::Caret) => Ok(BitwiseXor),
            Token::Symbol(Symbol::LeftShift) => Ok(LeftShift),
            Token::Symbol(Symbol::RightShift) => Ok(RightShift),

            Token::Symbol(Symbol::Equal) => Ok(Assign(AssignOp::Assign)),
            Token::Symbol(Symbol::PlusEqual) => Ok(Assign(AssignOp::Plus)),
            Token::Symbol(Symbol::MinusEqual) => Ok(Assign(AssignOp::Minus)),
            Token::Symbol(Symbol::StarEqual) => Ok(Assign(AssignOp::Multiply)),
            Token::Symbol(Symbol::SlashEqual) => Ok(Assign(AssignOp::Divide)),
            Token::Symbol(Symbol::ModuloEqual) => Ok(Assign(AssignOp::Modulo)),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(Assign(AssignOp::BitwiseAnd)),
            Token::Symbol(Symbol::PipeEqual) => Ok(Assign(AssignOp::BitwiseOr)),
            Token::Symbol(Symbol::CaretEqual) => Ok(Assign(AssignOp::BitwiseXor)),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(Assign(AssignOp::LeftShift)),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(Assign(AssignOp::RightShift)),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}
impl TryFrom<&Token> for AssignOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use AssignOp::*;
        match value {
            Token::Symbol(Symbol::Equal) => Ok(Assign),
            Token::Symbol(Symbol::PlusEqual) => Ok(Plus),
            Token::Symbol(Symbol::MinusEqual) => Ok(Minus),
            Token::Symbol(Symbol::StarEqual) => Ok(Multiply),
            Token::Symbol(Symbol::SlashEqual) => Ok(Divide),
            Token::Symbol(Symbol::ModuloEqual) => Ok(Modulo),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(BitwiseAnd),
            Token::Symbol(Symbol::PipeEqual) => Ok(BitwiseOr),
            Token::Symbol(Symbol::CaretEqual) => Ok(BitwiseXor),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(LeftShift),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(RightShift),
            _ => Err(()),
        }
    }
}
