use crate::ast::{ASTNode, Declaration, Expression, Statement};
use crate::error::{CompilerError, CompilerWarning};
use crate::tokens::Token as LexToken;
use derive_new::new;

pub type LocatableToken = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<Locatable<CompilerError>>>;
pub type Node = Locatable<ASTNode>;
pub type StatementNode = Locatable<Statement>;
pub type ExpressionNode = Locatable<Expression>;
pub type DeclarationNode = Locatable<Declaration>;
pub type CompoundExpression = Locatable<Vec<ExpressionNode>>;
pub type CompoundStatement = Locatable<Vec<StatementNode>>;
pub type CompoundDeclaration = Locatable<Vec<DeclarationNode>>;

#[derive(Debug, PartialEq, new)]
pub struct Locatable<T> {
    pub location: Span,
    pub value: T,
}

#[derive(Debug, PartialEq, new)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub struct Program {
    // I plan on adding more fields to this struct later
    pub body: Option<CompilerResult<Vec<Node>>>,
    pub warnings: Vec<CompilerWarning>,
    pub file_name: String,
}

impl Program {
    pub fn new(file_name: String) -> Self {
        Self {
            body: None,
            warnings: Vec::new(),
            file_name,
        }
    }
}
