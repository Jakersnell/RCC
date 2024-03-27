use crate::analysis::mlir::MlirType;
use crate::parser::ast::{StorageSpecifier, TypeQualifier, TypeSpecifier};
use crate::util::str_intern::InternedStr;
use crate::util::Span;
use thiserror::Error;

#[derive(Debug)]
pub struct Reporter {
    pub errors: Vec<CompilerError>,
    warnings: Vec<CompilerWarning>,
}
impl Default for Reporter {
    fn default() -> Self {
        Self::new()
    }
}
impl Reporter {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn report_error(&mut self, error: CompilerError) {
        if cfg!(not(test)) {
            println!("Error: {}", error);
        }
        self.errors.push(error);
    }

    pub fn report_warning(&mut self, warning: CompilerWarning) {
        if cfg!(not(test)) {
            println!("Warning: {}", warning);
        }
        self.warnings.push(warning);
    }

    pub fn status(&self) -> Result<(), ()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(())
        }
    }
}
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("{0}")]
    IoError(#[from] std::io::Error),

    #[error("Could not find fn 'main', no entry point!")]
    MissingMain,

    #[error("Invalid integer literal: {0}")]
    ParseIntError(Span),

    #[error("Invalid float literal: {0}")]
    ParseFloatError(Span),

    #[error("Invalid integer suffix: {0}")]
    InvalidIntegerSuffix(String, Span),

    #[error("Invalid float suffix: {0}")]
    InvalidFloatSuffix(String, Span),

    #[error("Invalid symbol: {0}")]
    InvalidSymbol(String, Span),

    #[error("Found `{0}` but expected one of the following: \n\t{1}\n")]
    ExpectedVariety(String, String, Span),

    #[error("Expected `{0}` but found {1}")]
    ExpectedButFound(String, String, Span),

    #[error("Invalid hex literal: {0}")]
    InvalidHexLiteral(Span),

    #[error("Invalid octal literal: {0}")]
    InvalidOctalLiteral(Span),

    #[error("Invalid binary literal: {0}")]
    InvalidBinaryLiteral(Span),

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(Span),

    #[error("Invalid character literal: {0}")]
    InvalidCharacterLiteral(Span),

    #[error("Unclosed string literal: {0}")]
    UnclosedStringLiteral(Span),

    #[error("Unclosed char literal: {0}")]
    UnclosedCharLiteral(Span),

    #[error("Cannot cast '{0}' to '{1}': {2}")]
    CannotCast(String, String, Span),

    #[error("Cannot assign '{1}' to lval with type of '{0}': {2}")]
    CannotAssign(String, String, Span),

    #[error("Unknown identifier \"{0}\"")]
    UnknownIdentifier(String, Span),

    #[error("Must return type {0} due to declared type")]
    MustReturn(String, Span),

    #[error("Unclosed parenthesis")]
    UnclosedParenthesis,

    #[error("Unclosed block")]
    UnclosedBlock,

    #[error("Unclosed array")]
    UnclosedArray,

    #[error("Parenthesis has no opening.")]
    ParenthesisHasNoOpening,

    #[error("Curly has no opening.")]
    BlockHasNoOpening,

    #[error("Unexpected end of file.")]
    UnexpectedEOF,

    #[error("This identifier already exists in this scope and cannot be redeclared: {0}")]
    IdentifierExists(Span),

    #[error("The identifier '{0}' cannot be found in the current scope: {1}")]
    IdentNotFound(InternedStr, Span),

    #[error("{0}")]
    CustomError(String, Span),

    #[error("Else without if")]
    ElseWithNoIf(Span),

    #[error("The arguments to this function are of incorrect types.")]
    FunctionTypeMismatch(Span),

    #[error("This is not a function.")]
    NotAFunction(Span),

    #[error("This is not a variable.")]
    NotAVariable(Span),

    #[error("Variable type mismatch. Cannot assign {0} to type {1}")]
    VariableTypeMismatch(Span, String, String),

    #[error("Declaration is missing identifier: {0}")]
    DeclarationMissingIdentifier(Span),

    #[error("Type '{0}' can not be signed or unsigned: {1}")]
    TypeCannotBeSignedOrUnsigned(String, Span),

    #[error("Cannot combine signed and unsigned: {0}")]
    CannotCombineSignedAndUnsigned(Span),

    #[error("Expected a full type specifier here: {0}")]
    ExpectedTypeSpecifier(Span),

    #[error("Array needs a size: {0}")]
    ArraySizeNotSpecified(Span),

    #[error("Invalid array operation: {0}")]
    InvalidArrayOperation(Span),

    #[error("Invalid binary operation '{0}' between '{1}' and '{2}': {3}")]
    InvalidBinaryOperation(String, String, String, Span),

    #[error("Left hand operand is not assignable: {0}")]
    LeftHandNotLVal(Span),

    #[error("Invalid type specifier: {0}")]
    InvalidTypeSpecifier(Span),

    #[error("Type specifier '{0}' is invalid in this position: {1}")]
    InvalidTypeSpecifierOrder(String, Span),

    #[error("Not a struct: {0}")]
    NotAStruct(Span),

    #[error("Ident '{0}' is not a member of the struct definition for 'struct {1}': {2}")]
    MemberNotFound(String, String, Span),

    #[error("Cannot assign to a const variable: {0}")]
    ConstAssignment(Span),

    #[error("Number to large to be represented with any type: {0}")]
    NumberTooLarge(Span),

    #[error("Cannot increment the type `{0}`: {1}")]
    CannotIncrementType(String, Span),

    #[error("Cannot negate a non-numeric type: {0}")]
    NonNumericNegation(Span),

    #[error("Cannot perform a bitwise operation on `{0}`: {1}")]
    CannotBitwise(String, Span),

    #[error("Cannot perform a logical operation on this type '{0}': {1}")]
    NotLogicalType(String, Span),

    #[error("Type `{0}` on left side of subscript cannot be indexed: {1}")]
    InvalidLeftOfSubScript(String, Span),

    #[error("Cannot index with non integer type `{0}`: {1}")]
    CannotIndexWith(String, Span),

    #[error("Cannot use '.' operator on type `{0}`: {1}")]
    CannotMemberAccessOnType(String, Span),

    #[error("Left hand side is pointer, did you mean to use '->'? : {0}")]
    DotOperatorOnPointer(Span),

    #[error("Left hand side is not a pointer, did you mean to use '.'? : {0}")]
    ArrowOnNonPointer(Span),

    #[error("Incomplete type: {0}")]
    IncompleteType(Span),

    #[error("Cannot perform equivalence operation between '{0}' and '{1}': {2}")]
    CannotEq(String, String, Span),

    #[error("Function requires an identifier: {0}")]
    FunctionRequiresIdentifier(Span),

    #[error("Function cannot have storage specifiers: {0}")]
    FunctionStorageSpecifiers(Span),

    #[error("Parameter requires identifier: {0}")]
    ParamRequiresIdent(Span),

    #[error("Parameter cannot have storage specifiers: {0}")]
    ParamStorageSpecifiers(Span),

    #[error("Cannot use '->' on type '{0}': {1}")]
    CannotPointerMemberAccess(String, Span),

    #[error("Argument type '{0}' does not match function argument type '{1}': {2}")]
    ArgumentTypeMismatch(String, String, Span),

    #[error("A member with the identifier '{0}' already exists in this scope: {1}")]
    MemberAlreadyExists(InternedStr, Span),

    #[error("Expected '{{' but found '*': {0}")]
    StructDeclarationPointer(Span),

    #[error("Struct definitions cannot be given declaration qualifiers: {0}")]
    StructDeclarationQualifiers(Span),

    #[error("Struct definitions cannot possess storage specifiers: {0}")]
    StructStorageSpecifiers(Span),

    #[error("Struct must be given an identifier: {0}")]
    StructMissingIdent(Span),

    #[error("The identifier 'main' is reserved as a function only: {0}")]
    MainIsReserved(Span),

    #[error("Cannot deref type '{0}' as it is not a pointer: {1}")]
    DerefOnNonPointer(String, Span),

    #[error("Cannot take an address of a pointer, not in this language, yet: {0}")]
    AttemptedAddressOfPointer(Span),

    #[error("Function must return type '{0}', cannot return '{1}': {0}")]
    InvalidReturnType(String, String, Span),

    #[error("Keyword 'continue' has no corresponding loop: {0}")]
    ContinueWithoutLoop(Span),

    #[error("Keyword 'break' has no corresponding loop: {0}")]
    BreakWithoutLoop(Span),
}

#[derive(Error, Debug)]
pub enum CompilerWarning {
    #[error("This expression has no effect: {0}")]
    ExprNoEffect(Span),

    #[error("Suffixes are currently ignored: {0}")]
    SuffixIgnored(Span),

    #[error("Unused variable: {0}")]
    UnusedVariable(Span),

    #[error("Unused function: {0}")]
    UnusedFunction(Span),

    #[error("Unused parameter: {0}")]
    UnusedParameter(Span),

    #[error("Unused constant: {0}")]
    UnusedConstant(Span),

    #[error("Unused struct: {0}")]
    UnusedStruct(Span),

    #[error("Unreachable code: {0}")]
    UnreachableCode(Span),

    #[error("Variable is not initialized at this point: {0}")]
    UninitializedVariable(Span),

    #[error("This storage specifier '{0}' is currently not supported: {1}")]
    UnsupportedStorageSpecifier(String, Span),

    #[error("This type qualifier '{0}' is currently not supported: {1}")]
    UnsupportedTypeQualifier(String, Span),

    #[error("Redundant usage of qualifier '{0}: {1}'")]
    RedundantUsage(String, Span),
}
