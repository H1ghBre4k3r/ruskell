use lachs::Span;

use super::statement::Statement;

#[derive(Debug, Clone)]
pub enum Expression<T> {
    Unit(Unit<T>),
    Ident(Ident<T>),
    Integer(Integer<T>),
    String(StringLiteral<T>),
    Boolean(Boolean<T>),
    FunctionCall(FunctionCall<T>),
    Lambda(Lambda<T>),
    BinaryOp(BinaryOp<T>),
    UnaryOp(UnaryOp<T>),
    IfThenElse(IfThenElse<T>),
    Match(Box<super::pattern::Match<T>>),
}

#[derive(Debug, Clone)]
pub struct Unit<T> {
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub enum LambdaBody<T> {
    Expression(Box<Expression<T>>),
    Block(Vec<Statement<T>>),
}

#[derive(Debug, Clone)]
pub struct Ident<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct Integer<T> {
    pub value: i128,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct StringLiteral<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct Boolean<T> {
    pub value: bool,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct FunctionCall<T> {
    pub func: Box<Expression<T>>,
    pub args: Vec<Expression<T>>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct Lambda<T> {
    pub params: Vec<LambdaParam<T>>,
    pub body: LambdaBody<T>,
    pub position: Span,
    pub info: T,
}

/// Lambda parameter - either a named identifier or unit pattern
#[derive(Debug, Clone)]
pub enum LambdaParam<T> {
    Ident(Ident<T>),
    Unit(Unit<T>),
}

/// Binary operator kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

/// Unary operator kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
    Not,
}

/// Binary operation expression
#[derive(Debug, Clone)]
pub struct BinaryOp<T> {
    pub op: BinOpKind,
    pub left: Box<Expression<T>>,
    pub right: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}

/// Unary operation expression
#[derive(Debug, Clone)]
pub struct UnaryOp<T> {
    pub op: UnaryOpKind,
    pub operand: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}

/// Conditional expression (if-then-else)
#[derive(Debug, Clone)]
pub struct IfThenElse<T> {
    pub condition: Box<Expression<T>>,
    pub then_expr: Box<Expression<T>>,
    pub else_expr: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}
