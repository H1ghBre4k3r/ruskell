use lachs::Span;

use super::expression::{Expression, Ident, LambdaBody};

#[derive(Debug, Clone)]
pub enum Pattern<T> {
    Literal(LiteralPattern<T>),
    Ident(Ident<T>),
    Wildcard(Wildcard<T>),
}

#[derive(Debug, Clone)]
pub enum LiteralPattern<T> {
    Integer(i128, Span, T),
    String(String, Span, T),
    Boolean(bool, Span, T),
    Unit(Span, T),
}

#[derive(Debug, Clone)]
pub struct Wildcard<T> {
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct MatchArm<T> {
    pub pattern: Pattern<T>,
    pub body: Expression<T>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct Match<T> {
    pub scrutinee: Box<Expression<T>>,
    pub arms: Vec<MatchArm<T>>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone)]
pub struct FunctionClause<T> {
    pub name: Ident<T>,
    pub patterns: Vec<Pattern<T>>,
    pub body: LambdaBody<T>,
    pub position: Span,
    pub info: T,
}
