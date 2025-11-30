use lachs::Span;

use crate::ast::expression::{Expression, Ident};

#[derive(Debug, Clone)]
pub enum Statement<T> {
    Assignment(Assignment<T>),
    Expression(Expression<T>),
}

#[derive(Debug, Clone)]
pub struct Assignment<T> {
    pub name: Ident<T>,
    pub value: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}
