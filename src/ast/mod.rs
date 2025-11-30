use crate::ast::{expression::Ident, statement::Statement};

pub mod expression;
pub mod statement;

#[derive(Debug, Clone)]
pub struct Program<T> {
    pub main: Function<T>,
    pub functions: Vec<Function<T>>,
}

#[derive(Debug, Clone)]
pub struct Function<T> {
    pub name: Ident<T>,
    pub args: Vec<Ident<T>>,
    pub expression: Vec<Statement<T>>,
}
