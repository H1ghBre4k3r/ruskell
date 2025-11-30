use crate::ast::expression::{Ident, Lambda};

pub mod expression;
pub mod statement;

#[derive(Debug, Clone)]
pub struct Program<T> {
    pub main: Function<T>,
    pub functions: Vec<Function<T>>,
}

/// A named function, which is essentially a named lambda
#[derive(Debug, Clone)]
pub struct Function<T> {
    pub name: Ident<T>,
    pub lambda: Lambda<T>,
}
