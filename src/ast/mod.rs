use crate::ast::expression::{Ident, Lambda};

pub mod expression;
pub mod pattern;
pub mod statement;

use pattern::FunctionClause;

#[derive(Debug, Clone)]
pub struct Program<T> {
    pub main: FunctionDef<T>,
    pub functions: Vec<FunctionDef<T>>,
}

/// A named function, which is essentially a named lambda
#[derive(Debug, Clone)]
pub struct Function<T> {
    pub name: Ident<T>,
    pub lambda: Lambda<T>,
}

#[derive(Debug, Clone)]
pub enum FunctionDef<T> {
    Single(Function<T>),
    Multi {
        name: Ident<T>,
        clauses: Vec<FunctionClause<T>>,
    },
}
