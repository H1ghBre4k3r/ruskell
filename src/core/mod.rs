//! Core AST - Simplified AST after desugaring
//!
//! This module contains a simplified AST where:
//! - All lambdas have exactly one parameter (multi-param lambdas are desugared to nested)
//! - Function calls are always single-argument (multi-arg calls become nested calls)
//! - The structure is simpler for type checking

use lachs::Span;

use crate::ast::expression::{BinOpKind, UnaryOpKind};

/// Core expression - simplified for type checking
#[derive(Debug, Clone, PartialEq)]
pub enum CoreExpr<T> {
    Unit(CoreUnit<T>),
    Ident(CoreIdent<T>),
    Integer(CoreInteger<T>),
    String(CoreString<T>),
    Boolean(CoreBoolean<T>),
    Lambda(CoreLambda<T>),
    FunctionCall(CoreFunctionCall<T>),
    BinaryOp(CoreBinaryOp<T>),
    UnaryOp(CoreUnaryOp<T>),
    IfThenElse(CoreIfThenElse<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreUnit<T> {
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreIdent<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreInteger<T> {
    pub value: i128,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreString<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreBoolean<T> {
    pub value: bool,
    pub position: Span,
    pub info: T,
}

/// Lambda with exactly one parameter (multi-param lambdas are desugared)
#[derive(Debug, Clone, PartialEq)]
pub struct CoreLambda<T> {
    pub param: CoreLambdaParam<T>,
    pub body: CoreLambdaBody<T>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CoreLambdaParam<T> {
    Unit(CoreUnit<T>),
    Ident(CoreIdent<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CoreLambdaBody<T> {
    Expression(Box<CoreExpr<T>>),
    Block(Vec<CoreStatement<T>>),
}

/// Function call with exactly one argument (multi-arg calls are desugared)
#[derive(Debug, Clone, PartialEq)]
pub struct CoreFunctionCall<T> {
    pub func: Box<CoreExpr<T>>,
    pub arg: Box<CoreExpr<T>>,
    pub position: Span,
    pub info: T,
}

/// Binary operation
#[derive(Debug, Clone, PartialEq)]
pub struct CoreBinaryOp<T> {
    pub op: BinOpKind,
    pub left: Box<CoreExpr<T>>,
    pub right: Box<CoreExpr<T>>,
    pub position: Span,
    pub info: T,
}

/// Unary operation
#[derive(Debug, Clone, PartialEq)]
pub struct CoreUnaryOp<T> {
    pub op: UnaryOpKind,
    pub operand: Box<CoreExpr<T>>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreIfThenElse<T> {
    pub condition: Box<CoreExpr<T>>,
    pub then_expr: Box<CoreExpr<T>>,
    pub else_expr: Box<CoreExpr<T>>,
    pub position: Span,
    pub info: T,
}

impl<T> CoreExpr<T> {
    pub fn position(&self) -> Span {
        match self {
            CoreExpr::Unit(u) => u.position.clone(),
            CoreExpr::Ident(i) => i.position.clone(),
            CoreExpr::Integer(i) => i.position.clone(),
            CoreExpr::String(s) => s.position.clone(),
            CoreExpr::Boolean(b) => b.position.clone(),
            CoreExpr::Lambda(l) => l.position.clone(),
            CoreExpr::FunctionCall(f) => f.position.clone(),
            CoreExpr::BinaryOp(b) => b.position.clone(),
            CoreExpr::UnaryOp(u) => u.position.clone(),
            CoreExpr::IfThenElse(i) => i.position.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CoreStatement<T> {
    Assignment(CoreAssignment<T>),
    Expression(CoreExpr<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreAssignment<T> {
    pub name: CoreIdent<T>,
    pub value: Box<CoreExpr<T>>,
    pub position: Span,
    pub info: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreFunction<T> {
    pub name: CoreIdent<T>,
    pub lambda: CoreLambda<T>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoreProgram<T> {
    pub main: CoreFunction<T>,
    pub functions: Vec<CoreFunction<T>>,
}
