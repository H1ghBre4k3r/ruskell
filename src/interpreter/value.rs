use std::collections::HashMap;
use std::fmt::Debug;

use crate::ast::expression::{Integer, Lambda, StringLiteral};
use crate::core::CoreLambda;

/// A captured environment for closures
#[derive(Debug, Clone)]
pub struct CapturedEnv<T>(pub HashMap<String, RValue<T>>);

/// Builtin function identifiers
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Print,
    ToString,
    ListIsEmpty,
    ListHead,
    ListTail,
    ListCons,
}

/// Runtime value representation
#[derive(Debug, Clone)]
pub enum RValue<T> {
    Unit,
    Integer(Integer<T>),
    String(StringLiteral<T>),
    Bool(bool),
    List(Vec<RValue<T>>),
    Lambda(Lambda<T>),
    /// Core lambda with captured environment (closure)
    CoreLambda(CoreLambda<T>, CapturedEnv<T>),
    /// Builtin function
    Builtin(Builtin),
}
