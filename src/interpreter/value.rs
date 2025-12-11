use std::fmt::Debug;

use crate::ast::expression::{Integer, Lambda, StringLiteral};
use crate::core::CoreLambda;

/// Runtime value representation
#[derive(Debug, Clone)]
pub enum RValue<T> {
    Unit,
    Integer(Integer<T>),
    String(StringLiteral<T>),
    Lambda(Lambda<T>),
    CoreLambda(CoreLambda<T>),
}
