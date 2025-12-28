//! # Type Error Definitions
//!
//! This module defines error types for the type checking phase.
//! Errors can occur during type inference when:
//!
//! - A variable is used but not defined (`UnboundVariable`)
//! - Two types cannot be unified (`TypeMismatch`)
//! - A type would be infinite (`OccursCheck`)
//!
//! ## Error Reporting
//!
//! All errors include source span information for precise error messages.
//! The `Display` implementation produces human-readable error messages
//! with source location context when available.
//!
//! ## Related Modules
//!
//! - [`crate::types::infer`] - Type inference that produces these errors
//! - [`crate::types::unify`] - Unification errors converted to `TypeError`
//! - [`crate::types::validate`] - Validation wraps these errors

use std::fmt;

use lachs::Span;

use super::ty::{Type, TypeVar};
use super::unify::UnifyError;

/// Type error encountered during type inference.
///
/// These errors represent failures in the type checking phase,
/// such as undefined variables, type mismatches, and infinite types.
///
/// # Variants
///
/// * `UnboundVariable` - Reference to an undefined variable
/// * `TypeMismatch` - Two types that should be equal are not
/// * `OccursCheck` - A type variable would need to contain itself
///
/// # Example Error Messages
///
/// ```text
/// Type error: unbound variable: x
///   --> file.rsk:10:5
///
/// Type error: type mismatch: expected Int, found String
///   --> file.rsk:15:10
///
/// Type error: cannot construct infinite type: 't0 = 't0 -> Int
///   --> file.rsk:20:3
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// Reference to an undefined variable.
    ///
    /// Occurs when an identifier is used that hasn't been bound
    /// in the current scope or any enclosing scope.
    UnboundVariable {
        /// The undefined variable name
        name: String,
        /// Source location of the reference
        span: Span,
    },

    /// Type mismatch during unification.
    ///
    /// Occurs when two types are required to be equal but cannot
    /// be unified, such as `Int` and `String`.
    TypeMismatch {
        /// The type that was expected
        expected: Type,
        /// The type that was actually found
        found: Type,
        /// Source location where the mismatch occurred
        span: Span,
        /// Optional additional context for the error
        context: Option<String>,
    },

    /// Occurs check failure (infinite type).
    ///
    /// Occurs when unification would create an infinite type,
    /// such as `'t0 = 't0 -> Int`. This is prevented by the
    /// occurs check in unification.
    OccursCheck {
        /// The type variable that would be infinite
        var: TypeVar,
        /// The type that contains the variable
        ty: Type,
        /// Source location
        span: Span,
    },
}

impl TypeError {
    /// Create an unbound variable error.
    ///
    /// # Arguments
    ///
    /// * `name` - The undefined variable name
    /// * `span` - Source location of the reference
    pub fn unbound_variable(name: String, span: Span) -> Self {
        TypeError::UnboundVariable { name, span }
    }

    /// Create a type mismatch error.
    ///
    /// # Arguments
    ///
    /// * `expected` - The type that was expected
    /// * `found` - The type that was actually found
    /// * `span` - Source location where the mismatch occurred
    pub fn type_mismatch(expected: Type, found: Type, span: Span) -> Self {
        TypeError::TypeMismatch {
            expected,
            found,
            span,
            context: None,
        }
    }

    /// Add context to this error (only affects `TypeMismatch`).
    ///
    /// # Arguments
    ///
    /// * `context` - Additional context message
    ///
    /// # Returns
    ///
    /// The error with context attached (chainable)
    pub fn with_context(mut self, context: String) -> Self {
        if let TypeError::TypeMismatch { context: ctx, .. } = &mut self {
            *ctx = Some(context);
        }
        self
    }

    /// Create an occurs check error.
    ///
    /// # Arguments
    ///
    /// * `var` - The type variable that would be infinite
    /// * `ty` - The type that contains the variable
    /// * `span` - Source location
    pub fn occurs_check(var: TypeVar, ty: Type, span: Span) -> Self {
        TypeError::OccursCheck { var, ty, span }
    }

    /// Convert a unification error to a type error.
    ///
    /// Unification errors from the `unify` module are converted
    /// to `TypeError` with the provided source span.
    ///
    /// # Arguments
    ///
    /// * `err` - The unification error
    /// * `span` - Source location to attach
    pub fn from_unify_error(err: UnifyError, span: Span) -> Self {
        match err {
            UnifyError::Mismatch { expected, found } => {
                TypeError::type_mismatch(expected, found, span)
            }
            UnifyError::OccursCheck { var, ty } => TypeError::occurs_check(var, ty, span),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::UnboundVariable { name, span } => {
                let msg = format!("unbound variable: {}", name);
                if span.source.is_empty() {
                    write!(f, "Type error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(&msg))
                }
            }
            TypeError::TypeMismatch {
                expected,
                found,
                span,
                context,
            } => {
                let msg = format!(
                    "type mismatch: expected {}, found {}",
                    expected.pretty(),
                    found.pretty()
                );
                let full_msg = if let Some(ctx) = context {
                    format!("{}\n  Note: {}", msg, ctx)
                } else {
                    msg
                };
                if span.source.is_empty() {
                    write!(f, "Type error: {}", full_msg)
                } else {
                    write!(f, "{}", span.to_string(&full_msg))
                }
            }
            TypeError::OccursCheck { var, ty, span } => {
                let msg = format!(
                    "cannot construct infinite type: {} = {}",
                    Type::Var(var.clone()).pretty(),
                    ty.pretty()
                );
                if span.source.is_empty() {
                    write!(f, "Type error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(&msg))
                }
            }
        }
    }
}

impl std::error::Error for TypeError {}
