//! # Statement AST Nodes
//!
//! This module defines all statement types in Surface AST.
//! Statements are used in do-blocks and lambda bodies where
//! sequential execution is needed.
//!
//! ## Overview
//!
//! The `Statement` enum represents two possible statement forms:
//!
//! - **Assignment**: `x := value` - Bind a value to a name
//! - **Expression**: Any expression that's evaluated for its effect
//!
//! ## Statements vs Expressions
//!
//! Ruskell distinguishes between expressions and statements:
//!
//! - **Expressions**: Always return a value (e.g., `x + 1`, `42`)
//! - **Statements**: Execute for side effects (e.g., `x := 42`, `print(x)`)
//!
//! In do-blocks, statements are executed sequentially:
//!
//! ```text
//! do
//!     x := 1          // Assignment statement
//!     y := x + 2      // Assignment statement
//!     print(y)         // Expression statement (evaluated for effect)
//! end
//! ```
//!
//! ## Semantics
//!
//! ### Assignment Statements
//!
//! Assignments introduce (or shadow) variables in the current scope:
//!
//! ```text
//! x := 42      // Introduce x = 42
//! x := 100     // Shadow x with x = 100
//! // Inner scope sees x = 100
//! // Outer scope still sees x = 42
//! ```
//!
//! ### Expression Statements
//!
//! Expression statements evaluate an expression and discard its result.
//! Typically used for side effects (e.g., function calls that print):
//!
//! ```text
//! print(42)     // Call print, ignore unit return value
//! ```
//!
//! ## Pipeline Position
//!
//! ```text
//! Lexer → Token Stream → Parser → [STATEMENT AST] → Desugaring → Core AST
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::ast::expression`] - Expression types (statements contain expressions)
//! - [`crate::ast::pattern`] - Pattern types (unused in statements)
//! - [`crate::core`] - Core statement types (after desugaring)

use lachs::Span;

use crate::ast::expression::{Expression, Ident};

/// All possible statement forms in Ruskell.
///
/// Statements are used in do-blocks and sequential contexts.
/// Unlike expressions, statements may not return meaningful values.
///
/// # Variants
///
/// * `Assignment` - Variable assignment (`x := value`)
/// * `Expression` - Any expression evaluated for effect (`print(x)`)
///
/// # Examples
///
/// ```text
/// // Assignment:
/// // x := 42
/// Statement::Assignment(Assignment {
///     name: Ident("x"),
///     value: Box::new(Integer(42)),
///     ...
/// })
///
/// // Expression statement:
/// // print(42)
/// Statement::Expression(FunctionCall {
///     func: Ident("print"),
///     args: [Integer(42)],
///     ...
/// })
/// ```
#[derive(Debug, Clone)]
pub enum Statement<T> {
    Assignment(Assignment<T>),
    Expression(Expression<T>),
}

/// Variable assignment statement.
///
/// Assignments bind a value to a name in the current scope.
/// Assignments in inner scopes shadow variables from outer scopes.
///
/// # Fields
///
/// * `name` - Identifier to bind the value to
/// * `value` - Expression to evaluate and assign
/// * `position` - Source location
/// * `info` - Metadata (currently unused)
///
/// # Semantics
///
/// When an assignment is executed:
///
/// 1. Evaluate the `value` expression
/// 2. Bind the resulting value to `name` in the current scope
/// 3. If `name` already exists in the scope, it's shadowed (not modified)
///
/// # Shadowing vs Mutation
///
/// Ruskell uses lexical scoping with shadowing:
///
/// ```text
/// do
///     x := 1        // Outer x = 1
///     do
///         x := 2    // Inner x = 2 (shadows outer)
///         print(x)   // Prints 2
///     end
///     print(x)       // Prints 1 (outer x unchanged)
/// end
/// ```
///
/// # Examples
///
/// ```text
/// // Simple assignment:
/// // x := 42
/// Assignment {
///     name: Ident("x"),
///     value: Box::new(Integer(42)),
///     position: ...,
///     info: (),
/// }
///
/// // Assignment with complex expression:
/// // result := (a + b) * 2
/// Assignment {
///     name: Ident("result"),
///     value: Box::new(BinaryOp {
///         op: Mul,
///         left: Box::new(BinaryOp { op: Add, left: "a", right: "b" }),
///         right: Box::new(Integer(2)),
///     }),
///     position: ...,
///     info: (),
/// }
///
/// // Assignment with function call:
/// // result := factorial(5)
/// Assignment {
///     name: Ident("result"),
///     value: Box::new(FunctionCall {
///         func: Ident("factorial"),
///         args: [Integer(5)],
///     }),
///     position: ...,
///     info: (),
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Assignment<T> {
    pub name: Ident<T>,
    pub value: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}
