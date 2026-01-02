//! # Pattern Matching AST Nodes
//!
//! This module defines all pattern matching types in the Surface AST.
//! Patterns are used in:
//!
//! - **Match expressions**: `match x of ...`
//! - **Multi-clause functions**: Pattern-based function definitions
//!
//! ## Overview
//!
//! The pattern matching system in Ruskell supports:
//!
//! - **Literal patterns**: Match specific values (`42`, `"hello"`, `true`, `()`)
//! - **Identifier patterns**: Bind variables to matched values (`x`, `name`)
//! - **Wildcard patterns**: Match any value and ignore it (`_`)
//!
//! ## Pattern Semantics
//!
//! Patterns are matched against a "scrutinee" value in order:
//!
//! ```text
//! match x of
//!     42 => "answer"
//!     n  => n * 2
//!     _  => "other"
//! end
//!
//! // If x is 42, returns "answer"
//! // If x is any other number, returns "x * 2"
//! // If x is not a number, returns "other"
//! ```
//!
//! ## Multi-Clause Functions
//!
//! Pattern matching enables defining functions with multiple clauses:
//!
//! ```text
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//!
//! // This becomes:
//! FunctionDef::Multi {
//!     name: "factorial",
//!     clauses: [
//!         FunctionClause { patterns: [0], body: "1" },
//!         FunctionClause { patterns: ["n"], body: "n * factorial(n - 1)" },
//!     ],
//! }
//! ```
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → [PATTERNS] → Desugaring → Core AST (if-then-else chains)
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::ast::expression`] - Expression types (patterns are used in match)
//! - [`crate::ast`] - Top-level AST that includes function definitions
//! - [`crate::desugar`] - Desugars patterns to if-then-else chains

use lachs::Span;

use super::expression::{Expression, Ident, LambdaBody};

/// All possible pattern forms in Ruskell.
///
/// Patterns are used to match against values and can bind
/// variables in the process.
///
/// # Variants
///
/// * `Literal` - Match a specific literal value (e.g., `42`, `"hello"`)
/// * `Ident` - Bind a variable to the matched value (e.g., `x`, `name`)
/// * `Wildcard` - Match any value without binding (e.g., `_`)
/// * `ListCons` - Match non-empty list with head and tail (e.g., `[x | xs]`)
///
/// # Examples
///
/// ```text
/// // Match specific number:
/// // match x of 42 => ...
/// Pattern::Literal(LiteralPattern::Integer(42, ...))
///
/// // Bind variable:
/// // match x of n => ...
/// Pattern::Ident(Ident { value: "n", ... })
///
/// // Wildcard (match anything):
/// // match x of _ => ...
/// Pattern::Wildcard(Wildcard { ... })
///
/// // List cons pattern:
/// // match xs of [x | rest] => ...
/// Pattern::ListCons(ListConsPattern {
///     head: Box::new(Pattern::Ident("x")),
///     tail: Box::new(Pattern::Ident("rest")),
///     ...
/// })
/// ```
#[derive(Debug, Clone)]
pub enum Pattern<T> {
    Literal(LiteralPattern<T>),
    Ident(Ident<T>),
    Wildcard(Wildcard<T>),
    ListCons(ListConsPattern<T>),
}

impl<T> Pattern<T> {
    /// Get the source position of this pattern
    pub fn position(&self) -> &Span {
        match self {
            Pattern::Literal(lit) => lit.position(),
            Pattern::Ident(id) => &id.position,
            Pattern::Wildcard(w) => &w.position,
            Pattern::ListCons(lc) => &lc.position,
        }
    }
}

/// Literal pattern - matches a specific value.
///
/// Literal patterns match against concrete values of various types.
/// Only successful if the scrutinee exactly equals the literal value.
///
/// # Variants
///
/// * `Integer` - Match a specific integer (e.g., `0`, `42`)
/// * `String` - Match a specific string (e.g., `"hello"`)
/// * `Boolean` - Match `true` or `false`
/// * `Unit` - Match the unit value `()`
/// * `EmptyList` - Match the empty list `[]`
///
/// # Examples
///
/// ```text
/// // match x of
/// //     0     => "zero"
/// //     1     => "one"
/// //     "yes" => "affirmative"
/// //     true   => "correct"
/// //     ()     => "nothing"
/// //     []    => "empty"
/// //     _     => "other"
/// // end
/// LiteralPattern::Integer(0, ...)     // Matches 0
/// LiteralPattern::Integer(1, ...)     // Matches 1
/// LiteralPattern::String("yes", ...)   // Matches "yes"
/// LiteralPattern::Boolean(true, ...)  // Matches true
/// LiteralPattern::Unit(...)            // Matches ()
/// LiteralPattern::EmptyList(...)       // Matches []
/// ```
#[derive(Debug, Clone)]
pub enum LiteralPattern<T> {
    Integer(i128, Span, T),
    String(String, Span, T),
    Boolean(bool, Span, T),
    Unit(Span, T),
    EmptyList(Span, T),
}

impl<T> LiteralPattern<T> {
    /// Get the source position of this literal pattern
    pub fn position(&self) -> &Span {
        match self {
            LiteralPattern::Integer(_, pos, _) => pos,
            LiteralPattern::String(_, pos, _) => pos,
            LiteralPattern::Boolean(_, pos, _) => pos,
            LiteralPattern::Unit(pos, _) => pos,
            LiteralPattern::EmptyList(pos, _) => pos,
        }
    }
}

/// Wildcard pattern.
///
/// Wildcard pattern (`_`) matches any value without binding it
/// to a variable. Typically used as final "catch-all" case
/// in pattern matching.
///
/// # Fields
///
/// * `position` - Source location of the underscore
/// * `info` - Metadata (currently unused)
///
/// # Example
///
/// ```text
/// // match x of
/// //     42 => "answer"
/// //     _  => "other"      // Wildcard catches everything else
/// // end
/// Wildcard { position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct Wildcard<T> {
    pub position: Span,
    pub info: T,
}

/// List cons pattern.
///
/// Matches non-empty lists by destructuring into head (first element)
/// and tail (rest of the list). Similar to Haskell's `(x:xs)` pattern.
///
/// # Fields
///
/// * `head` - Pattern to match the first element
/// * `tail` - Pattern to match the rest of the list
/// * `position` - Source location
/// * `info` - Metadata (currently unused)
///
/// # Examples
///
/// ```text
/// // match xs of
/// //     [x | rest] => ...    // Bind first element to x, rest to rest
/// //     [h | _]    => ...    // Bind first element to h, ignore rest
/// //     [_ | t]    => ...    // Ignore first element, bind rest to t
/// // end
///
/// ListConsPattern {
///     head: Box::new(Pattern::Ident(Ident("x"))),
///     tail: Box::new(Pattern::Ident(Ident("rest"))),
///     position: ...,
///     info: (),
/// }
/// ```
///
/// # Semantics
///
/// The cons pattern `[h | t]` matches a list if and only if:
/// 1. The list is non-empty (has at least one element)
/// 2. The head pattern `h` matches the first element
/// 3. The tail pattern `t` matches the rest of the list
///
/// # Example: Sum Function
///
/// ```text
/// // sum []      = 0
/// // sum [x|xs]  = x + sum(xs)
///
/// FunctionClause {
///     patterns: [
///         ListConsPattern {
///             head: Pattern::Ident("x"),
///             tail: Pattern::Ident("xs"),
///             ...
///         }
///     ],
///     body: BinaryOp(Add, "x", "sum(xs)"),
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ListConsPattern<T> {
    pub head: Box<Pattern<T>>,
    pub tail: Box<Pattern<T>>,
    pub position: Span,
    pub info: T,
}

/// Single match arm in a match expression.
///
/// A match arm consists of a pattern and an expression that
/// is evaluated if the pattern matches.
///
/// # Fields
///
/// * `pattern` - The pattern to match against
/// * `body` - Expression to evaluate if pattern matches
/// * `position` - Source location of this entire arm
/// * `info` - Metadata (currently unused)
///
/// # Example
///
/// ```text
/// // match x of
/// //     0 => "zero"      // This is one MatchArm
/// //     n => n.toString()  // This is another MatchArm
/// // end
///
/// MatchArm {
///     pattern: Pattern::Literal(Integer(0, ...)),
///     body: Expression::String("zero"),
///     position: ...,
///     info: (),
/// }
/// ```
#[derive(Debug, Clone)]
pub struct MatchArm<T> {
    pub pattern: Pattern<T>,
    pub body: Expression<T>,
    pub position: Span,
    pub info: T,
}

/// Match expression.
///
/// Represents a complete pattern matching expression, which evaluates
/// a scrutinee and tries each pattern in order until one matches.
///
/// # Fields
///
/// * `scrutinee` - The expression to match against
/// * `arms` - List of match arms, tried in order
/// * `position` - Source location
/// * `info` - Metadata (currently unused)
///
/// # Semantics
///
/// The match evaluates by trying each pattern in order:
/// 1. Evaluate scrutinee to a value
/// 2. For each arm (in order):
///    - Try to match pattern against value
///    - If match succeeds, evaluate body and return
///    - If match fails, try next arm
/// 3. If no arm matches, runtime error (should have wildcard)
///
/// # Examples
///
/// ```text
/// // Simple match:
/// // match x of
/// //     42 => "answer"
/// //     _  => "other"
/// // end
/// Match {
///     scrutinee: Box::new(Ident("x")),
///     arms: vec![
///         MatchArm {
///             pattern: Literal(Integer(42)),
///             body: String("answer"),
///             ...
///         },
///         MatchArm {
///             pattern: Wildcard(),
///             body: String("other"),
///             ...
///         },
///     ],
///     ...
/// }
///
/// // Multi-pattern match (factorial):
/// // match n of
/// //     0 => 1
/// //     1 => 1
/// //     n => n * factorial(n - 1)
/// // end
/// Match {
///     scrutinee: Box::new(Ident("n")),
///     arms: vec![
///         MatchArm { pattern: Literal(Integer(0)), body: Integer(1), ... },
///         MatchArm { pattern: Literal(Integer(1)), body: Integer(1), ... },
///         MatchArm { pattern: Ident("n"), body: BinaryOp(...), ... },
///     ],
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Match<T> {
    pub scrutinee: Box<Expression<T>>,
    pub arms: Vec<MatchArm<T>>,
    pub position: Span,
    pub info: T,
}

/// Single function clause for multi-clause pattern-based function.
///
/// Multi-clause functions use pattern matching instead of simple
/// parameters. Each clause can match different patterns.
///
/// # Fields
///
/// * `name` - The function name (all clauses share the same name)
/// * `patterns` - List of patterns for parameters (one per parameter position)
/// * `body` - Function body (expression or block)
/// * `position` - Source location
/// * `info` - Metadata (currently unused)
///
/// # Semantics
///
/// When calling a multi-clause function, we try each clause in order:
/// 1. For each clause, match all patterns against arguments simultaneously
/// 2. If all patterns match, evaluate that clause's body
/// 3. If patterns don't match, try next clause
/// 4. If no clause matches, runtime error
///
/// # Example: Factorial Function
///
/// ```text
/// // factorial 0 = 1
/// // factorial n = n * factorial(n - 1)
///
/// Clause 1:
/// FunctionClause {
///     name: Ident("factorial"),
///     patterns: [Pattern::Literal(Integer(0))],
///     body: Expression::Integer(1),
///     ...
/// }
///
/// Clause 2:
/// FunctionClause {
///     name: Ident("factorial"),
///     patterns: [Pattern::Ident(Ident("n"))],
///     body: Expression::BinaryOp(Mul, "n", "factorial(n-1)"),
///     ...
/// }
/// ```
///
/// # Example: Length Function
///
/// ```text
/// // length [] = 0
/// // length (_:xs) = 1 + length(xs)
///
/// Clause 1:
/// FunctionClause {
///     name: Ident("length"),
///     patterns: [Pattern::Wildcard()],  // Matches empty list
///     body: Expression::Integer(0),
///     ...
/// }
///
/// Clause 2:
/// FunctionClause {
///     name: Ident("length"),
///     patterns: [Pattern::Ident(Ident("xs"))],  // Bind to any list
///     body: Expression::BinaryOp(Add, 1, "length(xs)"),
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct FunctionClause<T> {
    pub name: Ident<T>,
    pub patterns: Vec<Pattern<T>>,
    pub body: LambdaBody<T>,
    pub position: Span,
    pub info: T,
}
