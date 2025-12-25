//! # Surface AST - Abstract Syntax Tree for Parsed Programs
//!
//! This module defines the **Surface Abstract Syntax Tree**, a tree structure
//! representing parsed Ruskell programs with all the rich, user-friendly syntax
//! from the source language.
//!
//! ## Pipeline Position
//!
//! ```text
//! Lexer → Token Stream → Parser → [SURFACE AST] → Desugaring → Core AST
//! ```
//!
//! ## What is the Surface AST?
//!
//! The Surface AST is the **direct representation** of source code as a tree structure.
//! It preserves all the syntactic sugar and convenient features that make the language
//! pleasant to use:
//!
//! - **Multi-parameter lambdas**: `\x, y, z => expr`
//! - **Multi-argument calls**: `f(x, y, z)`
//! - **Multi-clause functions**: Pattern matching on multiple clauses
//! - **Match expressions**: `match value of ...`
//! - **Do-blocks**: `do ... end` for sequential execution
//!
//! ## Surface vs Core AST
//!
//! The Surface AST is **rich** but **complex**. The Core AST is **simple** but **uniform**:
//!
//! | Feature | Surface AST | Core AST |
//! |---------|-------------|----------|
//! | Lambda params | Multiple allowed | Single only |
//! | Function calls | Multiple args | Single arg |
//! | Pattern matching | Multi-clause functions | If-then-else |
//! | Match expressions | Native construct | Desugared |
//! | Purpose | User-friendly | Type checking |
//!
//! ## Module Organization
//!
//! The Surface AST is split into semantic categories:
//!
//! - Top-level - Program and function definitions (this module)
//! - [`expression`] - Expressions (lambdas, calls, operators, literals)
//! - [`statement`] - Statements (assignments, expression statements)
//! - [`pattern`] - Patterns for matching (literals, variables, wildcards)
//!
//! ## Type Parameter `T`
//!
//! All AST nodes are generic over a type parameter `T`:
//!
//! ```rust
//! pub struct Expression<T> {
//!     // ... expression data ...
//!     pub info: T,
//! }
//! ```
//!
//! This allows the same AST structure to carry different information at different stages:
//!
//! - **After parsing**: `Expression<()>` (no type info yet)
//! - **After type checking**: `Expression<TypeInfo>` (with inferred types)
//!
//! Currently, Ruskell only uses `()` because type checking happens on Core AST,
//! but this design supports future optimizations like type-aware desugaring.
//!
//! ## Program Structure
//!
//! A complete Ruskell program consists of:
//!
//! ```text
//! pub struct Program<T> {
//!     pub main: FunctionDef<T>,      // Entry point
//!     pub functions: Vec<FunctionDef<T>>,  // Other functions
//! }
//! ```
//!
//! ### Function Definitions
//!
//! Functions can be single-clause or multi-clause:
//!
//! ```text
//! pub enum FunctionDef<T> {
//!     Single(Function<T>),           // name params = body
//!     Multi {                         // Pattern matching
//!         name: Ident<T>,
//!         clauses: Vec<FunctionClause<T>>,
//!     },
//! }
//! ```
//!
//! **Single-clause function:**
//! ```ruskell
//! add x y = x + y
//! ```
//!
//! **Multi-clause function:**
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::parser`] - Parses source code into Surface AST
//! - [`crate::core`] - Core AST (desugared version)
//! - [`crate::desugar`] - Transforms Surface AST to Core AST

use crate::ast::expression::{Ident, Lambda};

pub mod expression;
pub mod pattern;
pub mod statement;

use pattern::FunctionClause;

/// A complete Ruskell program.
///
/// Consists of a `main` function (entry point) and a list of other function definitions.
#[derive(Debug, Clone)]
pub struct Program<T> {
    pub main: FunctionDef<T>,
    pub functions: Vec<FunctionDef<T>>,
}

/// A named function, which internally contains a lambda expression.
///
/// This represents single-clause functions like:
/// ```ruskell
/// add x y = x + y
/// ```
#[derive(Debug, Clone)]
pub struct Function<T> {
    pub name: Ident<T>,
    pub lambda: Lambda<T>,
}

/// Function definition, either single-clause or multi-clause with pattern matching.
///
/// # Variants
///
/// - `Single`: A function with one clause, e.g., `add x y = x + y`
/// - `Multi`: A function with multiple pattern-matching clauses, e.g.:
///   ```ruskell
///   factorial 0 = 1
///   factorial n = n * factorial(n - 1)
///   ```
#[derive(Debug, Clone)]
pub enum FunctionDef<T> {
    Single(Function<T>),
    Multi {
        name: Ident<T>,
        clauses: Vec<FunctionClause<T>>,
    },
}
