//! # Ruskell - A Haskell-inspired Functional Programming Language
//!
//! Ruskell is a statically-typed functional programming language implemented in Rust.
//! It features a complete compiler pipeline from source code to execution, including
//! lexing, parsing, desugaring, type checking, and interpretation.
//!
//! ## Architecture Overview
//!
//! The compiler pipeline consists of the following stages:
//!
//! 1. **Lexer** (`lexer`) - Tokenizes source code into a stream of tokens using the `lachs` library
//! 2. **Parser** (`parser`) - Builds a Surface AST from tokens using combinator-based recursive descent
//! 3. **Desugaring** (`desugar`) - Transforms Surface AST to Core AST by simplifying syntactic sugar
//! 4. **Type Checking** (`types`) - Performs Hindley-Milner type inference on the Core AST
//! 5. **Interpreter** (`interpreter`) - Executes the typed program using a tree-walking interpreter
//!
//! ## Pipeline Flow
//!
//! ```text
//! Source Code (String)
//!     ↓
//! [Lexer] → Token Stream
//!     ↓
//! [Parser] → Surface AST (ast::Program<()>)
//!     ↓
//! [Desugaring] → Core AST (core::CoreProgram<()>)
//!     ↓
//! [Type Checker] → Typed Core AST (core::CoreProgram<TypeInfo>)
//!     ↓
//! [Interpreter] → Runtime Value (interpreter::RValue)
//! ```
//!
//! ## Key Design Decisions
//!
//! ### Two-Tier AST System
//! Ruskell uses two AST representations:
//!
//! - **Surface AST** (`ast`): Rich syntax matching the source language, including:
//!   - Multi-parameter lambdas (`\x, y => expr`)
//!   - Multi-clause functions with pattern matching
//!   - Syntactic sugar for convenience
//!   - Generic type parameter `T` for future type annotations
//!
//! - **Core AST** (`core`): Simplified representation after desugaring, featuring:
//!   - Only single-parameter lambdas (multi-params desugar to nested lambdas)
//!   - Single-argument function calls (multi-args desugar to nested calls)
//!   - Pattern matching converted to if-then-else chains
//!   - Simpler structure for type checking and interpretation
//!
//! ### Hindley-Milner Type System
//! The type checker implements standard Hindley-Milner type inference:
//! - Unification-based type inference
//! - Polymorphic types with `forall` quantification
//! - Type variables for generalization
//! - Principled type inference with let-polymorphism
//!
//! ### Closure Support
//! The interpreter properly supports closures with lexical scoping:
//! - Lambdas capture their environment at creation time
//! - Closures correctly reference outer variables
//! - Higher-order functions work as expected
//!
//! ## Module Structure
//!
//! - [`ast`] - Surface Abstract Syntax Tree definitions
//! - [`core`] - Core (desugared) AST definitions
//! - [`desugar`] - Desugaring transformations from surface to core
//! - [`types`] - Type checking and inference (Hindley-Milner)
//! - [`interpreter`] - Runtime evaluation and execution
//! - [`lexer`] - Tokenization using lachs
//! - [`parser`] - Parsing using combinator-based grammar
//! - [`fmt`] - Code formatting and pretty-printing
//!
//! ## Example Program
//!
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//!
//! main = factorial(10)
//! ```
//!
//! This program defines a factorial function with pattern matching and calls it with 10.
//! After desugaring, the multi-clause function becomes a single function with nested
//! if-then-else expressions for pattern matching.
//!
//! ## Getting Started
//!
//! To use this library:
//! 1. Parse source code with [`parser::parse()`]
//! 2. Desugar with [`desugar::desugar_program()`]
//! 3. Type check with [`types::validate_and_type_check()`]
//! 4. Run with [`interpreter::run()`]

pub mod ast;
pub mod core;
pub mod desugar;
pub mod fmt;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod types;

/// Type alias for a parsed program with no additional type information
///
/// This represents the output of the parsing stage, before desugaring and type checking.
pub type ParsedProgram = ast::Program<()>;
