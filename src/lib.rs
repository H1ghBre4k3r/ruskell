//! Ruskell - A Haskell-inspired functional programming language
//!
//! This library provides the core components for lexing, parsing, and interpreting
//! Ruskell programs.

pub mod ast;
pub mod core;
pub mod desugar;
pub mod interpreter;
pub mod lexer;
pub mod parser;

/// Type alias for a parsed program with no additional type information
pub type ParsedProgram = ast::Program<()>;
