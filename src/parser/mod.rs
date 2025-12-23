//! # Parser - Combinator-Based Recursive Descent Parsing
//!
//! This module implements the **parsing phase** of the Ruskell compiler, converting
//! a stream of tokens into a Surface AST using a combinator-based recursive descent
//! parser.
//!
//! ## Pipeline Position
//!
//! ```text
//! Lexer → Token Stream → [PARSER] → Surface AST → Desugaring → Type Checker
//! ```
//!
//! ## What is Parser Combinator?
//!
//! A **parser combinator** is a higher-order function that takes parsers as input
//! and returns a new parser as output. This allows building complex parsers by
//! combining simpler ones.
//!
//! ### Example Combinators
//!
//! - `p1 + p2` - **Sequence**: Parse p1, then p2
//! - `p1 | p2` - **Alternative**: Try p1, if it fails try p2  
//! - `p*` - **Repetition**: Parse p zero or more times
//! - `p >> f` - **Map**: Parse p and transform the result with function f
//!
//! ## Architecture
//!
//! The parser is organized into layers as separate modules (not publicly exposed).
//!
//! ### 1. Parser State
//!
//! Manages the token stream and position (see [`ParseState`]).
//!
//! ### 2. Combinators
//!
//! Generic parser combinators that work with any parser type.
//!
//! ### 3. Grammar Rules
//!
//! Ruskell-specific parsing rules organized by syntactic category.
//!
//! ## Operator Precedence
//!
//! Binary operators are parsed with the following precedence (lowest to highest):
//!
//! 1. Logical OR (`||`)
//! 2. Logical AND (`&&`)
//! 3. Equality (`==`, `!=`)
//! 4. Comparison (`<`, `>`, `<=`, `>=`)
//! 5. String concatenation (`++`)
//! 6. Addition/Subtraction (`+`, `-`)
//! 7. Multiplication/Division (`*`, `/`)
//!
//! Implemented using precedence climbing in `binary_op_expr()`.
//!
//! ## Error Recovery
//!
//! The parser implements error recovery to report multiple parse errors:
//!
//! 1. **Panic Mode Recovery** - Skip tokens until synchronization point
//! 2. **Multiple Errors** - Collect all errors instead of stopping at first
//! 3. **Detailed Messages** - Include source position and expected tokens
//!
//! When parsing fails:
//! - Record the error with position and expected token
//! - Try to recover by skipping to next statement/function
//! - Continue parsing to find more errors
//!
//! ## Grammar Overview
//!
//! The Ruskell grammar (simplified):
//!
//! ```text
//! Program := FunctionDef*
//!
//! FunctionDef :=
//!     | Ident Param* '=' Expr            // Single clause
//!     | (Ident Pattern+ '=' Expr)+       // Multi-clause (pattern matching)
//!
//! Expr :=
//!     | Literal
//!     | Ident
//!     | '\' Param+ '=>' (Expr | Block)   // Lambda
//!     | Expr '(' Expr, ... ')'           // Function call
//!     | Expr BinOp Expr                  // Binary operation
//!     | UnaryOp Expr                     // Unary operation
//!     | 'if' Expr 'then' Expr 'else' Expr
//!     | 'case' Expr 'of' (Pattern '=>' Expr)+ 'end'
//!
//! Block := 'do' Statement* 'end'
//!
//! Statement :=
//!     | Ident ':=' Expr                  // Assignment
//!     | Expr                             // Expression statement
//!
//! Pattern := Literal | Ident | '_'
//!
//! Literal := Integer | String | Boolean | Unit
//! ```
//!
//! ## Example Parse Tree
//!
//! Source:
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//! ```
//!
//! AST:
//! ```text
//! Program {
//!   functions: [
//!     FunctionDef::Multi {
//!       name: "factorial",
//!       clauses: [
//!         FunctionClause {
//!           patterns: [Literal(0)],
//!           body: Integer(1)
//!         },
//!         FunctionClause {
//!           patterns: [Ident("n")],
//!           body: BinaryOp {
//!             op: Mul,
//!             left: Ident("n"),
//!             right: FunctionCall {
//!               func: Ident("factorial"),
//!               args: [BinaryOp { op: Sub, left: Ident("n"), right: Integer(1) }]
//!             }
//!           }
//!         }
//!       ]
//!     }
//!   ]
//! }
//! ```
//!
//! ## Usage
//!
//! ```ignore
//! use ruskell::lexer::Token;
//! use ruskell::parser::{ParseState, parse};
//!
//! let source = "add x y = x + y";
//! let tokens = Token::lex(source)?;
//! let mut state = ParseState::new(tokens);
//! let (program, errors) = parse(&mut state);
//!
//! if !errors.is_empty() {
//!     // Handle parse errors
//! }
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::lexer`] - Tokenization (produces input to parser)
//! - [`crate::ast`] - Surface AST definitions (parser output)
//! - [`crate::desugar`] - Desugaring pass (consumes parser output)

mod combinators;
mod grammar;
mod state;

pub use grammar::parse;
pub use state::ParseState;
