//! # Core AST - Simplified AST After Desugaring
//!
//! This module defines the **Core Abstract Syntax Tree**, a simplified representation
//! of Ruskell programs after desugaring. The Core AST is easier to type check and
//! interpret than the Surface AST because it has fewer, more uniform constructs.
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → Desugaring → [CORE AST] → Type Checker → Interpreter
//! ```
//!
//! ## Surface AST vs Core AST
//!
//! | Feature | Surface AST | Core AST |
//! |---------|-------------|----------|
//! | Lambda parameters | Multiple (`\x, y => ...`) | Single (`\x => \y => ...`) |
//! | Function calls | Multiple args (`f(x, y)`) | Single arg (`f(x)(y)`) |
//! | Pattern matching | Multi-clause functions | If-then-else chains |
//! | Match expressions | `match ... of` | Nested if-then-else |
//!
//! ## Why a Separate Core AST?
//!
//! ### 1. Simpler Type Checking
//!
//! With uniform single-parameter lambdas and single-argument calls, type checking
//! becomes much simpler. We only need one rule for lambdas instead of handling
//! multi-parameter cases.
//!
//! ```text
//! Surface: (\x, y => x + y) : 't0 -> 't1 -> 't2 (complex!)
//! Core:    (\x => \y => x + y) : 't0 -> ('t1 -> 't2) (simple!)
//! ```
//!
//! ### 2. Simpler Interpreter
//!
//! The interpreter only needs to handle single-argument function application,
//! reducing code complexity and potential bugs.
//!
//! ### 3. Easier Optimizations
//!
//! Many optimizations are easier to implement on a uniform core language:
//! - Inlining
//! - Dead code elimination
//! - Constant folding
//! - Partial evaluation
//!
//! ### 4. Language Evolution
//!
//! We can add new surface syntax without changing the core language or type checker:
//!
//! ```text
//! New surface syntax → Desugar to core → Existing type checker & interpreter work!
//! ```
//!
//! ## Core AST Structure
//!
//! ### Programs
//!
//! A Core program consists of:
//! - A `main` function (entry point)
//! - A list of other functions
//!
//! All functions are top-level after lambda lifting.
//!
//! ### Expressions
//!
//! Core expressions are simpler than surface expressions:
//!
//! ```text
//! pub enum CoreExpr<T> {
//!     Unit(CoreUnit<T>),              // ()
//!     Ident(CoreIdent<T>),            // x
//!     Integer(CoreInteger<T>),        // 42
//!     String(CoreString<T>),          // "hello"
//!     Boolean(CoreBoolean<T>),        // true, false
//!     Lambda(CoreLambda<T>),          // \x => expr (single param only!)
//!     FunctionCall(CoreFunctionCall<T>), // f(x) (single arg only!)
//!     BinaryOp(CoreBinaryOp<T>),      // x + y, x == y, etc.
//!     UnaryOp(CoreUnaryOp<T>),        // !x
//!     IfThenElse(CoreIfThenElse<T>),  // if cond then e1 else e2
//! }
//! ```
//!
//! Note what's **missing** compared to Surface AST:
//! - No multi-parameter lambdas
//! - No multi-argument calls
//! - No pattern match expressions
//! - No multi-clause function definitions
//!
//! All of these were desugared away!
//!
//! ### Type Parameter `T`
//!
//! Like the Surface AST, Core AST is generic over a type parameter `T`:
//!
//! - **Before type checking**: `CoreExpr<()>` (no type info)
//! - **After type checking**: `CoreExpr<TypeInfo>` (with inferred types)
//!
//! This allows the same AST structure to carry different information at different
//! compiler phases.
//!
//! ## Key Invariants
//!
//! The Core AST maintains these invariants (enforced by desugaring):
//!
//! ### 1. Single-Parameter Lambdas
//!
//! Every `CoreLambda` has exactly one parameter. Multi-parameter functions are
//! represented as nested lambdas:
//!
//! ```ruskell
//! // Surface: add x y = x + y
//! // Core:
//! add = \x => \y => x + y
//! ```
//!
//! ### 2. Single-Argument Calls
//!
//! Every `CoreFunctionCall` passes exactly one argument. Multi-argument calls are
//! represented as nested applications:
//!
//! ```ruskell
//! // Surface: add(1, 2)
//! // Core:
//! add(1)(2)
//! ```
//!
//! ### 3. Explicit Pattern Matching
//!
//! Pattern matching is expanded to if-then-else chains with equality checks:
//!
//! ```ruskell
//! // Surface:
//! match x of
//!   0 => "zero"
//!   1 => "one"
//!   n => "other"
//! end
//!
//! // Core:
//! if x == 0
//! then "zero"
//! else if x == 1
//!      then "one"
//!      else (\n => "other")(x)
//! ```
//!
//! ## Position Information
//!
//! All Core AST nodes include a `position: Span` field that tracks their location
//! in the original source code. This is crucial for error reporting during type
//! checking - even though the code has been transformed, we can still point to
//! the original source location.
//!
//! ## Example Transformation
//!
//! Here's how a complete function transforms from Surface to Core:
//!
//! ### Surface AST
//!
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//! ```
//!
//! ### Core AST (After Desugaring)
//!
//! ```ruskell
//! factorial = \__arg0 =>
//!   if __arg0 == 0
//!   then 1
//!   else (\n => n * factorial(n - 1))(__arg0)
//! ```
//!
//! Notice:
//! - Two clauses → One lambda with if-then-else
//! - Pattern `0` → Equality check `__arg0 == 0`
//! - Pattern `n` → Lambda binding `\n => ...`
//!
//! ## Related Modules
//!
//! - [`crate::ast`] - Surface AST (what desugaring transforms from)
//! - [`crate::desugar`] - Desugaring pass (creates Core AST)
//! - [`crate::types`] - Type checker (works on Core AST)
//! - [`crate::interpreter`] - Interpreter (evaluates Core AST)

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
    List(CoreList<T>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct CoreList<T> {
    pub elements: Vec<CoreExpr<T>>,
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
            CoreExpr::List(l) => l.position.clone(),
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
