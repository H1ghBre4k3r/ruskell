//! # Expression AST Nodes
//!
//! This module defines all expression types in the Surface AST - the syntactic
//! structures that appear in Ruskell source code after parsing.
//!
//! ## Overview
//!
//! The `Expression` enum represents all possible expression forms in Ruskell:
//!
//! - **Literals**: `Unit`, `Integer`, `String`, `Boolean` - Basic values
//! - **Identifiers**: `Ident` - Variable and function references
//! - **Functions**: `Lambda`, `FunctionCall` - Function definitions and calls
//! - **Operations**: `BinaryOp`, `UnaryOp` - Arithmetic, comparison, logical operations
//! - **Control Flow**: `IfThenElse`, `Match` - Conditional and pattern matching
//!
//! ## Type Parameter `T`
//!
//! All AST nodes are generic over `T` to allow attaching metadata
//! at different compiler phases:
//!
//! - **After parsing**: `Expression<()>` - No metadata yet
//! - **After type checking**: `Expression<TypeInfo>` - With inferred types
//!
//! ## Pipeline Position
//!
//! ```text
//! Lexer → Token Stream → Parser → [EXPRESSION AST] → Desugaring → Core AST
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::ast::statement`] - Statement AST (expressions can appear in statements)
//! - [`crate::ast::pattern`] - Pattern AST (for pattern matching in Match)
//! - [`crate::core`] - Core expression types (after desugaring)

use lachs::Span;

use super::statement::Statement;

/// All possible expression forms in Ruskell.
///
/// `Expression` is the main enum for representing parsed Ruskell code.
/// Each variant corresponds to a syntactic form in the language.
///
/// # Variants
///
/// * `Unit` - The unit value `()`
/// * `Ident` - Variable or function name
/// * `Integer` - Integer literal (e.g., `42`)
/// * `String` - String literal (e.g., `"hello"`)
/// * `Boolean` - Boolean literal (`true` or `false`)
/// * `FunctionCall` - Function invocation (e.g., `add(1, 2)`)
/// * `Lambda` - Anonymous function (e.g., `\x, y => x + y`)
/// * `BinaryOp` - Binary operation (e.g., `x + y`)
/// * `UnaryOp` - Unary operation (e.g., `!x`)
/// * `IfThenElse` - Conditional expression
/// * `Match` - Pattern matching expression
///
/// # Example
///
/// ```text
/// // The expression "add(1, 2)" becomes:
/// Expression::FunctionCall(FunctionCall {
///     func: Box::new(Expression::Ident(Ident { value: "add", ... })),
///     args: vec![
///         Expression::Integer(Integer { value: 1, ... }),
///         Expression::Integer(Integer { value: 2, ... }),
///     ],
///     position: ...,
///     info: (),
/// })
/// ```
#[derive(Debug, Clone)]
pub enum Expression<T> {
    Unit(Unit<T>),
    Ident(Ident<T>),
    Integer(Integer<T>),
    String(StringLiteral<T>),
    Boolean(Boolean<T>),
    FunctionCall(FunctionCall<T>),
    Lambda(Lambda<T>),
    BinaryOp(BinaryOp<T>),
    UnaryOp(UnaryOp<T>),
    IfThenElse(IfThenElse<T>),
    Match(Box<super::pattern::Match<T>>),
}

/// Unit value `()`.
///
/// Represents the unit type, which is Ruskell's only "empty" value.
/// Used as a placeholder when no meaningful value is needed.
///
/// # Fields
///
/// * `position` - Source location in the original code
/// * `info` - Metadata (e.g., type information after type checking)
///
/// # Example
///
/// ```text
/// // In Ruskell: "()"
/// Unit { position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct Unit<T> {
    pub position: Span,
    pub info: T,
}

/// Body of a lambda expression or function definition.
///
/// Lambda bodies can be either a single expression or a block
/// of multiple statements, enabling flexible function definitions.
///
/// # Variants
///
/// * `Expression` - Single expression body (e.g., `\x => x + 1`)
/// * `Block` - Block of statements (e.g., `\x => do ... end`)
///
/// # Examples
///
/// ```text
/// // Single expression:
/// // \x => x + 1
/// LambdaBody::Expression(Box::new(BinaryOp { ... }))
///
/// // Block:
/// // \x => do
/// //   x := x + 1
/// //   print(x)
/// // end
/// LambdaBody::Block(vec![
///     Statement::Assignment(...),
///     Statement::Expression(...),
/// ])
/// ```
#[derive(Debug, Clone)]
pub enum LambdaBody<T> {
    Expression(Box<Expression<T>>),
    Block(Vec<Statement<T>>),
}

/// Identifier (variable or function name).
///
/// Represents references to named entities - either variables in scope
/// or functions being called.
///
/// # Fields
///
/// * `value` - The identifier name (e.g., `"add"`, `"x"`, `"main"`)
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Example
///
/// ```text
/// // In code: "myVariable"
/// Ident { value: "myVariable", position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct Ident<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

/// Integer literal.
///
/// Represents integer literals in source code.
///
/// # Fields
///
/// * `value` - The integer value (as i128 for large number support)
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Example
///
/// ```text
/// // In code: "42"
/// Integer { value: 42, position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct Integer<T> {
    pub value: i128,
    pub position: Span,
    pub info: T,
}

/// String literal.
///
/// Represents string literals in source code.
///
/// # Fields
///
/// * `value` - The string value (not including quotes)
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Example
///
/// ```text
/// // In code: "\"hello world\""
/// StringLiteral { value: "hello world", position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct StringLiteral<T> {
    pub value: String,
    pub position: Span,
    pub info: T,
}

/// Boolean literal.
///
/// Represents the two boolean values in Ruskell.
///
/// # Fields
///
/// * `value` - `true` or `false`
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Example
///
/// ```text
/// // In code: "true"
/// Boolean { value: true, position: ..., info: () }
/// ```
#[derive(Debug, Clone)]
pub struct Boolean<T> {
    pub value: bool,
    pub position: Span,
    pub info: T,
}

/// Function call expression.
///
/// Represents invoking a function with zero or more arguments.
/// Ruskell supports multi-argument calls which are later desugared
/// to nested single-argument calls.
///
/// # Fields
///
/// * `func` - Expression evaluating to the function to call
/// * `args` - List of arguments to pass to the function
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Examples
///
/// ```text
/// // No arguments: "print()"
/// FunctionCall {
///     func: Box::new(Expression::Ident("print")),
///     args: [],
///     ...
/// }
///
/// // Single argument: "add(1)"
/// FunctionCall {
///     func: Box::new(Expression::Ident("add")),
///     args: [Integer { value: 1 }],
///     ...
/// }
///
/// // Multiple arguments: "add(1, 2, 3)"
/// FunctionCall {
///     func: Box::new(Expression::Ident("add")),
///     args: [Integer { value: 1 }, Integer { value: 2 }, Integer { value: 3 }],
///     ...
/// }
///
/// // Nested call: "add(mul(2, 3), 4)"
/// FunctionCall {
///     func: Box::new(Expression::Ident("add")),
///     args: [
///         FunctionCall { func: "mul", args: [2, 3] },
///         Integer { value: 4 }
///     ],
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct FunctionCall<T> {
    pub func: Box<Expression<T>>,
    pub args: Vec<Expression<T>>,
    pub position: Span,
    pub info: T,
}

/// Lambda (anonymous function) expression.
///
/// Represents function literals in Ruskell, which can be assigned
/// to variables or passed as arguments to other functions.
///
/// Supports multi-parameter lambdas which are desugared to nested
/// single-parameter lambdas in the core language.
///
/// # Fields
///
/// * `params` - List of parameters (zero or more)
/// * `body` - Either a single expression or a block of statements
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Examples
///
/// ```text
/// // Identity function: "\x => x"
/// Lambda {
///     params: [LambdaParam::Ident("x")],
///     body: LambdaBody::Expression(Box::new(Expression::Ident("x"))),
///     ...
/// }
///
/// // Multi-parameter: "\x, y => x + y"
/// Lambda {
///     params: [
///         LambdaParam::Ident("x"),
///         LambdaParam::Ident("y")
///     ],
///     body: LambdaBody::Expression(Box::new(BinaryOp { ... })),
///     ...
/// }
///
/// // With block: "\x => do x := x + 1; print(x); end"
/// Lambda {
///     params: [LambdaParam::Ident("x")],
///     body: LambdaBody::Block(vec![...statements...]),
///     ...
/// }
///
/// // Zero parameters (equivalent to unit parameter): "\() => 42"
/// Lambda {
///     params: [LambdaParam::Unit(...)],
///     body: LambdaBody::Expression(Box::new(Integer { value: 42 })),
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Lambda<T> {
    pub params: Vec<LambdaParam<T>>,
    pub body: LambdaBody<T>,
    pub position: Span,
    pub info: T,
}

/// Lambda parameter - either a named identifier or unit pattern.
///
/// Lambda parameters specify what values the lambda accepts.
/// Parameters can be named (to bind variables) or unit (to
/// explicitly ignore argument).
///
/// # Variants
///
/// * `Ident` - Named parameter that binds a variable (e.g., `x`, `name`)
/// * `Unit` - Unit pattern, ignores argument (e.g., `()`)
///
/// # Examples
///
/// ```text
/// // Named parameter: "\x => ..."
/// LambdaParam::Ident(Ident { value: "x", ... })
///
/// // Unit parameter: "\() => ..."
/// LambdaParam::Unit(Unit { ... })
/// ```
#[derive(Debug, Clone)]
pub enum LambdaParam<T> {
    Ident(Ident<T>),
    Unit(Unit<T>),
}

/// Binary operator kinds.
///
/// All binary operations supported by Ruskell, categorized by type:
///
/// - **Arithmetic**: `Add`, `Sub`, `Mul`, `Div` - Int -> Int -> Int
/// - **Comparison**: `Eq`, `NotEq`, `Lt`, `Gt`, `LtEq`, `GtEq` - Int -> Int -> Bool
/// - **Logical**: `And`, `Or` - Bool -> Bool -> Bool
/// - **String**: `Concat` - String -> String -> String
///
/// # Example
///
/// ```text
/// // x + y
/// BinOpKind::Add
///
/// // x == y
/// BinOpKind::Eq
///
/// // x > y
/// BinOpKind::Gt
///
/// // x && y
/// BinOpKind::And
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    /// Addition: `+`
    Add,
    /// Subtraction: `-`
    Sub,
    /// Multiplication: `*`
    Mul,
    /// Division: `/`
    Div,
    /// Equality: `==`
    Eq,
    /// Inequality: `!=`
    NotEq,
    /// Less than: `<`
    Lt,
    /// Greater than: `>`
    Gt,
    /// Less than or equal: `<=`
    LtEq,
    /// Greater than or equal: `>=`
    GtEq,
    /// Logical AND: `&&`
    And,
    /// Logical OR: `||`
    Or,
    /// String concatenation: `++`
    Concat,
}

/// Unary operator kinds.
///
/// All unary operations supported by Ruskell.
///
/// - **Logical**: `Not` - Bool -> Bool
///
/// # Example
///
/// ```text
/// // !x
/// UnaryOpKind::Not
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
    /// Logical NOT: `!`
    Not,
}

/// Binary operation expression.
///
/// Represents a binary operator applied to two operands.
///
/// # Fields
///
/// * `op` - The binary operator
/// * `left` - Left operand expression
/// * `right` - Right operand expression
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Examples
///
/// ```text
/// // x + y
/// BinaryOp {
///     op: BinOpKind::Add,
///     left: Box::new(Ident("x")),
///     right: Box::new(Ident("y")),
///     ...
/// }
///
/// // Nested: (a + b) * (c - d)
/// BinaryOp {
///     op: BinOpKind::Mul,
///     left: Box::new(BinaryOp { op: Add, left: "a", right: "b" }),
///     right: Box::new(BinaryOp { op: Sub, left: "c", right: "d" }),
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct BinaryOp<T> {
    pub op: BinOpKind,
    pub left: Box<Expression<T>>,
    pub right: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}

/// Unary operation expression.
///
/// Represents a unary operator applied to one operand.
///
/// # Fields
///
/// * `op` - The unary operator
/// * `operand` - The operand expression
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Examples
///
/// ```text
/// // !x
/// UnaryOp {
///     op: UnaryOpKind::Not,
///     operand: Box::new(Ident("x")),
///     ...
/// }
///
/// // Nested: !!x (double negation)
/// UnaryOp {
///     op: UnaryOpKind::Not,
///     operand: Box::new(UnaryOp { op: Not, operand: "x" }),
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct UnaryOp<T> {
    pub op: UnaryOpKind,
    pub operand: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}

/// Conditional expression (if-then-else).
///
/// Represents Ruskell's conditional expression, which chooses between
/// two branches based on a boolean condition.
///
/// # Fields
///
/// * `condition` - Boolean expression to evaluate
/// * `then_expr` - Expression to evaluate if condition is true
/// * `else_expr` - Expression to evaluate if condition is false
/// * `position` - Source location
/// * `info` - Metadata
///
/// # Examples
///
/// ```text
/// // if true then 1 else 2
/// IfThenElse {
///     condition: Box::new(Boolean { value: true }),
///     then_expr: Box::new(Integer { value: 1 }),
///     else_expr: Box::new(Integer { value: 2 }),
///     ...
/// }
///
/// // Nested: if x > 0 then if x < 10 then "small" else "large" else "negative"
/// IfThenElse {
///     condition: Box::new(BinaryOp { op: Gt, left: "x", right: 0 }),
///     then_expr: Box::new(IfThenElse {
///         condition: Box::new(BinaryOp { op: Lt, left: "x", right: 10 }),
///         then_expr: "small",
///         else_expr: "large",
///         ...
///     }),
///     else_expr: "negative",
///     ...
/// }
/// ```
#[derive(Debug, Clone)]
pub struct IfThenElse<T> {
    pub condition: Box<Expression<T>>,
    pub then_expr: Box<Expression<T>>,
    pub else_expr: Box<Expression<T>>,
    pub position: Span,
    pub info: T,
}
