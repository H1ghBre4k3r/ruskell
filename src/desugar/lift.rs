//! # Lambda Lifting - Closure Conversion
//!
//! This module implements **lambda lifting**, also known as **closure conversion**,
//! which transforms closures (lambdas that capture free variables) into standalone
//! top-level functions with explicit capture parameters.
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → Desugaring → Core AST → [LAMBDA LIFTING] → Lifted Core AST → Type Checker
//! ```
//!
//! ## What is Lambda Lifting?
//!
//! Lambda lifting is a transformation that eliminates closures by:
//! 1. **Identifying captured variables** (free variables in lambda bodies)
//! 2. **Extracting lambdas to top-level functions** with explicit parameters for captures
//! 3. **Replacing lambdas with partial applications** of the new functions
//!
//! ## Why Do We Need Lambda Lifting?
//!
//! Lambda lifting makes closures **explicit** in the program structure:
//!
//! 1. **Simplifies Type Checking** - All captures are visible as parameters
//! 2. **Enables Optimizations** - Can analyze and optimize closure allocations
//! 3. **Supports Compilation** - Easier to compile to native code or bytecode
//! 4. **Clear Semantics** - Makes variable capture explicit in the AST
//!
//! ## How It Works
//!
//! ### Free Variable Analysis
//!
//! First, we identify which variables are **free** in a lambda expression:
//!
//! ```text
//! Free variables = Variables used - Variables bound
//!
//! \x => x + y
//! Used: {x, y}
//! Bound: {x}
//! Free: {y}  ← This is a closure!
//! ```
//!
//! ### Lambda Classification
//!
//! We distinguish between two kinds of lambdas:
//!
//! 1. **Parameter Lambdas** - From multi-parameter function desugaring
//! 2. **Closure Lambdas** - Actually capture free variables
//!
//! Only **closure lambdas** are lifted; parameter lambdas are preserved.
//!
//! ### Lifting Transformation
//!
//! For a lambda with captures, we:
//!
//! 1. Generate a unique name (e.g., `lambda_0`)
//! 2. Create a function with capture parameters first, then original parameters
//! 3. Replace the lambda with a partial application
//!
//! ## Example Transformations
//!
//! ### Example 1: Simple Closure
//!
//! **Before Lifting:**
//! ```ruskell
//! main = do
//!     a := 1
//!     f := \x => x + a
//!     f(5)
//! end
//! ```
//!
//! **Free Variable Analysis:**
//! ```text
//! Lambda: \x => x + a
//! Free: {a}  ← Captures 'a' from outer scope
//! ```
//!
//! **After Lifting:**
//! ```ruskell
//! lambda_0 a x = x + a
//!
//! main = do
//!     a := 1
//!     f := lambda_0(a)    // Partial application with captured value
//!     f(5)                // Equivalent to: lambda_0(a)(5)
//! end
//! ```
//!
//! ### Example 2: Multi-Parameter Function (No Lifting)
//!
//! **Before Lifting (after desugaring):**
//! ```ruskell
//! add = \x => \y => x + y
//! ```
//!
//! **Free Variable Analysis:**
//! ```text
//! Outer lambda: \x => ...
//!   Free: {}  ← No captures
//! Inner lambda: \y => x + y
//!   Free: {x}  ← But x is a parameter, not a capture!
//! ```
//!
//! **After Lifting:**
//! ```ruskell
//! add = \x => \y => x + y    // Unchanged! These are parameter lambdas
//! ```
//!
//! ### Example 3: Nested Closures
//!
//! **Before Lifting:**
//! ```ruskell
//! main = do
//!     a := 1
//!     b := 2
//!     f := \x => do
//!         g := \y => x + y + a + b
//!         g(10)
//!     end
//!     f(5)
//! end
//! ```
//!
//! **After Lifting:**
//! ```ruskell
//! lambda_1 a b x y = x + y + a + b
//!
//! lambda_0 a b x = do
//!     g := lambda_1(a)(b)(x)
//!     g(10)
//! end
//!
//! main = do
//!     a := 1
//!     b := 2
//!     f := lambda_0(a)(b)
//!     f(5)
//! end
//! ```
//!
//! ## Smart Lambda Lifting
//!
//! This module implements **smart lambda lifting** which:
//!
//! 1. **Preserves parameter structure** - Doesn't lift parameter lambdas from desugaring
//! 2. **Only lifts closures** - Lambdas that actually capture free variables
//! 3. **Maintains semantics** - Partial application has the same behavior as closure
//!
//! ### Why Preserve Parameter Lambdas?
//!
//! Consider:
//! ```ruskell
//! add x y = x + y    // Desugars to: add = \x => \y => x + y
//! ```
//!
//! If we lifted both lambdas:
//! ```ruskell
//! lambda_1 x y = x + y
//! lambda_0 x = lambda_1(x)
//! add = lambda_0
//! ```
//!
//! This is unnecessarily complex! Instead, we recognize that nested lambdas from
//! desugaring are just parameter structure and preserve them.
//!
//! ## Free Variable Calculation
//!
//! Free variables are computed recursively:
//!
//! ### Expressions
//!
//! - **Variables**: `free(x) = {x}` if not global
//! - **Literals**: `free(42) = ∅`
//! - **Lambdas**: `free(\x => e) = free(e) - {x}`
//! - **Applications**: `free(f(x)) = free(f) ∪ free(x)`
//! - **Binary Ops**: `free(x + y) = free(x) ∪ free(y)`
//!
//! ### Blocks
//!
//! For blocks, we track bindings sequentially:
//!
//! ```text
//! do
//!   a := e1
//!   b := e2
//!   e3
//! end
//!
//! free = (free(e1) ∪ (free(e2) - {a}) ∪ (free(e3) - {a, b}))
//! ```
//!
//! Variables bound earlier in the block aren't free in later statements.
//!
//! ## Global Functions
//!
//! We don't treat global function names as free variables:
//!
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//! ```
//!
//! Even though `factorial` appears in its own body, it's **not** a free variable
//! because it's a global function name.
//!
//! ## Implementation Details
//!
//! The lifting algorithm uses a context (`LiftContext`) that tracks:
//!
//! - `globals`: Set of global function names (don't capture these)
//! - `lifted_functions`: Accumulates extracted lambdas
//! - `lambda_counter`: Generates unique names for lifted lambdas
//!
//! ## Related Modules
//!
//! - [`crate::desugar`] - Runs before lambda lifting
//! - [`crate::types`] - Type checks lifted code
//! - [`crate::core`] - Core AST definitions

use std::collections::HashSet;

use crate::core::*;

/// Context for collecting lifted lambdas during traversal
struct LiftContext {
    /// Global function names (don't treat as captures)
    globals: HashSet<String>,
    /// Lifted lambdas collected during traversal
    lifted_functions: Vec<CoreFunction<()>>,
    /// Counter for generating unique lambda names
    lambda_counter: usize,
}

impl LiftContext {
    fn new(globals: HashSet<String>) -> Self {
        Self {
            globals,
            lifted_functions: Vec::new(),
            lambda_counter: 0,
        }
    }

    /// Generate a unique name for a lifted lambda
    fn generate_lambda_name(&mut self) -> String {
        let id = self.lambda_counter;
        self.lambda_counter += 1;
        format!("lambda_{}", id)
    }
}

/// Lift all lambdas in a program to top-level functions
pub fn lift_program(program: CoreProgram<()>) -> CoreProgram<()> {
    // Collect all global function names
    let mut global_names: HashSet<String> = HashSet::new();
    global_names.insert(program.main.name.value.clone());
    for func in &program.functions {
        global_names.insert(func.name.value.clone());
    }

    let mut ctx = LiftContext::new(global_names);

    // Lift main function
    let main = lift_function(&program.main, &mut ctx);

    // Lift other functions
    let mut functions: Vec<CoreFunction<()>> = program
        .functions
        .iter()
        .map(|f| lift_function(f, &mut ctx))
        .collect();

    // Add extracted lambdas to the function list
    functions.extend(ctx.lifted_functions);

    CoreProgram { main, functions }
}

/// Lift a function, extracting any lambdas in its body
fn lift_function(func: &CoreFunction<()>, ctx: &mut LiftContext) -> CoreFunction<()> {
    CoreFunction {
        name: func.name.clone(),
        lambda: lift_function_lambda(&func.lambda, ctx),
    }
}

/// Lift a function's lambda, treating nested parameter lambdas specially
fn lift_function_lambda(lambda: &CoreLambda<()>, ctx: &mut LiftContext) -> CoreLambda<()> {
    // For function lambdas, we recursively preserve the parameter structure
    // but extract any lambdas in the body
    match &lambda.body {
        CoreLambdaBody::Expression(expr) => {
            // If the body is another lambda, it's part of the parameter list (from desugaring)
            // Don't extract it, but recursively process it
            if matches!(expr.as_ref(), CoreExpr::Lambda(_)) {
                let lifted_body = match expr.as_ref() {
                    CoreExpr::Lambda(inner_lambda) => {
                        CoreExpr::Lambda(lift_function_lambda(inner_lambda, ctx))
                    }
                    _ => unreachable!(),
                };
                CoreLambda {
                    param: lambda.param.clone(),
                    body: CoreLambdaBody::Expression(Box::new(lifted_body)),
                    position: lambda.position.clone(),
                    info: (),
                }
            } else {
                // Body is not a lambda - extract any lambdas within it
                CoreLambda {
                    param: lambda.param.clone(),
                    body: CoreLambdaBody::Expression(Box::new(lift_expr(expr.as_ref(), ctx))),
                    position: lambda.position.clone(),
                    info: (),
                }
            }
        }
        CoreLambdaBody::Block(stmts) => CoreLambda {
            param: lambda.param.clone(),
            body: CoreLambdaBody::Block(stmts.iter().map(|s| lift_statement(s, ctx)).collect()),
            position: lambda.position.clone(),
            info: (),
        },
    }
}

/// Compute free variables in a lambda, excluding globals
fn free_vars_lambda(lambda: &CoreLambda<()>, globals: &HashSet<String>) -> HashSet<String> {
    let body_free = free_vars_body(&lambda.body, globals);
    let param_name = match &lambda.param {
        CoreLambdaParam::Ident(id) => Some(id.value.clone()),
        CoreLambdaParam::Unit(_) => None,
    };

    // Remove the parameter from free vars (it's bound by lambda)
    let mut result = body_free;
    if let Some(name) = param_name {
        result.remove(&name);
    }
    result
}

/// Compute free variables in a lambda body, excluding globals
fn free_vars_body(body: &CoreLambdaBody<()>, globals: &HashSet<String>) -> HashSet<String> {
    match body {
        CoreLambdaBody::Expression(expr) => free_vars_expr(expr, globals),
        CoreLambdaBody::Block(stmts) => free_vars_block(stmts, globals),
    }
}

/// Compute free variables in a block of statements, excluding globals
fn free_vars_block(stmts: &[CoreStatement<()>], globals: &HashSet<String>) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut bound = HashSet::new();

    for stmt in stmts {
        match stmt {
            CoreStatement::Assignment(assign) => {
                // RHS free vars (excluding already bound vars and globals)
                let mut rhs_free = free_vars_expr(&assign.value, globals);
                rhs_free.retain(|v| !bound.contains(v));
                result.extend(rhs_free);

                // LHS binds a new variable
                bound.insert(assign.name.value.clone());
            }
            CoreStatement::Expression(expr) => {
                let mut expr_free = free_vars_expr(expr, globals);
                expr_free.retain(|v| !bound.contains(v));
                result.extend(expr_free);
            }
        }
    }

    result
}

/// Compute free variables in a Core expression, excluding globals
fn free_vars_expr(expr: &CoreExpr<()>, globals: &HashSet<String>) -> HashSet<String> {
    match expr {
        CoreExpr::Ident(id) => {
            // Don't treat global functions as free variables
            if globals.contains(&id.value) {
                HashSet::new()
            } else {
                let mut set = HashSet::new();
                set.insert(id.value.clone());
                set
            }
        }
        CoreExpr::Lambda(lambda) => free_vars_lambda(lambda, globals),
        CoreExpr::FunctionCall(call) => {
            let mut result = free_vars_expr(&call.func, globals);
            result.extend(free_vars_expr(&call.arg, globals));
            result
        }
        CoreExpr::BinaryOp(binop) => {
            let mut result = free_vars_expr(&binop.left, globals);
            result.extend(free_vars_expr(&binop.right, globals));
            result
        }
        CoreExpr::UnaryOp(unop) => free_vars_expr(&unop.operand, globals),
        CoreExpr::IfThenElse(if_expr) => {
            let mut result = free_vars_expr(&if_expr.condition, globals);
            result.extend(free_vars_expr(&if_expr.then_expr, globals));
            result.extend(free_vars_expr(&if_expr.else_expr, globals));
            result
        }
        CoreExpr::List(list) => {
            let mut result = HashSet::new();
            for elem in &list.elements {
                result.extend(free_vars_expr(elem, globals));
            }
            result
        }
        // Literals have no free vars
        CoreExpr::Unit(_) | CoreExpr::Integer(_) | CoreExpr::String(_) | CoreExpr::Boolean(_) => {
            HashSet::new()
        }
    }
}

/// Lift all lambdas in a statement
fn lift_statement(stmt: &CoreStatement<()>, ctx: &mut LiftContext) -> CoreStatement<()> {
    match stmt {
        CoreStatement::Assignment(assign) => CoreStatement::Assignment(CoreAssignment {
            name: assign.name.clone(),
            value: Box::new(lift_expr(&assign.value, ctx)),
            position: assign.position.clone(),
            info: (),
        }),
        CoreStatement::Expression(expr) => CoreStatement::Expression(lift_expr(expr, ctx)),
    }
}

/// Recursively lift all lambdas in an expression, extracting them to standalone functions
fn lift_expr(expr: &CoreExpr<()>, ctx: &mut LiftContext) -> CoreExpr<()> {
    match expr {
        CoreExpr::Lambda(lambda) => {
            // Extract this lambda to a standalone function
            extract_lambda(lambda, ctx)
        }
        CoreExpr::FunctionCall(call) => CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(lift_expr(&call.func, ctx)),
            arg: Box::new(lift_expr(&call.arg, ctx)),
            position: call.position.clone(),
            info: (),
        }),
        CoreExpr::BinaryOp(binop) => CoreExpr::BinaryOp(CoreBinaryOp {
            op: binop.op,
            left: Box::new(lift_expr(&binop.left, ctx)),
            right: Box::new(lift_expr(&binop.right, ctx)),
            position: binop.position.clone(),
            info: (),
        }),
        CoreExpr::UnaryOp(unop) => CoreExpr::UnaryOp(CoreUnaryOp {
            op: unop.op,
            operand: Box::new(lift_expr(&unop.operand, ctx)),
            position: unop.position.clone(),
            info: (),
        }),
        CoreExpr::IfThenElse(if_expr) => CoreExpr::IfThenElse(CoreIfThenElse {
            condition: Box::new(lift_expr(&if_expr.condition, ctx)),
            then_expr: Box::new(lift_expr(&if_expr.then_expr, ctx)),
            else_expr: Box::new(lift_expr(&if_expr.else_expr, ctx)),
            position: if_expr.position.clone(),
            info: (),
        }),
        CoreExpr::List(list) => CoreExpr::List(CoreList {
            elements: list.elements.iter().map(|e| lift_expr(e, ctx)).collect(),
            position: list.position.clone(),
            info: (),
        }),
        // Literals and identifiers remain unchanged
        CoreExpr::Unit(_) | CoreExpr::Integer(_) | CoreExpr::String(_) | CoreExpr::Boolean(_) => {
            expr.clone()
        }
        CoreExpr::Ident(_) => expr.clone(),
    }
}

/// Extract a lambda to a standalone top-level function
fn extract_lambda(lambda: &CoreLambda<()>, ctx: &mut LiftContext) -> CoreExpr<()> {
    // Compute free variables (captures)
    let free = free_vars_lambda(lambda, &ctx.globals);

    // Generate a unique name for this lambda
    let lambda_name = ctx.generate_lambda_name();

    if free.is_empty() {
        // No captures - create a simple function
        let new_function = CoreFunction {
            name: CoreIdent {
                value: lambda_name.clone(),
                position: lambda.position.clone(),
                info: (),
            },
            lambda: lift_lambda_body(lambda, ctx),
        };

        ctx.lifted_functions.push(new_function);

        // Replace with reference to the new function
        CoreExpr::Ident(CoreIdent {
            value: lambda_name,
            position: lambda.position.clone(),
            info: (),
        })
    } else {
        // Has captures - create function with capture parameters, then partial application
        let mut captured: Vec<String> = free.into_iter().collect();
        captured.sort();

        // Build the new function with captures as initial parameters
        let lifted_lambda = build_lifted_function_lambda(&captured, lambda, ctx);

        let new_function = CoreFunction {
            name: CoreIdent {
                value: lambda_name.clone(),
                position: lambda.position.clone(),
                info: (),
            },
            lambda: lifted_lambda,
        };

        ctx.lifted_functions.push(new_function);

        // Replace with call to the function, partially applied with captured values
        let mut result = CoreExpr::Ident(CoreIdent {
            value: lambda_name,
            position: lambda.position.clone(),
            info: (),
        });

        for cap_var in &captured {
            result = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(result),
                arg: Box::new(CoreExpr::Ident(CoreIdent {
                    value: cap_var.clone(),
                    position: lambda.position.clone(),
                    info: (),
                })),
                position: lambda.position.clone(),
                info: (),
            });
        }

        result
    }
}

/// Lift the body of a lambda (recursively lift any nested lambdas)
fn lift_lambda_body(lambda: &CoreLambda<()>, ctx: &mut LiftContext) -> CoreLambda<()> {
    CoreLambda {
        param: lambda.param.clone(),
        body: lift_lambda_body_content(&lambda.body, ctx),
        position: lambda.position.clone(),
        info: (),
    }
}

/// Lift the content of a lambda body
fn lift_lambda_body_content(
    body: &CoreLambdaBody<()>,
    ctx: &mut LiftContext,
) -> CoreLambdaBody<()> {
    match body {
        CoreLambdaBody::Expression(expr) => {
            CoreLambdaBody::Expression(Box::new(lift_expr(expr, ctx)))
        }
        CoreLambdaBody::Block(stmts) => {
            CoreLambdaBody::Block(stmts.iter().map(|s| lift_statement(s, ctx)).collect())
        }
    }
}

/// Build a lambda for a lifted function with capture parameters prepended
fn build_lifted_function_lambda(
    captured: &[String],
    original: &CoreLambda<()>,
    ctx: &mut LiftContext,
) -> CoreLambda<()> {
    if captured.is_empty() {
        return lift_lambda_body(original, ctx);
    }

    let position = original.position.clone();

    // Build innermost: original lambda with lifted body
    let mut inner = lift_lambda_body(original, ctx);

    // Wrap with captured params (in reverse order to get correct nesting)
    for cap_var in captured.iter().rev() {
        inner = CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: cap_var.clone(),
                position: position.clone(),
                info: (),
            }),
            body: CoreLambdaBody::Expression(Box::new(CoreExpr::Lambda(inner))),
            position: position.clone(),
            info: (),
        };
    }

    inner
}

#[cfg(test)]
mod tests {
    use super::*;
    use lachs::Span;

    fn make_ident(name: &str) -> CoreIdent<()> {
        CoreIdent {
            value: name.to_string(),
            position: Span::default(),
            info: (),
        }
    }

    fn make_ident_expr(name: &str) -> CoreExpr<()> {
        CoreExpr::Ident(make_ident(name))
    }

    fn make_int(val: i128) -> CoreExpr<()> {
        CoreExpr::Integer(CoreInteger {
            value: val,
            position: Span::default(),
            info: (),
        })
    }

    #[test]
    fn test_free_vars_ident() {
        let expr = make_ident_expr("x");
        let globals = HashSet::new();
        let free = free_vars_expr(&expr, &globals);
        assert_eq!(free.len(), 1);
        assert!(free.contains("x"));
    }

    #[test]
    fn test_free_vars_literal() {
        let expr = make_int(42);
        let globals = HashSet::new();
        let free = free_vars_expr(&expr, &globals);
        assert!(free.is_empty());
    }

    #[test]
    fn test_free_vars_lambda_no_capture() {
        // \x => x
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("x")),
            body: CoreLambdaBody::Expression(Box::new(make_ident_expr("x"))),
            position: Span::default(),
            info: (),
        };
        let globals = HashSet::new();
        let free = free_vars_lambda(&lambda, &globals);
        assert!(free.is_empty());
    }

    #[test]
    fn test_free_vars_lambda_one_capture() {
        // \y => x + y
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("y")),
            body: CoreLambdaBody::Expression(Box::new(CoreExpr::BinaryOp(CoreBinaryOp {
                op: crate::ast::expression::BinOpKind::Add,
                left: Box::new(make_ident_expr("x")),
                right: Box::new(make_ident_expr("y")),
                position: Span::default(),
                info: (),
            }))),
            position: Span::default(),
            info: (),
        };
        let globals = HashSet::new();
        let free = free_vars_lambda(&lambda, &globals);
        assert_eq!(free.len(), 1);
        assert!(free.contains("x"));
    }

    #[test]
    fn test_free_vars_lambda_multiple_captures() {
        // \z => x + y + z
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("z")),
            body: CoreLambdaBody::Expression(Box::new(CoreExpr::BinaryOp(CoreBinaryOp {
                op: crate::ast::expression::BinOpKind::Add,
                left: Box::new(CoreExpr::BinaryOp(CoreBinaryOp {
                    op: crate::ast::expression::BinOpKind::Add,
                    left: Box::new(make_ident_expr("x")),
                    right: Box::new(make_ident_expr("y")),
                    position: Span::default(),
                    info: (),
                })),
                right: Box::new(make_ident_expr("z")),
                position: Span::default(),
                info: (),
            }))),
            position: Span::default(),
            info: (),
        };
        let globals = HashSet::new();
        let free = free_vars_lambda(&lambda, &globals);
        assert_eq!(free.len(), 2);
        assert!(free.contains("x"));
        assert!(free.contains("y"));
    }

    #[test]
    fn test_lambda_extraction_no_capture() {
        // Simple lambda with no captures
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("x")),
            body: CoreLambdaBody::Expression(Box::new(make_ident_expr("x"))),
            position: Span::default(),
            info: (),
        };

        let mut ctx = LiftContext::new(HashSet::new());
        let result = extract_lambda(&lambda, &mut ctx);

        // Should create lambda_0 and return reference to it
        assert_eq!(ctx.lifted_functions.len(), 1);
        assert_eq!(ctx.lifted_functions[0].name.value, "lambda_0");

        // Result should be identifier
        match result {
            CoreExpr::Ident(id) => assert_eq!(id.value, "lambda_0"),
            _ => panic!("Expected Ident"),
        }
    }

    #[test]
    fn test_lambda_extraction_with_capture() {
        // Lambda with one capture: \y => x + y
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("y")),
            body: CoreLambdaBody::Expression(Box::new(CoreExpr::BinaryOp(CoreBinaryOp {
                op: crate::ast::expression::BinOpKind::Add,
                left: Box::new(make_ident_expr("x")),
                right: Box::new(make_ident_expr("y")),
                position: Span::default(),
                info: (),
            }))),
            position: Span::default(),
            info: (),
        };

        let mut ctx = LiftContext::new(HashSet::new());
        let result = extract_lambda(&lambda, &mut ctx);

        // Should create lambda_0 with signature: lambda_0 x y = x + y
        assert_eq!(ctx.lifted_functions.len(), 1);
        assert_eq!(ctx.lifted_functions[0].name.value, "lambda_0");

        // Result should be lambda_0(x)
        match result {
            CoreExpr::FunctionCall(call) => {
                match *call.func {
                    CoreExpr::Ident(id) => assert_eq!(id.value, "lambda_0"),
                    _ => panic!("Expected Ident in func"),
                }
                match *call.arg {
                    CoreExpr::Ident(id) => assert_eq!(id.value, "x"),
                    _ => panic!("Expected Ident in arg"),
                }
            }
            _ => panic!("Expected FunctionCall"),
        }
    }
}
