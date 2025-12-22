//! Lambda Lifting - Extract closures to standalone top-level functions
//!
//! This module implements smart lambda lifting by extracting lambda expressions
//! that capture free variables to standalone top-level functions, making all
//! captures explicit as parameters.
//!
//! **Key distinction:**
//! - **Parameter lambdas** (from multi-param functions) are preserved
//! - **Closures** (lambdas that capture variables) are extracted
//!
//! Example - multi-parameter function (parameter lambdas preserved):
//! ```ruskell
//! add x y = x + y
//! ```
//! After desugaring: `add = \x => \y => x + y`
//! After lifting: **unchanged** (no captures, just parameter structure)
//!
//! Example - closure (extracted and captures made explicit):
//! ```ruskell
//! main = do
//!     a := 1
//!     f := \x => x + a
//!     f(5)
//! end
//! ```
//!
//! After lifting:
//! ```ruskell
//! lambda_0 a x = x + a
//!
//! main = do
//!     a := 1
//!     f := lambda_0(a)
//!     f(5)
//! end
//! ```

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
