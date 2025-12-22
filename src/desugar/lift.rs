//! Lambda Lifting - Transform lambdas with captured variables to explicit parameters
//!
//! This module implements closure conversion by analyzing free variables in lambdas
//! and transforming them to take captured variables as explicit parameters.
//!
//! Example transformation:
//! ```ruskell
//! add x = \y => x + y
//! ```
//!
//! The inner lambda captures `x`. After lifting:
//! ```ruskell
//! add x = \x => \y => x + y
//! ```

use std::collections::HashSet;

use crate::core::*;

/// Lift all lambdas in a program to make captures explicit
pub fn lift_program(program: CoreProgram<()>) -> CoreProgram<()> {
    CoreProgram {
        main: lift_function(program.main),
        functions: program.functions.into_iter().map(lift_function).collect(),
    }
}

/// Lift a function definition
fn lift_function(func: CoreFunction<()>) -> CoreFunction<()> {
    CoreFunction {
        name: func.name,
        lambda: lift_lambda(func.lambda),
    }
}

/// Compute free variables in a lambda
fn free_vars_lambda(lambda: &CoreLambda<()>) -> HashSet<String> {
    let body_free = free_vars_body(&lambda.body);
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

/// Compute free variables in a lambda body
fn free_vars_body(body: &CoreLambdaBody<()>) -> HashSet<String> {
    match body {
        CoreLambdaBody::Expression(expr) => free_vars_expr(expr),
        CoreLambdaBody::Block(stmts) => free_vars_block(stmts),
    }
}

/// Compute free variables in a block of statements
fn free_vars_block(stmts: &[CoreStatement<()>]) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut bound = HashSet::new();

    for stmt in stmts {
        match stmt {
            CoreStatement::Assignment(assign) => {
                // RHS free vars (excluding already bound vars)
                let mut rhs_free = free_vars_expr(&assign.value);
                rhs_free.retain(|v| !bound.contains(v));
                result.extend(rhs_free);

                // LHS binds a new variable
                bound.insert(assign.name.value.clone());
            }
            CoreStatement::Expression(expr) => {
                let mut expr_free = free_vars_expr(expr);
                expr_free.retain(|v| !bound.contains(v));
                result.extend(expr_free);
            }
        }
    }

    result
}

/// Compute free variables in a Core expression
fn free_vars_expr(expr: &CoreExpr<()>) -> HashSet<String> {
    match expr {
        CoreExpr::Ident(id) => {
            let mut set = HashSet::new();
            set.insert(id.value.clone());
            set
        }
        CoreExpr::Lambda(lambda) => free_vars_lambda(lambda),
        CoreExpr::FunctionCall(call) => {
            let mut result = free_vars_expr(&call.func);
            result.extend(free_vars_expr(&call.arg));
            result
        }
        CoreExpr::BinaryOp(binop) => {
            let mut result = free_vars_expr(&binop.left);
            result.extend(free_vars_expr(&binop.right));
            result
        }
        CoreExpr::UnaryOp(unop) => free_vars_expr(&unop.operand),
        CoreExpr::IfThenElse(if_expr) => {
            let mut result = free_vars_expr(&if_expr.condition);
            result.extend(free_vars_expr(&if_expr.then_expr));
            result.extend(free_vars_expr(&if_expr.else_expr));
            result
        }
        // Literals have no free vars
        CoreExpr::Unit(_) | CoreExpr::Integer(_) | CoreExpr::String(_) | CoreExpr::Boolean(_) => {
            HashSet::new()
        }
    }
}

/// Lift a lambda: prepend captured variables as parameters
fn lift_lambda(lambda: CoreLambda<()>) -> CoreLambda<()> {
    let free = free_vars_lambda(&lambda);

    if free.is_empty() {
        // No captures - just recursively lift inner expressions
        return lift_lambda_inner(lambda);
    }

    // Sort captured vars for deterministic output
    let mut captured: Vec<String> = free.into_iter().collect();
    captured.sort();

    // Build nested lambdas: \cap1 => \cap2 => \original_param => body
    build_lifted_lambda(captured, lambda)
}

/// Lift inner expressions of a lambda without adding capture parameters
fn lift_lambda_inner(lambda: CoreLambda<()>) -> CoreLambda<()> {
    CoreLambda {
        param: lambda.param,
        body: lift_body(lambda.body),
        position: lambda.position,
        info: lambda.info,
    }
}

/// Build a lambda with captured variables as explicit parameters
fn build_lifted_lambda(captured: Vec<String>, original: CoreLambda<()>) -> CoreLambda<()> {
    if captured.is_empty() {
        return lift_lambda_inner(original);
    }

    let position = original.position.clone();

    // Build innermost: original lambda with lifted body
    let mut inner = CoreLambda {
        param: original.param,
        body: lift_body(original.body),
        position: position.clone(),
        info: (),
    };

    // Wrap with captured params (in reverse order to get correct nesting)
    for cap_var in captured.into_iter().rev() {
        inner = CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: cap_var,
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

/// Lift all lambdas in a lambda body
fn lift_body(body: CoreLambdaBody<()>) -> CoreLambdaBody<()> {
    match body {
        CoreLambdaBody::Expression(expr) => CoreLambdaBody::Expression(Box::new(lift_expr(*expr))),
        CoreLambdaBody::Block(stmts) => {
            CoreLambdaBody::Block(stmts.into_iter().map(lift_statement).collect())
        }
    }
}

/// Lift all lambdas in a statement
fn lift_statement(stmt: CoreStatement<()>) -> CoreStatement<()> {
    match stmt {
        CoreStatement::Assignment(assign) => CoreStatement::Assignment(CoreAssignment {
            name: assign.name,
            value: Box::new(lift_expr(*assign.value)),
            position: assign.position,
            info: (),
        }),
        CoreStatement::Expression(expr) => CoreStatement::Expression(lift_expr(expr)),
    }
}

/// Recursively lift all lambdas in an expression
fn lift_expr(expr: CoreExpr<()>) -> CoreExpr<()> {
    match expr {
        CoreExpr::Lambda(lambda) => CoreExpr::Lambda(lift_lambda(lambda)),
        CoreExpr::FunctionCall(call) => CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(lift_expr(*call.func)),
            arg: Box::new(lift_expr(*call.arg)),
            position: call.position,
            info: (),
        }),
        CoreExpr::BinaryOp(binop) => CoreExpr::BinaryOp(CoreBinaryOp {
            op: binop.op,
            left: Box::new(lift_expr(*binop.left)),
            right: Box::new(lift_expr(*binop.right)),
            position: binop.position,
            info: (),
        }),
        CoreExpr::UnaryOp(unop) => CoreExpr::UnaryOp(CoreUnaryOp {
            op: unop.op,
            operand: Box::new(lift_expr(*unop.operand)),
            position: unop.position,
            info: (),
        }),
        CoreExpr::IfThenElse(if_expr) => CoreExpr::IfThenElse(CoreIfThenElse {
            condition: Box::new(lift_expr(*if_expr.condition)),
            then_expr: Box::new(lift_expr(*if_expr.then_expr)),
            else_expr: Box::new(lift_expr(*if_expr.else_expr)),
            position: if_expr.position,
            info: (),
        }),
        // Literals and identifiers remain unchanged
        CoreExpr::Unit(_) | CoreExpr::Integer(_) | CoreExpr::String(_) | CoreExpr::Boolean(_) => {
            expr
        }
        CoreExpr::Ident(_) => expr,
    }
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
        let free = free_vars_expr(&expr);
        assert_eq!(free.len(), 1);
        assert!(free.contains("x"));
    }

    #[test]
    fn test_free_vars_literal() {
        let expr = make_int(42);
        let free = free_vars_expr(&expr);
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
        let free = free_vars_lambda(&lambda);
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
        let free = free_vars_lambda(&lambda);
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
        let free = free_vars_lambda(&lambda);
        assert_eq!(free.len(), 2);
        assert!(free.contains("x"));
        assert!(free.contains("y"));
    }

    #[test]
    fn test_lift_lambda_no_capture() {
        // \x => x should remain unchanged
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("x")),
            body: CoreLambdaBody::Expression(Box::new(make_ident_expr("x"))),
            position: Span::default(),
            info: (),
        };
        let lifted = lift_lambda(lambda.clone());

        // Should be structurally the same
        match lifted.param {
            CoreLambdaParam::Ident(id) => assert_eq!(id.value, "x"),
            _ => panic!("Expected ident param"),
        }
    }

    #[test]
    fn test_lift_lambda_one_capture() {
        // \y => x should become \x => \y => x
        let lambda = CoreLambda {
            param: CoreLambdaParam::Ident(make_ident("y")),
            body: CoreLambdaBody::Expression(Box::new(make_ident_expr("x"))),
            position: Span::default(),
            info: (),
        };
        let lifted = lift_lambda(lambda);

        // Outer lambda should capture x
        match lifted.param {
            CoreLambdaParam::Ident(id) => assert_eq!(id.value, "x"),
            _ => panic!("Expected ident param for capture"),
        }

        // Inner should be the original
        match lifted.body {
            CoreLambdaBody::Expression(expr) => match *expr {
                CoreExpr::Lambda(inner) => match inner.param {
                    CoreLambdaParam::Ident(id) => assert_eq!(id.value, "y"),
                    _ => panic!("Expected ident param"),
                },
                _ => panic!("Expected lambda in body"),
            },
            _ => panic!("Expected expression body"),
        }
    }
}
