//! Desugaring - Transform surface AST to core AST
//!
//! This module handles desugaring transformations:
//! - Multi-parameter lambdas → nested single-parameter lambdas
//! - Multi-argument function calls → nested single-argument calls
//! - Lambda lifting (closure conversion) → explicit capture parameters

pub mod erase;
pub mod lift;

use crate::ast;
use crate::core::*;

/// Desugar a complete program
pub fn desugar_program(program: ast::Program<()>) -> CoreProgram<()> {
    CoreProgram {
        main: desugar_function_def(program.main),
        functions: program
            .functions
            .into_iter()
            .map(desugar_function_def)
            .collect(),
    }
}

fn desugar_function_def(def: ast::FunctionDef<()>) -> CoreFunction<()> {
    match def {
        ast::FunctionDef::Single(func) => desugar_function(func),
        ast::FunctionDef::Multi { name, clauses } => desugar_multi_clause_function(name, clauses),
    }
}

/// Desugar multi-clause function to single function with match expression
///
/// Example:
/// ```ruskell
/// factorial 0 = 1
/// factorial n = n * factorial(n - 1)
/// ```
///
/// Becomes:
/// ```ruskell
/// factorial = \__arg0 =>
///   case __arg0 of
///     0 => 1
///     n => n * factorial(n - 1)
///   end
/// ```
fn desugar_multi_clause_function(
    name: ast::expression::Ident<()>,
    clauses: Vec<ast::pattern::FunctionClause<()>>,
) -> CoreFunction<()> {
    if clauses.is_empty() {
        panic!("Multi-clause function with no clauses");
    }

    // Get the number of parameters from the first clause
    let num_params = clauses[0].patterns.len();

    // Verify all clauses have the same number of parameters
    for clause in &clauses {
        if clause.patterns.len() != num_params {
            panic!(
                "Function {} has clauses with different numbers of patterns: {} vs {}",
                name.value,
                num_params,
                clause.patterns.len()
            );
        }
    }

    let position = name.position.clone();

    // Build nested lambdas with match expressions
    let body = build_pattern_matching_lambda(clauses, num_params, position.clone());

    CoreFunction {
        name: desugar_ident(name),
        lambda: body,
    }
}

/// Build nested lambdas with pattern matching for multi-clause functions
fn build_pattern_matching_lambda(
    clauses: Vec<ast::pattern::FunctionClause<()>>,
    num_params: usize,
    position: lachs::Span,
) -> CoreLambda<()> {
    if num_params == 0 {
        // No parameters - function takes unit
        // Just use the body of the first (and should be only) clause
        if clauses.len() > 1 {
            panic!("Multi-clause function with no parameters doesn't make sense");
        }

        let clause = clauses.into_iter().next().unwrap();
        return CoreLambda {
            param: CoreLambdaParam::Unit(CoreUnit {
                position: position.clone(),
                info: (),
            }),
            body: match clause.body {
                ast::expression::LambdaBody::Expression(expr) => {
                    CoreLambdaBody::Expression(Box::new(desugar_expr(*expr)))
                }
                ast::expression::LambdaBody::Block(stmts) => {
                    CoreLambdaBody::Block(stmts.into_iter().map(desugar_statement).collect())
                }
            },
            position,
            info: (),
        };
    }

    // Generate fresh parameter names
    let param_names: Vec<String> = (0..num_params).map(|i| format!("__arg{}", i)).collect();

    // Build match arms for each clause
    let match_expr = build_nested_match(clauses, &param_names, 0, position.clone());

    // Wrap in nested lambdas
    build_nested_lambdas(&param_names, match_expr, position)
}

/// Build nested match expressions for multiple parameters
/// For single parameter, creates a simple match. For multiple, creates nested matches.
fn build_nested_match(
    clauses: Vec<ast::pattern::FunctionClause<()>>,
    param_names: &[String],
    param_index: usize,
    position: lachs::Span,
) -> CoreExpr<()> {
    if param_index >= param_names.len() {
        panic!("Parameter index out of bounds");
    }

    // Create scrutinee: the current parameter variable
    let scrutinee = ast::expression::Expression::Ident(ast::expression::Ident {
        value: param_names[param_index].clone(),
        position: position.clone(),
        info: (),
    });

    // Build match arms
    let arms: Vec<ast::pattern::MatchArm<()>> = clauses
        .into_iter()
        .map(|clause| {
            let pattern = clause.patterns[param_index].clone();

            // Build the body, wrapping remaining parameters if needed
            let body = if param_index == param_names.len() - 1 {
                // Last parameter - use the clause body directly
                clause_body_to_expression(clause.body, clause.position.clone())
            } else {
                // Not the last parameter - wrap body in lambdas for remaining pattern parameters
                // Collect remaining patterns and their corresponding argument names
                let remaining_patterns = &clause.patterns[param_index + 1..];
                let remaining_args = &param_names[param_index + 1..];

                let mut body_expr = clause_body_to_expression(clause.body, clause.position.clone());

                // Wrap in lambdas for each remaining pattern parameter (in reverse order)
                for (pat, arg_name) in remaining_patterns.iter().zip(remaining_args.iter()).rev() {
                    body_expr =
                        wrap_pattern_as_lambda(pat.clone(), body_expr, arg_name, position.clone());
                }

                body_expr
            };

            ast::pattern::MatchArm {
                pattern,
                body,
                position: clause.position.clone(),
                info: (),
            }
        })
        .collect();

    // Create the match expression and desugar it
    let match_expr = ast::pattern::Match {
        scrutinee: Box::new(scrutinee),
        arms,
        position: position.clone(),
        info: (),
    };

    desugar_match(match_expr)
}

/// Convert a clause body to an expression
fn clause_body_to_expression(
    body: ast::expression::LambdaBody<()>,
    position: lachs::Span,
) -> ast::expression::Expression<()> {
    match body {
        ast::expression::LambdaBody::Expression(expr) => *expr,
        ast::expression::LambdaBody::Block(stmts) => {
            // Multi-statement block: wrap in immediately-invoked lambda
            // do ... end becomes (\() => do ... end)()
            let lambda = ast::expression::Lambda {
                params: vec![ast::expression::LambdaParam::Unit(ast::expression::Unit {
                    position: position.clone(),
                    info: (),
                })],
                body: ast::expression::LambdaBody::Block(stmts),
                position: position.clone(),
                info: (),
            };

            ast::expression::Expression::FunctionCall(ast::expression::FunctionCall {
                func: Box::new(ast::expression::Expression::Lambda(lambda)),
                args: vec![ast::expression::Expression::Unit(ast::expression::Unit {
                    position: position.clone(),
                    info: (),
                })],
                position: position.clone(),
                info: (),
            })
        }
    }
}

/// Wrap an expression in a lambda that binds a pattern variable, then applies it to the argument
/// For pattern `x` and arg `__arg1`: body becomes `(\x => body)(__arg1)`
fn wrap_pattern_as_lambda(
    pattern: ast::pattern::Pattern<()>,
    body: ast::expression::Expression<()>,
    arg_name: &str,
    position: lachs::Span,
) -> ast::expression::Expression<()> {
    use ast::pattern::Pattern;

    match pattern {
        // Variable pattern: create lambda binding
        Pattern::Ident(ident) => {
            let lambda = ast::expression::Lambda {
                params: vec![ast::expression::LambdaParam::Ident(ident)],
                body: ast::expression::LambdaBody::Expression(Box::new(body)),
                position: position.clone(),
                info: (),
            };

            ast::expression::Expression::FunctionCall(ast::expression::FunctionCall {
                func: Box::new(ast::expression::Expression::Lambda(lambda)),
                args: vec![ast::expression::Expression::Ident(ast::expression::Ident {
                    value: arg_name.to_string(),
                    position: position.clone(),
                    info: (),
                })],
                position: position.clone(),
                info: (),
            })
        }
        // Unit pattern: no binding needed, just return body
        Pattern::Literal(ast::pattern::LiteralPattern::Unit(_, _)) => body,
        // Wildcard pattern: no binding needed, just return body
        Pattern::Wildcard(_) => body,
        // Other literal patterns: these would require a match, but for now just return body
        // (This case shouldn't happen in well-formed multi-param functions)
        Pattern::Literal(_) => body,
    }
}

/// Build nested lambdas: \arg0 => \arg1 => ... => body
fn build_nested_lambdas(
    param_names: &[String],
    body: CoreExpr<()>,
    position: lachs::Span,
) -> CoreLambda<()> {
    if param_names.is_empty() {
        panic!("Cannot build lambda with no parameters");
    }

    if param_names.len() == 1 {
        // Single parameter
        return CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: param_names[0].clone(),
                position: position.clone(),
                info: (),
            }),
            body: CoreLambdaBody::Expression(Box::new(body)),
            position,
            info: (),
        };
    }

    // Multiple parameters: build nested lambdas
    // Start from the innermost lambda (last parameter)
    let mut result = CoreLambda {
        param: CoreLambdaParam::Ident(CoreIdent {
            value: param_names[param_names.len() - 1].clone(),
            position: position.clone(),
            info: (),
        }),
        body: CoreLambdaBody::Expression(Box::new(body)),
        position: position.clone(),
        info: (),
    };

    // Wrap in outer lambdas
    for i in (0..param_names.len() - 1).rev() {
        result = CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: param_names[i].clone(),
                position: position.clone(),
                info: (),
            }),
            body: CoreLambdaBody::Expression(Box::new(CoreExpr::Lambda(result))),
            position: position.clone(),
            info: (),
        };
    }

    result
}

fn desugar_function(func: ast::Function<()>) -> CoreFunction<()> {
    CoreFunction {
        name: desugar_ident(func.name),
        lambda: desugar_lambda(func.lambda),
    }
}

fn desugar_lambda(lambda: ast::expression::Lambda<()>) -> CoreLambda<()> {
    use ast::expression::LambdaBody;

    // If no params, treat as unit parameter
    if lambda.params.is_empty() {
        return CoreLambda {
            param: CoreLambdaParam::Unit(CoreUnit {
                position: lambda.position.clone(),
                info: (),
            }),
            body: match lambda.body {
                LambdaBody::Expression(expr) => {
                    CoreLambdaBody::Expression(Box::new(desugar_expr(*expr)))
                }
                LambdaBody::Block(stmts) => {
                    CoreLambdaBody::Block(stmts.into_iter().map(desugar_statement).collect())
                }
            },
            position: lambda.position,
            info: (),
        };
    }

    // Single parameter - no desugaring needed
    if lambda.params.len() == 1 {
        return CoreLambda {
            param: desugar_lambda_param(lambda.params.into_iter().next().unwrap()),
            body: match lambda.body {
                LambdaBody::Expression(expr) => {
                    CoreLambdaBody::Expression(Box::new(desugar_expr(*expr)))
                }
                LambdaBody::Block(stmts) => {
                    CoreLambdaBody::Block(stmts.into_iter().map(desugar_statement).collect())
                }
            },
            position: lambda.position,
            info: (),
        };
    }

    // Multiple parameters: desugar to nested lambdas
    // \x, y, z => body  becomes  \x => \y => \z => body
    let mut params = lambda.params.into_iter();
    let first_param = params.next().unwrap();
    let rest_params: Vec<_> = params.collect();

    // Build the innermost lambda with the rest of the parameters
    let inner_lambda = ast::expression::Lambda {
        params: rest_params,
        body: lambda.body,
        position: lambda.position.clone(),
        info: (),
    };

    // Recursively desugar the inner lambda
    let desugared_inner = desugar_lambda(inner_lambda);

    // Wrap it in the first parameter
    CoreLambda {
        param: desugar_lambda_param(first_param),
        body: CoreLambdaBody::Expression(Box::new(CoreExpr::Lambda(desugared_inner))),
        position: lambda.position,
        info: (),
    }
}

fn desugar_lambda_param(param: ast::expression::LambdaParam<()>) -> CoreLambdaParam<()> {
    match param {
        ast::expression::LambdaParam::Unit(u) => CoreLambdaParam::Unit(CoreUnit {
            position: u.position,
            info: (),
        }),
        ast::expression::LambdaParam::Ident(id) => CoreLambdaParam::Ident(desugar_ident(id)),
    }
}

fn desugar_expr(expr: ast::expression::Expression<()>) -> CoreExpr<()> {
    use ast::expression::Expression;

    match expr {
        Expression::Unit(u) => CoreExpr::Unit(CoreUnit {
            position: u.position,
            info: (),
        }),
        Expression::Ident(id) => CoreExpr::Ident(desugar_ident(id)),
        Expression::Integer(i) => CoreExpr::Integer(CoreInteger {
            value: i.value,
            position: i.position,
            info: (),
        }),
        Expression::String(s) => CoreExpr::String(CoreString {
            value: s.value,
            position: s.position,
            info: (),
        }),
        Expression::Boolean(b) => CoreExpr::Boolean(CoreBoolean {
            value: b.value,
            position: b.position,
            info: (),
        }),
        Expression::Lambda(lambda) => CoreExpr::Lambda(desugar_lambda(lambda)),
        Expression::FunctionCall(call) => desugar_function_call(call),
        Expression::BinaryOp(binop) => CoreExpr::BinaryOp(CoreBinaryOp {
            op: binop.op,
            left: Box::new(desugar_expr(*binop.left)),
            right: Box::new(desugar_expr(*binop.right)),
            position: binop.position,
            info: (),
        }),
        Expression::UnaryOp(unop) => CoreExpr::UnaryOp(CoreUnaryOp {
            op: unop.op,
            operand: Box::new(desugar_expr(*unop.operand)),
            position: unop.position,
            info: (),
        }),
        Expression::IfThenElse(if_expr) => CoreExpr::IfThenElse(CoreIfThenElse {
            condition: Box::new(desugar_expr(*if_expr.condition)),
            then_expr: Box::new(desugar_expr(*if_expr.then_expr)),
            else_expr: Box::new(desugar_expr(*if_expr.else_expr)),
            position: if_expr.position,
            info: (),
        }),
        Expression::Match(match_expr) => desugar_match(*match_expr),
    }
}

/// Desugar a match expression to nested if-then-else
fn desugar_match(match_expr: ast::pattern::Match<()>) -> CoreExpr<()> {
    let scrutinee = desugar_expr(*match_expr.scrutinee);
    let arms = match_expr.arms;

    // Build the if-then-else chain from the arms
    desugar_arms(&scrutinee, arms)
}

/// Recursively desugar a list of match arms
fn desugar_arms(scrutinee: &CoreExpr<()>, arms: Vec<ast::pattern::MatchArm<()>>) -> CoreExpr<()> {
    if arms.is_empty() {
        // This shouldn't happen if parser enforces at least one arm
        panic!("match expression with no arms");
    }

    let mut arms_iter = arms.into_iter();
    let first_arm = arms_iter.next().unwrap();
    let remaining: Vec<_> = arms_iter.collect();

    desugar_match_arm(scrutinee, first_arm, remaining)
}

/// Desugar a single match arm, potentially with remaining arms
fn desugar_match_arm(
    scrutinee: &CoreExpr<()>,
    arm: ast::pattern::MatchArm<()>,
    remaining_arms: Vec<ast::pattern::MatchArm<()>>,
) -> CoreExpr<()> {
    use ast::pattern::Pattern;

    let body = desugar_expr(arm.body);

    match arm.pattern {
        // Wildcard pattern: always matches, just return the body
        Pattern::Wildcard(_) => body,

        // Variable pattern: bind the scrutinee to the variable name
        Pattern::Ident(ident) => {
            // Create a lambda that binds the pattern variable and returns the body
            // (\x => body)(scrutinee)
            CoreExpr::Lambda(CoreLambda {
                param: CoreLambdaParam::Ident(CoreIdent {
                    value: ident.value.clone(),
                    position: ident.position.clone(),
                    info: (),
                }),
                body: CoreLambdaBody::Expression(Box::new(body)),
                position: arm.position.clone(),
                info: (),
            })
            .applied_to(scrutinee.clone())
        }

        // Literal pattern: compare scrutinee to literal
        Pattern::Literal(lit) => {
            let condition = create_equality_check(scrutinee.clone(), lit, arm.position.clone());

            // If there are remaining arms, create else branch
            let else_branch = if remaining_arms.is_empty() {
                // No more arms - shouldn't happen with wildcard, but create a fallback
                CoreExpr::Unit(CoreUnit {
                    position: arm.position.clone(),
                    info: (),
                })
            } else {
                desugar_arms(scrutinee, remaining_arms)
            };

            CoreExpr::IfThenElse(CoreIfThenElse {
                condition: Box::new(condition),
                then_expr: Box::new(body),
                else_expr: Box::new(else_branch),
                position: arm.position,
                info: (),
            })
        }
    }
}

/// Create an equality check between scrutinee and literal pattern
fn create_equality_check(
    scrutinee: CoreExpr<()>,
    lit: ast::pattern::LiteralPattern<()>,
    position: lachs::Span,
) -> CoreExpr<()> {
    use ast::expression::BinOpKind;
    use ast::pattern::LiteralPattern;

    let literal_expr = match lit {
        LiteralPattern::Integer(val, pos, _) => CoreExpr::Integer(CoreInteger {
            value: val,
            position: pos,
            info: (),
        }),
        LiteralPattern::String(val, pos, _) => CoreExpr::String(CoreString {
            value: val,
            position: pos,
            info: (),
        }),
        LiteralPattern::Boolean(val, pos, _) => CoreExpr::Boolean(CoreBoolean {
            value: val,
            position: pos,
            info: (),
        }),
        LiteralPattern::Unit(pos, _) => CoreExpr::Unit(CoreUnit {
            position: pos,
            info: (),
        }),
    };

    CoreExpr::BinaryOp(CoreBinaryOp {
        op: BinOpKind::Eq,
        left: Box::new(scrutinee),
        right: Box::new(literal_expr),
        position,
        info: (),
    })
}

/// Helper to apply a lambda to an argument (for pattern bindings)
trait AppliedTo {
    fn applied_to(self, arg: CoreExpr<()>) -> CoreExpr<()>;
}

impl AppliedTo for CoreExpr<()> {
    fn applied_to(self, arg: CoreExpr<()>) -> CoreExpr<()> {
        let pos = match &self {
            CoreExpr::Lambda(l) => l.position.clone(),
            _ => lachs::Span::default(),
        };

        CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(self),
            arg: Box::new(arg),
            position: pos,
            info: (),
        })
    }
}

fn desugar_function_call(call: ast::expression::FunctionCall<()>) -> CoreExpr<()> {
    let func = *call.func;
    let args = call.args;
    let position = call.position;

    // No arguments - call with unit
    if args.is_empty() {
        return CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(desugar_expr(func)),
            arg: Box::new(CoreExpr::Unit(CoreUnit {
                position: position.clone(),
                info: (),
            })),
            position,
            info: (),
        });
    }

    // Single argument - no desugaring needed
    if args.len() == 1 {
        return CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(desugar_expr(func)),
            arg: Box::new(desugar_expr(args.into_iter().next().unwrap())),
            position,
            info: (),
        });
    }

    // Multiple arguments: desugar to nested calls
    // f(x, y, z)  becomes  f(x)(y)(z)
    let mut args_iter = args.into_iter();
    let first_arg = args_iter.next().unwrap();

    // Build the first call: f(first_arg)
    let mut result = CoreExpr::FunctionCall(CoreFunctionCall {
        func: Box::new(desugar_expr(func)),
        arg: Box::new(desugar_expr(first_arg)),
        position: position.clone(),
        info: (),
    });

    // Add the rest of the arguments as nested calls
    for arg in args_iter {
        result = CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(result),
            arg: Box::new(desugar_expr(arg)),
            position: position.clone(),
            info: (),
        });
    }

    result
}

fn desugar_statement(stmt: ast::statement::Statement<()>) -> CoreStatement<()> {
    match stmt {
        ast::statement::Statement::Assignment(assign) => {
            CoreStatement::Assignment(CoreAssignment {
                name: desugar_ident(assign.name),
                value: Box::new(desugar_expr(*assign.value)),
                position: assign.position,
                info: (),
            })
        }
        ast::statement::Statement::Expression(expr) => {
            CoreStatement::Expression(desugar_expr(expr))
        }
    }
}

fn desugar_ident(ident: ast::expression::Ident<()>) -> CoreIdent<()> {
    CoreIdent {
        value: ident.value,
        position: ident.position,
        info: (),
    }
}
