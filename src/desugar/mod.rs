//! # Desugaring - Surface AST to Core AST Transformation
//!
//! This module implements the **desugaring phase** of the Ruskell compiler pipeline,
//! transforming the rich, user-friendly Surface AST into the simpler Core AST that is
//! easier to type check and interpret.
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → [DESUGARING] → Core AST → Type Checker → Interpreter
//! ```
//!
//! ## What is Desugaring?
//!
//! Desugaring is the process of translating "syntactic sugar" (convenient but complex
//! language features) into simpler, more primitive constructs. This simplifies later
//! compiler stages by reducing the number of language constructs they need to handle.
//!
//! ## Transformations Performed
//!
//! ### 1. Multi-Parameter Lambdas → Nested Single-Parameter Lambdas
//!
//! **Surface Syntax:**
//! ```ruskell
//! \x, y, z => x + y + z
//! ```
//!
//! **Desugared to Core:**
//! ```ruskell
//! \x => \y => \z => x + y + z
//! ```
//!
//! This is called **currying** and allows all lambdas in Core AST to have exactly one
//! parameter, simplifying type checking and evaluation.
//!
//! ### 2. Multi-Argument Function Calls → Nested Single-Argument Calls
//!
//! **Surface Syntax:**
//! ```ruskell
//! add(1, 2, 3)
//! ```
//!
//! **Desugared to Core:**
//! ```ruskell
//! add(1)(2)(3)
//! ```
//!
//! This matches the curried lambda structure and ensures all function calls in Core AST
//! pass exactly one argument.
//!
//! ### 3. Multi-Clause Functions → Pattern Matching with If-Then-Else
//!
//! **Surface Syntax:**
//! ```ruskell
//! factorial 0 = 1
//! factorial n = n * factorial(n - 1)
//! ```
//!
//! **Desugared to Core:**
//! ```ruskell
//! factorial = \__arg0 =>
//!   if __arg0 == 0
//!   then 1
//!   else (\n => n * factorial(n - 1))(__arg0)
//! ```
//!
//! Pattern matching on function arguments is converted to equality checks and variable
//! bindings using lambda application.
//!
//! ### 4. Match Expressions → Nested If-Then-Else
//!
//! **Surface Syntax:**
//! ```ruskell
//! match value of
//!   0 => "zero"
//!   n => "other"
//! end
//! ```
//!
//! **Desugared to Core:**
//! ```ruskell
//! if value == 0
//! then "zero"
//! else (\n => "other")(value)
//! ```
//!
//! ## Key Functions
//!
//! - [`desugar_program()`] - Entry point for desugaring a complete program
//!
//! The module also contains internal helper functions for desugaring lambdas,
//! function calls, and multi-clause functions.
//!
//! ## Design Rationale
//!
//! By desugaring to a simpler Core AST:
//! 1. **Type Checking is Simpler** - Only need to handle single-param lambdas and single-arg calls
//! 2. **Interpreter is Simpler** - Fewer expression types to evaluate
//! 3. **Optimizations are Easier** - Work on uniform structure
//! 4. **Language Can Evolve** - Add surface features without changing core semantics
//!
//! ## Related Modules
//!
//! - [`crate::ast`] - Surface AST definitions (input to this module)
//! - [`crate::core`] - Core AST definitions (output of this module)
//! - [`lift`] - Lambda lifting pass (makes closures explicit)
//! - [`erase`] - Type erasure (converts Core back to Surface for interpreter)

pub mod erase;
pub mod lift;

use crate::ast;
use crate::core::*;

/// Desugar a complete program from Surface AST to Core AST.
///
/// This is the main entry point for the desugaring phase. It transforms a parsed
/// program with rich syntax into a simpler core representation.
///
/// # Arguments
///
/// * `program` - A Surface AST program (from the parser)
///
/// # Returns
///
/// A Core AST program with desugared constructs
///
/// # Examples
///
/// ```text
/// Surface: add x y = x + y
/// Core:    add = \x => \y => x + y
/// ```
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

/// Desugar multi-clause function to single function with pattern matching.
///
/// Multi-clause functions allow pattern matching directly on function parameters,
/// a common pattern in functional languages. This function converts them to a
/// single-clause function with nested if-then-else expressions for pattern checking.
///
/// # How It Works
///
/// 1. **Extract parameters**: All clauses must have the same number of patterns
/// 2. **Generate argument names**: Create fresh names like `__arg0`, `__arg1`, etc.
/// 3. **Build pattern checks**: For each clause, check if all patterns match
/// 4. **Create bindings**: Bind pattern variables using lambda application
/// 5. **Chain with if-then-else**: Try each clause sequentially
///
/// # Pattern Types Handled
///
/// - **Literal patterns** (`0`, `"hello"`, `true`): Checked with equality (`==`)
/// - **Variable patterns** (`n`, `x`): Always match, bind the value to the variable
/// - **Wildcard patterns** (`_`): Always match, don't bind anything
/// - **Unit pattern** (`()`): Matches only unit value
///
/// # Example Transformation
///
/// **Input (Surface AST):**
/// ```ruskell
/// factorial 0 = 1
/// factorial n = n * factorial(n - 1)
/// ```
///
/// **Output (Core AST):**
/// ```ruskell
/// factorial = \__arg0 =>
///   if __arg0 == 0
///   then 1
///   else (\n => n * factorial(n - 1))(__arg0)
/// ```
///
/// # Arguments
///
/// * `name` - The function name
/// * `clauses` - List of pattern clauses (must have same number of patterns)
///
/// # Panics
///
/// - If clauses is empty
/// - If clauses have different numbers of patterns
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
/// Processes clauses sequentially, checking all parameters of each clause before trying the next.
fn build_nested_match(
    clauses: Vec<ast::pattern::FunctionClause<()>>,
    param_names: &[String],
    param_index: usize,
    position: lachs::Span,
) -> CoreExpr<()> {
    if param_index >= param_names.len() {
        panic!("Parameter index out of bounds");
    }

    // Build clause-by-clause matching
    build_clause_by_clause(clauses, param_names, position)
}

/// Build pattern matching by processing clauses sequentially
/// Each clause becomes: if (all patterns match) then body else (try next clause)
fn build_clause_by_clause(
    clauses: Vec<ast::pattern::FunctionClause<()>>,
    param_names: &[String],
    position: lachs::Span,
) -> CoreExpr<()> {
    if clauses.is_empty() {
        // No clauses left - this shouldn't happen in well-formed programs
        return CoreExpr::Unit(CoreUnit { position, info: () });
    }

    let mut clauses_iter = clauses.into_iter();
    let first_clause = clauses_iter.next().unwrap();
    let remaining_clauses: Vec<_> = clauses_iter.collect();

    // Build the condition and body for the first clause
    let (condition, bindings) =
        build_clause_condition_and_bindings(&first_clause.patterns, param_names, position.clone());

    // Build the clause body with variable bindings applied
    let body = clause_body_to_expression(first_clause.body, first_clause.position.clone());
    let body_with_bindings = apply_bindings(bindings, desugar_expr(body), position.clone());

    // Build the else branch (remaining clauses)
    let else_branch = if remaining_clauses.is_empty() {
        // No more clauses - call __MATCH_FAILURE__ to indicate pattern match failure
        CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(CoreExpr::Ident(CoreIdent {
                value: "__MATCH_FAILURE__".to_string(),
                position: position.clone(),
                info: (),
            })),
            arg: Box::new(CoreExpr::Unit(CoreUnit {
                position: position.clone(),
                info: (),
            })),
            position: position.clone(),
            info: (),
        })
    } else {
        build_clause_by_clause(remaining_clauses, param_names, position.clone())
    };

    // Combine into if-then-else (or just return body if condition is always true)
    match condition {
        Some(cond) => CoreExpr::IfThenElse(CoreIfThenElse {
            condition: Box::new(cond),
            then_expr: Box::new(body_with_bindings),
            else_expr: Box::new(else_branch),
            position,
            info: (),
        }),
        None => {
            // All patterns are variables/wildcards - always matches, no condition needed
            body_with_bindings
        }
    }
}

/// Build the condition that checks if all patterns in a clause match
/// Returns (optional condition, variable bindings)
/// If all patterns are variables/wildcards, condition is None (always matches)
fn build_clause_condition_and_bindings(
    patterns: &[ast::pattern::Pattern<()>],
    param_names: &[String],
    position: lachs::Span,
) -> (Option<CoreExpr<()>>, Vec<(String, String)>) {
    let mut conditions = Vec::new();
    let mut bindings = Vec::new();

    for (pattern, param_name) in patterns.iter().zip(param_names.iter()) {
        let (pattern_conds, pattern_binds) =
            build_clause_condition_and_bindings_single(pattern, param_name, position.clone());

        if let Some(cond) = pattern_conds {
            conditions.push(cond);
        }
        bindings.extend(pattern_binds);
    }

    // Combine all conditions with AND
    let combined_condition = if conditions.is_empty() {
        None
    } else if conditions.len() == 1 {
        Some(conditions.into_iter().next().unwrap())
    } else {
        Some(
            conditions
                .into_iter()
                .reduce(|acc, cond| {
                    CoreExpr::BinaryOp(CoreBinaryOp {
                        op: ast::expression::BinOpKind::And,
                        left: Box::new(acc),
                        right: Box::new(cond),
                        position: position.clone(),
                        info: (),
                    })
                })
                .unwrap(),
        )
    };

    (combined_condition, bindings)
}

/// Build condition and bindings for a single pattern
/// Returns (optional condition, variable bindings)
fn build_clause_condition_and_bindings_single(
    pattern: &ast::pattern::Pattern<()>,
    param_name: &str,
    position: lachs::Span,
) -> (Option<CoreExpr<()>>, Vec<(String, String)>) {
    let mut conditions = Vec::new();
    let mut bindings = Vec::new();

    match pattern {
        ast::pattern::Pattern::Literal(lit) => {
            // Create equality check for literal pattern
            let scrutinee = CoreExpr::Ident(CoreIdent {
                value: param_name.to_string(),
                position: position.clone(),
                info: (),
            });

            let literal_expr = match lit {
                ast::pattern::LiteralPattern::Integer(val, pos, _) => {
                    CoreExpr::Integer(CoreInteger {
                        value: *val,
                        position: pos.clone(),
                        info: (),
                    })
                }
                ast::pattern::LiteralPattern::String(val, pos, _) => CoreExpr::String(CoreString {
                    value: val.clone(),
                    position: pos.clone(),
                    info: (),
                }),
                ast::pattern::LiteralPattern::Boolean(val, pos, _) => {
                    CoreExpr::Boolean(CoreBoolean {
                        value: *val,
                        position: pos.clone(),
                        info: (),
                    })
                }
                ast::pattern::LiteralPattern::Unit(pos, _) => CoreExpr::Unit(CoreUnit {
                    position: pos.clone(),
                    info: (),
                }),
                ast::pattern::LiteralPattern::EmptyList(pos, _) => CoreExpr::List(CoreList {
                    elements: vec![],
                    position: pos.clone(),
                    info: (),
                }),
            };

            conditions.push(CoreExpr::BinaryOp(CoreBinaryOp {
                op: ast::expression::BinOpKind::Eq,
                left: Box::new(scrutinee),
                right: Box::new(literal_expr),
                position: position.clone(),
                info: (),
            }));
        }
        ast::pattern::Pattern::Ident(id) => {
            // Variable pattern: bind the parameter value to this name
            bindings.push((id.value.clone(), param_name.to_string()));
        }
        ast::pattern::Pattern::Wildcard(_) => {
            // Wildcard: no condition, no binding
        }
        ast::pattern::Pattern::ListCons(cons_pattern) => {
            // List cons pattern: [head_pat | tail_pat]
            // For multi-clause functions, we only support simple patterns in head/tail.

            // Create the scrutinee expression
            let scrutinee = CoreExpr::Ident(CoreIdent {
                value: param_name.to_string(),
                position: position.clone(),
                info: (),
            });

            // Add condition: !listIsEmpty(param)
            let is_empty_call = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "listIsEmpty".to_string(),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(scrutinee),
                position: position.clone(),
                info: (),
            });

            let not_empty = CoreExpr::UnaryOp(CoreUnaryOp {
                op: ast::expression::UnaryOpKind::Not,
                operand: Box::new(is_empty_call),
                position: position.clone(),
                info: (),
            });

            conditions.push(not_empty);

            // Check if head and tail are simple patterns
            fn is_simple_pattern(pat: &ast::pattern::Pattern<()>) -> bool {
                matches!(
                    pat,
                    ast::pattern::Pattern::Ident(_) | ast::pattern::Pattern::Wildcard(_)
                )
            }

            if !is_simple_pattern(&cons_pattern.head) || !is_simple_pattern(&cons_pattern.tail) {
                panic!(
                    "Multi-clause functions only support simple patterns (identifiers/wildcards) in list cons head/tail.\n\
                         For complex patterns like [1 | xs] or [[x | xs] | rest], use a case expression instead:\n\
                         \n\
                         Example:\n\
                         foo xs = case xs of\n\
                             [1 | rest] => ...\n\
                             _ => ...\n\
                         end"
                );
            }

            // Process head pattern - bind directly to listHead extraction
            match &*cons_pattern.head {
                ast::pattern::Pattern::Ident(id) => {
                    bindings.push((id.value.clone(), format!("__LIST_HEAD__:{}", param_name)));
                }
                ast::pattern::Pattern::Wildcard(_) => {
                    // No binding needed
                }
                _ => unreachable!("Already checked for simple patterns"),
            }

            // Process tail pattern - bind directly to listTail extraction
            match &*cons_pattern.tail {
                ast::pattern::Pattern::Ident(id) => {
                    bindings.push((id.value.clone(), format!("__LIST_TAIL__:{}", param_name)));
                }
                ast::pattern::Pattern::Wildcard(_) => {
                    // No binding needed
                }
                _ => unreachable!("Already checked for simple patterns"),
            }
        }
    }

    // Combine all conditions with AND (for the single pattern)
    let combined_condition = if conditions.is_empty() {
        None
    } else if conditions.len() == 1 {
        Some(conditions.into_iter().next().unwrap())
    } else {
        Some(
            conditions
                .into_iter()
                .reduce(|acc, cond| {
                    CoreExpr::BinaryOp(CoreBinaryOp {
                        op: ast::expression::BinOpKind::And,
                        left: Box::new(acc),
                        right: Box::new(cond),
                        position: position.clone(),
                        info: (),
                    })
                })
                .unwrap(),
        )
    };

    (combined_condition, bindings)
}

/// Apply variable bindings to an expression
/// Wraps the expression in nested lambda applications: (\var1 => \var2 => expr)(arg1)(arg2)
/// Special handling for list extraction bindings marked with __LIST_HEAD__ or __LIST_TAIL__
fn apply_bindings(
    bindings: Vec<(String, String)>,
    expr: CoreExpr<()>,
    position: lachs::Span,
) -> CoreExpr<()> {
    if bindings.is_empty() {
        return expr;
    }

    // Build nested lambdas for all bindings
    let mut lambda_body = expr;
    for (var_name, _) in bindings.iter().rev() {
        lambda_body = CoreExpr::Lambda(CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: var_name.clone(),
                position: position.clone(),
                info: (),
            }),
            body: CoreLambdaBody::Expression(Box::new(lambda_body)),
            position: position.clone(),
            info: (),
        });
    }

    // Apply all arguments
    let mut result = lambda_body;
    for (_, arg_expr_or_var) in bindings.iter() {
        // Check if this is a special list extraction binding
        let arg_expr = if arg_expr_or_var.starts_with("__LIST_HEAD__:") {
            // Extract: listHead(param_name)
            let param_name = &arg_expr_or_var["__LIST_HEAD__:".len()..];
            CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "listHead".to_string(),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::Ident(CoreIdent {
                    value: param_name.to_string(),
                    position: position.clone(),
                    info: (),
                })),
                position: position.clone(),
                info: (),
            })
        } else if arg_expr_or_var.starts_with("__LIST_TAIL__:") {
            // Extract: listTail(param_name)
            let param_name = &arg_expr_or_var["__LIST_TAIL__:".len()..];
            CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "listTail".to_string(),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::Ident(CoreIdent {
                    value: param_name.to_string(),
                    position: position.clone(),
                    info: (),
                })),
                position: position.clone(),
                info: (),
            })
        } else {
            // Normal variable binding
            CoreExpr::Ident(CoreIdent {
                value: arg_expr_or_var.clone(),
                position: position.clone(),
                info: (),
            })
        };

        result = CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(result),
            arg: Box::new(arg_expr),
            position: position.clone(),
            info: (),
        });
    }

    result
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
        Expression::List(l) => CoreExpr::List(CoreList {
            elements: l.elements.into_iter().map(desugar_expr).collect(),
            position: l.position,
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

        // List cons pattern - desugar to isEmpty check + head/tail extraction
        Pattern::ListCons(cons_pattern) => {
            // Desugar [head_pat | tail_pat] to:
            // if !listIsEmpty(scrutinee) then
            //     (\ head_name => (\ tail_name => [nested pattern match])(listTail(scrutinee)))(listHead(scrutinee))
            // else
            //     [continue with remaining arms]

            // Create else branch from remaining arms
            let else_branch = if remaining_arms.is_empty() {
                // No more arms - create a runtime error
                // Call a non-existent builtin to cause a panic
                // This maintains type safety while signaling a match failure
                CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "__MATCH_FAILURE__".to_string(),
                        position: arm.position.clone(),
                        info: (),
                    })),
                    arg: Box::new(CoreExpr::Unit(CoreUnit {
                        position: arm.position.clone(),
                        info: (),
                    })),
                    position: arm.position.clone(),
                    info: (),
                })
            } else {
                desugar_arms(scrutinee, remaining_arms)
            };

            // Create !listIsEmpty(scrutinee) condition
            let is_empty_call = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "listIsEmpty".to_string(),
                    position: cons_pattern.position.clone(),
                    info: (),
                })),
                arg: Box::new(scrutinee.clone()),
                position: cons_pattern.position.clone(),
                info: (),
            });

            let not_empty = CoreExpr::UnaryOp(CoreUnaryOp {
                op: ast::expression::UnaryOpKind::Not,
                operand: Box::new(is_empty_call),
                position: cons_pattern.position.clone(),
                info: (),
            });

            // Generate unique names for head and tail temps
            static PATTERN_COUNTER: std::sync::atomic::AtomicUsize =
                std::sync::atomic::AtomicUsize::new(0);
            let id = PATTERN_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let head_var = format!("__head_{}", id);
            let tail_var = format!("__tail_{}", id);

            // Build the innermost body by matching patterns against the extracted variables
            // Start with tail pattern matching
            let body_with_tail = match_pattern_against_var(
                &cons_pattern.tail,
                &tail_var,
                body.clone(),
                cons_pattern.tail.position().clone(),
            );

            // Then head pattern matching
            let body_with_head = match_pattern_against_var(
                &cons_pattern.head,
                &head_var,
                body_with_tail,
                cons_pattern.head.position().clone(),
            );

            // Now wrap with let bindings in the correct order:
            // let __head = listHead(scrutinee) in
            //   let __tail = listTail(scrutinee) in
            //     body_with_head_and_tail

            // Extract tail: (\__tail => body_with_head_and_tail)(listTail(scrutinee))
            let with_tail_extraction = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Lambda(CoreLambda {
                    param: CoreLambdaParam::Ident(CoreIdent {
                        value: tail_var,
                        position: cons_pattern.tail.position().clone(),
                        info: (),
                    }),
                    body: CoreLambdaBody::Expression(Box::new(body_with_head)),
                    position: cons_pattern.position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "listTail".to_string(),
                        position: cons_pattern.position.clone(),
                        info: (),
                    })),
                    arg: Box::new(scrutinee.clone()),
                    position: cons_pattern.position.clone(),
                    info: (),
                })),
                position: cons_pattern.position.clone(),
                info: (),
            });

            // Extract head: (\__head => with_tail_extraction)(listHead(scrutinee))
            let with_head_extraction = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Lambda(CoreLambda {
                    param: CoreLambdaParam::Ident(CoreIdent {
                        value: head_var,
                        position: cons_pattern.head.position().clone(),
                        info: (),
                    }),
                    body: CoreLambdaBody::Expression(Box::new(with_tail_extraction)),
                    position: cons_pattern.position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "listHead".to_string(),
                        position: cons_pattern.position.clone(),
                        info: (),
                    })),
                    arg: Box::new(scrutinee.clone()),
                    position: cons_pattern.position.clone(),
                    info: (),
                })),
                position: cons_pattern.position.clone(),
                info: (),
            });

            // Wrap in isEmpty check
            CoreExpr::IfThenElse(CoreIfThenElse {
                condition: Box::new(not_empty),
                then_expr: Box::new(with_head_extraction),
                else_expr: Box::new(else_branch),
                position: arm.position,
                info: (),
            })
        }
    }
}

/// Match a pattern against a variable, wrapping the body appropriately
fn match_pattern_against_var(
    pattern: &ast::pattern::Pattern<()>,
    var_name: &str,
    body: CoreExpr<()>,
    position: lachs::Span,
) -> CoreExpr<()> {
    use ast::pattern::Pattern;

    match pattern {
        // Wildcard: no binding, just return body
        Pattern::Wildcard(_) => body,

        // Ident: bind the variable - (\pattern_var => body)(var_name)
        Pattern::Ident(ident) => CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(CoreExpr::Lambda(CoreLambda {
                param: CoreLambdaParam::Ident(CoreIdent {
                    value: ident.value.clone(),
                    position: ident.position.clone(),
                    info: (),
                }),
                body: CoreLambdaBody::Expression(Box::new(body)),
                position: position.clone(),
                info: (),
            })),
            arg: Box::new(CoreExpr::Ident(CoreIdent {
                value: var_name.to_string(),
                position: position.clone(),
                info: (),
            })),
            position,
            info: (),
        }),

        // Literal: check equality - if var_name == literal then body else match failure
        Pattern::Literal(lit) => {
            let var_expr = CoreExpr::Ident(CoreIdent {
                value: var_name.to_string(),
                position: position.clone(),
                info: (),
            });
            let condition = create_equality_check(var_expr, lit.clone(), position.clone());

            // If pattern doesn't match, this is a pattern match failure
            let else_expr = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "__MATCH_FAILURE__".to_string(),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::Unit(CoreUnit {
                    position: position.clone(),
                    info: (),
                })),
                position: position.clone(),
                info: (),
            });

            CoreExpr::IfThenElse(CoreIfThenElse {
                condition: Box::new(condition),
                then_expr: Box::new(body),
                else_expr: Box::new(else_expr),
                position,
                info: (),
            })
        }

        // Nested ListCons: recursively desugar
        Pattern::ListCons(cons) => {
            // Generate fresh names for nested head/tail
            static NESTED_COUNTER: std::sync::atomic::AtomicUsize =
                std::sync::atomic::AtomicUsize::new(1000);
            let id = NESTED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let nested_head_var = format!("__nested_head_{}", id);
            let nested_tail_var = format!("__nested_tail_{}", id);

            // Match tail pattern
            let mut result = match_pattern_against_var(
                &cons.tail,
                &nested_tail_var,
                body,
                cons.tail.position().clone(),
            );

            // Match head pattern
            result = match_pattern_against_var(
                &cons.head,
                &nested_head_var,
                result,
                cons.head.position().clone(),
            );

            // Wrap with isEmpty check and let bindings
            let var_expr = CoreExpr::Ident(CoreIdent {
                value: var_name.to_string(),
                position: position.clone(),
                info: (),
            });

            let is_empty = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Ident(CoreIdent {
                    value: "listIsEmpty".to_string(),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(var_expr.clone()),
                position: position.clone(),
                info: (),
            });

            let not_empty = CoreExpr::UnaryOp(CoreUnaryOp {
                op: ast::expression::UnaryOpKind::Not,
                operand: Box::new(is_empty),
                position: position.clone(),
                info: (),
            });

            // Extract tail
            let with_tail = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Lambda(CoreLambda {
                    param: CoreLambdaParam::Ident(CoreIdent {
                        value: nested_tail_var,
                        position: cons.tail.position().clone(),
                        info: (),
                    }),
                    body: CoreLambdaBody::Expression(Box::new(result)),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "listTail".to_string(),
                        position: position.clone(),
                        info: (),
                    })),
                    arg: Box::new(var_expr.clone()),
                    position: position.clone(),
                    info: (),
                })),
                position: position.clone(),
                info: (),
            });

            // Extract head
            let with_head = CoreExpr::FunctionCall(CoreFunctionCall {
                func: Box::new(CoreExpr::Lambda(CoreLambda {
                    param: CoreLambdaParam::Ident(CoreIdent {
                        value: nested_head_var,
                        position: cons.head.position().clone(),
                        info: (),
                    }),
                    body: CoreLambdaBody::Expression(Box::new(with_tail)),
                    position: position.clone(),
                    info: (),
                })),
                arg: Box::new(CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "listHead".to_string(),
                        position: position.clone(),
                        info: (),
                    })),
                    arg: Box::new(var_expr),
                    position: position.clone(),
                    info: (),
                })),
                position: position.clone(),
                info: (),
            });

            // Wrap with isEmpty check
            CoreExpr::IfThenElse(CoreIfThenElse {
                condition: Box::new(not_empty),
                then_expr: Box::new(with_head),
                else_expr: Box::new(CoreExpr::FunctionCall(CoreFunctionCall {
                    func: Box::new(CoreExpr::Ident(CoreIdent {
                        value: "__MATCH_FAILURE__".to_string(),
                        position: position.clone(),
                        info: (),
                    })),
                    arg: Box::new(CoreExpr::Unit(CoreUnit {
                        position: position.clone(),
                        info: (),
                    })),
                    position: position.clone(),
                    info: (),
                })),
                position,
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
        LiteralPattern::EmptyList(pos, _) => CoreExpr::List(CoreList {
            elements: vec![],
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
