//! Desugaring - Transform surface AST to core AST
//!
//! This module handles desugaring transformations:
//! - Multi-parameter lambdas → nested single-parameter lambdas
//! - Multi-argument function calls → nested single-argument calls

mod erase;

pub use erase::erase_program;

use crate::ast;
use crate::core::*;

/// Desugar a complete program
pub fn desugar_program(program: ast::Program<()>) -> CoreProgram<()> {
    CoreProgram {
        main: desugar_function(program.main),
        functions: program
            .functions
            .into_iter()
            .map(desugar_function)
            .collect(),
    }
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
