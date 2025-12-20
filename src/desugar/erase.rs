//! Convert Core AST back to Surface AST (type erasure)
//!
//! This module converts the desugared core AST back to the surface AST
//! so it can be run by the existing interpreter. This is a temporary solution
//! until we have a type-aware interpreter.

use crate::ast;
use crate::core::*;

pub fn erase_program(program: CoreProgram<()>) -> ast::Program<()> {
    ast::Program {
        main: erase_function(program.main),
        functions: program.functions.into_iter().map(erase_function).collect(),
    }
}

fn erase_function(func: CoreFunction<()>) -> ast::Function<()> {
    ast::Function {
        name: erase_ident(func.name),
        lambda: erase_lambda(func.lambda),
    }
}

fn erase_lambda(lambda: CoreLambda<()>) -> ast::expression::Lambda<()> {
    ast::expression::Lambda {
        params: vec![erase_lambda_param(lambda.param)],
        body: erase_lambda_body(lambda.body),
        position: lambda.position,
        info: (),
    }
}

fn erase_lambda_param(param: CoreLambdaParam<()>) -> ast::expression::LambdaParam<()> {
    match param {
        CoreLambdaParam::Unit(u) => ast::expression::LambdaParam::Unit(ast::expression::Unit {
            position: u.position,
            info: (),
        }),
        CoreLambdaParam::Ident(id) => ast::expression::LambdaParam::Ident(erase_ident(id)),
    }
}

fn erase_lambda_body(body: CoreLambdaBody<()>) -> ast::expression::LambdaBody<()> {
    match body {
        CoreLambdaBody::Expression(expr) => {
            ast::expression::LambdaBody::Expression(Box::new(erase_expr(*expr)))
        }
        CoreLambdaBody::Block(stmts) => {
            ast::expression::LambdaBody::Block(stmts.into_iter().map(erase_statement).collect())
        }
    }
}

fn erase_expr(expr: CoreExpr<()>) -> ast::expression::Expression<()> {
    use ast::expression::Expression;

    match expr {
        CoreExpr::Unit(u) => Expression::Unit(ast::expression::Unit {
            position: u.position,
            info: (),
        }),
        CoreExpr::Ident(id) => Expression::Ident(erase_ident(id)),
        CoreExpr::Integer(i) => Expression::Integer(ast::expression::Integer {
            value: i.value,
            position: i.position,
            info: (),
        }),
        CoreExpr::String(s) => Expression::String(ast::expression::StringLiteral {
            value: s.value,
            position: s.position,
            info: (),
        }),
        CoreExpr::Boolean(b) => Expression::Boolean(ast::expression::Boolean {
            value: b.value,
            position: b.position,
            info: (),
        }),
        CoreExpr::Lambda(lambda) => Expression::Lambda(erase_lambda(lambda)),
        CoreExpr::FunctionCall(call) => Expression::FunctionCall(ast::expression::FunctionCall {
            func: Box::new(erase_expr(*call.func)),
            args: vec![erase_expr(*call.arg)],
            position: call.position,
            info: (),
        }),
        CoreExpr::BinaryOp(binop) => Expression::BinaryOp(ast::expression::BinaryOp {
            op: binop.op,
            left: Box::new(erase_expr(*binop.left)),
            right: Box::new(erase_expr(*binop.right)),
            position: binop.position,
            info: (),
        }),
        CoreExpr::UnaryOp(unop) => Expression::UnaryOp(ast::expression::UnaryOp {
            op: unop.op,
            operand: Box::new(erase_expr(*unop.operand)),
            position: unop.position,
            info: (),
        }),
    }
}

fn erase_statement(stmt: CoreStatement<()>) -> ast::statement::Statement<()> {
    match stmt {
        CoreStatement::Assignment(assign) => {
            ast::statement::Statement::Assignment(ast::statement::Assignment {
                name: erase_ident(assign.name),
                value: Box::new(erase_expr(*assign.value)),
                position: assign.position,
                info: (),
            })
        }
        CoreStatement::Expression(expr) => ast::statement::Statement::Expression(erase_expr(expr)),
    }
}

fn erase_ident(ident: CoreIdent<()>) -> ast::expression::Ident<()> {
    ast::expression::Ident {
        value: ident.value,
        position: ident.position,
        info: (),
    }
}
