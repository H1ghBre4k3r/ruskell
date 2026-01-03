//! Pretty printer for Ruskell Core AST (desugared)
//!
//! This module provides Display implementations for the Core AST,
//! showing the simplified form after desugaring.

use std::fmt::{self, Display};

use crate::ast::expression::{BinOpKind, UnaryOpKind};
use crate::core::{
    CoreBinaryOp, CoreExpr, CoreFunction, CoreFunctionCall, CoreIfThenElse, CoreLambda,
    CoreLambdaBody, CoreLambdaParam, CoreProgram, CoreStatement, CoreUnaryOp,
};

const INDENT: &str = "  ";

struct Formatter {
    buffer: String,
    indent_level: usize,
}

impl Formatter {
    fn new() -> Self {
        Self {
            buffer: String::new(),
            indent_level: 0,
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.buffer.push_str(INDENT);
        }
    }

    fn write_str(&mut self, s: &str) {
        self.buffer.push_str(s);
    }

    fn write_newline(&mut self) {
        self.buffer.push('\n');
    }

    fn finish(self) -> String {
        self.buffer
    }
}

impl<T> Display for CoreProgram<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = Formatter::new();

        formatter.write_str("-- Core AST (Desugared + Lambda Lifted)");
        formatter.write_newline();
        formatter.write_str("-- Note: Multi-param lambdas are now nested single-param lambdas");
        formatter.write_newline();
        formatter.write_str("-- Note: Multi-arg calls are now nested single-arg calls");
        formatter.write_newline();
        formatter.write_str("-- Note: Lambdas extracted to standalone functions (lambda_N)");
        formatter.write_newline();
        formatter.write_newline();

        // Format all functions except main
        for func in &self.functions {
            format_function(func, &mut formatter);
            formatter.write_newline();
            formatter.write_newline();
        }

        // Format main last
        format_function(&self.main, &mut formatter);
        formatter.write_newline();

        write!(f, "{}", formatter.finish())
    }
}

fn format_function<T>(func: &CoreFunction<T>, fmt: &mut Formatter) {
    fmt.write_str(&func.name.value);
    fmt.write_str(" = ");
    format_lambda(&func.lambda, fmt);
}

fn format_lambda<T>(lambda: &CoreLambda<T>, fmt: &mut Formatter) {
    fmt.write_str("(\\");
    format_lambda_param(&lambda.param, fmt);
    fmt.write_str(" => ");

    match &lambda.body {
        CoreLambdaBody::Expression(expr) => {
            format_expression(expr, fmt, 0);
        }
        CoreLambdaBody::Block(stmts) => {
            format_block(stmts, fmt);
        }
    }
    fmt.write_str(")");
}

fn format_lambda_param<T>(param: &CoreLambdaParam<T>, fmt: &mut Formatter) {
    match param {
        CoreLambdaParam::Ident(ident) => fmt.write_str(&ident.value),
        CoreLambdaParam::Unit(_) => fmt.write_str("()"),
    }
}

fn format_expression<T>(expr: &CoreExpr<T>, fmt: &mut Formatter, precedence: u8) {
    match expr {
        CoreExpr::Unit(_) => fmt.write_str("()"),
        CoreExpr::Ident(ident) => fmt.write_str(&ident.value),
        CoreExpr::Integer(int) => {
            use std::fmt::Write;
            write!(fmt.buffer, "{}", int.value).unwrap();
        }
        CoreExpr::String(s) => {
            use std::fmt::Write;
            write!(fmt.buffer, "\"{}\"", s.value).unwrap();
        }
        CoreExpr::Boolean(b) => fmt.write_str(if b.value { "true" } else { "false" }),
        CoreExpr::List(list) => {
            fmt.write_str("[");
            for (i, elem) in list.elements.iter().enumerate() {
                if i > 0 {
                    fmt.write_str(", ");
                }
                format_expression(elem, fmt, 0);
            }
            fmt.write_str("]");
        }
        CoreExpr::Lambda(lambda) => format_lambda(lambda, fmt),
        CoreExpr::FunctionCall(call) => format_function_call(call, fmt),
        CoreExpr::BinaryOp(binop) => format_binary_op(binop, fmt, precedence),
        CoreExpr::UnaryOp(unop) => format_unary_op(unop, fmt, precedence),
        CoreExpr::IfThenElse(ite) => format_if_then_else(ite, fmt),
    }
}

fn format_function_call<T>(call: &CoreFunctionCall<T>, fmt: &mut Formatter) {
    format_expression(&call.func, fmt, 100);
    fmt.write_str("(");
    format_expression(&call.arg, fmt, 0);
    fmt.write_str(")");
}

fn format_binary_op<T>(binop: &CoreBinaryOp<T>, fmt: &mut Formatter, parent_prec: u8) {
    let op_prec = binop_precedence(binop.op);
    let needs_parens = op_prec < parent_prec;

    if needs_parens {
        fmt.write_str("(");
    }

    format_expression(&binop.left, fmt, op_prec);
    fmt.write_str(" ");
    fmt.write_str(binop_symbol(binop.op));
    fmt.write_str(" ");
    format_expression(&binop.right, fmt, op_prec + 1);

    if needs_parens {
        fmt.write_str(")");
    }
}

fn format_unary_op<T>(unop: &CoreUnaryOp<T>, fmt: &mut Formatter, parent_prec: u8) {
    let op_prec = unop_precedence(unop.op);
    let needs_parens = op_prec < parent_prec;

    if needs_parens {
        fmt.write_str("(");
    }

    fmt.write_str(unop_symbol(unop.op));
    format_expression(&unop.operand, fmt, op_prec);

    if needs_parens {
        fmt.write_str(")");
    }
}

fn format_if_then_else<T>(ite: &CoreIfThenElse<T>, fmt: &mut Formatter) {
    fmt.write_str("if ");
    format_expression(&ite.condition, fmt, 0);
    fmt.write_str(" then ");
    format_expression(&ite.then_expr, fmt, 0);
    fmt.write_str(" else ");
    format_expression(&ite.else_expr, fmt, 0);
    fmt.write_str(" end");
}

fn format_block<T>(stmts: &[CoreStatement<T>], fmt: &mut Formatter) {
    fmt.write_str("do");
    fmt.write_newline();

    fmt.indent();
    for stmt in stmts {
        format_statement(stmt, fmt);
        fmt.write_newline();
    }
    fmt.dedent();

    fmt.write_indent();
    fmt.write_str("end");
}

fn format_statement<T>(stmt: &CoreStatement<T>, fmt: &mut Formatter) {
    fmt.write_indent();
    match stmt {
        CoreStatement::Assignment(assign) => {
            fmt.write_str(&assign.name.value);
            fmt.write_str(" := ");
            format_expression(&assign.value, fmt, 0);
        }
        CoreStatement::Expression(expr) => {
            format_expression(expr, fmt, 0);
        }
    }
}

fn binop_precedence(kind: BinOpKind) -> u8 {
    match kind {
        BinOpKind::Or => 1,
        BinOpKind::And => 2,
        BinOpKind::Eq
        | BinOpKind::NotEq
        | BinOpKind::Lt
        | BinOpKind::Gt
        | BinOpKind::LtEq
        | BinOpKind::GtEq => 3,
        BinOpKind::Add | BinOpKind::Sub | BinOpKind::Concat => 4,
        BinOpKind::Mul | BinOpKind::Div => 5,
    }
}

fn binop_symbol(kind: BinOpKind) -> &'static str {
    match kind {
        BinOpKind::Add => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Eq => "==",
        BinOpKind::NotEq => "!=",
        BinOpKind::Lt => "<",
        BinOpKind::Gt => ">",
        BinOpKind::LtEq => "<=",
        BinOpKind::GtEq => ">=",
        BinOpKind::And => "&&",
        BinOpKind::Or => "||",
        BinOpKind::Concat => "++",
    }
}

fn unop_precedence(kind: UnaryOpKind) -> u8 {
    match kind {
        UnaryOpKind::Not => 6,
    }
}

fn unop_symbol(kind: UnaryOpKind) -> &'static str {
    match kind {
        UnaryOpKind::Not => "!",
    }
}
