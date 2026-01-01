//! Pretty printer for Ruskell source code
//!
//! This module provides Display implementations for the surface AST,
//! allowing formatted code output.

use std::fmt::{self, Display, Write};

use crate::ast::{
    FunctionDef, Program,
    expression::{
        BinOpKind, BinaryOp, Expression, FunctionCall, IfThenElse, Lambda, LambdaBody, LambdaParam,
        UnaryOp, UnaryOpKind,
    },
    pattern::{FunctionClause, LiteralPattern, Match, MatchArm, Pattern},
    statement::Statement,
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

impl<T> Display for Program<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = Formatter::new();

        // Format all functions except main
        for func in &self.functions {
            format_function_def(func, &mut formatter);
            formatter.write_newline();
            formatter.write_newline();
        }

        // Format main last
        format_function_def(&self.main, &mut formatter);
        formatter.write_newline();

        write!(f, "{}", formatter.finish())
    }
}

fn format_function_def<T>(func: &FunctionDef<T>, fmt: &mut Formatter) {
    match func {
        FunctionDef::Single(single) => {
            fmt.write_str(&single.name.value);
            format_lambda_inline(&single.lambda, fmt);
        }
        FunctionDef::Multi { name, clauses } => {
            for clause in clauses {
                fmt.write_str(&name.value);
                format_function_clause(clause, fmt);
                fmt.write_newline();
            }
            // Remove trailing newline
            fmt.buffer.pop();
        }
    }
}

fn format_function_clause<T>(clause: &FunctionClause<T>, fmt: &mut Formatter) {
    // Write patterns
    for pattern in &clause.patterns {
        fmt.write_str(" ");
        format_pattern(pattern, fmt);
    }

    fmt.write_str(" = ");

    // Format body
    match &clause.body {
        LambdaBody::Expression(expr) => {
            format_expression(expr, fmt, 0);
        }
        LambdaBody::Block(stmts) => {
            format_block(stmts, fmt);
        }
    }
}

fn format_lambda_inline<T>(lambda: &Lambda<T>, fmt: &mut Formatter) {
    // Write parameters
    for param in &lambda.params {
        fmt.write_str(" ");
        format_lambda_param(param, fmt);
    }

    fmt.write_str(" = ");

    // Format body
    match &lambda.body {
        LambdaBody::Expression(expr) => {
            format_expression(expr, fmt, 0);
        }
        LambdaBody::Block(stmts) => {
            format_block(stmts, fmt);
        }
    }
}

fn format_lambda_param<T>(param: &LambdaParam<T>, fmt: &mut Formatter) {
    match param {
        LambdaParam::Ident(ident) => fmt.write_str(&ident.value),
        LambdaParam::Unit(_) => fmt.write_str("()"),
    }
}

fn format_pattern<T>(pattern: &Pattern<T>, fmt: &mut Formatter) {
    match pattern {
        Pattern::Ident(ident) => fmt.write_str(&ident.value),
        Pattern::Literal(lit) => match lit {
            LiteralPattern::Integer(val, _, _) => write!(fmt.buffer, "{}", val).unwrap(),
            LiteralPattern::String(val, _, _) => write!(fmt.buffer, "\"{}\"", val).unwrap(),
            LiteralPattern::Boolean(val, _, _) => {
                fmt.write_str(if *val { "true" } else { "false" })
            }
            LiteralPattern::Unit(_, _) => fmt.write_str("()"),
            LiteralPattern::EmptyList(_, _) => fmt.write_str("[]"),
        },
        Pattern::Wildcard(_) => fmt.write_str("_"),
        Pattern::ListCons(cons) => {
            fmt.write_str("[");
            format_pattern(&cons.head, fmt);
            fmt.write_str(" | ");
            format_pattern(&cons.tail, fmt);
            fmt.write_str("]");
        }
    }
}

fn format_expression<T>(expr: &Expression<T>, fmt: &mut Formatter, precedence: u8) {
    match expr {
        Expression::Unit(_) => fmt.write_str("()"),
        Expression::Ident(ident) => fmt.write_str(&ident.value),
        Expression::Integer(int) => write!(fmt.buffer, "{}", int.value).unwrap(),
        Expression::String(s) => write!(fmt.buffer, "\"{}\"", s.value).unwrap(),
        Expression::Boolean(b) => fmt.write_str(if b.value { "true" } else { "false" }),
        Expression::List(list) => {
            fmt.write_str("[");
            for (i, elem) in list.elements.iter().enumerate() {
                if i > 0 {
                    fmt.write_str(", ");
                }
                format_expression(elem, fmt, 0);
            }
            fmt.write_str("]");
        }
        Expression::FunctionCall(call) => format_function_call(call, fmt),
        Expression::Lambda(lambda) => format_lambda_expression(lambda, fmt),
        Expression::BinaryOp(binop) => format_binary_op(binop, fmt, precedence),
        Expression::UnaryOp(unop) => format_unary_op(unop, fmt, precedence),
        Expression::IfThenElse(ite) => format_if_then_else(ite, fmt),
        Expression::Match(m) => format_match(m, fmt),
    }
}

fn format_lambda_expression<T>(lambda: &Lambda<T>, fmt: &mut Formatter) {
    fmt.write_str("\\");

    // Write parameters
    for (i, param) in lambda.params.iter().enumerate() {
        if i > 0 {
            fmt.write_str(", ");
        }
        format_lambda_param(param, fmt);
    }

    fmt.write_str(" => ");

    // Format body
    match &lambda.body {
        LambdaBody::Expression(expr) => {
            format_expression(expr, fmt, 0);
        }
        LambdaBody::Block(stmts) => {
            format_block(stmts, fmt);
        }
    }
}

fn format_function_call<T>(call: &FunctionCall<T>, fmt: &mut Formatter) {
    // Check if this is an immediately-invoked do-block: (\() => do ... end)(())
    // If so, just print the do-block directly
    if let Expression::Lambda(lambda) = &*call.func
        && let LambdaBody::Block(stmts) = &lambda.body
    {
        // Check if it's a unit lambda being called with unit
        if lambda.params.len() == 1
            && let LambdaParam::Unit(_) = &lambda.params[0]
            && call.args.len() == 1
            && matches!(&call.args[0], Expression::Unit(_))
        {
            // This is (\() => do ... end)(()), just print do ... end
            format_block(stmts, fmt);
            return;
        }
    }

    // Normal function call
    format_expression(&call.func, fmt, 100); // High precedence for function names
    fmt.write_str("(");
    for (i, arg) in call.args.iter().enumerate() {
        if i > 0 {
            fmt.write_str(", ");
        }
        format_expression(arg, fmt, 0);
    }
    fmt.write_str(")");
}

fn format_binary_op<T>(binop: &BinaryOp<T>, fmt: &mut Formatter, parent_prec: u8) {
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

fn format_unary_op<T>(unop: &UnaryOp<T>, fmt: &mut Formatter, parent_prec: u8) {
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

fn format_if_then_else<T>(ite: &IfThenElse<T>, fmt: &mut Formatter) {
    // Check if both branches are do-blocks (immediately-invoked unit lambdas)
    let then_is_do_block = is_do_block_expression(&ite.then_expr);
    let else_is_do_block = is_do_block_expression(&ite.else_expr);

    fmt.write_str("if ");
    format_expression(&ite.condition, fmt, 0);
    fmt.write_str(" then ");

    format_expression(&ite.then_expr, fmt, 0);

    fmt.write_str(" else ");

    format_expression(&ite.else_expr, fmt, 0);

    // Only add 'end' if at least one branch is NOT a do-block
    if !then_is_do_block || !else_is_do_block {
        fmt.write_str(" end");
    }
}

// Helper to check if an expression is a do-block (immediately-invoked unit lambda)
fn is_do_block_expression<T>(expr: &Expression<T>) -> bool {
    if let Expression::FunctionCall(call) = expr
        && let Expression::Lambda(lambda) = &*call.func
        && let LambdaBody::Block(_) = &lambda.body
        && lambda.params.len() == 1
        && let LambdaParam::Unit(_) = &lambda.params[0]
        && call.args.len() == 1
    {
        return matches!(&call.args[0], Expression::Unit(_));
    }
    false
}

fn format_match<T>(m: &Match<T>, fmt: &mut Formatter) {
    fmt.write_str("case ");
    format_expression(&m.scrutinee, fmt, 0);
    fmt.write_str(" of");
    fmt.write_newline();

    fmt.indent();
    for arm in &m.arms {
        format_match_arm(arm, fmt);
        fmt.write_newline();
    }
    fmt.dedent();

    fmt.write_indent();
    fmt.write_str("end");
}

fn format_match_arm<T>(arm: &MatchArm<T>, fmt: &mut Formatter) {
    fmt.write_indent();
    format_pattern(&arm.pattern, fmt);
    fmt.write_str(" => ");
    format_expression(&arm.body, fmt, 0);
}

fn format_block<T>(stmts: &[Statement<T>], fmt: &mut Formatter) {
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

fn format_statement<T>(stmt: &Statement<T>, fmt: &mut Formatter) {
    fmt.write_indent();
    match stmt {
        Statement::Assignment(assign) => {
            fmt.write_str(&assign.name.value);
            fmt.write_str(" := ");
            format_expression(&assign.value, fmt, 0);
        }
        Statement::Expression(expr) => {
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
