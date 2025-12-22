//! Expression parsers for the Ruskell language

use crate::ast::expression::{
    BinOpKind, BinaryOp, Expression, FunctionCall, IfThenElse, Lambda, LambdaBody, LambdaParam,
    UnaryOp, UnaryOpKind, Unit,
};
use crate::lexer::Token;

use crate::parser::combinators::{
    BoxedParser, expect_arrow, expect_backslash, expect_comma, expect_do, expect_else, expect_end,
    expect_if, expect_logical_and, expect_logical_not, expect_logical_or, expect_lparen,
    expect_rparen, expect_then, many, optional,
};
use crate::parser::state::{ParseState, Parser};

use super::literal::{boolean, ident, integer, string_literal, unit};
use super::statement::statement;

/// unit_literal := "()"
pub fn unit_literal() -> BoxedParser<Expression<()>> {
    unit() >> |u| Expression::Unit(u)
}

/// singular := unit_literal | boolean | ident | integer | string
pub fn singular_expression() -> BoxedParser<Expression<()>> {
    let ident_expr = ident() >> |id| Expression::Ident(id);
    let int_expr = integer() >> |i| Expression::Integer(i);
    let str_expr = string_literal() >> |s| Expression::String(s);
    let bool_expr = boolean() >> |b| Expression::Boolean(b);

    // Try unit first (before ident consumes something else)
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();
        if let Ok(expr) = unit_literal().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        (bool_expr.clone() | ident_expr.clone() | int_expr.clone() | str_expr.clone()).parse(state)
    })
}

/// call_args := expression ("," expression)*
fn call_args() -> BoxedParser<Vec<Expression<()>>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let first = expression().parse(state)?;
        let mut args = vec![first];

        loop {
            let pos = state.position();
            if optional(expect_comma()).parse(state)?.is_some() {
                match expression().parse(state) {
                    Ok(arg) => args.push(arg),
                    Err(_) => {
                        state.restore(pos);
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(args)
    })
}

/// function_call := ident "(" [call_args] ")"
pub fn function_call() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let func_ident = ident().parse(state)?;
        let position = func_ident.position.clone();
        let func_expr = Expression::Ident(func_ident);

        expect_lparen().parse(state)?;

        // Check for empty args: fn()
        let pos = state.position();
        let args = if expect_rparen().parse(state).is_ok() {
            vec![]
        } else {
            state.restore(pos);
            let args = call_args().parse(state)?;
            expect_rparen().parse(state)?;
            args
        };

        Ok(Expression::FunctionCall(FunctionCall {
            func: Box::new(func_expr),
            args,
            position,
            info: (),
        }))
    })
}

/// lambda_param := "()" | ident
fn lambda_param() -> BoxedParser<LambdaParam<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        // Try unit pattern first
        let pos = state.position();
        if let Ok(u) = unit().parse(state) {
            return Ok(LambdaParam::Unit(u));
        }
        state.restore(pos);

        // Otherwise parse identifier
        let id = ident().parse(state)?;
        Ok(LambdaParam::Ident(id))
    })
}

/// lambda_params := lambda_param ("," lambda_param)*
fn lambda_params() -> BoxedParser<Vec<LambdaParam<()>>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let first = lambda_param().parse(state)?;
        let mut params = vec![first];

        loop {
            let pos = state.position();
            if optional(expect_comma()).parse(state)?.is_some() {
                match lambda_param().parse(state) {
                    Ok(param) => params.push(param),
                    Err(_) => {
                        state.restore(pos);
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(params)
    })
}

/// lambda := "\" lambda_params "=>" expression
///         | "\" lambda_params "=>" "do" statement* "end"
pub fn lambda() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_backslash().parse(state)?.pos();
        let params = lambda_params().parse(state)?;
        expect_arrow().parse(state)?;

        // Check if it's a do-block or single expression
        let pos = state.position();
        if expect_do().parse(state).is_ok() {
            let body = many(statement()).parse(state)?;
            let end = expect_end().parse(state)?.pos();
            Ok(Expression::Lambda(Lambda {
                params,
                body: LambdaBody::Block(body),
                position: start.merge(&end),
                info: (),
            }))
        } else {
            state.restore(pos);
            let expr = expression().parse(state)?;
            Ok(Expression::Lambda(Lambda {
                params,
                body: LambdaBody::Expression(Box::new(expr)),
                position: start,
                info: (),
            }))
        }
    })
}

/// if_then_else := "if" expression "then" branch "else" branch ["end"]
/// branch := do_block | expression
/// Note: If both branches are do-blocks, the final "end" is optional
pub fn if_then_else() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_if().parse(state)?.pos();
        let condition = expression().parse(state)?;
        expect_then().parse(state)?;
        
        // Check if then-branch is a do-block
        let then_is_do_block = matches!(state.peek(), Some(Token::Do(_)));
        let then_expr = expression().parse(state)?;
        
        expect_else().parse(state)?;
        
        // Check if else-branch is a do-block  
        let else_is_do_block = matches!(state.peek(), Some(Token::Do(_)));
        let else_expr = expression().parse(state)?;
        
        // Only require 'end' if at least one branch is NOT a do-block
        let end = if then_is_do_block && else_is_do_block {
            // Both branches are do-blocks, 'end' is optional
            let pos = state.position();
            if let Ok(end_tok) = expect_end().parse(state) {
                end_tok.pos()
            } else {
                state.restore(pos);
                // Use start position as fallback (will be overridden by merge)
                start.clone()
            }
        } else {
            // At least one branch is not a do-block, require 'end'
            expect_end().parse(state)?.pos()
        };

        Ok(Expression::IfThenElse(IfThenElse {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
            position: start.merge(&end),
            info: (),
        }))
    })
}

/// case_expr := "case" expression "of" match_arm+ "end"
/// match_arm := pattern "=>" expression
pub fn case_expr() -> BoxedParser<Expression<()>> {
    use crate::ast::pattern::MatchArm;
    use crate::parser::combinators::{expect_case, expect_of};

    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_case().parse(state)?.pos();
        let scrutinee = expression().parse(state)?;
        expect_of().parse(state)?;

        // Parse at least one match arm
        let mut arms = Vec::new();
        loop {
            let pos = state.position();

            // Try to parse a match arm
            let arm_result: Result<MatchArm<()>, crate::parser::state::ParseError> = (|| {
                let pattern = super::pattern::pattern().parse(state)?;
                expect_arrow().parse(state)?;
                let body = expression().parse(state)?;
                let arm_pos = pattern_position(&pattern).merge(&expr_position(&body));

                Ok(MatchArm {
                    pattern,
                    body,
                    position: arm_pos,
                    info: (),
                })
            })(
            );

            match arm_result {
                Ok(arm) => arms.push(arm),
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            }

            // Check if we've reached "end"
            if matches!(state.peek(), Some(Token::End(_))) {
                break;
            }
        }

        if arms.is_empty() {
            return Err(crate::parser::state::ParseError::new(
                "case expression must have at least one arm",
            ));
        }

        let end = expect_end().parse(state)?.pos();

        Ok(Expression::Match(Box::new(crate::ast::pattern::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            position: start.merge(&end),
            info: (),
        })))
    })
}

/// Helper to extract position from a pattern
fn pattern_position(pattern: &crate::ast::pattern::Pattern<()>) -> lachs::Span {
    use crate::ast::pattern::Pattern;
    match pattern {
        Pattern::Literal(lit) => {
            use crate::ast::pattern::LiteralPattern;
            match lit {
                LiteralPattern::Integer(_, span, _) => span.clone(),
                LiteralPattern::String(_, span, _) => span.clone(),
                LiteralPattern::Boolean(_, span, _) => span.clone(),
                LiteralPattern::Unit(span, _) => span.clone(),
            }
        }
        Pattern::Ident(id) => id.position.clone(),
        Pattern::Wildcard(w) => w.position.clone(),
    }
}

/// comparison := additive (comp_op additive)?
/// comp_op := "==" | "!=" | "<" | ">" | "<=" | ">="
fn comparison_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let left = additive_expr().parse(state)?;

        let pos = state.position();
        let token = state.peek();

        let op = match token {
            Some(Token::DoubleEquals(_)) => {
                state.advance();
                BinOpKind::Eq
            }
            Some(Token::NotEquals(_)) => {
                state.advance();
                BinOpKind::NotEq
            }
            Some(Token::LessThan(_)) => {
                state.advance();
                BinOpKind::Lt
            }
            Some(Token::GreaterThan(_)) => {
                state.advance();
                BinOpKind::Gt
            }
            Some(Token::LessEquals(_)) => {
                state.advance();
                BinOpKind::LtEq
            }
            Some(Token::GreaterEquals(_)) => {
                state.advance();
                BinOpKind::GtEq
            }
            _ => return Ok(left),
        };

        let right = match additive_expr().parse(state) {
            Ok(r) => r,
            Err(_) => {
                state.restore(pos);
                return Ok(left);
            }
        };

        let position = match &left {
            Expression::Integer(i) => i.position.clone(),
            Expression::Ident(i) => i.position.clone(),
            Expression::Boolean(b) => b.position.clone(),
            Expression::BinaryOp(b) => b.position.clone(),
            Expression::FunctionCall(f) => f.position.clone(),
            Expression::Lambda(l) => l.position.clone(),
            Expression::String(s) => s.position.clone(),
            Expression::Unit(u) => u.position.clone(),
            Expression::UnaryOp(u) => u.position.clone(),
            Expression::IfThenElse(i) => i.position.clone(),
            Expression::Match(m) => m.position.clone(),
        };

        Ok(Expression::BinaryOp(BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            position,
            info: (),
        }))
    })
}

/// unary := "!" unary | comparison
fn unary_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try "!" prefix
        if expect_logical_not().parse(state).is_ok() {
            let operand = unary_expr().parse(state)?;
            let position = operand_position(&operand);
            return Ok(Expression::UnaryOp(UnaryOp {
                op: UnaryOpKind::Not,
                operand: Box::new(operand),
                position,
                info: (),
            }));
        }

        state.restore(pos);
        comparison_expr().parse(state)
    })
}

/// logical_and := unary ("&&" unary)*
fn logical_and_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut left = unary_expr().parse(state)?;

        loop {
            let pos = state.position();

            if expect_logical_and().parse(state).is_err() {
                state.restore(pos);
                break;
            }

            let right = match unary_expr().parse(state) {
                Ok(r) => r,
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            };

            let position = expr_position(&left);

            left = Expression::BinaryOp(BinaryOp {
                op: BinOpKind::And,
                left: Box::new(left),
                right: Box::new(right),
                position,
                info: (),
            });
        }

        Ok(left)
    })
}

/// logical_or := logical_and ("||" logical_and)*
fn logical_or_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut left = logical_and_expr().parse(state)?;

        loop {
            let pos = state.position();

            if expect_logical_or().parse(state).is_err() {
                state.restore(pos);
                break;
            }

            let right = match logical_and_expr().parse(state) {
                Ok(r) => r,
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            };

            let position = expr_position(&left);

            left = Expression::BinaryOp(BinaryOp {
                op: BinOpKind::Or,
                left: Box::new(left),
                right: Box::new(right),
                position,
                info: (),
            });
        }

        Ok(left)
    })
}

/// expression := lambda | if_then_else | case_expr | logical_or
pub fn expression() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try lambda
        if let Ok(expr) = lambda().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Try if-then-else
        if let Ok(expr) = if_then_else().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Try case expression
        if let Ok(expr) = case_expr().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Try logical_or expression (handles all binary and unary ops)
        logical_or_expr().parse(state)
    })
}

/// do_block := "do" statement* "end"
/// Parses a do-block as an expression (like Rust blocks)
fn do_block_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_do().parse(state)?.pos();
        let body = many(statement()).parse(state)?;
        let end = expect_end().parse(state)?.pos();

        // Wrap the do-block in an immediately-invoked lambda
        // do ... end becomes (\() => do ... end)()
        let lambda = Lambda {
            params: vec![LambdaParam::Unit(Unit {
                position: start.clone(),
                info: (),
            })],
            body: LambdaBody::Block(body),
            position: start.merge(&end),
            info: (),
        };

        let func_call = FunctionCall {
            func: Box::new(Expression::Lambda(lambda)),
            args: vec![Expression::Unit(Unit {
                position: start.clone(),
                info: (),
            })],
            position: start.merge(&end),
            info: (),
        };

        Ok(Expression::FunctionCall(func_call))
    })
}

/// primary := do_block | function_call | singular | "(" expression ")"
fn primary_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try do-block expression
        if let Ok(expr) = do_block_expr().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Try parenthesized expression or unit literal
        if let Ok(start) = expect_lparen().parse(state) {
            // Check for unit literal ()
            let inner_pos = state.position();
            if let Ok(end) = expect_rparen().parse(state) {
                return Ok(Expression::Unit(crate::ast::expression::Unit {
                    position: start.pos().merge(&end.pos()),
                    info: (),
                }));
            }
            state.restore(inner_pos);

            // Otherwise it's a parenthesized expression
            let expr = expression().parse(state)?;
            expect_rparen().parse(state)?;
            return Ok(expr);
        }
        state.restore(pos);

        // Try function call (ident followed by args)
        if let Ok(expr) = function_call().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Fall back to singular
        singular_expression().parse(state)
    })
}

/// multiplicative := primary (("*" | "/") primary)*
fn multiplicative_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut left = primary_expr().parse(state)?;

        loop {
            let pos = state.position();
            let token = state.peek();

            let op = match token {
                Some(Token::Star(_)) => {
                    state.advance();
                    BinOpKind::Mul
                }
                Some(Token::Slash(_)) => {
                    state.advance();
                    BinOpKind::Div
                }
                _ => break,
            };

            let right = match primary_expr().parse(state) {
                Ok(r) => r,
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            };

            let position = match &left {
                Expression::Integer(i) => i.position.clone(),
                Expression::Ident(i) => i.position.clone(),
                Expression::Boolean(b) => b.position.clone(),
                Expression::BinaryOp(b) => b.position.clone(),
                Expression::FunctionCall(f) => f.position.clone(),
                Expression::Lambda(l) => l.position.clone(),
                Expression::String(s) => s.position.clone(),
                Expression::Unit(u) => u.position.clone(),
                Expression::UnaryOp(u) => u.position.clone(),
                Expression::IfThenElse(i) => i.position.clone(),
                Expression::Match(m) => m.position.clone(),
            };

            left = Expression::BinaryOp(BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
                info: (),
            });
        }

        Ok(left)
    })
}

/// additive := multiplicative (("+" | "-") multiplicative)*
fn additive_expr() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut left = multiplicative_expr().parse(state)?;

        loop {
            let pos = state.position();
            let token = state.peek();

            let op = match token {
                Some(Token::Plus(_)) => {
                    state.advance();
                    BinOpKind::Add
                }
                Some(Token::Minus(_)) => {
                    state.advance();
                    BinOpKind::Sub
                }
                _ => break,
            };

            let right = match multiplicative_expr().parse(state) {
                Ok(r) => r,
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            };

            let position = match &left {
                Expression::Integer(i) => i.position.clone(),
                Expression::Ident(i) => i.position.clone(),
                Expression::Boolean(b) => b.position.clone(),
                Expression::BinaryOp(b) => b.position.clone(),
                Expression::FunctionCall(f) => f.position.clone(),
                Expression::Lambda(l) => l.position.clone(),
                Expression::String(s) => s.position.clone(),
                Expression::Unit(u) => u.position.clone(),
                Expression::UnaryOp(u) => u.position.clone(),
                Expression::IfThenElse(i) => i.position.clone(),
                Expression::Match(m) => m.position.clone(),
            };

            left = Expression::BinaryOp(BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
                info: (),
            });
        }

        Ok(left)
    })
}

/// Helper function to extract position from an expression
fn expr_position(expr: &Expression<()>) -> lachs::Span {
    match expr {
        Expression::Unit(u) => u.position.clone(),
        Expression::Ident(i) => i.position.clone(),
        Expression::Integer(i) => i.position.clone(),
        Expression::String(s) => s.position.clone(),
        Expression::Boolean(b) => b.position.clone(),
        Expression::FunctionCall(f) => f.position.clone(),
        Expression::Lambda(l) => l.position.clone(),
        Expression::BinaryOp(b) => b.position.clone(),
        Expression::UnaryOp(u) => u.position.clone(),
        Expression::IfThenElse(i) => i.position.clone(),
        Expression::Match(m) => m.position.clone(),
    }
}

/// Helper function to extract position from an operand (for unary ops)
fn operand_position(expr: &Expression<()>) -> lachs::Span {
    expr_position(expr)
}
