use crate::ast::{
    Assignment, Expression, Function, FunctionCall, Lambda, LambdaParam, Program,
    SingularExpression,
};

use super::{
    BoxedParser, ParseError, ParseResult, ParseState, Parser, expect_arrow, expect_assign,
    expect_backslash, expect_comma, expect_do, expect_end, expect_equals, expect_lparen,
    expect_rparen, ident, integer, many, optional, string_literal,
};

/// unit_literal := "()"
pub fn unit_literal() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        expect_lparen().parse(state)?;
        expect_rparen().parse(state)?;
        Ok(Expression::SingularExpression(SingularExpression::Unit))
    })
}

/// singular := unit_literal | ident | integer | string
pub fn singular_expression() -> BoxedParser<Expression> {
    let ident_expr = ident() >> |id| Expression::SingularExpression(SingularExpression::Ident(id));
    let int_expr = integer() >> |i| Expression::SingularExpression(SingularExpression::Integer(i));
    let str_expr =
        string_literal() >> |s| Expression::SingularExpression(SingularExpression::String(s));

    // Try unit first (before ident consumes something else)
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();
        if let Ok(expr) = unit_literal().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        (ident_expr.clone() | int_expr.clone() | str_expr.clone()).parse(state)
    })
}

/// call_args := expression ("," expression)*
pub fn call_args() -> BoxedParser<Vec<Expression>> {
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
pub fn function_call() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        let func_ident = ident().parse(state)?;
        let func_expr = Expression::SingularExpression(SingularExpression::Ident(func_ident));

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
        }))
    })
}

/// lambda_param := "()" | ident
pub fn lambda_param() -> BoxedParser<LambdaParam> {
    BoxedParser::new(move |state: &mut ParseState| {
        // Try unit pattern first
        let pos = state.position();
        if expect_lparen().parse(state).is_ok() && expect_rparen().parse(state).is_ok() {
            return Ok(LambdaParam::Unit);
        }
        state.restore(pos);

        // Otherwise parse identifier
        let id = ident().parse(state)?;
        Ok(LambdaParam::Ident(id))
    })
}

/// lambda_params := lambda_param ("," lambda_param)*
pub fn lambda_params() -> BoxedParser<Vec<LambdaParam>> {
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
///         | "\" lambda_params "=>" "do" expression* "end"
pub fn lambda() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        expect_backslash().parse(state)?;
        let params = lambda_params().parse(state)?;
        expect_arrow().parse(state)?;

        // Check if it's a do-block or single expression
        let pos = state.position();
        if expect_do().parse(state).is_ok() {
            let body = many(expression()).parse(state)?;
            expect_end().parse(state)?;
            Ok(Expression::Lambda(Lambda { params, body }))
        } else {
            state.restore(pos);
            let expr = expression().parse(state)?;
            Ok(Expression::Lambda(Lambda {
                params,
                body: vec![expr],
            }))
        }
    })
}

/// assignment := ident ":=" expression
pub fn assignment() -> BoxedParser<Expression> {
    ((ident() - expect_assign()) + expression())
        >> |(name, value)| {
            Expression::Assignment(Assignment {
                name,
                value: Box::new(value),
            })
        }
}

/// expression := assignment | lambda | function_call | singular
pub fn expression() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try assignment first (needs lookahead for `:=`)
        if let Ok(expr) = assignment().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Try lambda
        if let Ok(expr) = lambda().parse(state) {
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

/// function := ident "=" "do" expression* "end"
pub fn function() -> BoxedParser<Function> {
    ((ident() - expect_equals() - expect_do()) + (many(expression()) - expect_end()))
        >> |(name, exprs)| Function {
            name,
            args: vec![],
            expression: exprs,
        }
}

/// program := function*
pub fn program() -> BoxedParser<Program> {
    many(function())
        >> |functions| {
            let main = functions
                .iter()
                .find(|f| f.name.value == "main")
                .expect("program must have a main function")
                .clone();

            let other_functions = functions
                .into_iter()
                .filter(|f| f.name.value != "main")
                .collect();

            Program {
                main,
                functions: other_functions,
            }
        }
}

pub fn parse(state: &mut ParseState) -> ParseResult<Program> {
    program().parse(state)
}
