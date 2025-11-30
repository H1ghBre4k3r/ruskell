use crate::ast::{
    Function, Program,
    expression::{Expression, FunctionCall, Lambda, LambdaBody, LambdaParam},
    statement::{Assignment, Statement},
};

use super::{
    BoxedParser, ParseResult, ParseState, Parser, expect_arrow, expect_assign, expect_backslash,
    expect_comma, expect_do, expect_end, expect_equals, expect_lparen, expect_rparen, ident,
    integer, many, optional, string_literal,
};

/// unit_literal := "()"
pub fn unit_literal() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        expect_lparen().parse(state)?;
        expect_rparen().parse(state)?;
        Ok(Expression::Unit)
    })
}

/// singular := unit_literal | ident | integer | string
pub fn singular_expression() -> BoxedParser<Expression<()>> {
    let ident_expr = ident() >> |id| Expression::Ident(id);
    let int_expr = integer() >> |i| Expression::Integer(i);
    let str_expr = string_literal() >> |s| Expression::String(s);

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
pub fn call_args() -> BoxedParser<Vec<Expression<()>>> {
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
pub fn lambda_param() -> BoxedParser<LambdaParam<()>> {
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
pub fn lambda_params() -> BoxedParser<Vec<LambdaParam<()>>> {
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

/// assignment := ident ":=" expression
pub fn assignment() -> BoxedParser<Statement<()>> {
    ((ident() - expect_assign()) + expression())
        >> |(name, value)| {
            let position = name.position.clone();
            Statement::Assignment(Assignment {
                name,
                value: Box::new(value),
                position,
                info: (),
            })
        }
}

pub fn statement() -> BoxedParser<Statement<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try assignment first (needs lookahead for `:=`)
        if let Ok(expr) = assignment().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        expression().parse(state).map(Statement::Expression)
    })
}

/// expression := assignment | lambda | function_call | singular
pub fn expression() -> BoxedParser<Expression<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

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

/// function := ident "=" "do" statement* "end"
pub fn function() -> BoxedParser<Function<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let name = ident().parse(state)?;
        expect_equals().parse(state)?;
        let start = expect_do().parse(state)?.pos();
        let body = many(statement()).parse(state)?;
        let end = expect_end().parse(state)?.pos();

        Ok(Function {
            name,
            lambda: Lambda {
                params: vec![],
                body: LambdaBody::Block(body),
                position: start.merge(&end),
                info: (),
            },
        })
    })
}

/// program := function*
pub fn program() -> BoxedParser<Program<()>> {
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

pub fn parse(state: &mut ParseState) -> ParseResult<Program<()>> {
    program().parse(state)
}
