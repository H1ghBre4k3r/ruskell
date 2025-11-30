use crate::ast::{
    Assignment, Expression, Function, FunctionCall, Lambda, Program, SingularExpression,
};

use super::{
    BoxedParser, ParseError, ParseResult, ParseState, Parser, expect_arrow, expect_assign,
    expect_backslash, expect_comma, expect_do, expect_end, expect_equals, ident, integer, many,
    many1, optional, string_literal,
};

/// singular := ident | integer | string
pub fn singular_expression() -> BoxedParser<Expression> {
    let ident_expr = ident() >> |id| Expression::SingularExpression(SingularExpression::Ident(id));
    let int_expr = integer() >> |i| Expression::SingularExpression(SingularExpression::Integer(i));
    let str_expr =
        string_literal() >> |s| Expression::SingularExpression(SingularExpression::String(s));

    ident_expr | int_expr | str_expr
}

/// atom := integer | string (things that can be unambiguous arguments)
/// Note: identifiers are NOT atoms to avoid greedy parsing issues
pub fn atom() -> BoxedParser<Expression> {
    let int_expr = integer() >> |i| Expression::SingularExpression(SingularExpression::Integer(i));
    let str_expr =
        string_literal() >> |s| Expression::SingularExpression(SingularExpression::String(s));

    int_expr | str_expr
}

/// function_call := ident atom+
/// Only matches when there's at least one unambiguous argument (int/string)
pub fn function_call() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        let func_ident = ident().parse(state)?;
        let func_expr = Expression::SingularExpression(SingularExpression::Ident(func_ident));

        let args = many1(atom()).parse(state)?;

        Ok(Expression::FunctionCall(FunctionCall {
            func: Box::new(func_expr),
            args,
        }))
    })
}

/// lambda_params := ident ("," ident)*
pub fn lambda_params() -> BoxedParser<Vec<crate::lexer::Ident>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let first = ident().parse(state)?;
        let mut params = vec![first];

        loop {
            let pos = state.position();
            if optional(expect_comma()).parse(state)?.is_some() {
                match ident().parse(state) {
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
