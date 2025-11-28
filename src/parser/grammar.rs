use crate::ast::{Assignment, Expression, Function, Program, SingularExpression};

use super::{
    BoxedParser, ParseError, ParseResult, ParseState, Parser, expect_assign, expect_do, expect_end,
    expect_equals, ident, integer, many, string_literal,
};

/// singular := ident | integer | string
pub fn singular_expression() -> BoxedParser<Expression> {
    let ident_expr = ident() >> |id| Expression::SingularExpression(SingularExpression::Ident(id));
    let int_expr = integer() >> |i| Expression::SingularExpression(SingularExpression::Integer(i));
    let str_expr =
        string_literal() >> |s| Expression::SingularExpression(SingularExpression::String(s));

    ident_expr | int_expr | str_expr
}

/// assignment := ident ":=" expression
pub fn assignment() -> BoxedParser<Expression> {
    (ident() - expect_assign()) + expression()
        >> |(name, value)| {
            Expression::Assignment(Assignment {
                name,
                value: Box::new(value),
            })
        }
}

/// expression := assignment | singular
pub fn expression() -> BoxedParser<Expression> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try assignment first (needs lookahead for `:=`)
        if let Ok(expr) = assignment().parse(state) {
            return Ok(expr);
        }
        state.restore(pos);

        // Fall back to singular
        singular_expression().parse(state)
    })
}

/// function := ident "=" "do" expression* "end"
pub fn function() -> BoxedParser<Function> {
    (ident() - expect_equals() - expect_do()) + (many(expression()) - expect_end())
        >> |(name, exprs)| Function {
            name,
            args: (),
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
