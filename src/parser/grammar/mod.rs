//! Grammar module for the Ruskell language
//!
//! This module contains all the parsing rules organized by category:
//! - `literal`: Basic literals (identifiers, integers, strings, unit)
//! - `expression`: Expression forms (lambdas, function calls, etc.)
//! - `statement`: Statements (assignments, expression statements)

mod expression;
mod literal;
mod statement;

use crate::ast::expression::{Lambda, LambdaBody};
use crate::ast::{Function, Program};

use super::combinators::{BoxedParser, expect_do, expect_end, expect_equals, many};
use super::state::{ParseResult, ParseState, Parser};

use literal::ident;
use statement::statement;

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

/// Parse a complete program from the token stream
pub fn parse(state: &mut ParseState) -> ParseResult<Program<()>> {
    program().parse(state)
}
