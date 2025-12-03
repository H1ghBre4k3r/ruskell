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
use super::state::{ParseError, ParseResult, ParseState, Parser};

use literal::ident;
use statement::statement;

/// function := ident "=" "do" statement* "end"
pub fn function() -> BoxedParser<Function<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let name = ident().label("function name").parse(state)?;
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
    BoxedParser::new(move |state: &mut ParseState| {
        let functions = many(function()).parse(state)?;

        let main = functions.iter().find(|f| f.name.value == "main").cloned();

        match main {
            Some(main) => {
                let other_functions = functions
                    .into_iter()
                    .filter(|f| f.name.value != "main")
                    .collect();

                Ok(Program {
                    main,
                    functions: other_functions,
                })
            }
            None => {
                let err = ParseError::new("program must have a 'main' function");
                state.record_error(err.clone());
                Err(err)
            }
        }
    })
}

/// Parse a complete program from the token stream
pub fn parse(state: &mut ParseState) -> ParseResult<Program<()>> {
    match program().parse(state) {
        Ok(prog) => {
            // Check if all input was consumed
            if state.has_next() {
                // There's leftover input - return the furthest error which explains why
                // parsing stopped, or create a generic error
                if let Some(furthest) = state.get_furthest_error() {
                    Err(furthest.clone())
                } else {
                    let err = state.error_here("unexpected token");
                    Err(err)
                }
            } else {
                Ok(prog)
            }
        }
        Err(err) => {
            // Return the furthest error if available, as it's usually more informative
            if let Some(furthest) = state.get_furthest_error() {
                Err(furthest.clone())
            } else {
                Err(err)
            }
        }
    }
}
