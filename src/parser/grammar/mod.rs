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
use crate::lexer::Token;

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

/// Check if we're at the start of a function definition (ident followed by '=')
fn at_function_start(state: &ParseState) -> bool {
    matches!(state.peek(), Some(Token::Ident(_)))
}

/// Skip tokens until we reach what looks like a new function definition or end of input
fn skip_to_next_function(state: &mut ParseState) {
    // Skip current token first to make progress
    state.advance();

    // Skip until we see an identifier (potential function name) at "top level"
    // We track brace depth to avoid stopping inside nested structures
    let mut depth = 0;
    while let Some(tok) = state.peek() {
        match tok {
            Token::Do(_) => {
                depth += 1;
                state.advance();
            }
            Token::End(_) => {
                if depth > 0 {
                    depth -= 1;
                }
                state.advance();
            }
            Token::Ident(_) if depth == 0 => {
                // Potential function start - stop here
                break;
            }
            _ => {
                state.advance();
            }
        }
    }
}

/// program := function*
/// With error recovery: if a function fails to parse, skip to the next one
pub fn program() -> BoxedParser<Program<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut functions = Vec::new();

        while state.has_next() && at_function_start(state) {
            let pos = state.position();
            match function().parse(state) {
                Ok(f) => functions.push(f),
                Err(_) => {
                    // Commit the error and try to recover
                    state.commit_furthest_error();
                    state.restore(pos);
                    skip_to_next_function(state);
                }
            }
        }

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
    let result = program().parse(state);

    // Check for unconsumed input
    let result = match result {
        Ok(prog) if state.has_next() => {
            if let Some(furthest) = state.get_furthest_error() {
                state.collect_error(furthest.clone());
            } else {
                let err = state.error_here("unexpected token");
                state.collect_error(err);
            }
            Ok(prog) // Still return the program if we got one
        }
        Ok(prog) => Ok(prog),
        Err(err) => {
            if let Some(furthest) = state.get_furthest_error() {
                state.collect_error(furthest.clone());
            } else {
                state.collect_error(err.clone());
            }
            Err(err)
        }
    };

    // If we collected errors, return the first one (but all are available via state.get_errors())
    if state.has_errors() {
        let errors = state.get_errors();
        Err(errors[0].clone())
    } else {
        result
    }
}
