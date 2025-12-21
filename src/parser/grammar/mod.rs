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
use super::state::{ParseError, ParseState, Parser};

use expression::expression;
use literal::ident;
use statement::statement;

/// lambda_param := "()" | ident
fn lambda_param() -> BoxedParser<crate::ast::expression::LambdaParam<()>> {
    use crate::ast::expression::LambdaParam;
    use literal::unit;

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

/// function_params := lambda_param*
fn function_params() -> BoxedParser<Vec<crate::ast::expression::LambdaParam<()>>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut params = Vec::new();

        loop {
            let pos = state.position();
            match lambda_param().parse(state) {
                Ok(param) => params.push(param),
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            }
        }

        Ok(params)
    })
}

/// function := ident param* "=" ("do" statement* "end" | expression)
pub fn function() -> BoxedParser<Function<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let name = ident().label("function name").parse(state)?;
        let start_pos = name.position.clone();
        let params = function_params().parse(state)?;
        expect_equals().parse(state)?;

        // Check if it's a do-block or single expression
        let pos = state.position();
        if expect_do().parse(state).is_ok() {
            let body = many(statement()).parse(state)?;
            let end = expect_end().parse(state)?.pos();
            Ok(Function {
                name,
                lambda: Lambda {
                    params,
                    body: LambdaBody::Block(body),
                    position: start_pos.merge(&end),
                    info: (),
                },
            })
        } else {
            state.restore(pos);
            let expr = expression().parse(state)?;
            let position = match &expr {
                crate::ast::expression::Expression::Integer(i) => i.position.clone(),
                crate::ast::expression::Expression::Ident(i) => i.position.clone(),
                crate::ast::expression::Expression::Boolean(b) => b.position.clone(),
                crate::ast::expression::Expression::BinaryOp(b) => b.position.clone(),
                crate::ast::expression::Expression::FunctionCall(f) => f.position.clone(),
                crate::ast::expression::Expression::Lambda(l) => l.position.clone(),
                crate::ast::expression::Expression::String(s) => s.position.clone(),
                crate::ast::expression::Expression::Unit(u) => u.position.clone(),
                crate::ast::expression::Expression::UnaryOp(u) => u.position.clone(),
                crate::ast::expression::Expression::IfThenElse(i) => i.position.clone(),
            };
            Ok(Function {
                name,
                lambda: Lambda {
                    params,
                    body: LambdaBody::Expression(Box::new(expr)),
                    position: start_pos.merge(&position),
                    info: (),
                },
            })
        }
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
/// Returns the parsed program (if main was found) along with all collected errors
pub fn parse(state: &mut ParseState) -> (Option<Program<()>>, Vec<ParseError>) {
    let result = program().parse(state);

    // Check for unconsumed input
    let program = match result {
        Ok(prog) if state.has_next() => {
            if let Some(furthest) = state.get_furthest_error() {
                state.collect_error(furthest.clone());
            } else {
                let err = state.error_here("unexpected token");
                state.collect_error(err);
            }
            Some(prog)
        }
        Ok(prog) => Some(prog),
        Err(err) => {
            if let Some(furthest) = state.get_furthest_error() {
                state.collect_error(furthest.clone());
            } else {
                state.collect_error(err);
            }
            None
        }
    };

    let errors = state.take_errors();
    (program, errors)
}
