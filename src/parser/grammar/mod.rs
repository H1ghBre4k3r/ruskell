//! Grammar module for the Ruskell language
//!
//! This module contains all the parsing rules organized by category:
//! - `literal`: Basic literals (identifiers, integers, strings, unit)
//! - `expression`: Expression forms (lambdas, function calls, etc.)
//! - `statement`: Statements (assignments, expression statements)
//! - `pattern`: Pattern matching patterns

mod expression;
mod literal;
mod pattern;
mod statement;

use crate::ast::expression::{Lambda, LambdaBody};
use crate::ast::{self, Function, Program};
use crate::lexer::Token;

use super::combinators::{BoxedParser, expect_do, expect_end, expect_equals, many};
use super::state::{ParseError, ParseState, Parser};

use expression::expression;
use literal::ident;
use pattern::pattern;
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

/// Parse zero or more patterns for function parameters
fn function_patterns() -> BoxedParser<Vec<ast::pattern::Pattern<()>>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut patterns = Vec::new();

        loop {
            let pos = state.position();
            match pattern().parse(state) {
                Ok(pat) => patterns.push(pat),
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            }
        }

        Ok(patterns)
    })
}

/// Parse a function clause with patterns
/// function_clause := ident pattern* "=" ("do" statement* "end" | expression)
fn function_clause() -> BoxedParser<ast::pattern::FunctionClause<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let name = ident().label("function name").parse(state)?;
        let start_pos = name.position.clone();
        let patterns = function_patterns().parse(state)?;
        expect_equals().parse(state)?;

        // Check if it's a do-block or single expression
        let pos = state.position();
        if expect_do().parse(state).is_ok() {
            let body = many(statement()).parse(state)?;
            let end = expect_end().parse(state)?.pos();
            Ok(ast::pattern::FunctionClause {
                name,
                patterns,
                body: LambdaBody::Block(body),
                position: start_pos.merge(&end),
                info: (),
            })
        } else {
            state.restore(pos);
            let expr = expression().parse(state)?;
            let expr_position = match &expr {
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
                crate::ast::expression::Expression::Match(m) => m.position.clone(),
            };
            Ok(ast::pattern::FunctionClause {
                name,
                patterns,
                body: LambdaBody::Expression(Box::new(expr)),
                position: start_pos.merge(&expr_position),
                info: (),
            })
        }
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
                crate::ast::expression::Expression::Match(m) => m.position.clone(),
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

/// Check if a function clause can be converted to a simple function (all patterns are identifiers or unit)
fn can_convert_to_simple_function(clause: &ast::pattern::FunctionClause<()>) -> bool {
    clause.patterns.iter().all(|pat| {
        matches!(
            pat,
            ast::pattern::Pattern::Ident(_)
                | ast::pattern::Pattern::Literal(ast::pattern::LiteralPattern::Unit(_, _))
        )
    })
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
    use std::collections::HashMap;

    BoxedParser::new(move |state: &mut ParseState| {
        let mut clauses = Vec::new();

        // Parse all function clauses
        while state.has_next() && at_function_start(state) {
            let pos = state.position();
            match function_clause().parse(state) {
                Ok(f) => clauses.push(f),
                Err(_) => {
                    // Commit the error and try to recover
                    state.commit_furthest_error();
                    state.restore(pos);
                    skip_to_next_function(state);
                }
            }
        }

        // Group clauses by function name
        let mut grouped: HashMap<String, Vec<ast::pattern::FunctionClause<()>>> = HashMap::new();
        for clause in clauses {
            grouped
                .entry(clause.name.value.clone())
                .or_default()
                .push(clause);
        }

        // Convert to FunctionDef
        let mut function_defs = Vec::new();
        for (_name, mut clauses) in grouped {
            if clauses.len() == 1 {
                let clause = clauses.pop().unwrap();
                // Check if this is a simple function (all patterns are identifiers) - convert to old format
                if can_convert_to_simple_function(&clause) {
                    let params = clause
                        .patterns
                        .into_iter()
                        .map(|pat| match pat {
                            ast::pattern::Pattern::Ident(id) => {
                                ast::expression::LambdaParam::Ident(id)
                            }
                            ast::pattern::Pattern::Literal(ast::pattern::LiteralPattern::Unit(
                                pos,
                                _,
                            )) => ast::expression::LambdaParam::Unit(ast::expression::Unit {
                                position: pos,
                                info: (),
                            }),
                            _ => unreachable!(
                                "can_convert_to_simple_function ensures all are idents or unit"
                            ),
                        })
                        .collect();

                    function_defs.push(ast::FunctionDef::Single(Function {
                        name: clause.name,
                        lambda: Lambda {
                            params,
                            body: clause.body,
                            position: clause.position,
                            info: (),
                        },
                    }));
                } else {
                    // Clause has actual pattern matching - use Multi format
                    let name = clause.name.clone();
                    function_defs.push(ast::FunctionDef::Multi {
                        name,
                        clauses: vec![clause],
                    });
                }
            } else {
                // Multiple clauses - check if all clauses can be simple functions
                // If so, it's an error (can't have multiple simple functions with same name)
                // Otherwise, use Multi format
                let all_simple = clauses.iter().all(can_convert_to_simple_function);
                if all_simple {
                    // This is an error - multiple functions with same name and no patterns
                    // For now, just take the first one (parser should have caught this)
                    panic!("Multiple functions with same name but no pattern matching");
                }

                let name = clauses[0].name.clone();
                function_defs.push(ast::FunctionDef::Multi { name, clauses });
            }
        }

        // Find main function - if not found, use first function as placeholder
        // The validator will catch this and report MissingMain error
        let main = function_defs
            .iter()
            .find(|f| match f {
                ast::FunctionDef::Single(func) => func.name.value == "main",
                ast::FunctionDef::Multi { name, .. } => name.value == "main",
            })
            .cloned()
            .or_else(|| function_defs.first().cloned())
            .ok_or_else(|| ParseError::new("program must have at least one function"))?;

        let other_functions = function_defs
            .into_iter()
            .filter(|f| match f {
                ast::FunctionDef::Single(func) => func.name.value != "main",
                ast::FunctionDef::Multi { name, .. } => name.value != "main",
            })
            .collect();

        Ok(Program {
            main,
            functions: other_functions,
        })
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
