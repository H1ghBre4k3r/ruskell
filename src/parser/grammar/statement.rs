//! Statement parsers for the Ruskell language

use crate::ast::statement::{Assignment, Statement};

use crate::parser::combinators::{BoxedParser, expect_assign};
use crate::parser::state::{ParseState, Parser};

use super::expression::expression;
use super::literal::ident;

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

/// statement := assignment | expression
pub fn statement() -> BoxedParser<Statement<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();

        // Try assignment first (needs lookahead for `:=`)
        if let Ok(stmt) = assignment().parse(state) {
            return Ok(stmt);
        }
        state.restore(pos);

        expression().parse(state).map(Statement::Expression)
    })
}
