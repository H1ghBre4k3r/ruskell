//! Pattern parsers for the Ruskell language

use crate::ast::pattern::{LiteralPattern, Pattern, Wildcard};
use crate::lexer::Token;
use crate::parser::combinators::BoxedParser;
use crate::parser::state::{ParseError, ParseState, Parser};

use super::literal::{boolean, ident, integer, string_literal, unit};

/// Parse a literal pattern
/// literal_pattern := integer | string | boolean | unit
pub fn literal_pattern() -> BoxedParser<LiteralPattern<()>> {
    BoxedParser::new(|state: &mut ParseState| {
        let pos = state.position();

        // Try integer
        if let Ok(i) = integer().parse(state) {
            return Ok(LiteralPattern::Integer(i.value, i.position, ()));
        }
        state.restore(pos);

        // Try string
        if let Ok(s) = string_literal().parse(state) {
            return Ok(LiteralPattern::String(s.value, s.position, ()));
        }
        state.restore(pos);

        // Try boolean
        if let Ok(b) = boolean().parse(state) {
            return Ok(LiteralPattern::Boolean(b.value, b.position, ()));
        }
        state.restore(pos);

        // Try unit
        if let Ok(u) = unit().parse(state) {
            return Ok(LiteralPattern::Unit(u.position, ()));
        }

        Err(ParseError::new("expected literal pattern"))
    })
}

/// Parse a wildcard pattern
/// wildcard := "_"
pub fn wildcard() -> BoxedParser<Wildcard<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::Underscore(tok)) => {
            let pos = tok.position.clone();
            state.advance();
            Ok(Wildcard {
                position: pos,
                info: (),
            })
        }
        _ => Err(ParseError::new("expected '_'")),
    })
}

/// Parse a pattern
/// pattern := literal_pattern | wildcard | ident
pub fn pattern() -> BoxedParser<Pattern<()>> {
    BoxedParser::new(|state: &mut ParseState| {
        let pos = state.position();

        // Try literal pattern first
        if let Ok(lit) = literal_pattern().parse(state) {
            return Ok(Pattern::Literal(lit));
        }
        state.restore(pos);

        // Try wildcard
        if let Ok(w) = wildcard().parse(state) {
            return Ok(Pattern::Wildcard(w));
        }
        state.restore(pos);

        // Default to identifier (binds variable)
        let id = ident().parse(state)?;
        Ok(Pattern::Ident(id))
    })
}
