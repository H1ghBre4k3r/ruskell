//! Pattern parsers for the Ruskell language

use crate::ast::pattern::{ListConsPattern, LiteralPattern, Pattern, Wildcard};
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
/// pattern := list_cons_pattern | literal_pattern | wildcard | ident
pub fn pattern() -> BoxedParser<Pattern<()>> {
    BoxedParser::new(|state: &mut ParseState| {
        let pos = state.position();

        // Try list cons pattern or empty list literal first
        if let Ok(list_pat) = list_pattern().parse(state) {
            return Ok(list_pat);
        }
        state.restore(pos);

        // Try literal pattern
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

/// Parse a list pattern (either cons pattern [h | t] or empty list [])
/// list_pattern := "[" "]" | "[" pattern "|" pattern "]"
pub fn list_pattern() -> BoxedParser<Pattern<()>> {
    BoxedParser::new(|state: &mut ParseState| {
        // Expect opening bracket
        let start_pos = match state.peek() {
            Some(Token::LBracket(tok)) => {
                let pos = tok.position.clone();
                state.advance();
                pos
            }
            _ => return Err(ParseError::new("expected '['")),
        };

        // Check for empty list []
        if let Some(Token::RBracket(tok)) = state.peek() {
            let end_pos = tok.position.clone();
            state.advance();
            return Ok(Pattern::Literal(LiteralPattern::EmptyList(
                start_pos.merge(&end_pos),
                (),
            )));
        }

        // Parse head pattern
        let head = pattern().parse(state)?;

        // Check for pipe (cons pattern) vs comma or rbracket (list literal)
        match state.peek() {
            Some(Token::Pipe(_)) => {
                state.advance(); // consume |

                // Parse tail pattern
                let tail = pattern().parse(state)?;

                // Expect closing bracket
                let end_pos = match state.peek() {
                    Some(Token::RBracket(tok)) => {
                        let pos = tok.position.clone();
                        state.advance();
                        pos
                    }
                    _ => return Err(ParseError::new("expected ']' after tail pattern")),
                };

                Ok(Pattern::ListCons(ListConsPattern {
                    head: Box::new(head),
                    tail: Box::new(tail),
                    position: start_pos.merge(&end_pos),
                    info: (),
                }))
            }
            _ => {
                // Not a cons pattern - this is likely an error for now
                // (we don't support list literal patterns like [1, 2, 3] yet)
                Err(ParseError::new(
                    "expected '|' for cons pattern or ']' for empty list",
                ))
            }
        }
    })
}
