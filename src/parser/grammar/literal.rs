//! Literal parsers for the Ruskell language

use crate::ast::expression::{Ident, Integer, StringLiteral, Unit};
use crate::lexer::Token;

use crate::parser::combinators::{BoxedParser, expect_lparen, expect_rparen};
use crate::parser::state::{ParseError, ParseState, Parser};

/// Parse a unit literal: "()"
pub fn unit() -> BoxedParser<Unit<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_lparen().parse(state)?.pos();
        let end = expect_rparen().parse(state)?.pos();
        Ok(Unit {
            position: start.merge(&end),
            info: (),
        })
    })
}

/// Parse an identifier
pub fn ident() -> BoxedParser<Ident<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::Ident(_)) => {
            if let Token::Ident(id) = state.advance().unwrap() {
                Ok(Ident {
                    value: id.value,
                    position: id.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(tok) => Err(ParseError::new(format!(
            "expected identifier, got {:?}",
            tok
        ))),
        None => Err(ParseError::new("expected identifier, got end of input")),
    })
}

/// Parse an integer literal
pub fn integer() -> BoxedParser<Integer<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::Integer(_)) => {
            if let Token::Integer(int) = state.advance().unwrap() {
                Ok(Integer {
                    value: int.value.parse().expect("The grammar should prevent this"),
                    position: int.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(tok) => Err(ParseError::new(format!("expected integer, got {:?}", tok))),
        None => Err(ParseError::new("expected integer, got end of input")),
    })
}

/// Parse a string literal
pub fn string_literal() -> BoxedParser<StringLiteral<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::StringLiteral(_)) => {
            if let Token::StringLiteral(s) = state.advance().unwrap() {
                Ok(StringLiteral {
                    value: s.value,
                    position: s.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(tok) => Err(ParseError::new(format!("expected string, got {:?}", tok))),
        None => Err(ParseError::new("expected string, got end of input")),
    })
}
