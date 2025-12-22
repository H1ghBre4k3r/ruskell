//! Literal parsers for the Ruskell language

use crate::ast::expression::{Boolean, Ident, Integer, StringLiteral, Unit};
use crate::lexer::Token;

use crate::parser::combinators::{BoxedParser, expect_lparen, expect_rparen};
use crate::parser::state::{ParseError, ParseState, Parser};

/// Parse a unit literal: "()"
pub fn unit() -> BoxedParser<Unit<()>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let start = expect_lparen().parse(state)?;
        let end = expect_rparen().parse(state)?;
        Ok(Unit {
            position: start.pos().merge(&end.pos()),
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
        Some(tok) => {
            let err = ParseError::new("unexpected token")
                .expected("identifier")
                .found(tok.describe())
                .at(tok.pos());
            state.record_error(err.clone());
            Err(err)
        }
        None => {
            let err = ParseError::new("unexpected end of input").expected("identifier");
            state.record_error(err.clone());
            Err(err)
        }
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
        Some(tok) => {
            let err = ParseError::new("unexpected token")
                .expected("integer")
                .found(tok.describe())
                .at(tok.pos());
            state.record_error(err.clone());
            Err(err)
        }
        None => {
            let err = ParseError::new("unexpected end of input").expected("integer");
            state.record_error(err.clone());
            Err(err)
        }
    })
}

/// Parse a string literal
pub fn string_literal() -> BoxedParser<StringLiteral<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::StringLiteral(_)) => {
            if let Token::StringLiteral(s) = state.advance().unwrap() {
                // Strip surrounding quotes from the string literal
                let value =
                    if s.value.len() >= 2 && s.value.starts_with('"') && s.value.ends_with('"') {
                        s.value[1..s.value.len() - 1].to_string()
                    } else {
                        s.value
                    };
                Ok(StringLiteral {
                    value,
                    position: s.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(tok) => {
            let err = ParseError::new("unexpected token")
                .expected("string")
                .found(tok.describe())
                .at(tok.pos());
            state.record_error(err.clone());
            Err(err)
        }
        None => {
            let err = ParseError::new("unexpected end of input").expected("string");
            state.record_error(err.clone());
            Err(err)
        }
    })
}

/// Parse a boolean literal
pub fn boolean() -> BoxedParser<Boolean<()>> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::True(_)) => {
            if let Token::True(t) = state.advance().unwrap() {
                Ok(Boolean {
                    value: true,
                    position: t.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(Token::False(_)) => {
            if let Token::False(f) = state.advance().unwrap() {
                Ok(Boolean {
                    value: false,
                    position: f.position,
                    info: (),
                })
            } else {
                unreachable!()
            }
        }
        Some(tok) => {
            let err = ParseError::new("unexpected token")
                .expected("boolean")
                .found(tok.describe())
                .at(tok.pos());
            state.record_error(err.clone());
            Err(err)
        }
        None => {
            let err = ParseError::new("unexpected end of input").expected("boolean");
            state.record_error(err.clone());
            Err(err)
        }
    })
}
