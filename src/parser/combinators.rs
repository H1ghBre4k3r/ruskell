use std::ops::{Add, BitOr, Mul, Shr, Sub};
use std::rc::Rc;

use crate::lexer::Token;

use super::{ParseError, ParseResult, ParseState, Parser};

// === Boxed Parser for type erasure ===

pub struct BoxedParser<T> {
    parser: Rc<dyn Fn(&mut ParseState) -> ParseResult<T>>,
}

impl<T> Clone for BoxedParser<T> {
    fn clone(&self) -> Self {
        BoxedParser {
            parser: Rc::clone(&self.parser),
        }
    }
}

impl<T: 'static> BoxedParser<T> {
    pub fn new<P: Parser<T> + 'static>(parser: P) -> Self {
        BoxedParser {
            parser: Rc::new(move |state| parser.parse(state)),
        }
    }
}

impl<T> Parser<T> for BoxedParser<T> {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T> {
        (self.parser)(state)
    }
}

// === Combinators as methods ===

impl<T: 'static> BoxedParser<T> {
    /// Sequence: parse self then other, return (T, U)
    pub fn seq<U: 'static>(self, other: BoxedParser<U>) -> BoxedParser<(T, U)> {
        BoxedParser::new(move |state: &mut ParseState| {
            let a = self.parse(state)?;
            let b = other.parse(state)?;
            Ok((a, b))
        })
    }

    /// Keep left: parse self then other, discard other's result
    pub fn skip<U: 'static>(self, other: BoxedParser<U>) -> BoxedParser<T> {
        BoxedParser::new(move |state: &mut ParseState| {
            let a = self.parse(state)?;
            let _ = other.parse(state)?;
            Ok(a)
        })
    }

    /// Keep right: parse self then other, discard self's result
    pub fn skip_left<U: 'static>(self, other: BoxedParser<U>) -> BoxedParser<U> {
        BoxedParser::new(move |state: &mut ParseState| {
            let _ = self.parse(state)?;
            other.parse(state)
        })
    }

    /// Map: transform result
    pub fn map<U: 'static, F: Fn(T) -> U + 'static>(self, f: F) -> BoxedParser<U> {
        BoxedParser::new(move |state: &mut ParseState| {
            let a = self.parse(state)?;
            Ok(f(a))
        })
    }

    /// Choice: try self, if fails try other
    pub fn or(self, other: BoxedParser<T>) -> BoxedParser<T> {
        BoxedParser::new(move |state: &mut ParseState| {
            let pos = state.position();
            match self.parse(state) {
                Ok(a) => Ok(a),
                Err(_) => {
                    state.restore(pos);
                    other.parse(state)
                }
            }
        })
    }
}

// === Operator Overloading ===

/// `+` for sequence: A + B -> (A, B)
impl<T: 'static, U: 'static> Add<BoxedParser<U>> for BoxedParser<T> {
    type Output = BoxedParser<(T, U)>;

    fn add(self, rhs: BoxedParser<U>) -> Self::Output {
        self.seq(rhs)
    }
}

/// `-` for keep left: A - B -> A (parse B, discard result)
impl<T: 'static, U: 'static> Sub<BoxedParser<U>> for BoxedParser<T> {
    type Output = BoxedParser<T>;

    fn sub(self, rhs: BoxedParser<U>) -> Self::Output {
        self.skip(rhs)
    }
}

/// `*` for keep right: A * B -> B (parse A, discard result)
impl<T: 'static, U: 'static> Mul<BoxedParser<U>> for BoxedParser<T> {
    type Output = BoxedParser<U>;

    fn mul(self, rhs: BoxedParser<U>) -> Self::Output {
        self.skip_left(rhs)
    }
}

/// `|` for choice: A | B -> A or B
impl<T: 'static> BitOr<BoxedParser<T>> for BoxedParser<T> {
    type Output = BoxedParser<T>;

    fn bitor(self, rhs: BoxedParser<T>) -> Self::Output {
        self.or(rhs)
    }
}

/// `>>` for map: A >> fn -> B
impl<T: 'static, U: 'static, F: Fn(T) -> U + 'static> Shr<F> for BoxedParser<T> {
    type Output = BoxedParser<U>;

    fn shr(self, f: F) -> Self::Output {
        self.map(f)
    }
}

// === Primitive Parsers ===

pub fn token<F: Fn(&Token) -> bool + 'static>(predicate: F) -> BoxedParser<Token> {
    BoxedParser::new(move |state: &mut ParseState| match state.peek() {
        Some(tok) if predicate(tok) => Ok(state.next().unwrap()),
        Some(tok) => Err(ParseError::new(format!("unexpected token: {:?}", tok))),
        None => Err(ParseError::new("unexpected end of input")),
    })
}

pub fn expect_do() -> BoxedParser<Token> {
    token(|t| matches!(t, Token::Do(_)))
}

pub fn expect_end() -> BoxedParser<Token> {
    token(|t| matches!(t, Token::End(_)))
}

pub fn expect_equals() -> BoxedParser<Token> {
    token(|t| matches!(t, Token::Equals(_)))
}

pub fn expect_assign() -> BoxedParser<Token> {
    token(|t| matches!(t, Token::Assign(_)))
}

pub fn ident() -> BoxedParser<crate::lexer::Ident> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::Ident(_)) => {
            if let Token::Ident(id) = state.next().unwrap() {
                Ok(id)
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

pub fn integer() -> BoxedParser<crate::lexer::Integer> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::Integer(_)) => {
            if let Token::Integer(int) = state.next().unwrap() {
                Ok(int)
            } else {
                unreachable!()
            }
        }
        Some(tok) => Err(ParseError::new(format!("expected integer, got {:?}", tok))),
        None => Err(ParseError::new("expected integer, got end of input")),
    })
}

pub fn string_literal() -> BoxedParser<crate::lexer::StringLiteral> {
    BoxedParser::new(|state: &mut ParseState| match state.peek() {
        Some(Token::StringLiteral(_)) => {
            if let Token::StringLiteral(s) = state.next().unwrap() {
                Ok(s)
            } else {
                unreachable!()
            }
        }
        Some(tok) => Err(ParseError::new(format!("expected string, got {:?}", tok))),
        None => Err(ParseError::new("expected string, got end of input")),
    })
}

/// Parse zero or more occurrences
pub fn many<T: 'static>(parser: BoxedParser<T>) -> BoxedParser<Vec<T>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let mut results = Vec::new();
        loop {
            let pos = state.position();
            match parser.parse(state) {
                Ok(item) => results.push(item),
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            }
        }
        Ok(results)
    })
}

/// Parse one or more occurrences
pub fn many1<T: 'static>(parser: BoxedParser<T>) -> BoxedParser<Vec<T>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let first = parser.parse(state)?;
        let mut results = vec![first];
        loop {
            let pos = state.position();
            match parser.parse(state) {
                Ok(item) => results.push(item),
                Err(_) => {
                    state.restore(pos);
                    break;
                }
            }
        }
        Ok(results)
    })
}

/// Optional: parse zero or one
pub fn optional<T: 'static>(parser: BoxedParser<T>) -> BoxedParser<Option<T>> {
    BoxedParser::new(move |state: &mut ParseState| {
        let pos = state.position();
        match parser.parse(state) {
            Ok(item) => Ok(Some(item)),
            Err(_) => {
                state.restore(pos);
                Ok(None)
            }
        }
    })
}
