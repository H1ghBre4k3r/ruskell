use std::ops::{Add, BitOr, Mul, Shr, Sub};
use std::rc::Rc;

use crate::lexer::Token;

use super::state::{ParseError, ParseResult, ParseState, Parser};

type ParserFn<T> = Rc<dyn Fn(&mut ParseState) -> ParseResult<T>>;

// === Boxed Parser for type erasure ===

pub struct BoxedParser<T> {
    parser: ParserFn<T>,
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
                    // Error is already recorded in state by the parser
                    state.restore(pos);
                    other.parse(state)
                }
            }
        })
    }

    /// Add a label to this parser for better error messages
    pub fn label(self, name: &'static str) -> BoxedParser<T> {
        BoxedParser::new(move |state: &mut ParseState| match self.parse(state) {
            Ok(v) => Ok(v),
            Err(mut err) => {
                // Replace expected with our label
                err.expected = vec![name.to_string()];
                state.record_error(err.clone());
                Err(err)
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

/// Low-level token parser with custom error - for internal use
fn token_with_error<F: Fn(&Token) -> bool + 'static>(
    predicate: F,
    expected: &'static str,
) -> BoxedParser<Token> {
    BoxedParser::new(move |state: &mut ParseState| match state.peek() {
        Some(tok) if predicate(tok) => Ok(state.advance().unwrap()),
        Some(tok) => {
            let err = ParseError::new("unexpected token")
                .expected(expected)
                .found(tok.describe())
                .at(tok.pos());
            state.record_error(err.clone());
            Err(err)
        }
        None => {
            let err = ParseError::new("unexpected end of input").expected(expected);
            state.record_error(err.clone());
            Err(err)
        }
    })
}

pub fn expect_do() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Do(_)), "'do'")
}

pub fn expect_end() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::End(_)), "'end'")
}

pub fn expect_equals() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Equals(_)), "'='")
}

pub fn expect_assign() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Assign(_)), "':='")
}

pub fn expect_backslash() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Backslash(_)), "'\\'")
}

pub fn expect_arrow() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Arrow(_)), "'=>'")
}

pub fn expect_comma() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Comma(_)), "','")
}

pub fn expect_lparen() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LParen(_)), "'('")
}

pub fn expect_rparen() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::RParen(_)), "')'")
}

pub fn expect_true() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::True(_)), "'true'")
}

pub fn expect_false() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::False(_)), "'false'")
}

pub fn expect_double_equals() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::DoubleEquals(_)), "'=='")
}

pub fn expect_not_equals() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::NotEquals(_)), "'!='")
}

pub fn expect_less_than() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LessThan(_)), "'<'")
}

pub fn expect_greater_than() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::GreaterThan(_)), "'>'")
}

pub fn expect_less_equals() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LessEquals(_)), "'<='")
}

pub fn expect_greater_equals() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::GreaterEquals(_)), "'>='")
}

pub fn expect_logical_and() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LogicalAnd(_)), "'&&'")
}

pub fn expect_logical_or() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LogicalOr(_)), "'||'")
}

pub fn expect_logical_not() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::LogicalNot(_)), "'!'")
}

pub fn expect_if() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::If(_)), "'if'")
}

pub fn expect_then() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Then(_)), "'then'")
}

pub fn expect_else() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Else(_)), "'else'")
}

pub fn expect_case() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Case(_)), "'case'")
}

pub fn expect_of() -> BoxedParser<Token> {
    token_with_error(|t| matches!(t, Token::Of(_)), "'of'")
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
#[allow(dead_code)]
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
