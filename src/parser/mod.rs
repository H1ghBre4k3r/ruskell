mod combinators;
mod grammar;

pub use combinators::*;
pub use grammar::*;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct ParseState {
    tokens: Vec<Token>,
    index: usize,
}

impl ParseState {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.has_next() {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    pub fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }

    pub fn position(&self) -> usize {
        self.index
    }

    pub fn restore(&mut self, position: usize) {
        self.index = position;
    }
}

pub trait Parser<T>: Sized {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T>;
}

// Allow closures to be parsers
impl<T, F: Fn(&mut ParseState) -> ParseResult<T>> Parser<T> for F {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T> {
        self(state)
    }
}
