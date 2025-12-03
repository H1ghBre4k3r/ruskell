use lachs::Span;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Option<Box<Span>>,
    pub expected: Vec<String>,
    pub found: Option<String>,
}

impl ParseError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
            span: None,
            expected: vec![],
            found: None,
        }
    }

    pub fn expected(mut self, what: impl Into<String>) -> Self {
        self.expected.push(what.into());
        self
    }

    pub fn found(mut self, what: impl Into<String>) -> Self {
        self.found = Some(what.into());
        self
    }

    pub fn at(mut self, span: Span) -> Self {
        self.span = Some(Box::new(span));
        self
    }

    /// Merge expected tokens from another error (used when combining alternatives)
    pub fn merge_expected(mut self, other: &ParseError) -> Self {
        for exp in &other.expected {
            if !self.expected.contains(exp) {
                self.expected.push(exp.clone());
            }
        }
        self
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Build a nice error message
        let msg = if !self.expected.is_empty() {
            let expected_str = if self.expected.len() == 1 {
                self.expected[0].clone()
            } else {
                let (last, rest) = self.expected.split_last().unwrap();
                format!("{} or {}", rest.join(", "), last)
            };
            match &self.found {
                Some(found) => format!("expected {}, found {}", expected_str, found),
                None => format!("expected {}", expected_str),
            }
        } else {
            self.message.clone()
        };

        // If we have span info, render with source context
        if let Some(span) = &self.span {
            write!(f, "{}", span.to_string(&msg))
        } else {
            write!(f, "Parse error: {}", msg)
        }
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct ParseState {
    tokens: Vec<Token>,
    index: usize,
    furthest_error: Option<(usize, ParseError)>,
    collected_errors: Vec<ParseError>,
}

impl ParseState {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            furthest_error: None,
            collected_errors: Vec::new(),
        }
    }

    pub fn advance(&mut self) -> Option<Token> {
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

    /// Record an error, keeping track of the furthest position reached
    pub fn record_error(&mut self, error: ParseError) {
        match &self.furthest_error {
            Some((pos, _)) if *pos > self.index => {
                // Keep the existing error if it's further along
            }
            Some((pos, existing)) if *pos == self.index => {
                // Same position: merge expected tokens
                let merged = existing.clone().merge_expected(&error);
                self.furthest_error = Some((self.index, merged));
            }
            _ => {
                // New furthest position
                self.furthest_error = Some((self.index, error));
            }
        }
    }

    /// Get the furthest error encountered during parsing
    pub fn get_furthest_error(&self) -> Option<&ParseError> {
        self.furthest_error.as_ref().map(|(_, e)| e)
    }

    /// Collect an error for later reporting (used during error recovery)
    pub fn collect_error(&mut self, error: ParseError) {
        self.collected_errors.push(error);
    }

    /// Take the furthest error and add it to collected errors, then reset furthest tracking
    pub fn commit_furthest_error(&mut self) {
        if let Some((_, err)) = self.furthest_error.take() {
            self.collected_errors.push(err);
        }
    }

    /// Get all collected errors
    pub fn get_errors(&self) -> &[ParseError] {
        &self.collected_errors
    }

    /// Check if any errors were collected
    pub fn has_errors(&self) -> bool {
        !self.collected_errors.is_empty()
    }

    /// Skip tokens until the predicate returns true or end of input
    #[allow(dead_code)]
    pub fn skip_until<F: Fn(&Token) -> bool>(&mut self, predicate: F) {
        while let Some(tok) = self.peek() {
            if predicate(tok) {
                break;
            }
            self.advance();
        }
    }

    /// Create an error at the current position with span info
    #[allow(dead_code)]
    pub fn error_here(&self, message: impl Into<String>) -> ParseError {
        let msg = message.into();
        match self.peek() {
            Some(tok) => ParseError::new(&msg).at(tok.pos()).found(tok.describe()),
            None => ParseError::new(msg).found("end of input".to_string()),
        }
    }
}

pub trait Parser<T>: Sized {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T>;
}

impl<T, F: Fn(&mut ParseState) -> ParseResult<T>> Parser<T> for F {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T> {
        self(state)
    }
}
