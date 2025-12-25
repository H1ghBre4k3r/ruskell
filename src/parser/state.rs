//! # Parser State Management
//!
//! This module provides the core infrastructure for parser combinators, including:
//! - **ParseState**: Manages token stream position, backtracking, and error tracking
//! - **ParseError**: Structured error types for parse failures
//! - **Parser trait**: Generic interface for all parsers
//!
//! ## Parser State Management
//!
//! The `ParseState` struct maintains:
//!
//! 1. **Token stream position** (`tokens`, `index`) - Current position in token sequence
//! 2. **Backtracking support** - Ability to restore previous positions for backtracking parsers
//! 3. **Error tracking** - Records furthest error encountered and collects all errors
//!
//! ### Backtracking
//!
//! Parser combinators need to try alternative parsing strategies and fall back when they fail.
//! The `ParseState` supports this through:
//!
//! ```text
//! let pos = state.position();     // Save current position
//! match parser.parse(state) {
//!     Ok(result) => result,
//!     Err(_) => {
//!         state.restore(pos);     // Restore and try next alternative
//!         alternative.parse(state)
//!     }
//! }
//! ```
//!
//! ### Error Recovery
//!
//! The parser implements error recovery to report multiple errors instead of failing at the first:
//!
//! - **Furthest error tracking**: Records error at furthest position reached
//! - **Error collection**: Can collect errors while attempting to recover
//! - **Merge logic**: Combines errors from alternative parsers (e.g., "expected A or B")
//!
//! ## ParseError Design
//!
//! `ParseError` captures:
//!
//! - **message**: Human-readable error description
//! - **span**: Source location for error reporting
//! - **expected**: List of expected tokens/constructs
//! - **found**: What was actually found (if available)
//!
//! This enables rich error messages like:
//!
//! ```text
//! error: expected identifier or '(', found '123'
//!   --> file.ruskell:10:5
//!    |
//! 10 |     add(1, 2)
//!    |         ^^^^
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::parser::combinators`] - Parser combinators built on this state
//! - [`crate::lexer`] - Token types that flow through this state

use lachs::Span;

use crate::lexer::Token;

/// Structured parse error with context information.
///
/// Captures detailed information about why parsing failed, enabling rich
/// error messages with source location and helpful suggestions.
///
/// # Fields
///
/// * `message` - Human-readable error description
/// * `span` - Optional source location for error reporting
/// * `expected` - List of what tokens/constructs were expected
/// * `found` - What was actually found (if available)
///
/// # Example
///
/// ```text
/// ParseError {
///     message: "unexpected token",
///     expected: vec!["identifier", "'('"],
///     found: Some("123"),
///     span: Some(file.ruskell:10:5)
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Option<Box<Span>>,
    pub expected: Vec<String>,
    pub found: Option<String>,
}

impl ParseError {
    /// Create a new parse error with a message.
    ///
    /// # Arguments
    ///
    /// * `msg` - Error message (anything that can be converted to String)
    ///
    /// # Returns
    ///
    /// A new `ParseError` with the given message and no other context
    ///
    /// # Example
    ///
    /// ```text
    /// let err = ParseError::new("expected function definition");
    /// ```
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
            span: None,
            expected: vec![],
            found: None,
        }
    }

    /// Add an expected token/construct to this error.
    ///
    /// Used to build up error messages like "expected A, B, or C".
    ///
    /// # Arguments
    ///
    /// * `what` - Description of what was expected
    ///
    /// # Returns
    ///
    /// Self for method chaining
    ///
    /// # Example
    ///
    /// ```text
    /// ParseError::new("unexpected token")
    ///     .expected("identifier")
    ///     .expected("'('")
    /// ```
    pub fn expected(mut self, what: impl Into<String>) -> Self {
        self.expected.push(what.into());
        self
    }

    /// Set what was actually found.
    ///
    /// # Arguments
    ///
    /// * `what` - Description of what was found (e.g., a token description)
    ///
    /// # Returns
    ///
    /// Self for method chaining
    pub fn found(mut self, what: impl Into<String>) -> Self {
        self.found = Some(what.into());
        self
    }

    /// Set source location for this error.
    ///
    /// # Arguments
    ///
    /// * `span` - Source span (line/column information)
    ///
    /// # Returns
    ///
    /// Self for method chaining
    pub fn at(mut self, span: Span) -> Self {
        self.span = Some(Box::new(span));
        self
    }

    /// Merge expected tokens from another error.
    ///
    /// Used when combining alternative parsers - if both alternatives fail,
    /// we merge their "expected" lists to show all possibilities.
    ///
    /// # Example
    ///
    /// ```text
    /// // Parser 1 fails: expected "int"
    /// // Parser 2 fails: expected "string"
    /// // Merged: expected "int" or "string"
    /// ```
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

/// Result type for parse operations.
///
/// A convenience alias for `Result<T, ParseError>` used throughout
/// the parser combinator library.
pub type ParseResult<T> = Result<T, ParseError>;

/// Parser state: token stream, position, and error tracking.
///
/// `ParseState` is a mutable context that all parsers operate on.
/// It maintains the current position in the token stream, supports
/// backtracking, and tracks parse errors.
///
/// # Design Rationale
///
/// The parser combinator design requires:
/// 1. **Backtracking**: Parsers can fail and restore previous state
/// 2. **Error tracking**: Record furthest error to provide useful messages
/// 3. **Position tracking**: Know current location in token stream
///
/// All three are encapsulated in `ParseState` to keep parser functions
/// simple (they only take `&mut ParseState`).
///
/// # Fields
///
/// * `tokens` - The complete token stream being parsed
/// * `index` - Current position in the token stream (next token to consume)
/// * `furthest_error` - Error at the furthest position reached (for error reporting)
/// * `collected_errors` - All errors encountered during parsing (for error recovery)
///
/// # Example Usage
///
/// ```text
/// let mut state = ParseState::new(tokens);
///
/// let pos = state.position();  // Save position for backtracking
/// match parser.parse(&mut state) {
///     Ok(result) => result,
///     Err(_) => {
///         state.restore(pos);  // Restore and try alternative
///         alternative.parse(&mut state)
///     }
/// }
/// ```
pub struct ParseState {
    /// The complete token stream being parsed
    tokens: Vec<Token>,

    /// Current position in the token stream (index of next token to read)
    index: usize,

    /// Error at the furthest position reached during parsing
    ///
    /// We track the furthest error because the "real" parse error is
    /// usually at the furthest point the parser was able to read before
    /// failing completely.
    furthest_error: Option<(usize, ParseError)>,

    /// All errors collected during parsing (for error recovery scenarios)
    collected_errors: Vec<ParseError>,
}

impl ParseState {
    /// Create a new parse state from a token stream.
    ///
    /// # Arguments
    ///
    /// * `tokens` - Complete token stream from the lexer
    ///
    /// # Returns
    ///
    /// A new `ParseState` initialized to parse from the beginning of the stream
    ///
    /// # Example
    ///
    /// ```text
    /// let tokens = lexer::lex("main = 42")?;
    /// let mut state = ParseState::new(tokens);
    /// ```
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            furthest_error: None,
            collected_errors: Vec::new(),
        }
    }

    /// Consume and return the next token.
    ///
    /// Advances the position in the token stream by one and returns
    /// the token that was consumed.
    ///
    /// # Returns
    ///
    /// `Some(token)` if there are more tokens to consume
    /// `None` if at the end of the token stream
    ///
    /// # Example
    ///
    /// ```text
    /// let token = state.advance();  // Consumes token at current index
    /// // state.index is now incremented by 1
    /// ```
    pub fn advance(&mut self) -> Option<Token> {
        if self.has_next() {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Peek at the next token without consuming it.
    ///
    /// # Returns
    ///
    /// `Some(&token)` - Reference to the next token (if any remain)
    /// `None` - If at the end of the token stream
    ///
    /// # Example
    ///
    /// ```text
    /// if let Some(Token::Ident(_)) = state.peek() {
    ///     // Next token is an identifier, but we haven't consumed it yet
    /// }
    /// ```
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    /// Check if there are more tokens to consume.
    ///
    /// # Returns
    ///
    /// `true` if there are tokens remaining, `false` otherwise
    pub fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }

    /// Get the current position in the token stream.
    ///
    /// # Returns
    ///
    /// Current index (position of next token to consume)
    ///
    /// # Usage
    ///
    /// Used with `restore()` to implement backtracking:
    ///
    /// ```text
    /// let pos = state.position();
    /// // ... attempt to parse ...
    /// state.restore(pos);  // Backtrack to saved position
    /// ```
    pub fn position(&self) -> usize {
        self.index
    }

    /// Restore the parser to a previous position.
    ///
    /// Used for backtracking - if a parser fails, we restore
    /// the state to allow an alternative parser to try from
    /// the same position.
    ///
    /// # Arguments
    ///
    /// * `position` - Position to restore to (from `position()`)
    ///
    /// # Example
    ///
    /// ```text
    /// let pos = state.position();
    /// match parser1.parse(&mut state) {
    ///     Ok(result) => result,
    ///     Err(_) => {
    ///         state.restore(pos);
    ///         parser2.parse(&mut state)  // Try alternative
    ///     }
    /// }
    /// ```
    pub fn restore(&mut self, position: usize) {
        self.index = position;
    }

    /// Record an error, keeping track of the furthest position reached.
    ///
    /// The parser maintains a "furthest" error because in recursive
    /// parsing, the real error is usually at the furthest point
    /// the parser could get before failing completely.
    ///
    /// # Arguments
    ///
    /// * `error` - The parse error to record
    ///
    /// # Furthest Error Logic
    ///
    /// ```text
    /// state.record_error(err1)  // At position 5
    /// state.record_error(err2)  // At position 10 - becomes new furthest
    /// state.record_error(err3)  // At position 10 - merged with err2
    /// state.record_error(err4)  // At position 3 - ignored (not furthest)
    ///
    /// // Final furthest error is at position 10 (merged err2 and err3)
    /// ```
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

    /// Get the furthest error encountered during parsing.
    ///
    /// # Returns
    ///
    /// `Some(&error)` - The error at the furthest position
    /// `None` - If no errors were recorded
    pub fn get_furthest_error(&self) -> Option<&ParseError> {
        self.furthest_error.as_ref().map(|(_, e)| e)
    }

    /// Collect an error for later reporting (used during error recovery).
    ///
    /// Unlike `record_error()`, this adds errors to a list without
    /// comparing positions. Used when we want to collect multiple
    /// errors (e.g., for comprehensive error reporting).
    ///
    /// # Arguments
    ///
    /// * `error` - The parse error to collect
    pub fn collect_error(&mut self, error: ParseError) {
        self.collected_errors.push(error);
    }

    /// Take the furthest error and add it to the collected errors.
    ///
    /// Used to commit the tracked furthest error to the collected
    /// errors list, typically after we've finished parsing an
    /// alternative or recovered from an error.
    ///
    /// After calling this, `get_furthest_error()` will return `None`
    /// until another error is recorded.
    pub fn commit_furthest_error(&mut self) {
        if let Some((_, err)) = self.furthest_error.take() {
            self.collected_errors.push(err);
        }
    }

    /// Get all collected errors.
    ///
    /// # Returns
    ///
    /// Slice of all errors collected via `collect_error()`
    pub fn get_errors(&self) -> &[ParseError] {
        &self.collected_errors
    }

    /// Take all collected errors, leaving the list empty.
    ///
    /// # Returns
    ///
    /// Vector of all collected errors
    ///
    /// # Usage
    ///
    /// Used to retrieve errors after parsing is complete,
    /// typically to display them to the user.
    ///
    /// ```text
    /// let errors = state.take_errors();
    /// for err in errors {
    ///     eprintln!("{}", err);
    /// }
    /// ```
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.collected_errors)
    }

    /// Check if any errors were collected.
    ///
    /// # Returns
    ///
    /// `true` if there are collected errors, `false` otherwise
    pub fn has_errors(&self) -> bool {
        !self.collected_errors.is_empty()
    }

    /// Skip tokens until the predicate returns true or end of input.
    ///
    /// Used for error recovery - skip past problematic tokens
    /// to find a good recovery point (e.g., the next function definition).
    ///
    /// # Arguments
    ///
    /// * `predicate` - Function that returns `true` when the recovery point is found
    ///
    /// # Example
    ///
    /// ```text
    /// // Skip until we find an identifier (potential function name)
    /// state.skip_until(|tok| matches!(tok, Token::Ident(_)));
    /// ```
    #[allow(dead_code)]
    pub fn skip_until<F: Fn(&Token) -> bool>(&mut self, predicate: F) {
        while let Some(tok) = self.peek() {
            if predicate(tok) {
                break;
            }
            self.advance();
        }
    }

    /// Create an error at the current position with span info.
    ///
    /// Convenience function that creates a `ParseError` with
    /// source location information automatically attached.
    ///
    /// # Arguments
    ///
    /// * `message` - Error message
    ///
    /// # Returns
    ///
    /// A new `ParseError` positioned at the current token with appropriate
    /// `found` information
    #[allow(dead_code)]
    pub fn error_here(&self, message: impl Into<String>) -> ParseError {
        let msg = message.into();
        match self.peek() {
            Some(tok) => ParseError::new(&msg).at(tok.pos()).found(tok.describe()),
            None => ParseError::new(msg).found("end of input".to_string()),
        }
    }
}

/// Generic parser trait.
///
/// All parsers implement this trait, enabling a unified interface
/// for parser combinators. The trait is implemented for:
/// - Functions that take `&mut ParseState` and return `ParseResult<T>`
/// - Structs that wrap such functions
///
/// # Type Parameter
///
/// * `T` - The type of value produced by the parser (e.g., `Expression<()>`)
///
/// # Example
///
/// ```text
/// fn parse_int(state: &mut ParseState) -> ParseResult<i32> {
///     // ... parse integer ...
/// }
///
/// // Works because functions implement Parser
/// let parser: Box<dyn Parser<i32>> = Box::new(parse_int);
/// ```
pub trait Parser<T>: Sized {
    /// Parse from the given state.
    ///
    /// # Arguments
    ///
    /// * `state` - Mutable reference to the parse state
    ///
    /// # Returns
    ///
    /// `Ok(value)` if parsing succeeded
    /// `Err(ParseError)` if parsing failed (error is recorded in state)
    fn parse(&self, state: &mut ParseState) -> ParseResult<T>;
}

/// Auto-implement Parser for functions matching the expected signature.
///
/// This enables any function that takes `&mut ParseState` and returns
/// `ParseResult<T>` to be used as a parser without explicit wrapping.
impl<T, F: Fn(&mut ParseState) -> ParseResult<T>> Parser<T> for F {
    fn parse(&self, state: &mut ParseState) -> ParseResult<T> {
        self(state)
    }
}
