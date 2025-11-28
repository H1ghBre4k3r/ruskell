use crate::lexer::Token;

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
}
