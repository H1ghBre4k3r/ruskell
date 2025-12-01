use lachs::Span;

#[lachs::token]
pub enum Token {
    #[terminal("do")]
    Do,
    #[terminal("end")]
    End,
    #[literal("[a-zA-Z']*")]
    Ident,
    #[literal("[0-9]*")]
    Integer,
    #[literal(r#""([^"\\]|\\.)*""#)]
    StringLiteral,
    #[terminal("=")]
    Equals,
    #[terminal(":")]
    Colon,
    #[terminal("::")]
    DoubleColon,
    #[terminal(":=")]
    Assign,
    #[terminal("\\")]
    Backslash,
    #[terminal("=>")]
    Arrow,
    #[terminal(",")]
    Comma,
    #[terminal("(")]
    LParen,
    #[terminal(")")]
    RParen,
}

impl Token {
    pub fn pos(&self) -> Span {
        match self {
            Token::Do(inner) => inner.position.clone(),
            Token::End(inner) => inner.position.clone(),
            Token::Ident(inner) => inner.position.clone(),
            Token::Integer(inner) => inner.position.clone(),
            Token::StringLiteral(inner) => inner.position.clone(),
            Token::Equals(inner) => inner.position.clone(),
            Token::Colon(inner) => inner.position.clone(),
            Token::DoubleColon(inner) => inner.position.clone(),
            Token::Assign(inner) => inner.position.clone(),
            Token::Backslash(inner) => inner.position.clone(),
            Token::Arrow(inner) => inner.position.clone(),
            Token::Comma(inner) => inner.position.clone(),
            Token::LParen(inner) => inner.position.clone(),
            Token::RParen(inner) => inner.position.clone(),
        }
    }
}
