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
    #[terminal("+")]
    Plus,
    #[terminal("-")]
    Minus,
    #[terminal("*")]
    Star,
    #[terminal("/")]
    Slash,
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
            Token::Plus(inner) => inner.position.clone(),
            Token::Minus(inner) => inner.position.clone(),
            Token::Star(inner) => inner.position.clone(),
            Token::Slash(inner) => inner.position.clone(),
        }
    }

    /// Returns a human-readable description of the token
    pub fn describe(&self) -> String {
        match self {
            Token::Do(_) => "'do'".to_string(),
            Token::End(_) => "'end'".to_string(),
            Token::Ident(inner) => format!("identifier '{}'", inner.value),
            Token::Integer(inner) => format!("integer '{}'", inner.value),
            Token::StringLiteral(inner) => format!("string {}", inner.value),
            Token::Equals(_) => "'='".to_string(),
            Token::Colon(_) => "':'".to_string(),
            Token::DoubleColon(_) => "'::'".to_string(),
            Token::Assign(_) => "':='".to_string(),
            Token::Backslash(_) => "'\\'".to_string(),
            Token::Arrow(_) => "'=>'".to_string(),
            Token::Comma(_) => "','".to_string(),
            Token::LParen(_) => "'('".to_string(),
            Token::RParen(_) => "')'".to_string(),
            Token::Plus(_) => "'+'".to_string(),
            Token::Minus(_) => "'-'".to_string(),
            Token::Star(_) => "'*'".to_string(),
            Token::Slash(_) => "'/'".to_string(),
        }
    }
}
