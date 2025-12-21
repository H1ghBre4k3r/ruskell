use lachs::Span;

#[lachs::token]
pub enum Token {
    #[terminal("do")]
    Do,
    #[terminal("end")]
    End,
    #[terminal("true")]
    True,
    #[terminal("false")]
    False,
    #[terminal("if")]
    If,
    #[terminal("then")]
    Then,
    #[terminal("else")]
    Else,
    #[terminal("case")]
    Case,
    #[terminal("of")]
    Of,
    #[terminal("_")]
    Underscore,
    #[literal("[a-zA-Z'][a-zA-Z0-9']*")]
    Ident,
    #[literal("[0-9]+")]
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
    #[terminal("==")]
    DoubleEquals,
    #[terminal("!=")]
    NotEquals,
    #[terminal("<=")]
    LessEquals,
    #[terminal(">=")]
    GreaterEquals,
    #[terminal("<")]
    LessThan,
    #[terminal(">")]
    GreaterThan,
    #[terminal("&&")]
    LogicalAnd,
    #[terminal("||")]
    LogicalOr,
    #[terminal("!")]
    LogicalNot,
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
            Token::True(inner) => inner.position.clone(),
            Token::False(inner) => inner.position.clone(),
            Token::If(inner) => inner.position.clone(),
            Token::Then(inner) => inner.position.clone(),
            Token::Else(inner) => inner.position.clone(),
            Token::Case(inner) => inner.position.clone(),
            Token::Of(inner) => inner.position.clone(),
            Token::Underscore(inner) => inner.position.clone(),
            Token::DoubleEquals(inner) => inner.position.clone(),
            Token::NotEquals(inner) => inner.position.clone(),
            Token::LessEquals(inner) => inner.position.clone(),
            Token::GreaterEquals(inner) => inner.position.clone(),
            Token::LessThan(inner) => inner.position.clone(),
            Token::GreaterThan(inner) => inner.position.clone(),
            Token::LogicalAnd(inner) => inner.position.clone(),
            Token::LogicalOr(inner) => inner.position.clone(),
            Token::LogicalNot(inner) => inner.position.clone(),
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
            Token::True(_) => "'true'".to_string(),
            Token::False(_) => "'false'".to_string(),
            Token::If(_) => "'if'".to_string(),
            Token::Then(_) => "'then'".to_string(),
            Token::Else(_) => "'else'".to_string(),
            Token::Case(_) => "'case'".to_string(),
            Token::Of(_) => "'of'".to_string(),
            Token::Underscore(_) => "'_'".to_string(),
            Token::DoubleEquals(_) => "'=='".to_string(),
            Token::NotEquals(_) => "'!='".to_string(),
            Token::LessEquals(_) => "'<='".to_string(),
            Token::GreaterEquals(_) => "'>='".to_string(),
            Token::LessThan(_) => "'<'".to_string(),
            Token::GreaterThan(_) => "'>'".to_string(),
            Token::LogicalAnd(_) => "'&&'".to_string(),
            Token::LogicalOr(_) => "'||'".to_string(),
            Token::LogicalNot(_) => "'!'".to_string(),
        }
    }
}
