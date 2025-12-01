use ruskell::lexer::Token;

#[test]
fn lex_keywords() {
    let tokens = Token::lex("do end").unwrap();
    assert_eq!(tokens.len(), 2);
    assert!(matches!(tokens[0], Token::Do(_)));
    assert!(matches!(tokens[1], Token::End(_)));
}

#[test]
fn lex_identifiers() {
    let tokens = Token::lex("foo bar baz").unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], Token::Ident(i) if i.value == "foo"));
    assert!(matches!(&tokens[1], Token::Ident(i) if i.value == "bar"));
    assert!(matches!(&tokens[2], Token::Ident(i) if i.value == "baz"));
}

#[test]
fn lex_integers() {
    let tokens = Token::lex("42 0 123").unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], Token::Integer(i) if i.value == "42"));
    assert!(matches!(&tokens[1], Token::Integer(i) if i.value == "0"));
    assert!(matches!(&tokens[2], Token::Integer(i) if i.value == "123"));
}

#[test]
fn lex_string_literals() {
    let tokens = Token::lex(r#""hello" "world""#).unwrap();
    assert_eq!(tokens.len(), 2);
    assert!(matches!(&tokens[0], Token::StringLiteral(_)));
    assert!(matches!(&tokens[1], Token::StringLiteral(_)));
}

#[test]
fn lex_operators() {
    let tokens = Token::lex("= := => \\ , ( )").unwrap();
    assert_eq!(tokens.len(), 7);
    assert!(matches!(tokens[0], Token::Equals(_)));
    assert!(matches!(tokens[1], Token::Assign(_)));
    assert!(matches!(tokens[2], Token::Arrow(_)));
    assert!(matches!(tokens[3], Token::Backslash(_)));
    assert!(matches!(tokens[4], Token::Comma(_)));
    assert!(matches!(tokens[5], Token::LParen(_)));
    assert!(matches!(tokens[6], Token::RParen(_)));
}

#[test]
fn lex_function_definition() {
    let tokens = Token::lex("main = do end").unwrap();
    assert_eq!(tokens.len(), 4);
    assert!(matches!(&tokens[0], Token::Ident(i) if i.value == "main"));
    assert!(matches!(tokens[1], Token::Equals(_)));
    assert!(matches!(tokens[2], Token::Do(_)));
    assert!(matches!(tokens[3], Token::End(_)));
}

#[test]
fn lex_lambda() {
    let tokens = Token::lex(r#"\x => x"#).unwrap();
    assert_eq!(tokens.len(), 4);
    assert!(matches!(tokens[0], Token::Backslash(_)));
    assert!(matches!(&tokens[1], Token::Ident(i) if i.value == "x"));
    assert!(matches!(tokens[2], Token::Arrow(_)));
    assert!(matches!(&tokens[3], Token::Ident(i) if i.value == "x"));
}
