use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

#[test]
fn test_parse_empty_list() {
    let input = r#"
        main = []
    "#;
    let tokens = Token::lex(input).expect("lex failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(
        errors.is_empty(),
        "Failed to parse empty list: {:?}",
        errors
    );
    assert!(program.is_some(), "No program parsed");
}

#[test]
fn test_parse_list_literal() {
    let input = r#"
        main = [1, 2, 3]
    "#;
    let tokens = Token::lex(input).expect("lex failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(
        errors.is_empty(),
        "Failed to parse list literal: {:?}",
        errors
    );
    assert!(program.is_some(), "No program parsed");
}

#[test]
fn test_parse_nested_list() {
    let input = r#"
        main = [[1, 2], [3, 4]]
    "#;
    let tokens = Token::lex(input).expect("lex failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(
        errors.is_empty(),
        "Failed to parse nested list: {:?}",
        errors
    );
    assert!(program.is_some(), "No program parsed");
}
