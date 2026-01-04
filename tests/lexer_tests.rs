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

// ===== Boolean and Comparison Operator Lexer Tests =====

#[test]
fn lex_boolean_literals() {
    let tokens = Token::lex("true false").unwrap();
    assert_eq!(tokens.len(), 2);
    assert!(matches!(tokens[0], Token::True(_)));
    assert!(matches!(tokens[1], Token::False(_)));
}

#[test]
fn lex_boolean_not_identifiers() {
    // Ensure true/false are keywords, not identifiers
    let tokens = Token::lex("true").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::True(_)));
    assert!(!matches!(tokens[0], Token::Ident(_)));
}

#[test]
fn lex_comparison_operators() {
    let tokens = Token::lex("== != < > <= >=").unwrap();
    assert_eq!(tokens.len(), 6);
    assert!(matches!(tokens[0], Token::DoubleEquals(_)));
    assert!(matches!(tokens[1], Token::NotEquals(_)));
    assert!(matches!(tokens[2], Token::LessThan(_)));
    assert!(matches!(tokens[3], Token::GreaterThan(_)));
    assert!(matches!(tokens[4], Token::LessEquals(_)));
    assert!(matches!(tokens[5], Token::GreaterEquals(_)));
}

#[test]
fn lex_comparison_longest_match() {
    // Test that <= is one token, not < followed by =
    let tokens = Token::lex("<=").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::LessEquals(_)));

    // Same for >=
    let tokens = Token::lex(">=").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::GreaterEquals(_)));

    // And ==
    let tokens = Token::lex("==").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::DoubleEquals(_)));
}

#[test]
fn lex_arithmetic_operators() {
    let tokens = Token::lex("+ - * /").unwrap();
    assert_eq!(tokens.len(), 4);
    assert!(matches!(tokens[0], Token::Plus(_)));
    assert!(matches!(tokens[1], Token::Minus(_)));
    assert!(matches!(tokens[2], Token::Star(_)));
    assert!(matches!(tokens[3], Token::Slash(_)));
}

// ===== Logical Operator Lexer Tests =====

#[test]
fn lex_logical_operators() {
    let tokens = Token::lex("&& || !").unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(tokens[0], Token::LogicalAnd(_)));
    assert!(matches!(tokens[1], Token::LogicalOr(_)));
    assert!(matches!(tokens[2], Token::LogicalNot(_)));
}

#[test]
fn lex_logical_and_longest_match() {
    // Test that && is one token, not two separate tokens
    let tokens = Token::lex("&&").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::LogicalAnd(_)));
}

#[test]
fn lex_logical_or_longest_match() {
    // Test that || is one token
    let tokens = Token::lex("||").unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0], Token::LogicalOr(_)));
}

#[test]
fn lex_logical_not_separate() {
    // Test that ! is separate from !=
    let tokens = Token::lex("! !=").unwrap();
    assert_eq!(tokens.len(), 2);
    assert!(matches!(tokens[0], Token::LogicalNot(_)));
    assert!(matches!(tokens[1], Token::NotEquals(_)));
}

// Comment Tests

#[test]
fn strip_single_line_comment() {
    use ruskell::lexer::strip_comments;

    let source = "main = 42 -- this is a comment";
    let stripped = strip_comments(source);
    assert_eq!(stripped, "main = 42                     ");

    // Should preserve line structure
    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], Token::Ident(i) if i.value == "main"));
    assert!(matches!(tokens[1], Token::Equals(_)));
    assert!(matches!(&tokens[2], Token::Integer(i) if i.value == "42"));
}

#[test]
fn strip_single_line_comment_entire_line() {
    use ruskell::lexer::strip_comments;

    let source = "-- Full line comment\nmain = 42";
    let stripped = strip_comments(source);
    assert_eq!(stripped, "                    \nmain = 42");

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
}

#[test]
fn strip_multi_line_comment() {
    use ruskell::lexer::strip_comments;

    let source = "x = {- this is a comment -} 1";
    let stripped = strip_comments(source);
    assert_eq!(stripped, "x =                         1");

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], Token::Ident(i) if i.value == "x"));
    assert!(matches!(tokens[1], Token::Equals(_)));
    assert!(matches!(&tokens[2], Token::Integer(i) if i.value == "1"));
}

#[test]
fn strip_nested_multi_line_comment() {
    use ruskell::lexer::strip_comments;

    let source = "y = {- outer {- inner -} comment -} 2";
    let stripped = strip_comments(source);
    assert_eq!(stripped, "y =                                 2");

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], Token::Ident(i) if i.value == "y"));
}

#[test]
fn strip_multi_line_comment_multiline() {
    use ruskell::lexer::strip_comments;

    let source = "a = {- comment\nspanning\nlines -} 3";
    let stripped = strip_comments(source);
    // Should preserve newlines
    assert_eq!(stripped, "a =           \n        \n         3");

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
}

#[test]
fn strip_mixed_comments() {
    use ruskell::lexer::strip_comments;

    let source = r#"
-- Single line comment
main = do
    x := 42  -- inline comment
    {- multi-line
       comment -}
    y := x + 1
    y
end
"#;
    let stripped = strip_comments(source);
    let tokens = Token::lex(&stripped).unwrap();

    // Should have tokens for: main, =, do, x, :=, 42, y, :=, x, +, 1, y, end
    assert!(
        tokens
            .iter()
            .any(|t| matches!(t, Token::Ident(i) if i.value == "main"))
    );
    assert!(tokens.iter().any(|t| matches!(t, Token::Do(_))));
    assert!(tokens.iter().any(|t| matches!(t, Token::End(_))));
}

#[test]
fn preserve_minus_in_expressions() {
    use ruskell::lexer::strip_comments;

    // Make sure single minus is not treated as comment start
    let source = "x = 5 - 3";
    let stripped = strip_comments(source);
    assert_eq!(stripped, source); // Should be unchanged

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 5);
    assert!(matches!(tokens[3], Token::Minus(_))); // Token 3 is the minus
}

#[test]
fn comment_at_end_of_file() {
    use ruskell::lexer::strip_comments;

    let source = "main = 42\n-- comment at end";
    let stripped = strip_comments(source);

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
}

#[test]
fn unclosed_multi_line_comment() {
    use ruskell::lexer::strip_comments;

    // Unclosed comment - everything after {- is stripped
    let source = "x = {- unclosed comment\nmain = 42";
    let stripped = strip_comments(source);

    // Should strip everything after {-
    assert_eq!(stripped, "x =                    \n         ");
}

#[test]
fn deeply_nested_comments() {
    use ruskell::lexer::strip_comments;

    let source = "x = {- a {- b {- c -} b -} a -} 1";
    let stripped = strip_comments(source);

    let tokens = Token::lex(&stripped).unwrap();
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[2], Token::Integer(i) if i.value == "1"));
}
