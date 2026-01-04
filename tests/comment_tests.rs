use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

#[test]
fn single_line_comment_ignored() {
    let source = r#"
        -- This is a comment
        main = 42
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    // Should not have any tokens for the comment
    let has_comment = tokens.iter().any(|t| {
        if let Token::Ident(i) = t {
            i.value == "This" || i.value == "comment"
        } else {
            false
        }
    });
    assert!(!has_comment, "Comment tokens should be stripped");
}

#[test]
fn multi_line_comment_ignored() {
    let source = r#"
        {- This is a
           multi-line comment -}
        main = 42
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    let has_comment = tokens.iter().any(|t| {
        if let Token::Ident(i) = t {
            i.value == "This" || i.value == "comment"
        } else {
            false
        }
    });
    assert!(!has_comment, "Comment tokens should be stripped");
}

#[test]
fn inline_comments() {
    let source = r#"
        factorial 0 = 1  -- base case
        factorial n = n * factorial(n - 1)  -- recursive case
        main = factorial(5)
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    // Should have tokens for factorial but not for "base" or "case"
    let has_factorial = tokens
        .iter()
        .any(|t| matches!(t, Token::Ident(i) if i.value == "factorial"));
    let has_base = tokens
        .iter()
        .any(|t| matches!(t, Token::Ident(i) if i.value == "base"));

    assert!(has_factorial, "Should have factorial token");
    assert!(!has_base, "Should not have comment tokens");
}

#[test]
fn nested_comments() {
    let source = r#"
        {- Outer {- inner -} comment -}
        main = 1
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    let has_comment = tokens.iter().any(|t| {
        if let Token::Ident(i) = t {
            i.value == "Outer" || i.value == "inner"
        } else {
            false
        }
    });
    assert!(!has_comment);
}

#[test]
fn comments_preserve_line_numbers() {
    // Test that comments preserve line structure for error reporting
    let source = "-- comment\nmain = invalid syntax here";
    let stripped = ruskell::lexer::strip_comments(source);

    // Should have a newline preserved
    assert!(stripped.contains('\n'));

    // The error position should still point to the correct line
    let tokens = Token::lex(&stripped).unwrap();
    let mut state = ParseState::new(tokens);
    let (_program, errors) = parse(&mut state);

    // Should have parse errors
    assert!(!errors.is_empty());
}

#[test]
fn mixed_comment_types() {
    let source = r#"
        -- Single line comment
        factorial n = do
            {- Block comment -}
            result := n  -- inline comment
            result
        end
        main = factorial(5)
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    // Should parse successfully
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Should parse without errors");
    assert!(program.is_some(), "Should produce a program");
}

#[test]
fn comment_with_special_chars() {
    let source = r#"
        -- Comment with special chars: =, :=, =>, \, etc.
        {- Comment with {- nested -} and => arrows -}
        main = 42
    "#;

    let source = ruskell::lexer::strip_comments(source);
    let tokens = Token::lex(&source).unwrap();

    // Should only have main, =, 42
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty());
    assert!(program.is_some());
}
