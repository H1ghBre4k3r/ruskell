use ruskell::desugar::desugar_program;
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};
use ruskell::types::validate::ValidationError;
use ruskell::types::validate_and_type_check;

/// Helper to parse, desugar and validate a program
fn validate(input: &str) -> Result<(), Vec<ValidationError>> {
    let lexed = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(lexed);
    let (program, _) = parse(&mut state);
    let program = program.expect("parsing failed");
    let desugared = desugar_program(program);
    validate_and_type_check(desugared).map(|_| ())
}

#[test]
fn test_valid_main_returns_int() {
    let input = r#"
        main () = 42
    "#;
    let result = validate(input);
    assert!(result.is_ok(), "Expected validation to pass");
}

#[test]
fn test_valid_main_returns_unit() {
    let input = r#"
        main () = ()
    "#;
    let result = validate(input);
    assert!(result.is_ok(), "Expected validation to pass");
}

#[test]
fn test_valid_main_with_do_block() {
    let input = r#"
        main () = do
            x := 42
            x
        end
    "#;
    let result = validate(input);
    assert!(result.is_ok(), "Expected validation to pass");
}

#[test]
fn test_missing_main() {
    let input = r#"
        helper x = x + 1
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ValidationError::MissingMain { .. } => {
                // Expected error
            }
            other => panic!("Expected MissingMain error, got: {:?}", other),
        }
    }
}

#[test]
fn test_main_wrong_param_type() {
    let input = r#"
        main x = x + 1
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ValidationError::InvalidMainType { .. } => {
                // Expected error - main takes Int parameter instead of Unit
            }
            other => panic!("Expected InvalidMainType error, got: {:?}", other),
        }
    }
}

#[test]
fn test_main_wrong_return_type() {
    let input = r#"
        main () = "hello"
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ValidationError::InvalidMainType { .. } => {
                // Expected error - main returns String instead of Int/Unit
            }
            other => panic!("Expected InvalidMainType error, got: {:?}", other),
        }
    }
}

#[test]
fn test_main_returns_lambda() {
    let input = r#"
        main () = \x => x
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ValidationError::InvalidMainType { .. } => {
                // Expected error - main returns a function
            }
            other => panic!("Expected InvalidMainType error, got: {:?}", other),
        }
    }
}

#[test]
fn test_main_with_helpers() {
    let input = r#"
        add x y = x + y
        multiply x y = x * y

        main () = do
            sum := add(5, 10)
            product := multiply(sum, 2)
            product
        end
    "#;
    let result = validate(input);
    assert!(result.is_ok(), "Expected validation to pass");
}

#[test]
fn test_type_error_wrapped() {
    let input = r#"
        main () = do
            x := 42
            y := x + "hello"
            y
        end
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert!(!errors.is_empty());
        // Should contain a TypeError wrapped in ValidationError
        match &errors[0] {
            ValidationError::TypeError(_) => {
                // Expected - type error (Int + String)
            }
            other => panic!("Expected TypeError, got: {:?}", other),
        }
    }
}

#[test]
fn test_main_with_conditionals() {
    let input = r#"
        main () = if true then 42 else 0 end
    "#;
    let result = validate(input);
    assert!(result.is_ok(), "Expected validation to pass");
}

#[test]
fn test_main_conditional_wrong_types() {
    let input = r#"
        main () = if true then 42 else () end
    "#;
    let result = validate(input);
    assert!(result.is_err(), "Expected validation to fail");

    if let Err(errors) = result {
        assert!(!errors.is_empty());
        // Should be a type error - branches don't unify
        match &errors[0] {
            ValidationError::TypeError(_) => {
                // Expected
            }
            other => panic!("Expected TypeError, got: {:?}", other),
        }
    }
}
