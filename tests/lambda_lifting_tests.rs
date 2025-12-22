//! Integration tests for lambda lifting (closure conversion)
//!
//! These tests verify that lambda lifting correctly transforms lambdas with
//! captured variables into lambdas with explicit parameters.

use ruskell::core::{CoreExpr, CoreLambda, CoreLambdaBody, CoreLambdaParam};
use ruskell::desugar::{desugar_program, lift};
use ruskell::interpreter::{CapturedEnv, RValue, Scope};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};
use std::collections::HashMap;

/// Parse, desugar, lift, and evaluate a program
fn run_lifted_program(input: &str) -> RValue<()> {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("parsing failed: no program");

    // Desugar to core AST
    let core_program = desugar_program(program);

    // Lambda lifting
    let lifted_program = lift::lift_program(core_program);

    let mut scope = Scope::new(lifted_program.functions);
    let empty_capture = CapturedEnv(HashMap::new());
    lifted_program
        .main
        .lambda
        .run(RValue::Unit, &mut scope, &empty_capture)
}

/// Helper to count the number of nested lambda parameters
fn count_lambda_params(lambda: &CoreLambda<()>) -> usize {
    let mut count = 1; // Count the current parameter

    // Check if body contains a nested lambda
    if let CoreLambdaBody::Expression(expr) = &lambda.body {
        if let CoreExpr::Lambda(inner) = expr.as_ref() {
            count += count_lambda_params(inner);
        }
    }

    count
}

/// Parse and desugar a simple function to inspect its lambda structure
fn parse_and_lift_function(input: &str) -> CoreLambda<()> {
    let full_program = format!("{}\nmain = 0", input);
    let tokens = Token::lex(&full_program).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("parsing failed: no program");

    let core_program = desugar_program(program);
    let lifted_program = lift::lift_program(core_program);

    // Return the first non-main function's lambda
    lifted_program.functions[0].lambda.clone()
}

#[test]
fn test_no_capture_lambda() {
    // Lambda with no captured variables should work correctly
    let result = run_lifted_program(
        r#"
        identity x = x
        main = do
            identity(42)
        end
        "#,
    );
    eprintln!("Result: {:?}", result);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}

#[test]
fn test_function_with_two_params() {
    // Two-parameter function should work (tests basic desugaring + lifting)
    let result = run_lifted_program(
        r#"
        add x y = x + y
        main = do
            add(5, 3)
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 8);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}

#[test]
fn test_lambda_structure_no_capture() {
    // \x => x should remain a single-param lambda
    let lambda = parse_and_lift_function("f x = x");

    // Should still be one parameter (no captures added)
    match lambda.param {
        CoreLambdaParam::Ident(id) => assert_eq!(id.value, "x"),
        _ => panic!("expected ident param"),
    }
}

#[test]
fn test_nested_function_params() {
    // Three-parameter function
    let result = run_lifted_program(
        r#"
        addThree x y z = x + y + z
        main = do
            addThree(1, 2, 3)
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 6);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_higher_order_function() {
    // Higher-order function that returns a closure
    let result = run_lifted_program(
        r#"
        applyTwice f x = f(f(x))
        increment n = n + 1
        main = do
            applyTwice(increment, 5)
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 7); // increment(increment(5)) = 7
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_simple_arithmetic_no_lifting() {
    // Test without lambda lifting first
    let tokens = Token::lex("main = do 42 + 8 end").unwrap();
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(errors.is_empty());
    let program = program.unwrap();

    let core_program = desugar_program(program);

    let mut scope = Scope::new(core_program.functions);
    let empty_capture = CapturedEnv(HashMap::new());
    let result = core_program
        .main
        .lambda
        .run(RValue::Unit, &mut scope, &empty_capture);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 50);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}

#[test]
fn test_simple_arithmetic_with_lifting() {
    // Same test but WITH lambda lifting
    let tokens = Token::lex("main = do 42 + 8 end").unwrap();
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(errors.is_empty());
    let program = program.unwrap();

    let core_program = desugar_program(program);
    let lifted_program = lift::lift_program(core_program);

    let mut scope = Scope::new(lifted_program.functions);
    let empty_capture = CapturedEnv(HashMap::new());
    let result = lifted_program
        .main
        .lambda
        .run(RValue::Unit, &mut scope, &empty_capture);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 50);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}

#[test]
fn test_local_bindings() {
    let result = run_lifted_program(
        r#"
        main = do
            x := 10
            y := 20
            x + y
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 30);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_function_call_with_arithmetic() {
    let result = run_lifted_program(
        r#"
        double x = x * 2
        main = do
            double(21)
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_recursive_function() {
    let result = run_lifted_program(
        r#"
        factorial n = if n == 0 then 1 else n * factorial(n - 1) end
        main = do
            factorial(5)
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 120);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_conditional_expression() {
    let result = run_lifted_program(
        r#"
        main = do
            x := 15
            if x > 10 then 1 else 0 end
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_comparison_operators() {
    let result = run_lifted_program(
        r#"
        main = do
            a := 5
            b := 10
            if a < b then 42 else 0 end
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_logical_and() {
    let result = run_lifted_program(
        r#"
        main = do
            if true && true then 1 else 0 end
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_logical_or() {
    let result = run_lifted_program(
        r#"
        main = do
            if false || true then 1 else 0 end
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_pattern_matching_literals() {
    let result = run_lifted_program(
        r#"
        isZero n = case n of
            0 => true
            _ => false
        end
        main = do
            if isZero(0) then 1 else 0 end
        end
        "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer");
    }
}
