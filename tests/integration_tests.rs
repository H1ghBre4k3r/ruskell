//! End-to-end integration tests for complete program execution

use ruskell::interpreter::{RValue, Scope};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

/// Parse and evaluate a program, returning the result of main
fn run_program(input: &str) -> RValue<()> {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("parsing failed: no program");

    let mut scope = Scope::new(program.functions);
    program.main.lambda.run(&[], &mut scope)
}

#[test]
fn e2e_return_integer() {
    let result = run_program("main = do 42 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_return_unit() {
    let result = run_program("main = do () end");
    assert!(matches!(result, RValue::Unit));
}

#[test]
fn e2e_simple_binding() {
    let result = run_program("main = do x := 42 x end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_multiple_bindings() {
    let result = run_program("main = do x := 1 y := 2 y end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_identity_lambda() {
    let result = run_program(
        r#"
        main = do
            id := \x => x
            id(42)
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
fn e2e_lambda_with_block() {
    let result = run_program(
        r#"
        main = do
            f := \x => do
                y := x
                y
            end
            f(99)
        end
    "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 99);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_nested_lambda_calls() {
    let result = run_program(
        r#"
        main = do
            f := \x => x
            g := \x => f(x)
            g(123)
        end
    "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 123);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_lambda_multiple_params() {
    // Lambda takes two params, returns second
    let result = run_program(
        r#"
        main = do
            second := \x, y => y
            second(1, 2)
        end
    "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_shadowing() {
    let result = run_program(
        r#"
        main = do
            x := 1
            f := \x => x
            f(2)
        end
    "#,
    );
    // Lambda param shadows outer x
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_closure_captures_outer() {
    let result = run_program(
        r#"
        main = do
            x := 10
            f := \y => x
            f(999)
        end
    "#,
    );
    // Lambda should see outer x
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 10);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_top_level_function() {
    let result = run_program(
        r#"
        helper = do
            42
        end
        main = do
            helper
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
fn e2e_multiple_top_level_functions() {
    let result = run_program(
        r#"
        one = do 1 end
        two = do 2 end
        main = do
            two
        end
    "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_return_lambda() {
    let result = run_program(
        r#"
        main = do
            \x => x
        end
    "#,
    );
    assert!(matches!(result, RValue::Lambda(_)));
}

// Note: Full closure support (capturing environment at creation time) is not yet implemented.
// The following test demonstrates the expected behavior for currying once closures work:
// #[test]
// fn e2e_call_returned_lambda() {
//     let result = run_program(r#"
//         main = do
//             makeAdder := \x => \y => x
//             addTen := makeAdder(10)
//             addTen(5)
//         end
//     "#);
//     if let RValue::Integer(i) = result {
//         assert_eq!(i.value, 10);
//     } else {
//         panic!("expected integer");
//     }
// }
