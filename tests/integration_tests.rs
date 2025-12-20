//! End-to-end integration tests for complete program execution

use ruskell::desugar::desugar_program;
use ruskell::interpreter::{CapturedEnv, RValue, Scope};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};
use std::collections::HashMap;

/// Parse and evaluate a program, returning the result of main
fn run_program(input: &str) -> RValue<()> {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("parsing failed: no program");

    // Desugar to core AST
    let core_program = desugar_program(program);

    let mut scope = Scope::new(core_program.functions);
    let empty_capture = CapturedEnv(HashMap::new());
    core_program
        .main
        .lambda
        .run(RValue::Unit, &mut scope, &empty_capture)
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
            helper(())
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
            two(())
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
    assert!(matches!(result, RValue::CoreLambda(..)));
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

#[test]
fn e2e_arithmetic_addition() {
    let result = run_program("main = do 5 + 3 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 8);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_subtraction() {
    let result = run_program("main = do 10 - 3 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 7);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_multiplication() {
    let result = run_program("main = do 4 * 7 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 28);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_division() {
    let result = run_program("main = do 20 / 4 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 5);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_precedence() {
    // Tests that * and / have higher precedence than + and -
    // 2 + 3 * 4 should be 2 + (3 * 4) = 14, not (2 + 3) * 4 = 20
    let result = run_program("main = do 2 + 3 * 4 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 14);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_complex() {
    let result = run_program(
        r#"
        main = do
            x := 10
            y := 20
            sum := x + y
            prod := x * y
            diff := sum - prod
            quotient := prod / x
            quotient
        end
    "#,
    );
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 20);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_arithmetic_parentheses() {
    // (2 + 3) * 4 = 5 * 4 = 20
    let result = run_program("main = do (2 + 3) * 4 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 20);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_with_single_param() {
    let input = r#"
        increment x = x + 1

        main = do
            result := increment(5)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 6);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_with_multiple_params() {
    let input = r#"
        add x y = x + y

        main = do
            result := add(10, 20)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 30);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_with_expression_body() {
    let input = r#"
        double x = x * 2

        main = do
            result := double(21)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_with_do_block_body() {
    let input = r#"
        compute x y = do
            sum := x + y
            product := sum * 2
            product
        end

        main = do
            result := compute(3, 7)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 20);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_nested_function_calls_with_params() {
    let input = r#"
        add x y = x + y
        multiply x y = x * y

        main = do
            sum := add(2, 3)
            result := multiply(sum, 4)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 20);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_params_shadow_outer_scope() {
    let input = r#"
        useParam x = x + 100

        main = do
            x := 5
            result := useParam(10)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 110);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn e2e_function_with_unit_param() {
    let input = r#"
        always42 () = 42

        main = do
            result := always42(())
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

// ===== Boolean and Comparison Operator Tests =====

#[test]
fn e2e_boolean_true_literal() {
    let result = run_program("main = do true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_boolean_false_literal() {
    let result = run_program("main = do false end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_equal_true() {
    let result = run_program("main = do 5 == 5 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_equal_false() {
    let result = run_program("main = do 5 == 10 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_not_equal_true() {
    let result = run_program("main = do 5 != 10 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_not_equal_false() {
    let result = run_program("main = do 5 != 5 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_less_than_true() {
    let result = run_program("main = do 5 < 10 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_less_than_false() {
    let result = run_program("main = do 10 < 5 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_greater_than_true() {
    let result = run_program("main = do 10 > 5 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_greater_than_false() {
    let result = run_program("main = do 5 > 10 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_less_equal_true_less() {
    let result = run_program("main = do 5 <= 10 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_less_equal_true_equal() {
    let result = run_program("main = do 5 <= 5 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_less_equal_false() {
    let result = run_program("main = do 10 <= 5 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_greater_equal_true_greater() {
    let result = run_program("main = do 10 >= 5 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_greater_equal_true_equal() {
    let result = run_program("main = do 10 >= 10 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_greater_equal_false() {
    let result = run_program("main = do 5 >= 10 end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_comparison_with_precedence() {
    // 2 + 3 == 5 should parse as (2 + 3) == 5
    let result = run_program("main = do 2 + 3 == 5 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_with_complex_arithmetic() {
    // (2 + 3) * 4 > 10 should be true (20 > 10)
    let result = run_program("main = do (2 + 3) * 4 > 10 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_with_variables() {
    let input = r#"
        main = do
            x := 5
            y := 10
            x < y
        end
    "#;
    let result = run_program(input);
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_boolean_binding() {
    let input = r#"
        main = do
            result := 5 < 10
            result
        end
    "#;
    let result = run_program(input);
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_comparison_negative_numbers() {
    let result = run_program("main = do 0 - 5 < 0 end");
    assert!(matches!(result, RValue::Bool(true)));
}
