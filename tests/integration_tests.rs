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

// ===== Logical Operator Integration Tests =====

#[test]
fn e2e_logical_not_true() {
    let result = run_program("main = do !true end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_not_false() {
    let result = run_program("main = do !false end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_and_true_true() {
    let result = run_program("main = do true && true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_and_true_false() {
    let result = run_program("main = do true && false end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_and_false_true() {
    let result = run_program("main = do false && true end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_and_false_false() {
    let result = run_program("main = do false && false end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_or_true_true() {
    let result = run_program("main = do true || true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_or_true_false() {
    let result = run_program("main = do true || false end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_or_false_true() {
    let result = run_program("main = do false || true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_or_false_false() {
    let result = run_program("main = do false || false end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_not_precedence() {
    // !false && true → (!false) && true → true && true → true
    let result = run_program("main = do !false && true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_and_or_precedence() {
    // true && false || true → (true && false) || true → false || true → true
    let result = run_program("main = do true && false || true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_complex_precedence() {
    // !true || false && true → (!true) || (false && true) → false || false → false
    let result = run_program("main = do !true || false && true end");
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_logical_with_comparison() {
    // (5 < 10) && (10 < 20) → true && true → true
    let result = run_program("main = do (5 < 10) && (10 < 20) end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_comparison_no_parens() {
    // 5 < 10 && 10 < 20 → (5 < 10) && (10 < 20) → true && true → true
    let result = run_program("main = do 5 < 10 && 10 < 20 end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_with_variables() {
    let input = r#"
        main = do
            x := true
            y := false
            x && !y
        end
    "#;
    let result = run_program(input);
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_logical_binding() {
    let input = r#"
        main = do
            result := true && false
            result
        end
    "#;
    let result = run_program(input);
    assert!(matches!(result, RValue::Bool(false)));
}

#[test]
fn e2e_double_negation() {
    // !!true → !(!true) → !false → true
    let result = run_program("main = do !!true end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_complex_boolean_expression() {
    // (5 < 10 && 10 < 20) || (3 > 5) → (true && true) || false → true || false → true
    let result = run_program("main = do (5 < 10 && 10 < 20) || (3 > 5) end");
    assert!(matches!(result, RValue::Bool(true)));
}

#[test]
fn e2e_simple_if_true() {
    let result = run_program("main = do if true then 42 else 0 end end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer 42");
    }
}

#[test]
fn e2e_simple_if_false() {
    let result = run_program("main = do if false then 42 else 99 end end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 99);
    } else {
        panic!("expected integer 99");
    }
}

#[test]
fn e2e_if_with_variable() {
    let input = r#"
        main = do
            x := 10
            if x > 5 then 1 else 0 end
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer 1");
    }
}

#[test]
fn e2e_nested_if_positive() {
    let input = r#"
        main = do
            x := 7
            if x > 0 then 
                1 
            else 
                if x < 0 then 0 - 1 else 0 end
            end
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer 1");
    }
}

#[test]
fn e2e_nested_if_negative() {
    let input = r#"
        main = do
            x := 0 - 3
            if x > 0 then 
                1 
            else 
                if x < 0 then 0 - 1 else 0 end
            end
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, -1);
    } else {
        panic!("expected integer -1");
    }
}

#[test]
fn e2e_nested_if_zero() {
    let input = r#"
        main = do
            x := 0
            if x > 0 then 
                1 
            else 
                if x < 0 then 0 - 1 else 0 end
            end
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 0);
    } else {
        panic!("expected integer 0");
    }
}

#[test]
fn e2e_if_with_logical_and() {
    let input = r#"
        main = do
            x := 5
            if x > 0 && x < 10 then 100 else 0 end
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 100);
    } else {
        panic!("expected integer 100");
    }
}

#[test]
fn e2e_sign_function_with_if() {
    let input = r#"
        sign x = if x > 0 then 1 else if x < 0 then 0 - 1 else 0 end end
        
        main = do
            a := sign(10)
            b := sign(0 - 5)
            c := sign(0)
            a + b + c
        end
    "#;
    let result = run_program(input);
    // 1 + (-1) + 0 = 0
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 0);
    } else {
        panic!("expected integer 0");
    }
}

#[test]
fn e2e_absolute_value_with_if() {
    let input = r#"
        abs x = if x < 0 then 0 - x else x end
        
        main = do
            abs(0 - 42)
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer 42");
    }
}

#[test]
fn e2e_max_function_with_if() {
    let input = r#"
        max x y = if x > y then x else y end
        
        main = do
            max(10, 5)
        end
    "#;
    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 10);
    } else {
        panic!("expected integer 10");
    }
}

// ===== Pattern Matching Integration Tests =====

#[test]
fn test_case_with_integer_literal() {
    let input = r#"
        main = do
            case 2 of
                0 => 100
                1 => 200
                2 => 300
                _ => 400
            end
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 300);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}

#[test]
fn test_case_with_wildcard_only() {
    let input = r#"
        main = do
            case 42 of
                _ => 99
            end
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 99);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_case_with_variable_binding() {
    let input = r#"
        main = do
            case 10 of
                x => x * 2
            end
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
fn test_nested_case_expressions() {
    let input = r#"
        main = do
            x := 1
            y := 2
            case x of
                0 => 100
                1 => case y of
                    0 => 200
                    2 => 300
                    _ => 400
                end
                _ => 500
            end
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 300);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn test_case_fallthrough_to_wildcard() {
    let input = r#"
        main = do
            case 99 of
                0 => 1
                1 => 2
                _ => 42
            end
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
fn e2e_multi_clause_function_literal_match() {
    let input = r#"
        isZero 0 = true
        isZero n = false
        
        main = do
            result := isZero(0)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Bool(b) = result {
        assert_eq!(b, true);
    } else {
        panic!("expected boolean, got {:?}", result);
    }
}

#[test]
fn e2e_multi_clause_function_literal_no_match() {
    let input = r#"
        isZero 0 = true
        isZero n = false
        
        main = do
            result := isZero(5)
            result
        end
    "#;

    let result = run_program(input);
    if let RValue::Bool(b) = result {
        assert_eq!(b, false);
    } else {
        panic!("expected boolean, got {:?}", result);
    }
}

#[test]
fn e2e_multi_clause_function_with_wildcard() {
    let input = r#"
        check 0 = 100
        check 1 = 200
        check _ = 999
        
        main = do
            a := check(0)
            b := check(1)
            c := check(42)
            c
        end
    "#;

    let result = run_program(input);
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 999);
    } else {
        panic!("expected integer, got {:?}", result);
    }
}
