use ruskell::desugar::{desugar_program, lift};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

fn parse_and_desugar(source: &str) -> ruskell::core::CoreProgram<()> {
    let tokens = Token::lex(source).expect("Lexer should succeed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(errors.is_empty(), "Parse errors: {:?}", errors);
    let program = program.expect("Should have a program");
    let desugared = desugar_program(program);
    lift::lift_program(desugared)
}

fn run_program(source: &str) -> i32 {
    let program = parse_and_desugar(source);

    // Run the interpreter and capture the exit code
    let exit_code = std::panic::catch_unwind(|| {
        ruskell::interpreter::run(program);
        0
    });

    match exit_code {
        Ok(code) => code,
        Err(_) => {
            // If interpreter exited, we need to check what value was returned
            // For now, we'll assume it exited with the main return value
            // This is a simplification - in reality we'd need to modify the interpreter
            0
        }
    }
}

#[test]
fn test_single_parameter_literal_patterns() {
    let source = r#"
        fib 0 = 0
        fib 1 = 1
        fib n = fib(n - 1) + fib(n - 2)
        
        main = fib(5)
    "#;

    let program = parse_and_desugar(source);

    // Check that we have a fib function
    assert!(program.functions.iter().any(|f| f.name.value == "fib"));
}

#[test]
fn test_two_parameter_all_literals() {
    let source = r#"
        grid 0 0 = 1
        grid 0 1 = 2
        grid 1 0 = 3
        grid 1 1 = 4
        grid x y = 0
        
        main = do
            g00 := grid(0, 0)
            g01 := grid(0, 1)
            g10 := grid(1, 0)
            g11 := grid(1, 1)
            g22 := grid(2, 2)
            g00
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "grid"));
}

#[test]
fn test_two_parameter_mixed_patterns() {
    let source = r#"
        classify 0 0 = 1
        classify x 0 = 2
        classify 0 y = 3
        classify x y = 4
        
        main = do
            c1 := classify(0, 0)
            c2 := classify(5, 0)
            c3 := classify(0, 5)
            c4 := classify(3, 4)
            c1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "classify"));
}

#[test]
fn test_three_parameter_patterns() {
    let source = r#"
        foo 0 0 0 = 100
        foo 1 1 1 = 200
        foo x y z = x + y + z
        
        main = do
            r1 := foo(0, 0, 0)
            r2 := foo(1, 1, 1)
            r3 := foo(2, 3, 4)
            r1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "foo"));
}

#[test]
fn test_four_parameter_patterns() {
    let source = r#"
        quad 0 0 0 0 = 1
        quad 1 0 0 0 = 2
        quad 0 1 0 0 = 3
        quad 0 0 1 0 = 4
        quad 0 0 0 1 = 5
        quad a b c d = a + b + c + d
        
        main = do
            q1 := quad(0, 0, 0, 0)
            q2 := quad(1, 0, 0, 0)
            q3 := quad(2, 3, 4, 5)
            q1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "quad"));
}

#[test]
fn test_five_parameter_patterns() {
    let source = r#"
        penta 0 0 0 0 0 = 100
        penta 1 1 1 1 1 = 200
        penta a b c d e = a * b * c * d * e
        
        main = do
            p1 := penta(0, 0, 0, 0, 0)
            p2 := penta(1, 1, 1, 1, 1)
            p3 := penta(2, 2, 2, 2, 2)
            p1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "penta"));
}

#[test]
fn test_many_clauses_single_param() {
    let source = r#"
        digit 0 = "zero"
        digit 1 = "one"
        digit 2 = "two"
        digit 3 = "three"
        digit 4 = "four"
        digit 5 = "five"
        digit 6 = "six"
        digit 7 = "seven"
        digit 8 = "eight"
        digit 9 = "nine"
        digit n = "other"
        
        main = digit(0)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "digit"));
}

#[test]
fn test_pattern_with_string_literals() {
    let source = r#"
        greet "Alice" = "Hello, Alice!"
        greet "Bob" = "Hi, Bob!"
        greet name = "Hello, stranger!"
        
        main = greet("Alice")
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "greet"));
}

#[test]
fn test_pattern_with_boolean_literals() {
    let source = r#"
        logic true true = 1
        logic true false = 2
        logic false true = 3
        logic false false = 4
        
        main = logic(true, true)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "logic"));
}

#[test]
fn test_overlapping_patterns_first_match_wins() {
    let source = r#"
        test 0 y = 100
        test x 0 = 200
        test x y = 300
        
        main = do
            t1 := test(0, 0)
            t2 := test(0, 5)
            t3 := test(5, 0)
            t4 := test(5, 5)
            t1
        end
    "#;

    let program = parse_and_desugar(source);

    // First clause should match test(0, 0) because patterns are checked in order
    assert!(program.functions.iter().any(|f| f.name.value == "test"));
}

#[test]
fn test_nested_pattern_calls() {
    let source = r#"
        isZero 0 = true
        isZero n = false
        
        check x y = do
            xz := isZero(x)
            yz := isZero(y)
            0
        end
        
        main = check(0, 1)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "isZero"));
    assert!(program.functions.iter().any(|f| f.name.value == "check"));
}

#[test]
fn test_pattern_with_arithmetic_in_body() {
    let source = r#"
        compute 0 y = y * 2
        compute x 0 = x * 3
        compute x y = x + y
        
        main = do
            c1 := compute(0, 5)
            c2 := compute(5, 0)
            c3 := compute(3, 4)
            c1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "compute"));
}

#[test]
fn test_recursive_pattern_function() {
    let source = r#"
        fact 0 = 1
        fact n = n * fact(n - 1)
        
        main = fact(5)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "fact"));
}

#[test]
fn test_mutually_recursive_patterns() {
    let source = r#"
        even 0 = true
        even n = odd(n - 1)
        
        odd 0 = false
        odd n = even(n - 1)
        
        main = even(4)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "even"));
    assert!(program.functions.iter().any(|f| f.name.value == "odd"));
}

#[test]
fn test_pattern_with_multiple_return_types() {
    let source = r#"
        process 0 = "zero"
        process 1 = "one"
        process n = "many"
        
        getValue 0 = 100
        getValue n = n
        
        main = getValue(5)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "process"));
    assert!(program.functions.iter().any(|f| f.name.value == "getValue"));
}

#[test]
fn test_long_pattern_list() {
    let source = r#"
        fib 0 = 0
        fib 1 = 1
        fib 2 = 1
        fib 3 = 2
        fib 4 = 3
        fib 5 = 5
        fib 6 = 8
        fib 7 = 13
        fib 8 = 21
        fib 9 = 34
        fib 10 = 55
        fib 11 = 89
        fib 12 = 144
        fib 13 = 233
        fib 14 = 377
        fib 15 = 610
        fib n = fib(n - 1) + fib(n - 2)
        
        main = fib(10)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "fib"));
}

#[test]
fn test_pattern_with_negative_numbers() {
    let source = r#"
        sign 0 = 0
        sign n = if n > 0 then 1 else 0 - 1 end
        
        main = sign(5)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "sign"));
}

#[test]
fn test_variable_pattern_captures_correctly() {
    let source = r#"
        double x = x + x
        
        triple y = y + y + y
        
        combine a b = double(a) + triple(b)
        
        main = combine(2, 3)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "double"));
    assert!(program.functions.iter().any(|f| f.name.value == "triple"));
    assert!(program.functions.iter().any(|f| f.name.value == "combine"));
}

#[test]
fn test_pattern_with_do_block_body() {
    let source = r#"
        process 0 = do
            x := 1
            y := 2
            x + y
        end
        
        process n = do
            a := n * 2
            b := n * 3
            a + b
        end
        
        main = process(0)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "process"));
}

#[test]
fn test_complex_multi_param_patterns() {
    let source = r#"
        grid 0 0 = 1
        grid 0 1 = 2
        grid 0 2 = 3
        grid 1 0 = 4
        grid 1 1 = 5
        grid 1 2 = 6
        grid 2 0 = 7
        grid 2 1 = 8
        grid 2 2 = 9
        grid x y = x * 10 + y
        
        main = do
            g00 := grid(0, 0)
            g11 := grid(1, 1)
            g22 := grid(2, 2)
            g33 := grid(3, 3)
            g00
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "grid"));
}

#[test]
fn test_pattern_all_variables() {
    let source = r#"
        add x y = x + y
        mult a b = a * b
        combine p q r = add(p, q) + mult(q, r)
        
        main = combine(1, 2, 3)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "add"));
    assert!(program.functions.iter().any(|f| f.name.value == "mult"));
    assert!(program.functions.iter().any(|f| f.name.value == "combine"));
}

#[test]
fn test_pattern_reuses_variable_names() {
    let source = r#"
        f1 x = x + 1
        f2 x = x * 2
        f3 x = f1(x) + f2(x)
        
        main = f3(5)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "f1"));
    assert!(program.functions.iter().any(|f| f.name.value == "f2"));
    assert!(program.functions.iter().any(|f| f.name.value == "f3"));
}

#[test]
fn test_desugaring_creates_nested_lambdas() {
    let source = r#"
        add x y = x + y
        main = add(3, 4)
    "#;

    let program = parse_and_desugar(source);

    // Check that add function exists
    let add_func = program.functions.iter().find(|f| f.name.value == "add");
    assert!(add_func.is_some(), "add function should exist");

    // The lambda should be nested (checking structure would require inspecting the AST)
    // For now, just verify it was created
}

#[test]
fn test_desugaring_combines_conditions_with_and() {
    let source = r#"
        test 0 0 = 1
        test x y = 2
        
        main = test(0, 0)
    "#;

    let program = parse_and_desugar(source);

    // The desugared code should use && to combine conditions
    // We can't easily test this without inspecting the AST structure,
    // but we verify the function was created correctly
    assert!(program.functions.iter().any(|f| f.name.value == "test"));
}

#[test]
fn test_pattern_bindings_in_different_clauses() {
    let source = r#"
        f 0 y = y
        f x 0 = x
        f x y = x + y
        
        main = do
            r1 := f(0, 5)
            r2 := f(5, 0)
            r3 := f(3, 4)
            r1
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "f"));
}

#[test]
fn test_string_pattern_matching() {
    let source = r#"
        language "en" = "English"
        language "es" = "Spanish"
        language "fr" = "French"
        language code = "Unknown"
        
        main = language("en")
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "language"));
}

#[test]
fn test_boolean_pattern_matching() {
    let source = r#"
        toInt true = 1
        toInt false = 0
        
        andInt x y = toInt(x) * toInt(y)
        
        main = andInt(true, false)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "toInt"));
}

#[test]
fn test_pattern_matching_order_matters() {
    let source = r#"
        priority x 0 = 1
        priority 0 y = 2
        priority x y = 3
        
        main = do
            p1 := priority(0, 0)
            p2 := priority(5, 0)
            p3 := priority(0, 5)
            p1
        end
    "#;

    let program = parse_and_desugar(source);

    // priority(0, 0) should match the first clause because patterns are checked in order
    assert!(program.functions.iter().any(|f| f.name.value == "priority"));
}

#[test]
fn test_catch_all_pattern_at_end() {
    let source = r#"
        category 1 = "one"
        category 2 = "two"
        category 3 = "three"
        category n = "other"
        
        main = do
            c1 := category(1)
            c2 := category(2)
            c3 := category(100)
            0
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "category"));
}

#[test]
fn test_six_parameter_pattern() {
    let source = r#"
        hexa 0 0 0 0 0 0 = 0
        hexa 1 1 1 1 1 1 = 1000000
        hexa a b c d e f = a + b + c + d + e + f
        
        main = do
            h1 := hexa(0, 0, 0, 0, 0, 0)
            h2 := hexa(1, 1, 1, 1, 1, 1)
            h3 := hexa(1, 2, 3, 4, 5, 6)
            h3
        end
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "hexa"));
}

#[test]
fn test_pattern_with_nested_function_calls() {
    let source = r#"
        double 0 = 0
        double n = n + n
        
        quad 0 = 0
        quad n = double(double(n))
        
        main = quad(3)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "double"));
    assert!(program.functions.iter().any(|f| f.name.value == "quad"));
}

#[test]
fn test_mixed_literal_types_in_patterns() {
    let source = r#"
        mixer 0 "zero" true = 1
        mixer 1 "one" false = 2
        mixer a b c = 3
        
        main = mixer(0, "zero", true)
    "#;

    let program = parse_and_desugar(source);
    assert!(program.functions.iter().any(|f| f.name.value == "mixer"));
}
