use ruskell::desugar::desugar_program;
use ruskell::interpreter::{CapturedEnv, RValue, Scope};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};
use std::collections::HashMap;

/// Helper to parse and desugar a program for testing
fn parse_and_desugar(input: &str) -> ruskell::core::CoreProgram<()> {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("no program");
    desugar_program(program)
}

/// Helper to run a program and get the result
fn eval_program(input: &str) -> RValue<()> {
    let core_program = parse_and_desugar(input);
    let mut scope = Scope::new(core_program.functions);
    let empty_capture = CapturedEnv(HashMap::new());
    core_program
        .main
        .lambda
        .run(RValue::Unit, &mut scope, &empty_capture)
}

#[test]
fn eval_integer_literal() {
    let result = eval_program("main = do 42 end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_unit_literal() {
    let result = eval_program("main = do () end");
    assert!(matches!(result, RValue::Unit));
}

#[test]
fn eval_lambda_returns_lambda() {
    let result = eval_program("main = do \\x => x end");
    assert!(matches!(result, RValue::CoreLambda(..)));
}

#[test]
fn eval_variable_binding() {
    let result = eval_program("main = do x := 42\nx end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_lambda_call() {
    let result = eval_program("main = do id := \\x => x\nid(42) end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_nested_scopes() {
    // Test shadowing with nested lambdas
    let result = eval_program("main = do x := 1\nf := \\x => x\nf(2) end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_lambda_with_block_body() {
    let result = eval_program("main = do f := \\x => do x end\nf(42) end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_assignment_in_block() {
    let result = eval_program("main = do f := \\x => do y := 100\ny end\nf(()) end");
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 100);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_empty_block_returns_unit() {
    let result = eval_program("main = do end");
    assert!(matches!(result, RValue::Unit));
}
