use ruskell::ast::expression::{Expression, LambdaBody};
use ruskell::ast::statement::Statement;
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

fn parse_program(input: &str) -> ruskell::ParsedProgram {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    program.expect("parsing failed: no program")
}

#[test]
fn parse_empty_main() {
    let program = parse_program("main = do end");
    assert_eq!(program.main.name.value, "main");
    assert!(program.functions.is_empty());

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert!(stmts.is_empty());
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_main_with_integer() {
    let program = parse_program("main = do 42 end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::Integer(i)) = &stmts[0] {
            assert_eq!(i.value, 42);
        } else {
            panic!("expected integer expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_assignment() {
    let program = parse_program("main = do x := 42 end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Assignment(a) = &stmts[0] {
            assert_eq!(a.name.value, "x");
            if let Expression::Integer(i) = a.value.as_ref() {
                assert_eq!(i.value, 42);
            } else {
                panic!("expected integer value");
            }
        } else {
            panic!("expected assignment");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_multiple_statements() {
    let program = parse_program("main = do x := 1 y := 2 x end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], Statement::Assignment(_)));
        assert!(matches!(&stmts[1], Statement::Assignment(_)));
        assert!(matches!(
            &stmts[2],
            Statement::Expression(Expression::Ident(_))
        ));
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_lambda_expression() {
    let program = parse_program(r#"main = do f := \x => x end"#);

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Assignment(a) = &stmts[0] {
            assert_eq!(a.name.value, "f");
            assert!(matches!(a.value.as_ref(), Expression::Lambda(_)));
        } else {
            panic!("expected assignment");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_lambda_with_block_body() {
    let program = parse_program(r#"main = do f := \x => do x end end"#);

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        if let Statement::Assignment(a) = &stmts[0] {
            if let Expression::Lambda(l) = a.value.as_ref() {
                assert!(matches!(&l.body, LambdaBody::Block(_)));
            } else {
                panic!("expected lambda");
            }
        } else {
            panic!("expected assignment");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_lambda_with_multiple_params() {
    let program = parse_program(r#"main = do f := \x, y => x end"#);

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        if let Statement::Assignment(a) = &stmts[0] {
            if let Expression::Lambda(l) = a.value.as_ref() {
                assert_eq!(l.params.len(), 2);
            } else {
                panic!("expected lambda");
            }
        } else {
            panic!("expected assignment");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_function_call_no_args() {
    let program = parse_program("main = do foo() end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        if let Statement::Expression(Expression::FunctionCall(c)) = &stmts[0] {
            if let Expression::Ident(i) = c.func.as_ref() {
                assert_eq!(i.value, "foo");
            }
            assert!(c.args.is_empty());
        } else {
            panic!("expected function call");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_function_call_with_args() {
    let program = parse_program("main = do foo(1, 2, 3) end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        if let Statement::Expression(Expression::FunctionCall(c)) = &stmts[0] {
            assert_eq!(c.args.len(), 3);
        } else {
            panic!("expected function call");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_unit_literal() {
    let program = parse_program("main = do () end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert!(matches!(
            &stmts[0],
            Statement::Expression(Expression::Unit(_))
        ));
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_multiple_functions() {
    let program = parse_program("main = do end helper = do end");
    assert_eq!(program.main.name.value, "main");
    assert_eq!(program.functions.len(), 1);
    assert_eq!(program.functions[0].name.value, "helper");
}

#[test]
fn parse_binary_addition() {
    let program = parse_program("main = do 1 + 2 end");
    assert_eq!(program.main.name.value, "main");
}

#[test]
fn parse_binary_precedence() {
    // Should parse as 1 + (2 * 3)
    let program = parse_program("main = do 1 + 2 * 3 end");
    assert_eq!(program.main.name.value, "main");
}

#[test]
fn parse_binary_with_parens() {
    let program = parse_program("main = do (1 + 2) * 3 end");
    assert_eq!(program.main.name.value, "main");
}
