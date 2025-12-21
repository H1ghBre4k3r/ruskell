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

#[test]
fn parse_function_with_single_param() {
    let input = r#"
        add x = x + 1
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let add_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "add")
        .unwrap();
    assert_eq!(add_fn.lambda.params.len(), 1);
}

#[test]
fn parse_function_with_multiple_params() {
    let input = r#"
        add x y = x + y
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let add_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "add")
        .unwrap();
    assert_eq!(add_fn.lambda.params.len(), 2);
}

#[test]
fn parse_function_with_unit_param() {
    let input = r#"
        ignore () = 42
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let ignore_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "ignore")
        .unwrap();
    assert_eq!(ignore_fn.lambda.params.len(), 1);
}

#[test]
fn parse_function_no_params_backwards_compat() {
    let input = r#"
        noparams = do
            x := 5
            x
        end
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let noparams_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "noparams")
        .unwrap();
    assert_eq!(noparams_fn.lambda.params.len(), 0);
}

#[test]
fn parse_function_single_expression_body() {
    let input = r#"
        double x = x + x
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let double_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "double")
        .unwrap();
    assert_eq!(double_fn.lambda.params.len(), 1);
    assert!(matches!(double_fn.lambda.body, LambdaBody::Expression(_)));
}

#[test]
fn parse_function_do_block_body() {
    let input = r#"
        compute x y = do
            sum := x + y
            sum * 2
        end
        main = do end
    "#;

    let lexed = Token::lex(input).unwrap();
    let mut state = ParseState::new(lexed);
    let (program, errors) = parse(&mut state);

    assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    assert!(program.is_some());

    let prog = program.unwrap();
    let compute_fn = prog
        .functions
        .iter()
        .find(|f| f.name.value == "compute")
        .unwrap();
    assert_eq!(compute_fn.lambda.params.len(), 2);
    assert!(matches!(compute_fn.lambda.body, LambdaBody::Block(_)));
}

// ===== Boolean and Comparison Operator Parser Tests =====

#[test]
fn parse_boolean_true() {
    let program = parse_program("main = do true end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::Boolean(b)) = &stmts[0] {
            assert!(b.value);
        } else {
            panic!("expected boolean expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_boolean_false() {
    let program = parse_program("main = do false end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::Boolean(b)) = &stmts[0] {
            assert!(!b.value);
        } else {
            panic!("expected boolean expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_comparison_equal() {
    let program = parse_program("main = do 5 == 10 end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::Eq));
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_comparison_with_precedence() {
    // 2 + 3 < 10 should parse with + binding tighter than <
    let program = parse_program("main = do 2 + 3 < 10 end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            // The outer operation should be <
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::Lt));
            // The left side should be a BinaryOp (2 + 3)
            if let Expression::BinaryOp(left_binop) = &*binop.left {
                assert!(matches!(
                    left_binop.op,
                    ruskell::ast::expression::BinOpKind::Add
                ));
            } else {
                panic!("expected left side to be binary op");
            }
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_all_comparison_operators() {
    use ruskell::ast::expression::BinOpKind;

    let tests = vec![
        ("main = do 1 == 2 end", BinOpKind::Eq),
        ("main = do 1 != 2 end", BinOpKind::NotEq),
        ("main = do 1 < 2 end", BinOpKind::Lt),
        ("main = do 1 > 2 end", BinOpKind::Gt),
        ("main = do 1 <= 2 end", BinOpKind::LtEq),
        ("main = do 1 >= 2 end", BinOpKind::GtEq),
    ];

    for (input, expected_op) in tests {
        let program = parse_program(input);
        if let LambdaBody::Block(stmts) = &program.main.lambda.body {
            if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
                assert_eq!(binop.op, expected_op);
            } else {
                panic!("expected binary op for {}", input);
            }
        } else {
            panic!("expected block body");
        }
    }
}

// ===== Logical Operator Parser Tests =====

#[test]
fn parse_logical_not() {
    let program = parse_program("main = do !true end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::UnaryOp(unop)) = &stmts[0] {
            assert!(matches!(
                unop.op,
                ruskell::ast::expression::UnaryOpKind::Not
            ));
            // Operand should be a boolean
            assert!(matches!(*unop.operand, Expression::Boolean(_)));
        } else {
            panic!("expected unary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_logical_and() {
    let program = parse_program("main = do true && false end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::And));
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_logical_or() {
    let program = parse_program("main = do true || false end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::Or));
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_logical_precedence_not_and() {
    // !x && y should parse as (!x) && y
    let program = parse_program("main = do !true && false end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            // Outer operation should be &&
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::And));
            // Left side should be a UnaryOp (!)
            assert!(matches!(*binop.left, Expression::UnaryOp(_)));
            // Right side should be Boolean
            assert!(matches!(*binop.right, Expression::Boolean(_)));
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_logical_precedence_and_or() {
    // x && y || z should parse as (x && y) || z
    let program = parse_program("main = do true && false || true end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[0] {
            // Outer operation should be ||
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::Or));
            // Left side should be a BinaryOp (&&)
            if let Expression::BinaryOp(left_binop) = &*binop.left {
                assert!(matches!(
                    left_binop.op,
                    ruskell::ast::expression::BinOpKind::And
                ));
            } else {
                panic!("expected left side to be binary op");
            }
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_logical_with_comparison() {
    // x < 5 && y > 10 should parse as (x < 5) && (y > 10)
    let program = parse_program("main = do x := 1 y := 2 x < 5 && y > 10 end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        // Last statement should be the logical expression
        if let Statement::Expression(Expression::BinaryOp(binop)) = &stmts[2] {
            // Outer operation should be &&
            assert!(matches!(binop.op, ruskell::ast::expression::BinOpKind::And));
            // Both sides should be comparison operators
            assert!(matches!(*binop.left, Expression::BinaryOp(_)));
            assert!(matches!(*binop.right, Expression::BinaryOp(_)));
        } else {
            panic!("expected binary op expression");
        }
    } else {
        panic!("expected block body");
    }
}

#[test]
fn parse_double_negation() {
    // !!x should parse as !(!x)
    let program = parse_program("main = do !!true end");

    if let LambdaBody::Block(stmts) = &program.main.lambda.body {
        assert_eq!(stmts.len(), 1);
        if let Statement::Expression(Expression::UnaryOp(unop)) = &stmts[0] {
            // Outer operation should be !
            assert!(matches!(
                unop.op,
                ruskell::ast::expression::UnaryOpKind::Not
            ));
            // Operand should also be a UnaryOp (!)
            assert!(matches!(*unop.operand, Expression::UnaryOp(_)));
        } else {
            panic!("expected unary op expression");
        }
    } else {
        panic!("expected block body");
    }
}
