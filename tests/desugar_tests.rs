//! Tests for desugaring transformations

use ruskell::core::*;
use ruskell::desugar::{desugar_program, erase_program};
use ruskell::lexer::Token;
use ruskell::parser::{ParseState, parse};

fn desugar_test(input: &str) -> CoreProgram<()> {
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    if !errors.is_empty() {
        panic!("parsing failed: {}", errors[0]);
    }
    let program = program.expect("no program");
    desugar_program(program)
}

#[test]
fn desugar_and_erase_roundtrip() {
    // Test that desugar + erase can round-trip a simple program
    let input = "main = do f := \\x, y => x\nresult := f(1, 2)\nresult end";
    let tokens = Token::lex(input).expect("lexing failed");
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);
    assert!(errors.is_empty());
    let program = program.expect("no program");

    // Desugar
    let desugared = desugar_program(program);

    // Erase back
    let erased = erase_program(desugared);

    // Should have main with the right structure
    assert_eq!(erased.main.name.value, "main");
}

#[test]
fn desugar_single_param_lambda() {
    let input = "main = do f := \\x => x end";
    let desugared = desugar_test(input);

    // Should have main function
    assert_eq!(desugared.main.name.value, "main");

    // Main body should have one assignment
    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        assert_eq!(stmts.len(), 1);

        // The assignment value should be a lambda with single param
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            assert_eq!(assign.name.value, "f");

            if let CoreExpr::Lambda(lambda) = assign.value.as_ref() {
                // Should have single parameter 'x'
                if let CoreLambdaParam::Ident(id) = &lambda.param {
                    assert_eq!(id.value, "x");
                } else {
                    panic!("expected ident param");
                }

                // Body should be just 'x'
                if let CoreLambdaBody::Expression(expr) = &lambda.body {
                    if let CoreExpr::Ident(id) = expr.as_ref() {
                        assert_eq!(id.value, "x");
                    } else {
                        panic!("expected ident expression");
                    }
                } else {
                    panic!("expected expression body");
                }
            } else {
                panic!("expected lambda");
            }
        } else {
            panic!("expected assignment");
        }
    } else {
        panic!("expected block");
    }
}

#[test]
fn desugar_multi_param_lambda() {
    let input = "main = do f := \\x, y, z => x end";
    let desugared = desugar_test(input);

    // Extract the lambda: f := \x, y, z => x
    // Should desugar to: \x => \y => \z => x
    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            if let CoreExpr::Lambda(outer) = assign.value.as_ref() {
                // Outer lambda should have param 'x'
                if let CoreLambdaParam::Ident(id) = &outer.param {
                    assert_eq!(id.value, "x");
                } else {
                    panic!("expected x param");
                }

                // Body should be another lambda
                if let CoreLambdaBody::Expression(expr) = &outer.body {
                    if let CoreExpr::Lambda(middle) = expr.as_ref() {
                        // Middle lambda should have param 'y'
                        if let CoreLambdaParam::Ident(id) = &middle.param {
                            assert_eq!(id.value, "y");
                        } else {
                            panic!("expected y param");
                        }

                        // Body should be another lambda
                        if let CoreLambdaBody::Expression(expr) = &middle.body {
                            if let CoreExpr::Lambda(inner) = expr.as_ref() {
                                // Inner lambda should have param 'z'
                                if let CoreLambdaParam::Ident(id) = &inner.param {
                                    assert_eq!(id.value, "z");
                                } else {
                                    panic!("expected z param");
                                }

                                // Body should be 'x'
                                if let CoreLambdaBody::Expression(expr) = &inner.body {
                                    if let CoreExpr::Ident(id) = expr.as_ref() {
                                        assert_eq!(id.value, "x");
                                    } else {
                                        panic!("expected x in body");
                                    }
                                } else {
                                    panic!("expected expression body");
                                }
                            } else {
                                panic!("expected inner lambda");
                            }
                        } else {
                            panic!("expected expression body");
                        }
                    } else {
                        panic!("expected middle lambda");
                    }
                } else {
                    panic!("expected expression body");
                }
            } else {
                panic!("expected outer lambda");
            }
        }
    }
}

#[test]
fn desugar_multi_arg_call() {
    let input = "main = do x := f(1, 2, 3) end";
    let desugared = desugar_test(input);

    // f(1, 2, 3) should desugar to f(1)(2)(3)
    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            // Outermost call: f(1)(2)(3)
            if let CoreExpr::FunctionCall(outer_call) = assign.value.as_ref() {
                // Argument should be 3
                if let CoreExpr::Integer(i) = outer_call.arg.as_ref() {
                    assert_eq!(i.value, 3);
                } else {
                    panic!("expected 3 as outer arg");
                }

                // Function should be f(1)(2)
                if let CoreExpr::FunctionCall(middle_call) = outer_call.func.as_ref() {
                    // Argument should be 2
                    if let CoreExpr::Integer(i) = middle_call.arg.as_ref() {
                        assert_eq!(i.value, 2);
                    } else {
                        panic!("expected 2 as middle arg");
                    }

                    // Function should be f(1)
                    if let CoreExpr::FunctionCall(inner_call) = middle_call.func.as_ref() {
                        // Argument should be 1
                        if let CoreExpr::Integer(i) = inner_call.arg.as_ref() {
                            assert_eq!(i.value, 1);
                        } else {
                            panic!("expected 1 as inner arg");
                        }

                        // Function should be 'f'
                        if let CoreExpr::Ident(id) = inner_call.func.as_ref() {
                            assert_eq!(id.value, "f");
                        } else {
                            panic!("expected f as function");
                        }
                    } else {
                        panic!("expected inner call");
                    }
                } else {
                    panic!("expected middle call");
                }
            } else {
                panic!("expected outer call");
            }
        }
    }
}

#[test]
fn desugar_single_arg_call() {
    let input = "main = do x := f(42) end";
    let desugared = desugar_test(input);

    // f(42) should remain as is (single call)
    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            if let CoreExpr::FunctionCall(call) = assign.value.as_ref() {
                // Function should be 'f'
                if let CoreExpr::Ident(id) = call.func.as_ref() {
                    assert_eq!(id.value, "f");
                } else {
                    panic!("expected f");
                }

                // Argument should be 42
                if let CoreExpr::Integer(i) = call.arg.as_ref() {
                    assert_eq!(i.value, 42);
                } else {
                    panic!("expected 42");
                }
            } else {
                panic!("expected function call");
            }
        }
    }
}

#[test]
fn desugar_no_arg_call() {
    let input = "main = do x := f() end";
    let desugared = desugar_test(input);

    // f() should become f(())
    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            if let CoreExpr::FunctionCall(call) = assign.value.as_ref() {
                // Function should be 'f'
                if let CoreExpr::Ident(id) = call.func.as_ref() {
                    assert_eq!(id.value, "f");
                } else {
                    panic!("expected f");
                }

                // Argument should be unit
                if !matches!(call.arg.as_ref(), CoreExpr::Unit(_)) {
                    panic!("expected unit argument");
                }
            } else {
                panic!("expected function call");
            }
        }
    }
}

#[test]
fn desugar_nested_lambdas() {
    // Test: \x => \y => x + y (already single-param, should pass through)
    let input = "main = do f := \\x => \\y => x end";
    let desugared = desugar_test(input);

    if let CoreLambdaBody::Block(stmts) = &desugared.main.lambda.body {
        if let CoreStatement::Assignment(assign) = &stmts[0] {
            if let CoreExpr::Lambda(outer) = assign.value.as_ref() {
                if let CoreLambdaParam::Ident(id) = &outer.param {
                    assert_eq!(id.value, "x");
                }

                if let CoreLambdaBody::Expression(expr) = &outer.body {
                    if let CoreExpr::Lambda(inner) = expr.as_ref() {
                        if let CoreLambdaParam::Ident(id) = &inner.param {
                            assert_eq!(id.value, "y");
                        }
                    }
                }
            }
        }
    }
}
