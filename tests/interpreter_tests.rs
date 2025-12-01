use lachs::Span;
use ruskell::ast::expression::{Expression, Ident, Integer, Lambda, LambdaBody, LambdaParam};
use ruskell::ast::statement::Statement;
use ruskell::interpreter::{RValue, Scope};

fn dummy_span() -> Span {
    Span {
        start: (0, 0),
        end: (0, 0),
        source: String::new(),
    }
}

fn make_ident(name: &str) -> Ident<()> {
    Ident {
        value: name.to_string(),
        position: dummy_span(),
        info: (),
    }
}

fn make_integer(value: i128) -> Integer<()> {
    Integer {
        value,
        position: dummy_span(),
        info: (),
    }
}

fn make_lambda(params: Vec<&str>, body_expr: Expression<()>) -> Lambda<()> {
    Lambda {
        params: params
            .into_iter()
            .map(|p| LambdaParam::Ident(make_ident(p)))
            .collect(),
        body: LambdaBody::Expression(Box::new(body_expr)),
        position: dummy_span(),
        info: (),
    }
}

#[test]
fn eval_integer_literal() {
    let mut scope = Scope::new(vec![]);
    let expr = Expression::Integer(make_integer(42));

    let result = expr.eval(&mut scope);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_unit_literal() {
    let mut scope = Scope::new(vec![]);
    let expr = Expression::Unit(ruskell::ast::expression::Unit {
        position: dummy_span(),
        info: (),
    });

    let result = expr.eval(&mut scope);
    assert!(matches!(result, RValue::Unit));
}

#[test]
fn eval_lambda_returns_lambda() {
    let mut scope = Scope::new(vec![]);
    let lambda = make_lambda(vec!["x"], Expression::Ident(make_ident("x")));
    let expr = Expression::Lambda(lambda);

    let result = expr.eval(&mut scope);
    assert!(matches!(result, RValue::Lambda(_)));
}

#[test]
fn eval_variable_binding() {
    let mut scope = Scope::new(vec![]);
    scope.enter();
    scope.add("x", RValue::Integer(make_integer(42)));

    let expr = Expression::Ident(make_ident("x"));
    let result = expr.eval(&mut scope);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_lambda_call() {
    let mut scope = Scope::new(vec![]);

    // Create identity lambda: \x => x
    let lambda = make_lambda(vec!["x"], Expression::Ident(make_ident("x")));

    // Call it with 42
    let result = lambda.run(&[RValue::Integer(make_integer(42))], &mut scope);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 42);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_nested_scopes() {
    let mut scope = Scope::new(vec![]);
    scope.enter();
    scope.add("x", RValue::Integer(make_integer(1)));

    scope.enter();
    scope.add("x", RValue::Integer(make_integer(2)));

    // Inner scope should shadow outer
    let result = scope.resolve("x").unwrap();
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 2);
    } else {
        panic!("expected integer");
    }

    scope.leave();

    // After leaving, outer scope value should be visible
    let result = scope.resolve("x").unwrap();
    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 1);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_lambda_with_block_body() {
    let mut scope = Scope::new(vec![]);

    // Lambda: \x => do x end
    let lambda = Lambda {
        params: vec![LambdaParam::Ident(make_ident("x"))],
        body: LambdaBody::Block(vec![Statement::Expression(Expression::Ident(make_ident(
            "x",
        )))]),
        position: dummy_span(),
        info: (),
    };

    let result = lambda.run(&[RValue::Integer(make_integer(99))], &mut scope);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 99);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_assignment_in_block() {
    use ruskell::ast::statement::Assignment;

    let mut scope = Scope::new(vec![]);

    // Lambda: \() => do y := 100 y end
    let lambda = Lambda {
        params: vec![],
        body: LambdaBody::Block(vec![
            Statement::Assignment(Assignment {
                name: make_ident("y"),
                value: Box::new(Expression::Integer(make_integer(100))),
                position: dummy_span(),
                info: (),
            }),
            Statement::Expression(Expression::Ident(make_ident("y"))),
        ]),
        position: dummy_span(),
        info: (),
    };

    let result = lambda.run(&[], &mut scope);

    if let RValue::Integer(i) = result {
        assert_eq!(i.value, 100);
    } else {
        panic!("expected integer");
    }
}

#[test]
fn eval_empty_block_returns_unit() {
    let mut scope = Scope::new(vec![]);

    let lambda = Lambda {
        params: vec![],
        body: LambdaBody::Block(vec![]),
        position: dummy_span(),
        info: (),
    };

    let result = lambda.run(&[], &mut scope);
    assert!(matches!(result, RValue::Unit));
}
