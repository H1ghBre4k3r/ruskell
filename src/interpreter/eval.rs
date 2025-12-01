use std::fmt::Debug;

use crate::ast::expression::{Expression, FunctionCall, Lambda, LambdaBody, LambdaParam};
use crate::ast::statement::{Assignment, Statement};

use super::scope::Scope;
use super::value::RValue;

impl<T> Expression<T>
where
    T: Clone + Debug,
{
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            Expression::Ident(ident) => {
                let value = scope
                    .resolve(&ident.value)
                    .unwrap_or_else(|| panic!("undefined identifier: {}", ident.value));
                // If it's a nullary lambda (no params), call it immediately
                match &value {
                    RValue::Lambda(lambda) if lambda.params.is_empty() => lambda.run(&[], scope),
                    _ => value,
                }
            }
            Expression::Integer(integer) => RValue::Integer(integer.clone()),
            Expression::String(string_literal) => RValue::String(string_literal.clone()),
            Expression::Unit(_) => RValue::Unit,
            Expression::Lambda(lambda) => RValue::Lambda(lambda.clone()),
            Expression::FunctionCall(FunctionCall { func, args, .. }) => {
                let func_value = func.eval(scope);
                let evaluated_args: Vec<RValue<T>> = args.iter().map(|a| a.eval(scope)).collect();

                match func_value {
                    RValue::Lambda(lambda) => lambda.run(&evaluated_args, scope),
                    other => panic!("cannot call non-function value: {:?}", other),
                }
            }
        }
    }
}

impl<T> Lambda<T>
where
    T: Clone + Debug,
{
    pub fn run(&self, args: &[RValue<T>], scope: &mut Scope<T>) -> RValue<T> {
        scope.enter();

        // Bind parameters to arguments
        for (param, arg) in self.params.iter().zip(args.iter()) {
            match param {
                LambdaParam::Ident(ident) => scope.add(&ident.value, arg.clone()),
                LambdaParam::Unit(_) => {
                    // Unit pattern - just verify the arg is unit (or ignore)
                    // For now, we don't enforce type checking
                }
            }
        }

        let result = match &self.body {
            LambdaBody::Expression(expr) => expr.eval(scope),
            LambdaBody::Block(statements) => eval_block(statements, scope),
        };

        scope.leave();
        result
    }
}

impl<T> Statement<T>
where
    T: Clone + Debug,
{
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            Statement::Assignment(Assignment { name, value, .. }) => {
                let evaluated = value.eval(scope);
                scope.add(&name.value, evaluated);
                RValue::Unit
            }
            Statement::Expression(expression) => expression.eval(scope),
        }
    }
}

/// Evaluate a block of statements, returning the value of the last statement (or Unit if empty)
pub fn eval_block<T>(statements: &[Statement<T>], scope: &mut Scope<T>) -> RValue<T>
where
    T: Clone + Debug,
{
    statements
        .iter()
        .map(|stmt| stmt.eval(scope))
        .last()
        .unwrap_or(RValue::Unit)
}
