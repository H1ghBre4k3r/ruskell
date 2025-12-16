//! Interpreter for Core AST
//!
//! This evaluates the desugared Core AST directly, without converting back to Surface AST.
//! This is more efficient and will eventually support type information.

use std::fmt::Debug;

use crate::core::*;

use super::scope::Scope;
use super::value::RValue;

impl<T> CoreExpr<T>
where
    T: Clone + Debug,
{
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            CoreExpr::Unit(_) => RValue::Unit,
            CoreExpr::Ident(ident) => {
                let value = scope
                    .resolve(&ident.value)
                    .unwrap_or_else(|| panic!("undefined identifier: {}", ident.value));
                // If it's a nullary lambda (no params), call it immediately
                match &value {
                    RValue::CoreLambda(lambda, captured)
                        if matches!(lambda.param, CoreLambdaParam::Unit(_)) =>
                    {
                        lambda.run(RValue::Unit, scope, captured)
                    }
                    _ => value,
                }
            }
            CoreExpr::Integer(integer) => RValue::Integer(crate::ast::expression::Integer {
                value: integer.value,
                position: integer.position.clone(),
                info: integer.info.clone(),
            }),
            CoreExpr::String(string) => RValue::String(crate::ast::expression::StringLiteral {
                value: string.value.clone(),
                position: string.position.clone(),
                info: string.info.clone(),
            }),
            CoreExpr::Lambda(lambda) => {
                // Capture the current environment for the closure
                let captured = scope.capture();
                RValue::CoreLambda(lambda.clone(), captured)
            }
            CoreExpr::FunctionCall(call) => {
                let func_value = call.func.eval(scope);
                let arg_value = call.arg.eval(scope);

                match func_value {
                    RValue::CoreLambda(lambda, captured) => lambda.run(arg_value, scope, &captured),
                    other => panic!("cannot call non-function value: {:?}", other),
                }
            }
        }
    }
}

impl<T> CoreStatement<T>
where
    T: Clone + Debug,
{
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            CoreStatement::Assignment(assignment) => {
                let value = assignment.value.eval(scope);
                scope.add(assignment.name.value.clone(), value.clone());
                value
            }
            CoreStatement::Expression(expr) => expr.eval(scope),
        }
    }
}

impl<T> CoreLambda<T>
where
    T: Clone + Debug,
{
    pub fn run(
        &self,
        arg: RValue<T>,
        scope: &mut Scope<T>,
        captured: &super::value::CapturedEnv<T>,
    ) -> RValue<T> {
        // First, restore the captured environment
        scope.with_captured(captured);

        // Then enter a new scope for the lambda's parameters
        scope.enter();

        // Bind the parameter
        match &self.param {
            CoreLambdaParam::Unit(_) => {
                // Unit parameter - no binding needed, but check it's unit
                if !matches!(arg, RValue::Unit) {
                    panic!("expected unit argument");
                }
            }
            CoreLambdaParam::Ident(ident) => {
                scope.add(ident.value.clone(), arg);
            }
        }

        // Evaluate body
        let result = match &self.body {
            CoreLambdaBody::Expression(expr) => expr.eval(scope),
            CoreLambdaBody::Block(stmts) => {
                let mut result = RValue::Unit;
                for stmt in stmts {
                    result = stmt.eval(scope);
                }
                result
            }
        };

        // Leave the parameter scope
        scope.leave();
        // Leave the captured environment scope
        scope.leave();
        result
    }
}
