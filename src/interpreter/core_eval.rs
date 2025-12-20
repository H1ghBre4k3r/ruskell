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
            CoreExpr::Ident(ident) => scope
                .resolve(&ident.value)
                .unwrap_or_else(|| panic!("undefined identifier: {}", ident.value)),
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
            CoreExpr::Boolean(boolean) => RValue::Bool(boolean.value),
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
            CoreExpr::BinaryOp(binop) => {
                let left = binop.left.eval(scope);
                let right = binop.right.eval(scope);

                let left_val = match left {
                    RValue::Integer(i) => i.value,
                    _ => panic!("left operand must be integer"),
                };

                let right_val = match right {
                    RValue::Integer(i) => i.value,
                    _ => panic!("right operand must be integer"),
                };

                let result = match binop.op {
                    crate::ast::expression::BinOpKind::Add => left_val + right_val,
                    crate::ast::expression::BinOpKind::Sub => left_val - right_val,
                    crate::ast::expression::BinOpKind::Mul => left_val * right_val,
                    crate::ast::expression::BinOpKind::Div => {
                        if right_val == 0 {
                            panic!("division by zero");
                        }
                        left_val / right_val
                    }
                    crate::ast::expression::BinOpKind::Eq => {
                        return RValue::Bool(left_val == right_val);
                    }
                    crate::ast::expression::BinOpKind::NotEq => {
                        return RValue::Bool(left_val != right_val);
                    }
                    crate::ast::expression::BinOpKind::Lt => {
                        return RValue::Bool(left_val < right_val);
                    }
                    crate::ast::expression::BinOpKind::Gt => {
                        return RValue::Bool(left_val > right_val);
                    }
                    crate::ast::expression::BinOpKind::LtEq => {
                        return RValue::Bool(left_val <= right_val);
                    }
                    crate::ast::expression::BinOpKind::GtEq => {
                        return RValue::Bool(left_val >= right_val);
                    }
                };

                RValue::Integer(crate::ast::expression::Integer {
                    value: result,
                    position: binop.position.clone(),
                    info: binop.info.clone(),
                })
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
