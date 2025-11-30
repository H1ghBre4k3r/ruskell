use crate::interpreter::{RValue, SimulationScope};
use crate::lexer::{Ident, Integer, StringLiteral};

#[derive(Debug, Clone)]
pub enum Expression {
    SingularExpression(SingularExpression),
    Assignment(Assignment),
    Lambda(Lambda),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

impl Expression {
    pub fn eval(&self, scope: &mut SimulationScope) -> RValue {
        match self {
            Expression::SingularExpression(singular_expression) => match singular_expression {
                SingularExpression::Ident(ident) => {
                    let value = scope
                        .resolve(&ident.value)
                        .unwrap_or_else(|| panic!("undefined identifier: {}", ident.value));
                    // If it's a function with no args, call it immediately
                    match &value {
                        RValue::Function(func) if func.args.is_empty() => func.run(&[], scope),
                        _ => value,
                    }
                }
                SingularExpression::Integer(integer) => RValue::Integer(integer.clone()),
                SingularExpression::String(string_literal) => {
                    RValue::String(string_literal.clone())
                }
            },
            Expression::Assignment(Assignment { name, value }) => {
                let evaluated = value.eval(scope);
                scope.add(&name.value, evaluated);
                RValue::Unit
            }
            Expression::Lambda(lambda) => RValue::Lambda(lambda.clone()),
            Expression::FunctionCall(FunctionCall { func, args }) => {
                let func_value = func.eval(scope);
                let evaluated_args: Vec<RValue> = args.iter().map(|a| a.eval(scope)).collect();

                match func_value {
                    RValue::Lambda(lambda) => lambda.run(&evaluated_args, scope),
                    RValue::Function(function) => function.run(&evaluated_args, scope),
                    other => panic!("cannot call non-function value: {:?}", other),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<Ident>,
    pub body: Vec<Expression>,
}

impl Lambda {
    pub fn run(&self, args: &[RValue], scope: &mut SimulationScope) -> RValue {
        scope.enter();

        // Bind parameters to arguments
        for (param, arg) in self.params.iter().zip(args.iter()) {
            scope.add(&param.value, arg.clone());
        }

        let mut i = 0;
        while i < self.body.len() {
            let expr = &self.body[i];

            if i == self.body.len() - 1 {
                let val = expr.eval(scope);
                scope.leave();
                return val;
            } else {
                expr.eval(scope);
            }

            i += 1;
        }

        scope.leave();
        RValue::Unit
    }
}

#[derive(Debug, Clone)]
pub enum SingularExpression {
    Ident(Ident),
    Integer(Integer),
    String(StringLiteral),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Ident,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub expression: Vec<Expression>,
}

impl Function {
    pub fn run(&self, args: &[RValue], scope: &mut SimulationScope) -> RValue {
        scope.enter();

        // Bind parameters to arguments
        for (param, arg) in self.args.iter().zip(args.iter()) {
            scope.add(&param.value, arg.clone());
        }

        let mut i = 0;
        while i < self.expression.len() {
            let expr = &self.expression[i];

            if i == self.expression.len() - 1 {
                let val = expr.eval(scope);
                scope.leave();
                return val;
            } else {
                expr.eval(scope);
            }

            i += 1;
        }

        scope.leave();
        RValue::Unit
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
}
