use std::process;
use std::{collections::HashMap, fmt::Debug};

use crate::ast::expression::{
    Expression, FunctionCall, Integer, Lambda, LambdaBody, LambdaParam, StringLiteral,
};
use crate::ast::statement::{Assignment, Statement};
use crate::ast::{Function, Program};

#[derive(Debug, Clone)]
pub enum RValue<T> {
    Unit,
    Integer(Integer<T>),
    String(StringLiteral<T>),
    Lambda(Lambda<T>),
}

type ScopeFrame<T> = HashMap<String, RValue<T>>;

pub struct SimulationScope<T> {
    frames: Vec<ScopeFrame<T>>,
}

impl<T> SimulationScope<T>
where
    T: Clone,
{
    pub fn new(functions: Vec<Function<T>>) -> Self {
        Self {
            frames: vec![
                functions
                    .into_iter()
                    .map(|func| (func.name.value.clone(), RValue::Lambda(func.lambda)))
                    .collect::<HashMap<_, _>>(),
            ],
        }
    }

    pub fn enter(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn leave(&mut self) {
        self.frames.pop();
    }

    pub fn resolve(&self, name: impl ToString) -> Option<RValue<T>> {
        let key = name.to_string();
        self.frames
            .iter()
            .rev()
            .find(|scope| scope.contains_key(&key))
            .and_then(|scope| scope.get(&key))
            .cloned()
    }

    pub fn add(&mut self, name: impl ToString, value: RValue<T>) {
        self.frames
            .last_mut()
            .expect("scope stack should not be empty")
            .insert(name.to_string(), value);
    }
}

impl<T> Expression<T>
where
    T: Clone + std::fmt::Debug,
{
    pub fn eval(&self, scope: &mut SimulationScope<T>) -> RValue<T> {
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
            Expression::Unit => RValue::Unit,
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
    pub fn run(&self, args: &[RValue<T>], scope: &mut SimulationScope<T>) -> RValue<T> {
        scope.enter();

        // Bind parameters to arguments
        for (param, arg) in self.params.iter().zip(args.iter()) {
            match param {
                LambdaParam::Ident(ident) => scope.add(&ident.value, arg.clone()),
                LambdaParam::Unit => {
                    // Unit pattern - just verify the arg is unit (or ignore)
                    // For now, we don't enforce type checking
                }
            }
        }

        let result = match &self.body {
            LambdaBody::Expression(expr) => expr.eval(scope),
            LambdaBody::Block(statements) => {
                let mut result = RValue::Unit;
                for (i, stmt) in statements.iter().enumerate() {
                    if i == statements.len() - 1 {
                        result = stmt.eval(scope);
                    } else {
                        stmt.eval(scope);
                    }
                }
                result
            }
        };

        scope.leave();
        result
    }
}

impl<T> Statement<T>
where
    T: Clone + Debug,
{
    pub fn eval(&self, scope: &mut SimulationScope<T>) -> RValue<T> {
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

pub fn simulate(Program { main, functions }: Program<()>) {
    let mut scope = SimulationScope::new(functions);

    let return_value = main.lambda.run(&[], &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::Lambda(_) => process::exit(0),
    }
}
