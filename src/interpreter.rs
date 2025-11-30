use std::collections::HashMap;
use std::process;

use crate::ast::{Function, Lambda, Program};
use crate::lexer::{Integer, StringLiteral};

#[derive(Debug, Clone)]
pub enum RValue {
    Integer(Integer),
    String(StringLiteral),
    Unit,
    Lambda(Lambda),
    Function(Function),
}

type ScopeFrame = HashMap<String, RValue>;

pub struct SimulationScope {
    frames: Vec<ScopeFrame>,
}

impl SimulationScope {
    pub fn new(functions: Vec<Function>) -> Self {
        Self {
            frames: vec![
                functions
                    .into_iter()
                    .map(|func| (func.name.value.clone(), RValue::Function(func)))
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

    pub fn resolve(&self, name: impl ToString) -> Option<RValue> {
        let key = name.to_string();
        self.frames
            .iter()
            .rev()
            .find(|scope| scope.contains_key(&key))
            .and_then(|scope| scope.get(&key))
            .cloned()
    }

    pub fn add(&mut self, name: impl ToString, value: RValue) {
        self.frames
            .last_mut()
            .expect("scope stack should not be empty")
            .insert(name.to_string(), value);
    }
}

pub fn simulate(Program { main, functions }: Program) {
    let mut scope = SimulationScope::new(functions);

    let return_value = main.run(&[], &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(
            integer
                .value
                .parse()
                .expect("This should, by definition, be an integer"),
        ),
        RValue::Unit => process::exit(0),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::Lambda(_) => process::exit(0),
        RValue::Function(_) => process::exit(0),
    }
}
