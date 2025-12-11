use std::collections::HashMap;

use crate::core::CoreFunction;

use super::value::RValue;

type ScopeFrame<T> = HashMap<String, RValue<T>>;

/// Manages lexical scoping during interpretation
pub struct Scope<T> {
    frames: Vec<ScopeFrame<T>>,
}

impl<T> Scope<T>
where
    T: Clone,
{
    /// Create a new scope with top-level functions pre-loaded
    pub fn new(functions: Vec<CoreFunction<T>>) -> Self {
        Self {
            frames: vec![
                functions
                    .into_iter()
                    .map(|func| (func.name.value.clone(), RValue::CoreLambda(func.lambda)))
                    .collect::<HashMap<_, _>>(),
            ],
        }
    }

    /// Enter a new scope frame
    pub fn enter(&mut self) {
        self.frames.push(HashMap::new());
    }

    /// Leave the current scope frame
    pub fn leave(&mut self) {
        self.frames.pop();
    }

    /// Resolve a name by searching from innermost to outermost scope
    pub fn resolve(&self, name: impl ToString) -> Option<RValue<T>> {
        let key = name.to_string();
        self.frames
            .iter()
            .rev()
            .find(|scope| scope.contains_key(&key))
            .and_then(|scope| scope.get(&key))
            .cloned()
    }

    /// Add a binding to the current (innermost) scope
    pub fn add(&mut self, name: impl ToString, value: RValue<T>) {
        self.frames
            .last_mut()
            .expect("scope stack should not be empty")
            .insert(name.to_string(), value);
    }
}
