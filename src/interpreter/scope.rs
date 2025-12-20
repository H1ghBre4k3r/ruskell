use std::collections::HashMap;

use crate::core::CoreFunction;

use super::value::{CapturedEnv, RValue};

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
                    .map(|func| {
                        (
                            func.name.value.clone(),
                            RValue::CoreLambda(func.lambda, CapturedEnv(HashMap::new())),
                        )
                    })
                    .collect::<HashMap<_, _>>(),
            ],
        }
    }

    /// Capture the current environment for a closure
    pub fn capture(&self) -> CapturedEnv<T> {
        let mut captured = HashMap::new();
        // Capture all bindings from all frames
        for frame in &self.frames {
            captured.extend(frame.clone());
        }
        CapturedEnv(captured)
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

    /// Create a temporary scope with captured environment
    pub fn with_captured(&mut self, captured: &CapturedEnv<T>) {
        self.frames.push(captured.0.clone());
    }
}
