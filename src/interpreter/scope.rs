use std::collections::HashMap;

use crate::core::CoreFunction;

use super::value::{Builtin, CapturedEnv, RValue};

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
    /// Functions capture the global environment, enabling recursion and mutual recursion
    pub fn new(functions: Vec<CoreFunction<T>>) -> Self {
        // First pass: build the global scope frame with empty captures
        let mut global_frame: HashMap<String, RValue<T>> = HashMap::new();

        // Add builtins
        global_frame.insert("print".to_string(), RValue::Builtin(Builtin::Print));

        for func in &functions {
            global_frame.insert(
                func.name.value.clone(),
                RValue::CoreLambda(func.lambda.clone(), CapturedEnv(HashMap::new())),
            );
        }

        // Second pass: create captured environment with all functions
        let global_env = CapturedEnv(global_frame.clone());

        // Third pass: replace with properly captured versions
        // Now each function can see all other functions (including itself)
        for func in functions {
            global_frame.insert(
                func.name.value.clone(),
                RValue::CoreLambda(func.lambda, global_env.clone()),
            );
        }

        Self {
            frames: vec![global_frame],
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
