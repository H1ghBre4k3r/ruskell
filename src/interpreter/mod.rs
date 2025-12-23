//! # Interpreter - Tree-Walking Interpreter for Core AST
//!
//! This module implements the **execution phase** of the Ruskell compiler, evaluating
//! Core AST programs using a tree-walking interpreter with lexical scoping.
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → Desugaring → Core AST → Type Checking → [INTERPRETER]
//! ```
//!
//! ## What is a Tree-Walking Interpreter?
//!
//! A tree-walking interpreter directly executes the AST by recursively traversing
//! the tree structure and performing operations at each node:
//!
//! ```text
//! Eval(5 + 3):
//!   1. Eval left: 5 → RValue::Integer(5)
//!   2. Eval right: 3 → RValue::Integer(3)
//!   3. Apply +: 5 + 3 → RValue::Integer(8)
//! ```
//!
//! ### Advantages
//!
//! - **Simple to implement** - Direct mapping from AST to execution
//! - **Easy to debug** - Can trace execution through the AST
//! - **Flexible** - Easy to add new features and debug
//!
//! ### Disadvantages
//!
//! - **Slower than compiled code** - Interprets rather than executing native code
//! - **Higher memory usage** - Keeps entire AST in memory
//!
//! ## Runtime Values ([`value`])
//!
//! During execution, expressions evaluate to runtime values (`RValue`):
//!
//! ```rust
//! pub enum RValue {
//!     Unit,                           // ()
//!     Integer(i128),                  // 42
//!     String(String),                 // "hello"
//!     Bool(bool),                     // true
//!     CoreLambda(CoreLambda, CapturedEnv),  // Functions with captures
//!     Builtin(BuiltinFunction),       // print, toString, etc.
//! }
//! ```
//!
//! ### Closures and Captured Environments
//!
//! Lambdas capture their lexical environment at creation time:
//!
//! ```ruskell
//! main = do
//!     x := 5
//!     f := \y => x + y    // Captures x=5
//!     f(3)                // Returns 8
//! end
//! ```
//!
//! The `CapturedEnv` stores the values of free variables:
//!
//! ```rust
//! pub struct CapturedEnv(HashMap<String, RValue>);
//! ```
//!
//! ## Lexical Scoping ([`scope`])
//!
//! The interpreter maintains a scope chain for variable resolution:
//!
//! ```rust
//! pub struct Scope {
//!     frames: Vec<HashMap<String, RValue>>,
//!     functions: HashMap<String, CoreFunction>,
//! }
//! ```
//!
//! ### Scope Operations
//!
//! - **push_frame()** - Enter a new scope (function call, do-block)
//! - **pop_frame()** - Exit the current scope
//! - **set()** - Bind a variable in the current scope
//! - **get()** - Look up a variable (searches from innermost to outermost)
//!
//! ### Scope Chain Example
//!
//! ```ruskell
//! main = do
//!     a := 1          // Frame 0: {a: 1}
//!     f := \x => do   // Frame 1: {x: 5, f: <lambda>}
//!         b := 2      // Frame 2: {b: 2}
//!         x + a + b   // Looks up: x in Frame 1, a in Frame 0, b in Frame 2
//!     end
//!     f(5)
//! end
//! ```
//!
//! ## Evaluation Algorithm ([`core_eval`])
//!
//! The evaluation algorithm recursively processes Core AST nodes:
//!
//! ### Literals
//!
//! Literals evaluate to their corresponding runtime values:
//! - `42` → `RValue::Integer(42)`
//! - `"hello"` → `RValue::String("hello")`
//! - `true` → `RValue::Bool(true)`
//! - `()` → `RValue::Unit`
//!
//! ### Variables
//!
//! Look up the variable in scope:
//! 1. Search current scope frame
//! 2. If not found, search parent frames
//! 3. If not found, search global functions
//! 4. If not found, panic (should be caught by type checker!)
//!
//! ### Lambdas
//!
//! Create a closure capturing the current environment:
//!
//! ```rust
//! RValue::CoreLambda(lambda, captured_env)
//! ```
//!
//! The captured environment includes all free variables from the current scope.
//!
//! ### Function Calls
//!
//! For `f(arg)`:
//! 1. Evaluate function expression: get `RValue::CoreLambda(lambda, env)`
//! 2. Evaluate argument expression: get `arg_value`
//! 3. Create new scope with captured environment
//! 4. Bind parameter to argument value
//! 5. Evaluate lambda body in new scope
//! 6. Return result
//!
//! ### Binary Operators
//!
//! Evaluate both operands and apply the operation:
//!
//! ```rust
//! match (left_val, right_val) {
//!     (Integer(a), Integer(b)) => match op {
//!         Add => Integer(a + b),
//!         Sub => Integer(a - b),
//!         Mul => Integer(a * b),
//!         Div => Integer(a / b),
//!         // ... comparison operators ...
//!     },
//!     (String(a), String(b)) if op == Concat => String(a + &b),
//!     (Bool(a), Bool(b)) if op == And => Bool(a && b),
//!     // ... etc ...
//! }
//! ```
//!
//! ### If-Then-Else
//!
//! 1. Evaluate condition
//! 2. If true, evaluate then-branch
//! 3. If false, evaluate else-branch
//!
//! ### Do-Blocks
//!
//! Execute statements sequentially:
//!
//! 1. Create new scope frame
//! 2. For each statement:
//!    - If assignment: bind variable in current frame
//!    - If expression: evaluate (discard result except for last)
//! 3. Pop scope frame
//! 4. Return value of last statement
//!
//! ## Builtin Functions
//!
//! The interpreter provides builtin functions:
//!
//! ### `print`
//!
//! ```ruskell
//! print(42)        // Prints: 42
//! print("hello")   // Prints: hello
//! ```
//!
//! Type: `forall a. a -> Unit`
//!
//! ### `toString`
//!
//! ```ruskell
//! toString(42)     // Returns: "42"
//! toString(true)   // Returns: "true"
//! ```
//!
//! Type: `forall a. a -> String`
//!
//! ## Exit Code Behavior
//!
//! The `run()` function uses main's return value as the process exit code:
//!
//! - `RValue::Integer(n)` → Exit with code `n`
//! - `RValue::Bool(true)` → Exit with code `0`
//! - `RValue::Bool(false)` → Exit with code `1`
//! - `RValue::Unit` → Exit with code `0`
//! - `RValue::Lambda(_)` → Exit with code `0`
//!
//! ## Example Execution Trace
//!
//! ```ruskell
//! add x y = x + y
//! main = add(2)(3)
//! ```
//!
//! Execution:
//! ```text
//! 1. Eval main:
//!    - Look up 'main' in global functions
//!    - Get lambda: \() => add(2)(3)
//!    - Call with Unit
//!
//! 2. Eval add(2)(3):
//!    a. Eval add(2):
//!       - Look up 'add' → CoreLambda(\x => \y => x + y, {})
//!       - Eval 2 → Integer(2)
//!       - Call: bind x=2, return CoreLambda(\y => x + y, {x: 2})
//!    
//!    b. Eval partial_add(3):
//!       - Have CoreLambda(\y => x + y, {x: 2})
//!       - Eval 3 → Integer(3)
//!       - Call: bind y=3, eval x + y with scope {x: 2, y: 3}
//!       - Eval x → Integer(2)
//!       - Eval y → Integer(3)
//!       - Apply + → Integer(5)
//!
//! 3. Return Integer(5)
//! 4. Exit with code 5
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::core`] - Core AST definitions (input to interpreter)
//! - [`crate::types`] - Type checker (runs before interpreter)
//!
//! Public exports from this module:
//! - [`Scope`] - Scope management for variable lookup
//! - [`RValue`] - Runtime value definitions
//! - [`CapturedEnv`] - Captured environment for closures

mod core_eval;
mod scope;
mod value;

pub use scope::Scope;
pub use value::{CapturedEnv, RValue};

use std::collections::HashMap;
use std::process;

use crate::core::CoreProgram;

/// Run a Core AST program and exit with the appropriate exit code.
///
/// This is the main entry point for program execution. It:
/// 1. Creates a scope with all global functions
/// 2. Executes the `main` function
/// 3. Exits the process with a code based on main's return value
///
/// # Exit Codes
///
/// - Integer value: Uses that integer as exit code
/// - Bool: 0 for true, 1 for false
/// - Unit/Lambda: Exits with 0
///
/// # Example
///
/// ```ignore
/// let program = desugar_and_lift(parsed_program);
/// interpreter::run(program);  // Executes and exits
/// ```
pub fn run(CoreProgram { main, functions }: CoreProgram<()>) {
    let mut scope = Scope::new(functions);

    // Main function doesn't capture anything (it's top-level)
    let empty_capture = CapturedEnv(HashMap::new());
    let return_value = main.lambda.run(RValue::Unit, &mut scope, &empty_capture);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::Bool(b) => process::exit(if b { 0 } else { 1 }),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::CoreLambda(_, _) => process::exit(0),
        RValue::Builtin(_) => process::exit(0),
        // Legacy: shouldn't happen with Core AST
        RValue::Lambda(_) => process::exit(0),
    }
}
