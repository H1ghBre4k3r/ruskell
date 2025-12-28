//! # Type System - Hindley-Milner Type Inference
//!
//! This module implements the **type checking phase** of the Ruskell compiler, using
//! the Hindley-Milner type inference algorithm to automatically infer types for all
//! expressions without requiring type annotations.
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → Desugaring → Core AST → [TYPE CHECKING] → Typed Core AST → Interpreter
//! ```
//!
//! ## What is Hindley-Milner Type Inference?
//!
//! Hindley-Milner (HM) is a type inference algorithm that can automatically determine
//! the most general type for any expression, without requiring the programmer to write
//! type annotations. It's the foundation of type systems in languages like Haskell,
//! OCaml, and ML.
//!
//! ### Key Properties
//!
//! 1. **Complete Type Inference** - No type annotations needed (they're still helpful!)
//! 2. **Principal Types** - Every expression has a most general type
//! 3. **Polymorphism** - Supports parametric polymorphism (generics)
//! 4. **Soundness** - If type checking succeeds, no type errors at runtime
//!
//! ## How Type Inference Works
//!
//! ### 1. Type Variables
//!
//! Unknown types are represented as **type variables** (like `'t0`, `'t1`):
//!
//! ```ruskell
//! id = \x => x        // inferred as: 'a -> 'a
//! ```
//!
//! ### 2. Constraint Generation
//!
//! As we traverse the AST, we generate **type constraints**:
//!
//! ```ruskell
//! f(x)
//! // If f : t1 and x : t2
//! // Then t1 must equal (t2 -> t3) for some return type t3
//! ```
//!
//! ### 3. Unification
//!
//! **Unification** solves these constraints by finding substitutions that make types equal:
//!
//! ```text
//! Unify('t1, Int -> Bool):
//!   Result: [t1 := Int -> Bool]
//!
//! Unify('t2 -> 't3, Int -> 't3):
//!   Result: [t2 := Int]
//! ```
//!
//! The unification algorithm handles:
//! - Matching type variables with concrete types
//! - Matching type variables with other type variables
//! - Recursive unification of function types
//! - **Occurs check** to prevent infinite types (e.g., `'t0 = 't0 -> Int`)
//!
//! ### 4. Generalization (Let-Polymorphism)
//!
//! When we assign a value in a `do` block, we **generalize** its type by quantifying
//! free type variables:
//!
//! ```ruskell
//! do
//!   id := \x => x        // Generalized to: forall 'a. 'a -> 'a
//!   a := id(5)           // 'a instantiated to Int
//!   b := id("hi")        // 'a instantiated to String
//!   ...
//! end
//! ```
//!
//! This allows polymorphic functions to be used at multiple types.
//!
//! ### 5. Instantiation
//!
//! When we use a polymorphic value, we **instantiate** it with fresh type variables:
//!
//! ```text
//! id has type: forall 'a. 'a -> 'a
//! Use 1: id(5)        // Instantiate as: 't0 -> 't0, then unify t0 = Int
//! Use 2: id("hello")  // Instantiate as: 't1 -> 't1, then unify t1 = String
//! ```
//!
//! ## Type Inference Algorithm (Detailed)
//!
//! The inference algorithm (`infer_expr`) works recursively on expressions:
//!
//! ### Literals
//!
//! Literals have fixed types:
//! - `42` → `Int`
//! - `"hello"` → `String`
//! - `true` → `Bool`
//! - `()` → `Unit`
//!
//! ### Variables
//!
//! Look up the variable in the type environment and **instantiate** its type scheme:
//!
//! ```text
//! Env: { x: forall 'a. 'a -> 'a }
//! Infer(x): Instantiate to 't0 -> 't0
//! ```
//!
//! ### Lambdas
//!
//! For `\x => body`:
//! 1. Create a fresh type variable `'t0` for parameter `x`
//! 2. Add `x: 't0` to the environment
//! 3. Infer the type of `body`, getting type `'t1` and substitution `S1`
//! 4. Apply `S1` to `'t0`, getting the actual parameter type
//! 5. Return type: `S1('t0) -> 't1`
//!
//! ```ruskell
//! \x => x + 1
//! // Step 1: x : 't0
//! // Step 2: Infer x + 1
//! //   - x has type 't0
//! //   - 1 has type Int
//! //   - + requires Int -> Int -> Int
//! //   - Unify 't0 with Int, getting substitution [t0 := Int]
//! // Step 3: Apply substitution: Int -> Int
//! ```
//!
//! ### Function Calls
//!
//! For `f(arg)`:
//! 1. Infer type of `f`: get type `tf` and substitution `S1`
//! 2. Apply `S1` to environment and infer type of `arg`: get `targ` and `S2`
//! 3. Create fresh type variable `'tresult` for the result
//! 4. Unify `S2(tf)` with `targ -> 'tresult`, getting `S3`
//! 5. Return `S3(S2(S1(tresult)))` and composed substitution
//!
//! ```ruskell
//! (\x => x + 1)(5)
//! // Step 1: Infer lambda: Int -> Int
//! // Step 2: Infer 5: Int
//! // Step 3: Unify (Int -> Int) with (Int -> 'tresult)
//! // Step 4: Result type: Int
//! ```
//!
//! ### If-Then-Else
//!
//! For `if cond then e1 else e2`:
//! 1. Infer type of `cond`, must unify with `Bool`
//! 2. Infer type of `e1`, getting type `t1`
//! 3. Infer type of `e2`, getting type `t2`
//! 4. Unify `t1` with `t2` (both branches must have same type)
//! 5. Return unified type
//!
//! ### Binary Operators
//!
//! Each operator has a fixed type signature:
//! - Arithmetic (`+`, `-`, `*`, `/`): `Int -> Int -> Int`
//! - Comparison (`==`, `<`, etc.): `Int -> Int -> Bool`
//! - Logical (`&&`, `||`): `Bool -> Bool -> Bool`
//! - Concatenation (`++`): `String -> String -> String`
//!
//! ## Module Structure
//!
//! - [`ty`] - Type representation (`Type`, `TypeVar`, `TypeScheme`)
//! - [`mod@env`] - Type environment (maps variables to type schemes)
//! - [`subst`] - Substitutions (maps type variables to types)
//! - [`unify`] - Unification algorithm (solves type equations)
//! - [`infer`] - Main type inference algorithm
//! - [`validate`] - Semantic validation and inference entry point
//!
//! ## Example: Complete Inference Trace
//!
//! ```ruskell
//! compose f g = \x => f(g(x))
//! ```
//!
//! ```text
//! Step 1: Create fresh vars for f, g
//!   f : 't0
//!   g : 't1
//!
//! Step 2: Infer lambda \x => f(g(x))
//!   x : 't2
//!
//! Step 3: Infer g(x)
//!   - g has type 't1
//!   - x has type 't2
//!   - Create fresh result var 't3
//!   - Unify 't1 with ('t2 -> 't3)
//!   - Substitution: [t1 := 't2 -> 't3]
//!   - Result: 't3
//!
//! Step 4: Infer f(g(x))
//!   - f has type 't0
//!   - g(x) has type 't3
//!   - Create fresh result var 't4
//!   - Unify 't0 with ('t3 -> 't4)
//!   - Substitution: [t0 := 't3 -> 't4]
//!   - Result: 't4
//!
//! Step 5: Build lambda type
//!   - Parameter: 't2
//!   - Body: 't4
//!   - Lambda type: 't2 -> 't4
//!
//! Step 6: Build function type
//!   - Apply all substitutions:
//!     f : 't3 -> 't4
//!     g : 't2 -> 't3
//!   - Function type: ('t3 -> 't4) -> ('t2 -> 't3) -> 't2 -> 't4
//!
//! Step 7: Generalize (no free vars in empty environment)
//!   compose : forall a b c. (b -> c) -> (a -> b) -> a -> c
//! ```
//!
//! ## Error Handling
//!
//! Type errors can occur in several situations:
//!
//! ### Unbound Variable
//!
//! ```ruskell
//! main = x  // Error: unbound variable 'x'
//! ```
//!
//! ### Type Mismatch
//!
//! ```ruskell
//! main = 5 + "hello"  // Error: expected Int, found String
//! ```
//!
//! ### Occurs Check
//!
//! ```ruskell
//! // This would create: 't0 = 't0 -> Int (infinite type!)
//! omega = \x => x(x)  // Error: occurs check
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::core`] - Core AST (input to type checker)
//! - [`crate::desugar`] - Desugaring pass (runs before type checking)
//! - [`crate::interpreter`] - Runtime evaluation (runs after type checking)

pub mod env;
pub mod error;
pub mod infer;
pub mod subst;
pub mod ty;
pub mod unify;
pub mod validate;

pub use validate::validate_and_type_check;
