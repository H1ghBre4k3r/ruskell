//! # Core Type System Definitions
//!
//! This module defines foundational types used in Hindley-Milner
//! type inference system: types, type variables, and type schemes.
//!
//! ## Overview
//!
//! The type system is built from three main components:
//!
//! - **Type** - Represents concrete and polymorphic types (Int, String, Bool, Unit, Var, Func)
//! - **TypeVar** - Type variable for polymorphism (e.g., `'a`, `'b`)
//! - **TypeScheme** - Polymorphic type with quantified variables (e.g., `forall 'a. 'a -> 'a`)
//!
//! ## Types
//!
//! Ruskell supports following concrete types:
//!
//! - `Int` - Integer values
//! - `String` - String values
//! - `Bool` - Boolean values (true, false)
//! - `Unit` - The unit value `()`
//!
//! And polymorphic types:
//!
//! - `Var('a)` - Type variable (represents any type in polymorphic context)
//! - `Func(t1, t2)` - Function type `t1 -> t2`
//!
//! ## Type Variables
//!
//! Type variables are placeholders that can be unified with any concrete type
//! during type inference. They're represented with a unique ID and optional
//! human-readable name:
//!
//! ```text
//! TypeVar { id: 0, name: Some("a") }   // 'a
//! TypeVar { id: 1, name: None }        // 't1
//! ```
//!
//! ## Type Schemes
//!
//! Type schemes represent polymorphic types by quantifying type variables:
//!
//! ```text
//! Identity function type:
//! TypeScheme {
//!     vars: [TypeVar { id: 0 }],
//!     ty: Func(Var(0), Var(0))  // 'a -> 'a
//! }
//!
//! // When used, we instantiate with fresh vars:
//! // Instance 1: 't5 -> 't5
//! // Instance 2: 't6 -> 't6
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::types::infer`] - Type inference using these types
//! - [`crate::types::env`] - Type environment for storing type schemes
//! - [`crate::types::subst`] - Substitutions for type variable unification

use std::collections::HashSet;
use std::fmt;

/// Type variable for polymorphism.
///
/// Type variables represent unknown or polymorphic types during type inference.
/// Each type variable is uniquely identified by an integer ID.
///
/// # Fields
///
/// * `id` - Unique identifier for this type variable
/// * `name` - Optional human-readable name (e.g., "a", "b") for pretty printing
///
/// # Example
///
/// ```text
/// // Fresh type variable (no name):
/// TypeVar { id: 0, name: None }
/// // Pretty prints as: 't0
///
/// // Named type variable (from source or inferred):
/// TypeVar { id: 1, name: Some("a") }
/// // Pretty prints as: 'a
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub name: Option<String>,
}

impl TypeVar {
    /// Create a new type variable with the given ID.
    ///
    /// # Arguments
    ///
    /// * `id` - Unique identifier for this type variable
    ///
    /// # Returns
    ///
    /// A new `TypeVar` with no human-readable name
    ///
    /// # Example
    ///
    /// ```text
    /// TypeVar::new(0)   // Creates unnamed variable 't0
    /// TypeVar::new(1)   // Creates unnamed variable 't1
    /// ```
    pub fn new(id: usize) -> Self {
        Self { id, name: None }
    }

    /// Create a new type variable with the given ID and name.
    ///
    /// # Arguments
    ///
    /// * `id` - Unique identifier for this type variable
    /// * `name` - Human-readable name for pretty printing
    ///
    /// # Returns
    ///
    /// A new `TypeVar` with the specified name
    ///
    /// # Example
    ///
    /// ```text
    /// TypeVar::with_name(0, "a")   // Creates variable 'a
    /// TypeVar::with_name(1, "b")   // Creates variable 'b
    /// ```
    pub fn with_name(id: usize, name: String) -> Self {
        Self {
            id,
            name: Some(name),
        }
    }
}

/// All possible types in Ruskell.
///
/// `Type` represents both concrete types (Int, String, Bool, Unit)
/// and polymorphic types (type variables and function types).
///
/// # Variants
///
/// * `Int` - Integer type
/// * `String` - String type
/// * `Unit` - Unit type (only value is `()`)
/// * `Bool` - Boolean type
/// * `List` - List type (homogeneous collection)
/// * `Var` - Type variable (for polymorphism)
/// * `Func` - Function type `parameter_type -> return_type`
///
/// # Examples
///
/// ```text
/// // Concrete types:
/// Type::Int                  // Int
/// Type::String               // String
/// Type::Bool                 // Bool
/// Type::Unit                 // Unit
///
/// // List types:
/// Type::List(Box::new(Type::Int))     // [Int]
/// Type::List(Box::new(Type::String))  // [String]
/// Type::List(Box::new(Type::List(Box::new(Type::Int))))  // [[Int]]
///
/// // Type variable:
/// Type::Var(TypeVar::new(0))  // 't0 (or 'a if named)
///
/// // Function types:
/// Type::Func(Int, Int)        // Int -> Int
/// Type::Func(Int, String)     // Int -> String
/// Type::Func(Int, Func(Int, Int))  // Int -> (Int -> Int)
///                                 // or: Int -> Int -> Int
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Unit,
    Bool,
    List(Box<Type>),
    Var(TypeVar),
    Func(Box<Type>, Box<Type>),
}

impl Type {
    /// Create a function type from parameter and return types.
    ///
    /// # Arguments
    ///
    /// * `t1` - Parameter type (type of function's input)
    /// * `t2` - Return type (type of function's output)
    ///
    /// # Returns
    ///
    /// A function type `t1 -> t2`
    ///
    /// # Examples
    ///
    /// ```text
    /// Type::func(Int, Int)         // Int -> Int
    /// Type::func(Int, String)      // Int -> String
    /// Type::func(Int, func(Int, Int))  // Int -> Int -> Int
    /// ```
    pub fn func(t1: Type, t2: Type) -> Self {
        Type::Func(Box::new(t1), Box::new(t2))
    }

    /// Get the set of free type variables in this type.
    ///
    /// Free type variables are those that are not bound by a
    /// quantifier (i.e., not in a type scheme's variable list).
    ///
    /// # Returns
    ///
    /// A `HashSet` containing all free type variables in this type
    ///
    /// # Examples
    ///
    /// ```text
    /// // Concrete types have no free vars:
    /// Int.free_type_vars()            // {}
    /// String.free_type_vars()         // {}
    ///
    /// // Type variables are free by default:
    /// Var('a).free_type_vars()       // {'a'}
    ///
    /// // Function types collect free vars from both parts:
    /// Func(Var('a), Int).free_type_vars()     // {'a'}
    /// Func(Var('a), Var('b')).free_type_vars() // {'a', 'b'}
    /// Func(Var('a), Var('a')).free_type_vars() // {'a'} (single element)
    /// ```
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Int | Type::String | Type::Unit | Type::Bool => HashSet::new(),
            Type::Var(v) => {
                let mut set = HashSet::new();
                set.insert(v.clone());
                set
            }
            Type::List(elem_ty) => elem_ty.free_type_vars(),
            Type::Func(t1, t2) => {
                let mut set = t1.free_type_vars();
                set.extend(t2.free_type_vars());
                set
            }
        }
    }

    /// Convert this type to a human-readable string representation.
    ///
    /// Uses Haskell-style notation with type variables prefixed with `'`.
    ///
    /// # Returns
    ///
    /// A string representation of this type
    ///
    /// # Examples
    ///
    /// ```text
    /// Int.pretty()                    // "Int"
    /// String.pretty()                 // "String"
    /// Unit.pretty()                    // "Unit"
    /// Bool.pretty()                    // "Bool"
    ///
    /// Var('a).pretty()               // "'a"
    /// Var(unamed).pretty()            // "'t0" (or "'t5", etc.)
    ///
    /// Func(Int, String).pretty()       // "Int -> String"
    /// Func(Int, Int).pretty()        // "Int -> Int"
    ///
    /// // Nested functions use parentheses for clarity:
    /// Func(Func(Int, Int), String).pretty()  // "(Int -> Int) -> String"
    /// Func(Int, Func(Int, String)).pretty()  // "Int -> Int -> String"
    /// ```
    pub fn pretty(&self) -> String {
        match self {
            Type::Int => "Int".to_string(),
            Type::String => "String".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::List(elem_ty) => format!("[{}]", elem_ty.pretty()),
            Type::Var(v) => {
                if let Some(name) = &v.name {
                    format!("'{}", name)
                } else {
                    format!("'t{}", v.id)
                }
            }
            Type::Func(t1, t2) => {
                let t1_str = if matches!(**t1, Type::Func(_, _)) {
                    format!("({})", t1.pretty())
                } else {
                    t1.pretty()
                };
                format!("{} -> {}", t1_str, t2.pretty())
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pretty())
    }
}

/// Polymorphic type scheme.
///
/// Type schemes represent polymorphic types by quantifying (binding)
/// type variables. A type scheme like `forall 'a. 'a -> 'a` means
/// "for all types 'a, this is a function from 'a to 'a".
///
/// # Fields
///
/// * `vars` - List of quantified type variables (these are bound)
/// * `ty` - The underlying type with potentially free variables
///
/// # Semantics
///
/// The `vars` list specifies which type variables are quantified (bound).
/// These bound variables can be instantiated with fresh type variables when
/// the type scheme is **used**, but they are not considered free when
/// the type scheme is **defined**.
///
/// # Example: Identity Function
///
/// ```text
/// // Type scheme for identity function:
/// // forall 'a. 'a -> 'a
/// TypeScheme {
///     vars: [TypeVar { id: 0, name: Some("a") }],
///     ty: Func(Var(0), Var(0)),
/// }
///
/// // When used with integers:
/// // Instantiate 'a with Int → Int -> Int
///
/// // When used with strings:
/// // Instantiate 'a with String → String -> String
///
/// // When used with function:
/// // Instantiate 'a with Int->Int → (Int->Int) -> (Int->Int)
/// ```
///
/// # Monomorphic vs Polymorphic
///
/// - **Monomorphic**: No quantified variables, concrete type
/// - **Polymorphic**: Has quantified variables, can be instantiated
///
/// ```text
/// // Monomorphic:
/// TypeScheme { vars: [], ty: Int }
///
/// // Polymorphic:
/// TypeScheme { vars: ['a'], ty: Func(Var('a), Var('a)) }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl TypeScheme {
    /// Create a monomorphic type scheme (no type variables).
    ///
    /// Use this for concrete types that cannot be instantiated.
    ///
    /// # Arguments
    ///
    /// * `ty` - The concrete type
    ///
    /// # Returns
    ///
    /// A type scheme with no quantified variables
    ///
    /// # Example
    ///
    /// ```text
    /// TypeScheme::monomorphic(Int)
    /// // Equivalent to: TypeScheme { vars: [], ty: Int }
    /// ```
    pub fn monomorphic(ty: Type) -> Self {
        TypeScheme {
            vars: Vec::new(),
            ty,
        }
    }

    /// Create a polymorphic type scheme with quantified variables.
    ///
    /// Use this for polymorphic types that can be instantiated.
    ///
    /// # Arguments
    ///
    /// * `vars` - List of quantified type variables
    /// * `ty` - The type with potentially free variables
    ///
    /// # Returns
    ///
    /// A type scheme with the given variables quantified
    ///
    /// # Example
    ///
    /// ```text
    /// let a = TypeVar::new(0);
    /// TypeScheme::polymorphic(vec![a.clone()], Type::func(Type::Var(a), Type::Var(a)))
    /// // Equivalent to: forall 'a. 'a -> 'a
    /// ```
    pub fn polymorphic(vars: Vec<TypeVar>, ty: Type) -> Self {
        TypeScheme { vars, ty }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_free_type_vars_concrete() {
        let ty = Type::func(Type::Int, Type::String);
        assert!(ty.free_type_vars().is_empty());
    }

    #[test]
    fn test_free_type_vars_single() {
        let var = TypeVar::new(0);
        let ty = Type::Var(var.clone());
        let free = ty.free_type_vars();
        assert_eq!(free.len(), 1);
        assert!(free.contains(&var));
    }

    #[test]
    fn test_free_type_vars_function() {
        let var1 = TypeVar::new(0);
        let var2 = TypeVar::new(1);
        let ty = Type::func(Type::Var(var1.clone()), Type::Var(var2.clone()));
        let free = ty.free_type_vars();
        assert_eq!(free.len(), 2);
        assert!(free.contains(&var1));
        assert!(free.contains(&var2));
    }

    #[test]
    fn test_pretty_print_simple() {
        assert_eq!(Type::Int.pretty(), "Int");
        assert_eq!(Type::String.pretty(), "String");
        assert_eq!(Type::Unit.pretty(), "Unit");
    }

    #[test]
    fn test_pretty_print_var() {
        let var = TypeVar::with_name(0, "a".to_string());
        assert_eq!(Type::Var(var).pretty(), "'a");
    }

    #[test]
    fn test_pretty_print_function() {
        let ty = Type::func(Type::Int, Type::String);
        assert_eq!(ty.pretty(), "Int -> String");
    }

    #[test]
    fn test_pretty_print_nested_function() {
        let ty = Type::func(Type::func(Type::Int, Type::Int), Type::String);
        assert_eq!(ty.pretty(), "(Int -> Int) -> String");
    }
}
