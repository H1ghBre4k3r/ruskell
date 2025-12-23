//! # Type Environment for Type Inference
//!
//! This module implements the type environment used in Hindley-Milner
//! type inference to store type schemes for named variables and functions.
//!
//! ## Overview
//!
//! The type environment (`TypeEnv`) maps names to type schemes:
//!
//! - Stores type information for all variables in scope
//! - Supports lexical scoping through parent environments
//! - Tracks free type variables for polymorphism
//! - Supports substitution application during unification
//!
//! ## Design
//!
//! `TypeEnv` is designed as a persistent data structure (functional map):
//!
//! - **Immutability**: Operations return new environments, never modify in-place
//! - **Lexical scoping**: Child environments can shadow parent bindings
//! - **Efficiency**: Uses hash maps for O(1) lookups
//!
//! ## Type Schemes in Environment
//!
//! The environment stores `TypeScheme` values (not raw `Type`):
//!
//! - **Polymorphic values**: Type schemes with quantified variables
//! - **Monomorphic values**: Type schemes with empty variable list
//!
//! When looking up a variable, we **instantiate** its type scheme
//! with fresh type variables, enabling polymorphism:
//!
//! ```text
//! // Environment contains:
//! // id: forall 'a. 'a -> 'a
//!
//! // First use:
//! // instantiate 'a -> 'a with 't0 -> 't0
//! // id(42) : Int -> Int (after unification)
//!
//! // Second use:
//! // instantiate 'a -> 'a with 't1 -> 't1
//! // id("hello") : String -> String (after unification)
//! ```
//!
//! ## Lexical Scoping and Shadowing
//!
//! The environment supports hierarchical scoping through parent links:
//!
//! ```text
//! // Outer environment:
//! // { x: Int, y: String }
//!
//! // Inner environment (with parent):
//! // { x: Bool }  // Shadows outer x
//!
//! // Lookup in inner:
//! // x -> Bool (from inner, shadows outer)
//! // y -> String (from parent)
//! ```
//!
//! ## Free Type Variables
//!
//! The environment tracks free type variables to determine
//! which variables can be generalized in let-polymorphism:
//!
//! - A variable is **free** in an environment if it appears
//!   in any binding's type scheme but is NOT quantified
//! - When generalizing a type, we only quantify over variables
//!   that are free in the type but NOT free in the environment
//!
//! This prevents **value restriction**: top-level let bindings that
//! reference themselves or external variables cannot be fully polymorphic.
//!
//! ## Related Modules
//!
//! - [`crate::types::ty`] - Type and TypeScheme definitions
//! - [`crate::types::infer`] - Type inference using type environments
//! - [`crate::types::subst`] - Substitution application to environments

use std::collections::{HashMap, HashSet};

use super::subst::Substitution;
use super::ty::{TypeScheme, TypeVar};

/// Type environment mapping names to type schemes.
///
/// The type environment stores type information for all variables
/// and functions in scope during type inference. It supports
/// lexical scoping through parent environment links.
///
/// # Design
///
/// - **Persistent**: Operations return new environments (never modify in-place)
/// - **Lexical**: Child environments can look up bindings in parents
/// - **Shadowing**: Inner bindings can shadow outer ones with same name
///
/// # Fields
///
/// * `bindings` - Map from name to type scheme in this environment
/// * `parent` - Optional parent environment for lexical scoping
///
/// # Example
///
/// ```text
/// // Create empty environment:
/// let env = TypeEnv::empty();
///
/// // Extend with variable:
/// let env1 = env.extend("x".to_string(), TypeScheme::monomorphic(Int));
///
/// // Create child with parent:
/// let env2 = TypeEnv::with_parent(env1);
///
/// // Shadow x in child:
/// let env3 = env2.extend("x".to_string(), TypeScheme::monomorphic(String));
/// // Lookup x in env3 -> String (shadows parent)
/// // Lookup x in env2 -> Int (from parent)
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv {
    bindings: HashMap<String, TypeScheme>,
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    /// Create a new empty type environment.
    ///
    /// # Returns
    ///
    /// A new `TypeEnv` with no bindings and no parent
    ///
    /// # Example
    ///
    /// ```text
    /// let env = TypeEnv::empty();
    /// // env contains no variables
    /// assert!(env.lookup("x").is_none());
    /// ```
    pub fn empty() -> Self {
        TypeEnv {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new environment with initial bindings.
    ///
    /// # Arguments
    ///
    /// * `bindings` - List of (name, type_scheme) pairs to initialize with
    ///
    /// # Returns
    ///
    /// A new `TypeEnv` with the specified bindings and no parent
    ///
    /// # Example
    ///
    /// ```text
    /// let env = TypeEnv::with_bindings(vec![
    ///     ("x".to_string(), TypeScheme::monomorphic(Int)),
    ///     ("y".to_string(), TypeScheme::monomorphic(String)),
    /// ]);
    /// // env contains x: Int and y: String
    /// ```
    pub fn with_bindings(bindings: Vec<(String, TypeScheme)>) -> Self {
        TypeEnv {
            bindings: bindings.into_iter().collect(),
            parent: None,
        }
    }

    /// Look up a name in this environment and all parents.
    ///
    /// Searches this environment first, then recursively searches
    /// parent environments until binding is found or root is reached.
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier name to look up
    ///
    /// # Returns
    ///
    /// `Some(&TypeScheme)` if name is found in this or any parent environment
    /// `None` if name is not bound
    ///
    /// # Example
    ///
    /// ```text
    /// let outer = TypeEnv::with_bindings(vec![
    ///     ("x".to_string(), TypeScheme::monomorphic(Int)),
    /// ]);
    /// let inner = TypeEnv::with_parent(outer);
    ///
    /// // Find x in inner (comes from parent):
    /// assert!(inner.lookup("x").is_some());
    ///
    /// // y is not bound anywhere:
    /// assert!(inner.lookup("y").is_none());
    /// ```
    pub fn lookup(&self, name: &str) -> Option<&TypeScheme> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }

    /// Extend this environment with a new binding.
    ///
    /// Returns a new environment with the new binding added.
    /// If `name` already exists, it's shadowed (the existing binding
    /// is hidden but not modified).
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier name to bind
    /// * `scheme` - The type scheme to bind to `name`
    ///
    /// # Returns
    ///
    /// A new environment with the binding added
    ///
    /// # Example
    ///
    /// ```text
    /// let env1 = TypeEnv::empty()
    ///     .extend("x".to_string(), TypeScheme::monomorphic(Int));
    ///
    /// // Shadow x:
    /// let env2 = env1.extend("x".to_string(), TypeScheme::monomorphic(String));
    ///
    /// // env1 still has x: Int
    /// assert!(matches!(env1.lookup("x").unwrap().ty, Int));
    ///
    /// // env2 has x: String
    /// assert!(matches!(env2.lookup("x").unwrap().ty, String));
    /// ```
    pub fn extend(&self, name: String, scheme: TypeScheme) -> TypeEnv {
        let mut new_bindings = self.bindings.clone();
        new_bindings.insert(name, scheme);
        TypeEnv {
            bindings: new_bindings,
            parent: self.parent.clone(),
        }
    }

    /// Extend this environment with multiple bindings.
    ///
    /// Similar to `extend()`, but accepts a list of bindings for efficiency.
    ///
    /// # Arguments
    ///
    /// * `bindings` - List of (name, type_scheme) pairs to add
    ///
    /// # Returns
    ///
    /// A new environment with all bindings added
    ///
    /// # Example
    ///
    /// ```text
    /// let env = TypeEnv::empty().extend_many(vec![
    ///     ("x".to_string(), TypeScheme::monomorphic(Int)),
    ///     ("y".to_string(), TypeScheme::monomorphic(String)),
    /// ]);
    /// ```
    #[allow(dead_code)]
    pub fn extend_many(&self, bindings: Vec<(String, TypeScheme)>) -> TypeEnv {
        let mut new_bindings = self.bindings.clone();
        for (name, scheme) in bindings {
            new_bindings.insert(name, scheme);
        }
        TypeEnv {
            bindings: new_bindings,
            parent: self.parent.clone(),
        }
    }

    /// Create a new environment with this one as parent.
    ///
    /// Creates an empty environment that can look up bindings in
    /// the parent environment. Used to create new lexical scopes.
    ///
    /// # Arguments
    ///
    /// * `parent` - The parent environment for lexical scoping
    ///
    /// # Returns
    ///
    /// A new empty environment with the specified parent
    ///
    /// # Example
    ///
    /// ```text
    /// let outer = TypeEnv::with_bindings(vec![
    ///     ("x".to_string(), TypeScheme::monomorphic(Int)),
    /// ]);
    ///
    /// // Create inner scope:
    /// let inner = TypeEnv::with_parent(outer);
    ///
    /// // Inner can see outer's bindings:
    /// assert!(inner.lookup("x").is_some());
    /// ```
    #[allow(dead_code)]
    pub fn with_parent(parent: TypeEnv) -> Self {
        TypeEnv {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Get set of free type variables in this environment.
    ///
    /// Free type variables are those that appear in any binding's type
    /// but are NOT quantified by that binding's type scheme.
    ///
    /// # Returns
    ///
    /// A `HashSet` of all free type variables in this environment
    /// and all parent environments
    ///
    /// # Purpose
    ///
    /// Free variables in the environment determine what variables can
    /// be generalized in let-polymorphism. When defining a variable with:
    ///
    /// ```text
    /// let x = expr
    /// ```
    ///
    /// We quantify over variables that are free in `expr.type` but NOT
    /// free in the environment. This prevents generalizing variables
    /// that are constrained by the context.
    ///
    /// # Example
    ///
    /// ```text
    /// // Monomorphic binding (no quantification):
    /// // x: 'a
    /// // Free vars: {'a'}
    /// env.extend("x", TypeScheme { vars: [], ty: Var('a') })
    /// env.free_type_vars()  // {'a'}
    ///
    /// // Polymorphic binding (quantified):
    /// // id: forall 'a. 'a -> 'a
    /// // Free vars: {} (none - 'a is quantified)
    /// let var_a = TypeVar::new(0);
    /// env.extend("id", TypeScheme {
    ///     vars: vec![var_a.clone()],
    ///     ty: Func(Var(var_a.clone()), Var(var_a)),
    /// })
    /// env.free_type_vars()  // {} (no free vars)
    ///
    /// // With parent:
    /// // Parent has: y: 'b
    /// let var_b = TypeVar::new(1);
    /// let parent = TypeEnv::with_bindings(vec![
    ///     ("y".to_string(), TypeScheme { vars: [], ty: Var(var_b.clone()) }),
    /// ]);
    /// let child = TypeEnv::with_parent(parent);
    /// child.free_type_vars()  // {'b'} (from parent)
    /// ```
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut free = HashSet::new();
        for scheme in self.bindings.values() {
            // Variables appearing in type but not in vars list are free
            let ty_free = scheme.ty.free_type_vars();
            for var in ty_free {
                if !scheme.vars.contains(&var) {
                    free.insert(var);
                }
            }
        }
        if let Some(parent) = &self.parent {
            free.extend(parent.free_type_vars());
        }
        free
    }

    /// Apply a substitution to all types in this environment.
    ///
    /// Returns a new environment with the substitution applied to
    /// all type schemes in this environment and all parents.
    ///
    /// # Arguments
    ///
    /// * `subst` - The substitution to apply
    ///
    /// # Returns
    ///
    /// A new environment with substitution applied
    ///
    /// # Purpose
    ///
    /// During type inference, we discover substitutions that unify
    /// types (e.g., `'a := Int`). We apply these substitutions
    /// to environments to update stored type information.
    ///
    /// # Example
    ///
    /// ```text
    /// // Environment contains:
    /// // x: 'a
    ///
    /// // After unification, we know 'a := Int:
    /// let subst = Substitution::singleton(var_a, Type::Int);
    /// let new_env = env.apply_subst(&subst);
    ///
    /// // new_env contains:
    /// // x: Int (not 'a anymore)
    /// ```
    pub fn apply_subst(&self, subst: &Substitution) -> TypeEnv {
        let bindings = self
            .bindings
            .iter()
            .map(|(name, scheme)| {
                let ty = subst.apply(&scheme.ty);
                (
                    name.clone(),
                    TypeScheme {
                        vars: scheme.vars.clone(),
                        ty,
                    },
                )
            })
            .collect();

        TypeEnv {
            bindings,
            parent: self.parent.as_ref().map(|p| Box::new(p.apply_subst(subst))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::ty::Type;

    #[test]
    fn test_empty_env() {
        let env = TypeEnv::empty();
        assert!(env.lookup("x").is_none());
    }

    #[test]
    fn test_with_bindings() {
        let env =
            TypeEnv::with_bindings(vec![("x".to_string(), TypeScheme::monomorphic(Type::Int))]);
        assert!(env.lookup("x").is_some());
        assert_eq!(env.lookup("x").unwrap().ty, Type::Int);
    }

    #[test]
    fn test_extend() {
        let env = TypeEnv::empty();
        let env = env.extend("x".to_string(), TypeScheme::monomorphic(Type::Int));
        assert!(env.lookup("x").is_some());
    }

    #[test]
    fn test_extend_shadows() {
        let env = TypeEnv::empty();
        let env = env.extend("x".to_string(), TypeScheme::monomorphic(Type::Int));
        let env = env.extend("x".to_string(), TypeScheme::monomorphic(Type::String));
        assert_eq!(env.lookup("x").unwrap().ty, Type::String);
    }

    #[test]
    fn test_parent_lookup() {
        let parent = TypeEnv::empty();
        let parent = parent.extend("x".to_string(), TypeScheme::monomorphic(Type::Int));
        let child = TypeEnv::with_parent(parent);
        assert!(child.lookup("x").is_some());
        assert_eq!(child.lookup("x").unwrap().ty, Type::Int);
    }

    #[test]
    fn test_child_shadows_parent() {
        let parent = TypeEnv::empty();
        let parent = parent.extend("x".to_string(), TypeScheme::monomorphic(Type::Int));
        let child = TypeEnv::with_parent(parent);
        let child = child.extend("x".to_string(), TypeScheme::monomorphic(Type::String));
        assert_eq!(child.lookup("x").unwrap().ty, Type::String);
    }

    #[test]
    fn test_free_type_vars_monomorphic() {
        let var = TypeVar::new(0);
        let env = TypeEnv::with_bindings(vec![(
            "x".to_string(),
            TypeScheme::monomorphic(Type::Var(var.clone())),
        )]);
        let free = env.free_type_vars();
        assert_eq!(free.len(), 1);
        assert!(free.contains(&var));
    }

    #[test]
    fn test_free_type_vars_polymorphic() {
        let var = TypeVar::new(0);
        let env = TypeEnv::with_bindings(vec![(
            "x".to_string(),
            TypeScheme::polymorphic(vec![var.clone()], Type::Var(var)),
        )]);
        let free = env.free_type_vars();
        assert!(free.is_empty());
    }
}
