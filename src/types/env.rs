use std::collections::{HashMap, HashSet};

use super::subst::Substitution;
use super::ty::{TypeScheme, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv {
    bindings: HashMap<String, TypeScheme>,
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    pub fn empty() -> Self {
        TypeEnv {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_bindings(bindings: Vec<(String, TypeScheme)>) -> Self {
        TypeEnv {
            bindings: bindings.into_iter().collect(),
            parent: None,
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&TypeScheme> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }

    pub fn extend(&self, name: String, scheme: TypeScheme) -> TypeEnv {
        let mut new_bindings = self.bindings.clone();
        new_bindings.insert(name, scheme);
        TypeEnv {
            bindings: new_bindings,
            parent: self.parent.clone(),
        }
    }

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

    pub fn with_parent(parent: TypeEnv) -> Self {
        TypeEnv {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut free = HashSet::new();
        for scheme in self.bindings.values() {
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
    use crate::types::Type;

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
