use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub name: Option<String>,
}

impl TypeVar {
    pub fn new(id: usize) -> Self {
        Self { id, name: None }
    }

    pub fn with_name(id: usize, name: String) -> Self {
        Self {
            id,
            name: Some(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Unit,
    Bool,
    Var(TypeVar),
    Func(Box<Type>, Box<Type>),
}

impl Type {
    pub fn func(t1: Type, t2: Type) -> Self {
        Type::Func(Box::new(t1), Box::new(t2))
    }

    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Int | Type::String | Type::Unit | Type::Bool => HashSet::new(),
            Type::Var(v) => {
                let mut set = HashSet::new();
                set.insert(v.clone());
                set
            }
            Type::Func(t1, t2) => {
                let mut set = t1.free_type_vars();
                set.extend(t2.free_type_vars());
                set
            }
        }
    }

    pub fn pretty(&self) -> String {
        match self {
            Type::Int => "Int".to_string(),
            Type::String => "String".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Bool => "Bool".to_string(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl TypeScheme {
    pub fn monomorphic(ty: Type) -> Self {
        TypeScheme {
            vars: Vec::new(),
            ty,
        }
    }

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
