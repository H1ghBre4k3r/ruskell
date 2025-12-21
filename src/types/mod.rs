pub mod env;
pub mod infer;
pub mod subst;
pub mod ty;
pub mod unify;
pub mod validate;

pub use env::TypeEnv;
pub use infer::{Infer, TypeError};
pub use subst::Substitution;
pub use ty::{Type, TypeScheme, TypeVar};
pub use unify::{UnifyError, unify};
pub use validate::{ValidationError, validate_and_type_check};
