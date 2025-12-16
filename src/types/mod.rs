pub mod env;
pub mod infer;
pub mod subst;
pub mod ty;
pub mod unify;

pub use env::TypeEnv;
pub use infer::{Infer, TypeError};
pub use subst::Substitution;
pub use ty::{Type, TypeScheme, TypeVar};
pub use unify::{UnifyError, unify};
