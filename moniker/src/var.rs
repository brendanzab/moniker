use std::fmt;

use binder::Binder;
use bound_var::BoundVar;
use free_var::FreeVar;

/// A variable that can either be free or bound
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Var<N> {
    /// A free variable
    Free(FreeVar<N>),
    /// A variable that is bound by a scope
    Bound(BoundVar<N>),
}

impl<N> Var<N> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<N>>(ident: T) -> Var<N> {
        Var::Free(FreeVar::user(ident))
    }
}

impl<N> Eq for Var<N> where N: Eq {}

impl<N: fmt::Display> fmt::Display for Var<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(ref bound_var) => write!(f, "{}", bound_var),
            Var::Free(ref free_var) => write!(f, "{}", free_var),
        }
    }
}

impl<N> PartialEq<Binder<N>> for Var<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &Binder<N>) -> bool {
        match self {
            Var::Free(ref lhs) => other == lhs,
            _ => false,
        }
    }
}

impl<N> PartialEq<FreeVar<N>> for Var<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &FreeVar<N>) -> bool {
        match *self {
            Var::Free(ref lhs) => lhs == other,
            _ => false,
        }
    }
}

impl<N> PartialEq<Var<N>> for FreeVar<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &Var<N>) -> bool {
        match *other {
            Var::Free(ref lhs) => lhs == self,
            _ => false,
        }
    }
}

impl<N> PartialEq<Var<N>> for Binder<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &Var<N>) -> bool {
        match other {
            Var::Free(ref rhs) => self == rhs,
            _ => false,
        }
    }
}
