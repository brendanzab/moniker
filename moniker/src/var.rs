use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;

use binder::{Binder, BinderIndex};
use free_var::FreeVar;

/// The [Debruijn index] of the binder that introduced the variable
///
/// For example:
///
/// ```text
/// λx.∀y.λz. x z (y z)
/// λ  ∀  λ   2 0 (1 0)
/// ```
///
/// [Debruijn index]: https://en.wikipedia.org/wiki/De_Bruijn_index
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ScopeOffset(pub u32);

impl ScopeOffset {
    /// Move the current Debruijn index into an inner binder
    pub fn succ(self) -> ScopeOffset {
        ScopeOffset(self.0 + 1)
    }

    pub fn pred(self) -> Option<ScopeOffset> {
        match self {
            ScopeOffset(0) => None,
            ScopeOffset(i) => Some(ScopeOffset(i - 1)),
        }
    }
}

impl fmt::Display for ScopeOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// A variable that can either be free or bound
#[derive(Debug, Clone)]
pub enum Var<N> {
    /// A free variable
    Free(FreeVar<N>),
    /// A variable that is bound by a lambda or pi binder
    Bound(ScopeOffset, BinderIndex, Option<N>),
}

impl<N> Var<N> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<N>>(ident: T) -> Var<N> {
        Var::Free(FreeVar::user(ident))
    }
}

impl<N> PartialEq for Var<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &Var<N>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => lhs == rhs,
            (
                &Var::Bound(scope_offset_lhs, binder_index_lhs, _),
                &Var::Bound(scope_offset_rhs, binder_index_rhs, _),
            ) => scope_offset_lhs == scope_offset_rhs && binder_index_lhs == binder_index_rhs,
            _ => false,
        }
    }
}

impl<N> Eq for Var<N> where N: Eq {}

impl<N> Hash for Var<N>
where
    N: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Var::Free(ref free_var) => free_var.hash(state),
            Var::Bound(scope, pattern, _) => {
                scope.hash(state);
                pattern.hash(state);
            },
        }
    }
}

impl<N: fmt::Display> fmt::Display for Var<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(scope_offset, binder_index, None) => {
                write!(f, "@{}.{}", scope_offset, binder_index)
            },
            Var::Bound(scope_offset, binder_index, Some(ref hint)) => {
                write!(f, "{}@{}.{}", hint, scope_offset, binder_index)
            },
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
