use std::fmt;
use std::hash::{Hash, Hasher};

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BinderIndex(pub u32);

impl BinderIndex {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for BinderIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone)]
pub struct BoundVar<N> {
    pub scope: ScopeOffset,
    pub binder: BinderIndex,
    pub pretty_name: Option<N>,
}

impl<N> PartialEq for BoundVar<N> {
    fn eq(&self, other: &BoundVar<N>) -> bool {
        self.scope == other.scope && self.binder == other.binder
    }
}

impl<N> Eq for BoundVar<N> {}

impl<N> Hash for BoundVar<N>
where
    N: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.scope.hash(state);
        self.binder.hash(state);
    }
}

impl<N: fmt::Display> fmt::Display for BoundVar<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.pretty_name {
            None => write!(f, "@{}.{}", self.scope, self.binder),
            Some(ref pretty_name) => write!(f, "{}@{}.{}", pretty_name, self.scope, self.binder),
        }
    }
}
