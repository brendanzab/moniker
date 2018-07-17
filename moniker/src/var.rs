use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;

/// A generated id
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GenId(u32);

impl GenId {
    /// Generate a new, globally unique id
    pub fn fresh() -> GenId {
        use std::sync::atomic::{AtomicUsize, Ordering};

        lazy_static! {
            static ref NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        }

        // FIXME: check for integer overflow
        GenId(NEXT_ID.fetch_add(1, Ordering::SeqCst) as u32)
    }
}

impl fmt::Display for GenId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// A free variable
#[derive(Debug, Clone)]
pub enum FreeVar<Ident> {
    /// Names originating from user input
    User(Ident),
    /// A generated id with an optional string that may have come from user
    /// input (for debugging purposes)
    Gen(GenId, Option<Ident>),
}

impl<Ident> FreeVar<Ident> {
    /// Create a name from a human-readable string
    pub fn user<T: Into<Ident>>(ident: T) -> FreeVar<Ident> {
        FreeVar::User(ident.into())
    }

    pub fn ident(&self) -> Option<&Ident> {
        match *self {
            FreeVar::User(ref name) => Some(name),
            FreeVar::Gen(_, ref hint) => hint.as_ref(),
        }
    }
}

impl<Ident> From<GenId> for FreeVar<Ident> {
    fn from(src: GenId) -> FreeVar<Ident> {
        FreeVar::Gen(src, None)
    }
}

impl<Ident> PartialEq for FreeVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &FreeVar<Ident>) -> bool {
        match (self, other) {
            (&FreeVar::User(ref lhs), &FreeVar::User(ref rhs)) => lhs == rhs,
            (&FreeVar::Gen(lhs, _), &FreeVar::Gen(rhs, _)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Ident> Eq for FreeVar<Ident> where Ident: Eq {}

impl<Ident> Hash for FreeVar<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            FreeVar::User(ref name) => name.hash(state),
            FreeVar::Gen(id, _) => id.hash(state),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for FreeVar<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FreeVar::User(ref name) => write!(f, "{}", name),
            FreeVar::Gen(ref gen_id, ref name_hint) => match *name_hint {
                None => write!(f, "{}", gen_id),
                Some(ref name) => write!(f, "{}{}", name, gen_id),
            },
        }
    }
}

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
pub struct PatternIndex(pub u32);

impl fmt::Display for PatternIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// A bound variable
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BoundVar {
    pub scope: ScopeOffset,
    pub pattern: PatternIndex,
}

impl fmt::Display for BoundVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.scope, self.pattern)
    }
}

/// A variable that can either be free or bound
#[derive(Debug, Clone)]
pub enum Var<Ident> {
    /// A free variable
    Free(FreeVar<Ident>),
    /// A variable that is bound by a lambda or pi binder
    Bound(BoundVar, Option<Ident>),
}

impl<Ident> Var<Ident> {
    pub fn user<T: Into<Ident>>(ident: T) -> Var<Ident> {
        Var::Free(FreeVar::user(ident))
    }
}

impl<Ident> PartialEq for Var<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &Var<Ident>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => lhs == rhs,
            (&Var::Bound(bound_lhs, _), &Var::Bound(bound_rhs, _)) => bound_lhs == bound_rhs,
            _ => false,
        }
    }
}

impl<Ident> Eq for Var<Ident> where Ident: Eq {}

impl<Ident> Hash for Var<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Var::Free(ref name) => name.hash(state),
            Var::Bound(bound_var, _) => {
                bound_var.hash(state);
            },
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for Var<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(bound_var, None) => write!(f, "@{}", bound_var),
            Var::Bound(bound_var, Some(ref hint)) => write!(f, "{}@{}", hint, bound_var),
            Var::Free(ref free) => write!(f, "{}", free),
        }
    }
}
