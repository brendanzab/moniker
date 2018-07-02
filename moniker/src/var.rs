use std::fmt;

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct DebruijnIndex(pub u32);

impl DebruijnIndex {
    /// Move the current Debruijn index into an inner binder
    pub fn succ(self) -> DebruijnIndex {
        DebruijnIndex(self.0 + 1)
    }

    pub fn pred(self) -> Option<DebruijnIndex> {
        match self {
            DebruijnIndex(0) => None,
            DebruijnIndex(i) => Some(DebruijnIndex(i - 1)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct PatternIndex(pub u32);

/// A bound variable
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct BoundVar {
    pub scope: DebruijnIndex,
    pub pattern: PatternIndex,
}

impl fmt::Display for BoundVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.scope.0, self.pattern.0)
    }
}

/// A variable that can either be free or bound
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Var<Ident> {
    /// A free variable
    Free(FreeVar<Ident>),
    /// A variable that is bound by a lambda or pi binder
    Bound(BoundVar, Option<Ident>),
}

impl<Ident: fmt::Display> fmt::Display for Var<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(bound, None) => write!(f, "@{}", bound),
            Var::Bound(bound, Some(ref hint)) => write!(f, "{}@{}", hint, bound),
            Var::Free(ref free) => write!(f, "{}", free),
        }
    }
}
