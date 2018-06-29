use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl<'a> From<&'a str> for Ident {
    fn from(src: &'a str) -> Ident {
        Ident(String::from(src))
    }
}

impl From<String> for Ident {
    fn from(src: String) -> Ident {
        Ident(src)
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        self.0 == *other
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
pub enum FreeVar {
    /// Names originating from user input
    User(Ident),
    /// A generated id with an optional string that may have come from user
    /// input (for debugging purposes)
    Gen(GenId, Option<Ident>),
}

impl FreeVar {
    /// Create a name from a human-readable string
    pub fn user<S: Into<Ident>>(name: S) -> FreeVar {
        FreeVar::User(name.into())
    }

    pub fn ident(&self) -> Option<&Ident> {
        match *self {
            FreeVar::User(ref name) => Some(name),
            FreeVar::Gen(_, ref hint) => hint.as_ref(),
        }
    }
}

impl From<GenId> for FreeVar {
    fn from(src: GenId) -> FreeVar {
        FreeVar::Gen(src, None)
    }
}

impl PartialEq<str> for FreeVar {
    fn eq(&self, other: &str) -> bool {
        match *self {
            FreeVar::User(ref name) => name == other,
            FreeVar::Gen(_, _) => false,
        }
    }
}

impl PartialEq<String> for FreeVar {
    fn eq(&self, other: &String) -> bool {
        match *self {
            FreeVar::User(ref name) => name == other,
            FreeVar::Gen(_, _) => false,
        }
    }
}

impl fmt::Display for FreeVar {
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
pub enum Var {
    /// A free variable
    Free(FreeVar),
    /// A variable that is bound by a lambda or pi binder
    Bound(BoundVar, Option<Ident>),
}

impl PartialEq<str> for Var {
    fn eq(&self, other: &str) -> bool {
        match *self {
            Var::Free(ref name) => name == other,
            Var::Bound(_, _) => false,
        }
    }
}

impl PartialEq<String> for Var {
    fn eq(&self, other: &String) -> bool {
        match *self {
            Var::Free(ref name) => name == other,
            Var::Bound(_, _) => false,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(bound, None) => write!(f, "@{}", bound),
            Var::Bound(bound, Some(ref hint)) => write!(f, "{}@{}", hint, bound),
            Var::Free(ref free) => write!(f, "{}", free),
        }
    }
}
