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
pub enum FreeVar<N> {
    /// Names originating from user input
    User(N),
    /// A generated id with an optional string that may have come from user
    /// input (for debugging purposes)
    Gen(GenId, Option<N>),
}

impl<N> FreeVar<N> {
    /// Create a name from a human-readable string
    pub fn user<T: Into<N>>(ident: T) -> FreeVar<N> {
        FreeVar::User(ident.into())
    }

    pub fn freshen(self) -> FreeVar<N> {
        match self {
            FreeVar::User(name) => FreeVar::Gen(GenId::fresh(), Some(name)),
            FreeVar::Gen(_, _) => self,
        }
    }

    pub fn ident(&self) -> Option<&N> {
        match *self {
            FreeVar::User(ref name) => Some(name),
            FreeVar::Gen(_, ref hint) => hint.as_ref(),
        }
    }
}

impl<N> From<GenId> for FreeVar<N> {
    fn from(src: GenId) -> FreeVar<N> {
        FreeVar::Gen(src, None)
    }
}

impl<N> PartialEq for FreeVar<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &FreeVar<N>) -> bool {
        match (self, other) {
            (&FreeVar::User(ref lhs), &FreeVar::User(ref rhs)) => lhs == rhs,
            (&FreeVar::Gen(lhs, _), &FreeVar::Gen(rhs, _)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<N> Eq for FreeVar<N> where N: Eq {}

impl<N> Hash for FreeVar<N>
where
    N: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            FreeVar::User(ref name) => name.hash(state),
            FreeVar::Gen(id, _) => id.hash(state),
        }
    }
}

impl<N: fmt::Display> fmt::Display for FreeVar<N> {
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
