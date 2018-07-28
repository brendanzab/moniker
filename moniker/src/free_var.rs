use std::fmt;
use std::hash::{Hash, Hasher};

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
pub struct FreeVar<N> {
    /// A generated id
    pub id: GenId,
    /// programmer-provided name for pretty-printing
    pub pretty_name: Option<N>,
}

impl<N> FreeVar<N> {
    pub fn fresh(pretty_name: Option<N>) -> FreeVar<N> {
        FreeVar {
            id: GenId::fresh(),
            pretty_name,
        }
    }
}

impl<N> PartialEq for FreeVar<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &FreeVar<N>) -> bool {
        self.id == other.id
    }
}

impl<N> Eq for FreeVar<N> where N: Eq {}

impl<N> Hash for FreeVar<N>
where
    N: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<N: fmt::Display> fmt::Display for FreeVar<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.pretty_name {
            None => write!(f, "{}", self.id),
            Some(ref pretty_name) => write!(f, "{}{}", pretty_name, self.id),
        }
    }
}
