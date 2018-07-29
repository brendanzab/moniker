use std::fmt;
use std::hash::{Hash, Hasher};

use unique_id::UniqueId;

/// A free variable
#[derive(Debug, Clone)]
pub struct FreeVar<N> {
    /// A generated id
    pub unique_id: UniqueId,
    /// programmer-provided name for pretty-printing
    pub pretty_name: Option<N>,
}

impl<N> FreeVar<N> {
    pub fn fresh(pretty_name: Option<N>) -> FreeVar<N> {
        FreeVar {
            unique_id: UniqueId::new(),
            pretty_name,
        }
    }
}

impl<N> PartialEq for FreeVar<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &FreeVar<N>) -> bool {
        self.unique_id == other.unique_id
    }
}

impl<N> Eq for FreeVar<N> where N: Eq {}

impl<N> Hash for FreeVar<N>
where
    N: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.unique_id.hash(state);
    }
}

impl<N: fmt::Display> fmt::Display for FreeVar<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.pretty_name {
            None => write!(f, "${}", self.unique_id),
            Some(ref pretty_name) => write!(f, "{}${}", pretty_name, self.unique_id),
        }
    }
}
