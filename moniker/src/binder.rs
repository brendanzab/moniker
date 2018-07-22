use std::fmt;
use std::ops;

use free_var::FreeVar;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BinderOffset(pub u32);

impl BinderOffset {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl ops::Add for BinderOffset {
    type Output = BinderOffset;

    fn add(self, other: BinderOffset) -> BinderOffset {
        BinderOffset(self.0 + other.0)
    }
}

impl ops::AddAssign for BinderOffset {
    fn add_assign(&mut self, other: BinderOffset) {
        self.0 += other.0;
    }
}

impl ops::Sub for BinderOffset {
    type Output = BinderOffset;

    fn sub(self, other: BinderOffset) -> BinderOffset {
        BinderOffset(self.0 - other.0)
    }
}

impl ops::SubAssign for BinderOffset {
    fn sub_assign(&mut self, other: BinderOffset) {
        self.0 -= other.0;
    }
}

impl fmt::Display for BinderOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BinderIndex(pub BinderOffset);

impl BinderIndex {
    pub fn to_usize(self) -> usize {
        self.0.to_usize()
    }
}

impl ops::Add<BinderOffset> for BinderIndex {
    type Output = BinderIndex;

    fn add(self, other: BinderOffset) -> BinderIndex {
        BinderIndex(self.0 + other)
    }
}

impl ops::AddAssign<BinderOffset> for BinderIndex {
    fn add_assign(&mut self, other: BinderOffset) {
        self.0 += other;
    }
}

impl fmt::Display for BinderIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binder<N>(pub FreeVar<N>);

impl<N> Binder<N> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<N>>(ident: T) -> Binder<N> {
        Binder(FreeVar::user(ident))
    }

    pub fn fresh(self) -> Binder<N> {
        Binder(self.0.fresh())
    }
}

impl<N: fmt::Display> fmt::Display for Binder<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<N> PartialEq<FreeVar<N>> for Binder<N>
where
    N: PartialEq,
{
    fn eq(&self, other: &FreeVar<N>) -> bool {
        self.0 == *other
    }
}
