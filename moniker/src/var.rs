use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops;

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

    pub fn fresh(self) -> FreeVar<Ident> {
        match self {
            FreeVar::User(name) => FreeVar::Gen(GenId::fresh(), Some(name)),
            FreeVar::Gen(_, _) => self,
        }
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

/// A variable that can either be free or bound
#[derive(Debug, Clone)]
pub enum Var<Ident> {
    /// A free variable
    Free(FreeVar<Ident>),
    /// A variable that is bound by a lambda or pi binder
    Bound(ScopeOffset, BinderIndex, Option<Ident>),
}

impl<Ident> Var<Ident> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<Ident>>(ident: T) -> Var<Ident> {
        Var::Free(FreeVar::user(ident))
    }

    pub fn try_into_free_var(self) -> Result<FreeVar<Ident>, ()> {
        match self {
            Var::Free(name) => Ok(name),
            Var::Bound(_, _, _) => Err(()),
        }
    }
}

impl<Ident> PartialEq for Var<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &Var<Ident>) -> bool {
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

impl<Ident> Eq for Var<Ident> where Ident: Eq {}

impl<Ident> Hash for Var<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Var::Free(ref name) => name.hash(state),
            Var::Bound(scope, pattern, _) => {
                scope.hash(state);
                pattern.hash(state);
            },
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for Var<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(scope_offset, binder_index, None) => {
                write!(f, "@{}.{}", scope_offset, binder_index)
            },
            Var::Bound(scope_offset, binder_index, Some(ref hint)) => {
                write!(f, "{}@{}.{}", hint, scope_offset, binder_index)
            },
            Var::Free(ref free) => write!(f, "{}", free),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Binder<Ident> {
    Free(FreeVar<Ident>),
    Bound(BinderIndex, Option<Ident>),
}

impl<Ident> Binder<Ident> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<Ident>>(ident: T) -> Binder<Ident> {
        Binder::Free(FreeVar::user(ident))
    }

    pub fn fresh(self) -> Binder<Ident> {
        match self {
            Binder::Free(free_var) => Binder::Free(free_var.fresh()),
            Binder::Bound(_, _) => self,
        }
    }

    pub fn to_var(self, scope: ScopeOffset) -> Var<Ident> {
        match self {
            Binder::Free(name) => Var::Free(name),
            Binder::Bound(pattern, name) => Var::Bound(scope, pattern, name),
        }
    }

    pub fn try_into_free_var(self) -> Result<FreeVar<Ident>, ()> {
        match self {
            Binder::Free(name) => Ok(name),
            Binder::Bound(_, _) => Err(()),
        }
    }
}

impl<Ident> PartialEq for Binder<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &Binder<Ident>) -> bool {
        match (self, other) {
            (&Binder::Free(ref lhs), &Binder::Free(ref rhs)) => lhs == rhs,
            (&Binder::Bound(binder_index_lhs, _), &Binder::Bound(binder_index_rhs, _)) => {
                binder_index_lhs == binder_index_rhs
            },
            _ => false,
        }
    }
}

impl<Ident> Eq for Binder<Ident> where Ident: Eq {}

impl<Ident> Hash for Binder<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Binder::Free(ref name) => name.hash(state),
            Binder::Bound(pattern, _) => pattern.hash(state),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for Binder<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Binder::Bound(binder_index, None) => write!(f, "@{}", binder_index),
            Binder::Bound(binder_index, Some(ref hint)) => write!(f, "{}@{}", hint, binder_index),
            Binder::Free(ref free) => write!(f, "{}", free),
        }
    }
}

impl<Ident> PartialEq<Binder<Ident>> for Var<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &Binder<Ident>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Binder::Free(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<FreeVar<Ident>> for Var<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &FreeVar<Ident>) -> bool {
        match *self {
            Var::Free(ref lhs) => lhs == other,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<Var<Ident>> for Binder<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &Var<Ident>) -> bool {
        match (self, other) {
            (&Binder::Free(ref lhs), &Var::Free(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<FreeVar<Ident>> for Binder<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &FreeVar<Ident>) -> bool {
        match *self {
            Binder::Free(ref lhs) => lhs == other,
            _ => false,
        }
    }
}
