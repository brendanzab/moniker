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
pub struct PVarOffset(pub u32);

impl PVarOffset {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl ops::Add for PVarOffset {
    type Output = PVarOffset;

    fn add(self, other: PVarOffset) -> PVarOffset {
        PVarOffset(self.0 + other.0)
    }
}

impl ops::AddAssign for PVarOffset {
    fn add_assign(&mut self, other: PVarOffset) {
        self.0 += other.0;
    }
}

impl ops::Sub for PVarOffset {
    type Output = PVarOffset;

    fn sub(self, other: PVarOffset) -> PVarOffset {
        PVarOffset(self.0 - other.0)
    }
}

impl ops::SubAssign for PVarOffset {
    fn sub_assign(&mut self, other: PVarOffset) {
        self.0 -= other.0;
    }
}

impl fmt::Display for PVarOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PVarIndex(pub PVarOffset);

impl PVarIndex {
    pub fn to_usize(self) -> usize {
        self.0.to_usize()
    }
}

impl ops::Add<PVarOffset> for PVarIndex {
    type Output = PVarIndex;

    fn add(self, other: PVarOffset) -> PVarIndex {
        PVarIndex(self.0 + other)
    }
}

impl ops::AddAssign<PVarOffset> for PVarIndex {
    fn add_assign(&mut self, other: PVarOffset) {
        self.0 += other;
    }
}

impl fmt::Display for PVarIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// A variable that can either be free or bound
#[derive(Debug, Clone)]
pub enum TVar<Ident> {
    /// A free variable
    Free(FreeVar<Ident>),
    /// A variable that is bound by a lambda or pi binder
    Bound(ScopeOffset, PVarIndex, Option<Ident>),
}

impl<Ident> TVar<Ident> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<Ident>>(ident: T) -> TVar<Ident> {
        TVar::Free(FreeVar::user(ident))
    }

    pub fn try_into_free_var(self) -> Result<FreeVar<Ident>, ()> {
        match self {
            TVar::Free(name) => Ok(name),
            TVar::Bound(_, _, _) => Err(()),
        }
    }
}

impl<Ident> PartialEq for TVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &TVar<Ident>) -> bool {
        match (self, other) {
            (&TVar::Free(ref lhs), &TVar::Free(ref rhs)) => lhs == rhs,
            (
                &TVar::Bound(scope_offset_lhs, pvar_index_lhs, _),
                &TVar::Bound(scope_offset_rhs, pvar_index_rhs, _),
            ) => scope_offset_lhs == scope_offset_rhs && pvar_index_lhs == pvar_index_rhs,
            _ => false,
        }
    }
}

impl<Ident> Eq for TVar<Ident> where Ident: Eq {}

impl<Ident> Hash for TVar<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            TVar::Free(ref name) => name.hash(state),
            TVar::Bound(scope, pattern, _) => {
                scope.hash(state);
                pattern.hash(state);
            },
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for TVar<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TVar::Bound(scope_offset, pvar_index, None) => {
                write!(f, "@{}.{}", scope_offset, pvar_index)
            },
            TVar::Bound(scope_offset, pvar_index, Some(ref hint)) => {
                write!(f, "{}@{}.{}", hint, scope_offset, pvar_index)
            },
            TVar::Free(ref free) => write!(f, "{}", free),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PVar<Ident> {
    Free(FreeVar<Ident>),
    Bound(PVarIndex, Option<Ident>),
}

impl<Ident> PVar<Ident> {
    /// Create a variable from a human-readable string
    pub fn user<T: Into<Ident>>(ident: T) -> PVar<Ident> {
        PVar::Free(FreeVar::user(ident))
    }

    pub fn fresh(self) -> PVar<Ident> {
        match self {
            PVar::Free(free_var) => PVar::Free(free_var.fresh()),
            PVar::Bound(_, _) => self,
        }
    }

    pub fn to_var(self, scope: ScopeOffset) -> TVar<Ident> {
        match self {
            PVar::Free(name) => TVar::Free(name),
            PVar::Bound(pattern, name) => TVar::Bound(scope, pattern, name),
        }
    }

    pub fn try_into_free_var(self) -> Result<FreeVar<Ident>, ()> {
        match self {
            PVar::Free(name) => Ok(name),
            PVar::Bound(_, _) => Err(()),
        }
    }
}

impl<Ident> PartialEq for PVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &PVar<Ident>) -> bool {
        match (self, other) {
            (&PVar::Free(ref lhs), &PVar::Free(ref rhs)) => lhs == rhs,
            (&PVar::Bound(pvar_index_lhs, _), &PVar::Bound(pvar_index_rhs, _)) => {
                pvar_index_lhs == pvar_index_rhs
            },
            _ => false,
        }
    }
}

impl<Ident> Eq for PVar<Ident> where Ident: Eq {}

impl<Ident> Hash for PVar<Ident>
where
    Ident: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            PVar::Free(ref name) => name.hash(state),
            PVar::Bound(pattern, _) => pattern.hash(state),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for PVar<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PVar::Bound(pvar_index, None) => write!(f, "@{}", pvar_index),
            PVar::Bound(pvar_index, Some(ref hint)) => write!(f, "{}@{}", hint, pvar_index),
            PVar::Free(ref free) => write!(f, "{}", free),
        }
    }
}

impl<Ident> PartialEq<PVar<Ident>> for TVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &PVar<Ident>) -> bool {
        match (self, other) {
            (&TVar::Free(ref lhs), &PVar::Free(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<FreeVar<Ident>> for TVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &FreeVar<Ident>) -> bool {
        match *self {
            TVar::Free(ref lhs) => lhs == other,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<TVar<Ident>> for PVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &TVar<Ident>) -> bool {
        match (self, other) {
            (&PVar::Free(ref lhs), &TVar::Free(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Ident> PartialEq<FreeVar<Ident>> for PVar<Ident>
where
    Ident: PartialEq,
{
    fn eq(&self, other: &FreeVar<Ident>) -> bool {
        match *self {
            PVar::Free(ref lhs) => lhs == other,
            _ => false,
        }
    }
}
