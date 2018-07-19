use bound::{BoundPattern, Permutations, ScopeState};
use var::{Binder, BinderIndex, BinderOffset, FreeVar};

/// Recursively bind a pattern in itself
///
/// Mutually recursive bindings can be modelled by combining this type with
/// the pattern implementations for `Vec<P>` and `(P1, P2)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rec<P> {
    pub unsafe_pattern: P,
}

impl<P> Rec<P> {
    pub fn new<Ident>(pattern: &P) -> Rec<P>
    where
        P: BoundPattern<Ident> + Clone,
    {
        let mut unsafe_pattern = pattern.clone();
        unsafe_pattern.close_pattern(ScopeState::new(), pattern);
        Rec { unsafe_pattern }
    }

    pub fn unrec<Ident>(&self) -> P
    where
        P: BoundPattern<Ident> + Clone,
    {
        let mut pattern = self.unsafe_pattern.clone();
        pattern.open_pattern(ScopeState::new(), self);
        pattern
    }
}

impl<Ident, P> BoundPattern<Ident> for Rec<P>
where
    P: BoundPattern<Ident>,
{
    fn pattern_eq(&self, other: &Rec<P>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
    }

    fn freshen(&mut self, permutations: &mut Permutations<Ident>) {
        self.unsafe_pattern.freshen(permutations)
    }

    fn swaps(&mut self, permutations: &Permutations<Ident>) {
        self.unsafe_pattern.swaps(permutations)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.open_pattern(state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<Ident>) -> Result<BinderIndex, BinderOffset> {
        self.unsafe_pattern.find_binder_index(free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<Ident>, BinderOffset> {
        self.unsafe_pattern.find_binder_at_offset(offset)
    }
}
