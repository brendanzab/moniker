use binder::{Binder, BinderIndex, BinderOffset};
use bound::{BoundPattern, Permutations, ScopeState};
use free_var::FreeVar;

/// Recursively bind a pattern in itself
///
/// Mutually recursive bindings can be modelled by combining this type with
/// the pattern implementations for `Vec<P>` and `(P1, P2)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rec<P> {
    pub unsafe_pattern: P,
}

impl<P> Rec<P> {
    pub fn new<N>(pattern: &P) -> Rec<P>
    where
        P: BoundPattern<N> + Clone,
    {
        let mut unsafe_pattern = pattern.clone();
        unsafe_pattern.close_pattern(ScopeState::new(), pattern);
        Rec { unsafe_pattern }
    }

    pub fn unrec<N>(&self) -> P
    where
        P: BoundPattern<N> + Clone,
    {
        let mut pattern = self.unsafe_pattern.clone();
        pattern.open_pattern(ScopeState::new(), self);
        pattern
    }
}

impl<N, P> BoundPattern<N> for Rec<P>
where
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Rec<P>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
    }

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        self.unsafe_pattern.freshen(permutations)
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        self.unsafe_pattern.swaps(permutations)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.unsafe_pattern.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.unsafe_pattern.open_pattern(state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        self.unsafe_pattern.find_binder_index(free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        self.unsafe_pattern.find_binder_at_offset(offset)
    }
}
