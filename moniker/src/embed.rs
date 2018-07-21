use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use var::{Binder, BinderIndex, BinderOffset, FreeVar};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<N, T> BoundPattern<N> for Embed<T>
where
    T: BoundTerm<N>,
{
    fn pattern_eq(&self, other: &Embed<T>) -> bool {
        T::term_eq(&self.0, &other.0)
    }

    fn freshen(&mut self, _: &mut Permutations<N>) {}

    fn swaps(&mut self, _: &Permutations<N>) {}

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.close_term(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.open_term(state, pattern);
    }

    fn find_binder_index(&self, _: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        Err(BinderOffset(0))
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        Err(offset)
    }
}
