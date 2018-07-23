use binder::Binder;
use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};

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

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.close_term(state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.open_term(state, binders);
    }

    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}
