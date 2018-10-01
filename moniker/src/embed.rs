use binder::Binder;
use bound::{BoundPattern, BoundTerm, OnBoundFn, OnFreeFn, ScopeState};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<N, T> BoundPattern<N> for Embed<T>
where
    N: Clone + PartialEq,
    T: BoundTerm<N>,
{
    fn pattern_eq(&self, other: &Embed<T>) -> bool {
        T::term_eq(&self.0, &other.0)
    }

    fn close_pattern(&mut self, state: ScopeState, on_free: &impl OnFreeFn<N>) {
        self.0.close_term(state, on_free);
    }

    fn open_pattern(&mut self, state: ScopeState, on_bound: &impl OnBoundFn<N>) {
        self.0.open_term(state, on_bound);
    }

    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}
