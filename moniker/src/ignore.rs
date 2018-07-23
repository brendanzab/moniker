use binder::Binder;
use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use var::Var;

/// Data that does not participate in name binding
///
/// This can be useful for adding information like source code locations to
/// syntax trees.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct Ignore<T>(pub T);

impl<N, T> BoundTerm<N> for Ignore<T> {
    fn term_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn open_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N, T> BoundPattern<N> for Ignore<T> {
    fn pattern_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn freshen(&mut self, _: &mut Permutations<N>) {}

    fn swaps(&mut self, _: &Permutations<N>) {}

    fn close_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn open_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}
