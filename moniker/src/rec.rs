use binder::Binder;
use bound::{BoundPattern, OnBoundFn, OnFreeFn, ScopeState};
use var::Var;

/// Recursively bind a pattern in itself
///
/// Mutually recursive bindings can be modelled by combining this type with
/// the pattern implementations for `Vec<P>` and `(P1, P2)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rec<P> {
    pub unsafe_pattern: P,
}

impl<P> Rec<P> {
    pub fn new<N>(mut pattern: P) -> Rec<P>
    where
        N: Clone + PartialEq,
        P: BoundPattern<N>,
    {
        let binders = pattern.binders();
        pattern.close_pattern(ScopeState::new(), &binders);
        Rec {
            unsafe_pattern: pattern,
        }
    }

    pub fn unrec<N>(mut self) -> P
    where
        N: Clone + PartialEq,
        P: BoundPattern<N>,
    {
        let binders = self.unsafe_pattern.binders();
        self.open_pattern(ScopeState::new(), &binders);
        self.unsafe_pattern
    }
}

impl<N, P> BoundPattern<N> for Rec<P>
where
    N: Clone + PartialEq,
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Rec<P>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
    }

    fn close_pattern(&mut self, state: ScopeState, on_free: &impl OnFreeFn<N>) {
        self.unsafe_pattern.close_pattern(state, on_free);
    }

    fn open_pattern(&mut self, state: ScopeState, on_bound: &impl OnBoundFn<N>) {
        self.unsafe_pattern.open_pattern(state, on_bound);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        self.unsafe_pattern.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        self.unsafe_pattern.visit_mut_vars(on_var);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        self.unsafe_pattern.visit_binders(on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        self.unsafe_pattern.visit_mut_binders(on_binder);
    }
}
