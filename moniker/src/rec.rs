use binder::Binder;
use bound::{BoundPattern, ScopeState};

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

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.unsafe_pattern.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.unsafe_pattern.open_pattern(state, pattern);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        self.unsafe_pattern.visit_binders(on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        self.unsafe_pattern.visit_mut_binders(on_binder);
    }
}
