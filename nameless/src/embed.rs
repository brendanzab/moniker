use {AlphaEq, Bound, Pattern, ScopeState, Term};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<T1: AlphaEq> AlphaEq for Embed<T1> {
    fn alpha_eq(&self, other: &Embed<T1>) -> bool {
        T1::alpha_eq(&self.0, &other.0)
    }
}

impl<T: Term> Pattern for Embed<T> {
    type Free = T::Free;

    fn freshen(&mut self) -> Vec<T::Free> {
        Vec::new()
    }

    fn rename(&mut self, _perm: &[T::Free]) {}

    fn close_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.0.close_term_at(state, pattern);
    }

    fn open_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.0.open_term_at(state, pattern);
    }

    fn on_free(&self, _state: ScopeState, _name: &Self::Free) -> Option<Bound> {
        None
    }

    fn on_bound(&self, _state: ScopeState, _name: Bound) -> Option<Self::Free> {
        None
    }
}
