use bound::{BoundPattern, BoundTerm, PatternSubsts, ScopeState};
use var::{BoundVar, FreeVar};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<T: BoundTerm> BoundPattern for Embed<T> {
    fn pattern_eq(&self, other: &Embed<T>) -> bool {
        T::term_eq(&self.0, &other.0)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        PatternSubsts::new(Vec::new())
    }

    fn rename(&mut self, _perm: &PatternSubsts<FreeVar>) {}

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.close_term(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.open_term(state, pattern);
    }

    fn on_free(&self, _state: ScopeState, _name: &FreeVar) -> Option<BoundVar> {
        None
    }

    fn on_bound(&self, _state: ScopeState, _name: BoundVar) -> Option<FreeVar> {
        None
    }
}
