use {BoundName, BoundPattern, BoundTerm, Name, ScopeState};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<T: BoundTerm> BoundPattern for Embed<T> {
    fn pattern_eq(&self, other: &Embed<T>) -> bool {
        T::term_eq(&self.0, &other.0)
    }

    fn freshen(&mut self) -> Vec<Name> {
        Vec::new()
    }

    fn rename(&mut self, _perm: &[Name]) {}

    fn close_pattern<P: BoundPattern>(&mut self, state: ScopeState, pattern: &P) {
        self.0.close_term(state, pattern);
    }

    fn open_pattern<P: BoundPattern>(&mut self, state: ScopeState, pattern: &P) {
        self.0.open_term(state, pattern);
    }

    fn on_free(&self, _state: ScopeState, _name: &Name) -> Option<BoundName> {
        None
    }

    fn on_bound(&self, _state: ScopeState, _name: BoundName) -> Option<Name> {
        None
    }
}
