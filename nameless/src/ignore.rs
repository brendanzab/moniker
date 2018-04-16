use {BoundName, BoundPattern, BoundTerm, Name, ScopeState};

/// Data that does not participate in name binding
///
/// This can be useful for adding information like source code locations to
/// syntax trees.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct Ignore<T>(pub T);

impl<T> BoundTerm for Ignore<T> {
    fn term_eq(&self, _: &Ignore<T>) -> bool {
        true
    }
}

impl<T> BoundPattern for Ignore<T> {
    fn pattern_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn freshen(&mut self) -> Vec<Name> {
        Vec::new()
    }

    fn rename(&mut self, _: &[Name]) {}

    fn on_free(&self, _: ScopeState, _: &Name) -> Option<BoundName> {
        None
    }

    fn on_bound(&self, _: ScopeState, _: BoundName) -> Option<Name> {
        None
    }
}
