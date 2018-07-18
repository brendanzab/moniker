use bound::{BoundPattern, BoundTerm, PatternSubsts, ScopeState};
use var::{BoundVar, FreeVar};

/// Embed a term in a pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Embed<T>(pub T);

impl<Ident, T> BoundPattern<Ident> for Embed<T>
where
    T: BoundTerm<Ident>,
{
    fn pattern_eq(&self, other: &Embed<T>) -> bool {
        T::term_eq(&self.0, &other.0)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        PatternSubsts::new(Vec::new())
    }

    fn swaps(&mut self, _: &PatternSubsts<FreeVar<Ident>>) {}

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.close_term(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.open_term(state, pattern);
    }

    fn on_free(&self, _: ScopeState, _: &FreeVar<Ident>) -> Option<BoundVar> {
        None
    }

    fn on_bound(&self, _: ScopeState, _: BoundVar) -> Option<FreeVar<Ident>> {
        None
    }
}
