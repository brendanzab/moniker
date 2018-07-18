use bound::{BoundPattern, BoundTerm, PatternSubsts, ScopeState};
use var::{BoundVar, FreeVar, Var};

/// Data that does not participate in name binding
///
/// This can be useful for adding information like source code locations to
/// syntax trees.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct Ignore<T>(pub T);

impl<Ident, T> BoundTerm<Ident> for Ignore<T> {
    fn term_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<Ident>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<Ident>)) {}
}

impl<Ident, T> BoundPattern<Ident> for Ignore<T> {
    fn pattern_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        PatternSubsts::new(Vec::new())
    }

    fn swaps(&mut self, _: &PatternSubsts<FreeVar<Ident>>) {}

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn on_free(&self, _: ScopeState, _: &FreeVar<Ident>) -> Option<BoundVar> {
        None
    }

    fn on_bound(&self, _: ScopeState, _: BoundVar) -> Option<FreeVar<Ident>> {
        None
    }
}
