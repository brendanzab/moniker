use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use var::{FreeVar, PVar, PVarIndex, PVarOffset, TVar};

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

    fn visit_vars(&self, _: &mut impl FnMut(&TVar<Ident>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut TVar<Ident>)) {}
}

impl<Ident, T> BoundPattern<Ident> for Ignore<T> {
    fn pattern_eq(&self, _: &Ignore<T>) -> bool {
        true
    }

    fn freshen(&mut self, _: &mut Permutations<Ident>) {}

    fn swaps(&mut self, _: &Permutations<Ident>) {}

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn find_pvar_index(&self, _: &FreeVar<Ident>) -> Result<PVarIndex, PVarOffset> {
        Err(PVarOffset(0))
    }

    fn find_pvar_at_offset(&self, offset: PVarOffset) -> Result<PVar<Ident>, PVarOffset> {
        Err(offset)
    }
}
