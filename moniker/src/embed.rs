use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use var::{FreeVar, PVar, PVarIndex, PVarOffset};

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

    fn freshen(&mut self, _: &mut Permutations<Ident>) {}

    fn swaps(&mut self, _: &Permutations<Ident>) {}

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.close_term(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.open_term(state, pattern);
    }

    fn find_pvar_index(&self, _: &FreeVar<Ident>) -> Result<PVarIndex, PVarOffset> {
        Err(PVarOffset(0))
    }

    fn find_pvar_at_offset(&self, offset: PVarOffset) -> Result<PVar<Ident>, PVarOffset> {
        Err(offset)
    }
}
