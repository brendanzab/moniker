use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use subst::Subst;
use var::{Binder, BinderIndex, BinderOffset, FreeVar};

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

    fn find_binder_index(&self, _: &FreeVar<Ident>) -> Result<BinderIndex, BinderOffset> {
        Err(BinderOffset(0))
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<Ident>, BinderOffset> {
        Err(offset)
    }
}

impl<Ident, T, U> Subst<Ident, U> for Embed<T>
where
    T: Subst<Ident, U>,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &U) {
        self.0.subst(name, replacement);
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, U)]) {
        self.0.substs(mappings);
    }
}
