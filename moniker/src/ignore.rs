use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use subst::Subst;
use var::{Binder, BinderIndex, BinderOffset, FreeVar, Var};

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

    fn freshen(&mut self, _: &mut Permutations<Ident>) {}

    fn swaps(&mut self, _: &Permutations<Ident>) {}

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn find_binder_index(&self, _: &FreeVar<Ident>) -> Result<BinderIndex, BinderOffset> {
        Err(BinderOffset(0))
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<Ident>, BinderOffset> {
        Err(offset)
    }
}

impl<Ident, T, U> Subst<Ident, U> for Ignore<T> {
    fn subst(&mut self, _: &FreeVar<Ident>, _: &U) {}
    fn substs(&mut self, _: &[(FreeVar<Ident>, U)]) {}
}
