use bound::{BoundPattern, PatternSubsts, ScopeState};
use var::{BoundVar, FreeVar};

/// Recursive patterns
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rec<P> {
    pub unsafe_pattern: P,
}

impl<P> Rec<P> {
    pub fn new<Ident>(pattern: &P) -> Rec<P>
    where
        P: BoundPattern<Ident> + Clone,
    {
        let mut unsafe_pattern = pattern.clone();
        unsafe_pattern.close_pattern(ScopeState::new(), pattern);
        Rec { unsafe_pattern }
    }

    pub fn unrec<Ident>(&self) -> P
    where
        P: BoundPattern<Ident> + Clone,
    {
        let mut pattern = self.unsafe_pattern.clone();
        pattern.open_pattern(ScopeState::new(), self);
        pattern
    }
}

impl<Ident, P> BoundPattern<Ident> for Rec<P>
where
    P: BoundPattern<Ident>,
{
    fn pattern_eq(&self, other: &Rec<P>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        self.unsafe_pattern.freshen()
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        self.unsafe_pattern.rename(perm)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.open_pattern(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        self.unsafe_pattern.on_free(state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
        self.unsafe_pattern.on_bound(state, name)
    }
}
