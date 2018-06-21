use {BoundPattern, BoundVar, FreeVar, ScopeState};

/// Recursive patterns
pub struct Rec<P> {
    pub unsafe_pattern: P,
}

impl<P> Rec<P>
where
    P: BoundPattern + Clone,
{
    pub fn new(pattern: &P) -> Rec<P> {
        let mut unsafe_pattern = pattern.clone();
        unsafe_pattern.close_pattern(ScopeState::new(), pattern);
        Rec { unsafe_pattern }
    }

    pub fn unrec(&self) -> P {
        let mut pattern = self.unsafe_pattern.clone();
        pattern.open_pattern(ScopeState::new(), self);
        pattern
    }
}

impl<P> BoundPattern for Rec<P>
where
    P: BoundPattern,
{
    fn pattern_eq(&self, other: &Rec<P>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
    }

    fn freshen(&mut self) -> Vec<FreeVar> {
        self.unsafe_pattern.freshen()
    }

    fn rename(&mut self, perm: &[FreeVar]) {
        self.unsafe_pattern.rename(perm)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.unsafe_pattern.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.unsafe_pattern.open_pattern(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar> {
        self.unsafe_pattern.on_free(state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        self.unsafe_pattern.on_bound(state, name)
    }
}
