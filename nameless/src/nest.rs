use {BoundPattern, BoundVar, FreeVar, PatternSubsts, ScopeState};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nest<P> {
    pub unsafe_patterns: Vec<P>,
}

impl<P> Nest<P>
where
    P: BoundPattern,
{
    /// Nest a term with the given patterns
    pub fn new(patterns: Vec<P>) -> Nest<P> {
        // FIXME: Avoid allocating new vector
        let mut rebound_patterns = Vec::<P>::with_capacity(patterns.len());

        for mut pattern in patterns {
            let mut state = ScopeState::new();
            for rebound_pattern in &rebound_patterns {
                state = state.incr();
                pattern.close_pattern(state, rebound_pattern);
            }
            rebound_patterns.push(pattern);
        }

        Nest {
            unsafe_patterns: rebound_patterns,
        }
    }

    /// Unnest a term, returning the freshened patterns
    pub fn unnest(self) -> Vec<P> {
        // FIXME: Avoid allocating new vector
        let mut unrebound_patterns = Vec::<P>::with_capacity(self.unsafe_patterns.len());

        for mut pattern in self.unsafe_patterns {
            let mut state = ScopeState::new();
            for bound_pattern in &unrebound_patterns {
                state = state.incr();
                pattern.open_pattern(state, bound_pattern);
            }
            unrebound_patterns.push(pattern);
        }

        unrebound_patterns
    }
}

impl<P: BoundPattern> BoundPattern for Nest<P> {
    fn pattern_eq(&self, other: &Nest<P>) -> bool {
        Vec::pattern_eq(&self.unsafe_patterns, &other.unsafe_patterns)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        Vec::freshen(&mut self.unsafe_patterns)
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>) {
        Vec::rename(&mut self.unsafe_patterns, perm)
    }

    fn close_pattern(&mut self, mut state: ScopeState, pattern: &impl BoundPattern) {
        for elem in &mut self.unsafe_patterns {
            elem.close_pattern(state, pattern);
            state = state.incr();
        }
    }

    fn open_pattern(&mut self, mut state: ScopeState, pattern: &impl BoundPattern) {
        for elem in &mut self.unsafe_patterns {
            elem.close_pattern(state, pattern);
            state = state.incr();
        }
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar> {
        Vec::on_free(&self.unsafe_patterns, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        Vec::on_bound(&self.unsafe_patterns, state, name)
    }
}
