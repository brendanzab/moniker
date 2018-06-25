use {BoundPattern, BoundVar, FreeVar, PatternIndex, PatternSubsts, ScopeState};

/// Nested binding patterns
///
/// Contrast with `Multi`
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
        self.unsafe_patterns.len() == other.unsafe_patterns.len()
            && <_>::zip(self.unsafe_patterns.iter(), other.unsafe_patterns.iter())
                .all(|(lhs, rhs)| P::pattern_eq(lhs, rhs))
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        // FIXME: intermediate allocations
        PatternSubsts::new(
            self.unsafe_patterns
                .iter_mut()
                .flat_map(P::freshen)
                .collect(),
        )
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>) {
        assert_eq!(self.unsafe_patterns.len(), perm.len()); // FIXME: assertion

        for (pattern, perm) in <_>::zip(self.unsafe_patterns.iter_mut(), perm.iter()) {
            pattern.rename(&PatternSubsts::new(vec![perm.clone()])); // FIXME: clone
        }
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
        self.unsafe_patterns
            .iter()
            .enumerate()
            .filter_map(|(i, pattern)| {
                pattern.on_free(state, name).map(|bound| {
                    assert_eq!(bound.pattern, PatternIndex(0));
                    BoundVar {
                        pattern: PatternIndex(i as u32),
                        ..bound
                    }
                })
            })
            .next()
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        self.unsafe_patterns
            .get(name.pattern.0 as usize)
            .and_then(|pattern| {
                pattern.on_bound(
                    state,
                    BoundVar {
                        pattern: PatternIndex(0),
                        ..name
                    },
                )
            })
    }
}
