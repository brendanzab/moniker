use {Bound, PatternIndex, ScopeState};

pub trait Pattern {
    // TODO: This is kinda yuck - need to figure out a better way!
    type Free;

    fn freshen(&mut self) -> Vec<Self::Free>;
    fn rename(&mut self, perm: &[Self::Free]);

    fn close_pattern<P: Pattern<Free = Self::Free>>(&mut self, pattern: &P) {
        self.close_pattern_at(ScopeState::new(), pattern);
    }

    fn open_pattern<P: Pattern<Free = Self::Free>>(&mut self, pattern: &P) {
        self.open_pattern_at(ScopeState::new(), pattern);
    }

    fn close_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P);
    fn open_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P);

    /// A callback that is used when `unbind`ing `Scope`s to replace free names
    /// with bound names based on the contents of the pattern
    fn on_free(&self, state: ScopeState, name: &Self::Free) -> Option<Bound>;

    /// A callback that is used when `bind`ing `Scope`s to replace bound names
    /// with free names based on the contents of the pattern
    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free>;
}

impl<P: Pattern> Pattern for [P]
where
    P::Free: Clone,
{
    type Free = P::Free;

    fn freshen(&mut self) -> Vec<P::Free> {
        // FIXME: intermediate allocations
        self.iter_mut()
            .flat_map(|pattern| pattern.freshen())
            .collect()
    }

    fn rename(&mut self, perm: &[P::Free]) {
        assert_eq!(self.len(), perm.len()); // FIXME: assertion

        for (pattern, perm) in <_>::zip(self.iter_mut(), perm.iter()) {
            pattern.rename(&[perm.clone()]); // FIXME: clone
        }
    }

    fn close_pattern_at<P1: Pattern<Free = P::Free>>(&mut self, state: ScopeState, pattern: &P1) {
        for elem in self {
            elem.close_pattern_at(state, pattern);
        }
    }

    fn open_pattern_at<P1: Pattern<Free = P::Free>>(&mut self, state: ScopeState, pattern: &P1) {
        for elem in self {
            elem.open_pattern_at(state, pattern);
        }
    }

    fn on_free(&self, state: ScopeState, name: &P::Free) -> Option<Bound> {
        self.iter()
            .enumerate()
            .filter_map(|(i, pattern)| {
                pattern.on_free(state, name).map(|bound| {
                    assert_eq!(bound.pattern, PatternIndex(0));
                    Bound {
                        pattern: PatternIndex(i as u32),
                        ..bound
                    }
                })
            })
            .next()
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<P::Free> {
        self.get(name.pattern.0 as usize).and_then(|pattern| {
            pattern.on_bound(
                state,
                Bound {
                    pattern: PatternIndex(0),
                    ..name
                },
            )
        })
    }
}

impl<P: Pattern> Pattern for Vec<P>
where
    P::Free: Clone,
{
    type Free = P::Free;

    fn freshen(&mut self) -> Vec<P::Free> {
        <[P]>::freshen(self)
    }

    fn rename(&mut self, perm: &[P::Free]) {
        <[P]>::rename(self, perm)
    }

    fn close_pattern_at<P1: Pattern<Free = P::Free>>(&mut self, state: ScopeState, pattern: &P1) {
        <[P]>::close_pattern_at(self, state, pattern)
    }

    fn open_pattern_at<P1: Pattern<Free = P::Free>>(&mut self, state: ScopeState, pattern: &P1) {
        <[P]>::open_pattern_at(self, state, pattern)
    }

    fn on_free(&self, state: ScopeState, name: &P::Free) -> Option<Bound> {
        <[P]>::on_free(self, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<P::Free> {
        <[P]>::on_bound(self, state, name)
    }
}

impl<P1: Pattern, P2: Pattern<Free = P1::Free>> Pattern for (P1, P2) {
    type Free = P1::Free;

    fn freshen(&mut self) -> Vec<P1::Free> {
        let mut perm = self.0.freshen();
        perm.extend(self.1.freshen());
        perm
    }

    fn rename(&mut self, perm: &[P1::Free]) {
        self.0.rename(perm);
        self.1.rename(perm);
    }

    fn close_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.0.close_pattern_at(state, pattern);
        self.1.close_pattern_at(state, pattern);
    }

    fn open_pattern_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.0.open_pattern_at(state, pattern);
        self.1.open_pattern_at(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &Self::Free) -> Option<Bound> {
        self.0.on_free(state, name)?;
        self.1.on_free(state, name)
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free> {
        self.0.on_bound(state, name)?;
        self.1.on_bound(state, name)
    }
}
