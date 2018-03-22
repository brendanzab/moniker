use {Bound, Free, PatternIndex, ScopeState};

pub trait Pattern {
    // TODO: This is kinda yuck - need to figure out a better way!
    type Free: Free;
    type NamePerm;

    fn freshen(&mut self) -> Self::NamePerm;
    fn rename(&mut self, perm: &Self::NamePerm);

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

impl<P: Pattern + Clone> Pattern for [P] {
    type Free = P::Free;
    type NamePerm = Vec<P::NamePerm>;

    fn freshen(&mut self) -> Vec<P::NamePerm> {
        self.iter_mut().map(|pattern| pattern.freshen()).collect()
    }

    fn rename(&mut self, perm: &Vec<P::NamePerm>) {
        assert_eq!(self.len(), perm.len());

        for (pattern, perm) in <_>::zip(self.iter_mut(), perm.iter()) {
            pattern.rename(perm);
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

impl<P: Pattern + Clone> Pattern for Vec<P> {
    type Free = P::Free;
    type NamePerm = Vec<P::NamePerm>;

    fn freshen(&mut self) -> Vec<P::NamePerm> {
        <[P]>::freshen(self)
    }

    fn rename(&mut self, perm: &Vec<P::NamePerm>) {
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
