use {Bound, Name, PatternIndex, ScopeState};

pub trait BoundPattern {
    fn pattern_eq(&self, other: &Self) -> bool;

    fn freshen(&mut self) -> Vec<Name>;
    fn rename(&mut self, perm: &[Name]);

    #[allow(unused_variables)]
    fn close_pattern<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: BoundPattern,
    {
    }

    #[allow(unused_variables)]
    fn open_pattern<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: BoundPattern,
    {
    }

    /// A callback that is used when `unbind`ing `Scope`s to replace free names
    /// with bound names based on the contents of the pattern
    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound>;

    /// A callback that is used when `bind`ing `Scope`s to replace bound names
    /// with free names based on the contents of the pattern
    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Name>;
}

/// Asserts that two expressions are alpha equalent to each other (using
/// `BoundPattern::pattern_eq`).
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations.
///
/// Like `assert!`, this macro has a second form, where a custom
/// panic message can be provided.
#[macro_export]
macro_rules! assert_pattern_eq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !::nameless::BoundPattern::pattern_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::pattern_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`"#, left_val, right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_pattern_eq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !::nameless::BoundPattern::pattern_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::pattern_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}

impl<P> BoundPattern for [P]
where
    P: BoundPattern,
{
    fn pattern_eq(&self, other: &[P]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| P::pattern_eq(lhs, rhs))
    }

    fn freshen(&mut self) -> Vec<Name> {
        // FIXME: intermediate allocations
        self.iter_mut()
            .flat_map(|pattern| pattern.freshen())
            .collect()
    }

    fn rename(&mut self, perm: &[Name]) {
        assert_eq!(self.len(), perm.len()); // FIXME: assertion

        for (pattern, perm) in <_>::zip(self.iter_mut(), perm.iter()) {
            pattern.rename(&[perm.clone()]); // FIXME: clone
        }
    }

    fn close_pattern<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        for elem in self {
            elem.close_pattern(state, pattern);
        }
    }

    fn open_pattern<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        for elem in self {
            elem.open_pattern(state, pattern);
        }
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
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

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Name> {
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

impl<P> BoundPattern for Vec<P>
where
    P: BoundPattern,
{
    fn pattern_eq(&self, other: &Vec<P>) -> bool {
        <[P]>::pattern_eq(self, other)
    }

    fn freshen(&mut self) -> Vec<Name> {
        <[P]>::freshen(self)
    }

    fn rename(&mut self, perm: &[Name]) {
        <[P]>::rename(self, perm)
    }

    fn close_pattern<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        <[P]>::close_pattern(self, state, pattern)
    }

    fn open_pattern<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        <[P]>::open_pattern(self, state, pattern)
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
        <[P]>::on_free(self, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Name> {
        <[P]>::on_bound(self, state, name)
    }
}

impl<P1, P2> BoundPattern for (P1, P2)
where
    P1: BoundPattern,
    P2: BoundPattern,
{
    fn pattern_eq(&self, other: &(P1, P2)) -> bool {
        P1::pattern_eq(&self.0, &other.0) && P2::pattern_eq(&self.1, &other.1)
    }

    fn freshen(&mut self) -> Vec<Name> {
        let mut perm = self.0.freshen();
        perm.extend(self.1.freshen());
        perm
    }

    fn rename(&mut self, perm: &[Name]) {
        self.0.rename(perm);
        self.1.rename(perm);
    }

    fn close_pattern<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: BoundPattern,
    {
        self.0.close_pattern(state, pattern);
        self.1.close_pattern(state, pattern);
    }

    fn open_pattern<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: BoundPattern,
    {
        self.0.open_pattern(state, pattern);
        self.1.open_pattern(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
        self.0
            .on_free(state, name)
            .or_else(|| self.1.on_free(state, name))
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Name> {
        self.0
            .on_bound(state, name)
            .or_else(|| self.1.on_bound(state, name))
    }
}
