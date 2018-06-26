use std::{slice, vec};

use bound_term::ScopeState;
use var::{BoundVar, FreeVar, PatternIndex};

/// A mapping of `PatternIndex`s to `T`s
pub struct PatternSubsts<T> {
    perm: Vec<T>,
}

impl<T> PatternSubsts<T> {
    pub fn new(perm: Vec<T>) -> PatternSubsts<T> {
        PatternSubsts { perm }
    }

    pub fn lookup(&self, index: PatternIndex) -> Option<&T> {
        self.perm.get(index.0 as usize)
    }

    pub fn len(&self) -> usize {
        self.perm.len()
    }

    pub fn iter(&self) -> slice::Iter<T> {
        self.perm.iter()
    }
}

impl<T> Extend<T> for PatternSubsts<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.perm.extend(iter)
    }
}

impl<T> IntoIterator for PatternSubsts<T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> vec::IntoIter<T> {
        self.perm.into_iter()
    }
}

pub trait BoundPattern {
    /// Alpha equivalence in a pattern context
    fn pattern_eq(&self, other: &Self) -> bool;

    fn freshen(&mut self) -> PatternSubsts<FreeVar>;

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>);

    #[allow(unused_variables)]
    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {}

    #[allow(unused_variables)]
    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {}

    /// A callback that is used when `unbind`ing `Bind`s to replace free names
    /// with bound names based on the contents of the pattern
    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar>;

    /// A callback that is used when `bind`ing `Bind`s to replace bound names
    /// with free names based on the contents of the pattern
    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar>;
}

/// Asserts that two expressions are alpha equivalent to each other (using
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

impl<P1, P2> BoundPattern for (P1, P2)
where
    P1: BoundPattern,
    P2: BoundPattern,
{
    fn pattern_eq(&self, other: &(P1, P2)) -> bool {
        P1::pattern_eq(&self.0, &other.0) && P2::pattern_eq(&self.1, &other.1)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        let mut perm = self.0.freshen();
        perm.extend(self.1.freshen());
        perm
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>) {
        self.0.rename(perm);
        self.1.rename(perm);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.close_pattern(state, pattern);
        self.1.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.open_pattern(state, pattern);
        self.1.open_pattern(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar> {
        self.0
            .on_free(state, name)
            .or_else(|| self.1.on_free(state, name))
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        self.0
            .on_bound(state, name)
            .or_else(|| self.1.on_bound(state, name))
    }
}
