use std::rc::Rc;

use {Debruijn, Pattern};

#[derive(Debug, Copy, Clone)]
pub struct ScopeState {
    depth: u32,
}

impl ScopeState {
    pub fn new() -> ScopeState {
        ScopeState { depth: 0 }
    }

    pub fn depth(&self) -> Debruijn {
        Debruijn(self.depth)
    }

    pub fn incr(mut self) -> ScopeState {
        self.depth += 1;
        self
    }
}

pub trait Term {
    type Free;

    fn term_eq(&self, other: &Self) -> bool;

    fn close_term<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: Pattern<Free = Self::Free>;

    fn open_term<P>(&mut self, state: ScopeState, pattern: &P)
    where
        P: Pattern<Free = Self::Free>;
}

/// Asserts that two expressions are alpha equalent to each other (using
/// `Term::term_eq`).
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations.
///
/// Like `assert!`, this macro has a second form, where a custom
/// panic message can be provided.
#[macro_export]
macro_rules! assert_term_eq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !::nameless::Term::term_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::term_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`"#, left_val, right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_term_eq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !::nameless::Term::term_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::term_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}

impl<T: Term> Term for Option<T> {
    type Free = T::Free;

    fn term_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::term_eq(lhs, rhs),
            (_, _) => false,
        }
    }

    fn close_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.close_term(state, pattern);
        }
    }

    fn open_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.open_term(state, pattern);
        }
    }
}

impl<T: Term> Term for Box<T> {
    type Free = T::Free;

    fn term_eq(&self, other: &Box<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).close_term(state, pattern);
    }

    fn open_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).open_term(state, pattern);
    }
}

impl<T: Term + Clone> Term for Rc<T> {
    type Free = T::Free;

    fn term_eq(&self, other: &Rc<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).close_term(state, pattern);
    }

    fn open_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).open_term(state, pattern);
    }
}

impl<T: Term + Clone> Term for [T] {
    type Free = T::Free;

    fn term_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.close_term(state, pattern);
        }
    }

    fn open_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.open_term(state, pattern);
        }
    }
}

impl<T: Term + Clone> Term for Vec<T> {
    type Free = T::Free;

    fn term_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::term_eq(self, other)
    }

    fn close_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::close_term(self, state, pattern)
    }

    fn open_term<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::open_term(self, state, pattern)
    }
}
