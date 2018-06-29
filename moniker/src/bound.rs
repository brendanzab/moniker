use std::collections::HashSet;
use std::rc::Rc;
use std::{slice, vec};

use var::{BoundVar, DebruijnIndex, FreeVar, GenId, PatternIndex, Var};

#[derive(Debug, Copy, Clone)]
pub struct ScopeState {
    depth: u32,
}

impl ScopeState {
    pub fn new() -> ScopeState {
        ScopeState { depth: 0 }
    }

    pub fn depth(&self) -> DebruijnIndex {
        DebruijnIndex(self.depth)
    }

    pub fn incr(mut self) -> ScopeState {
        self.depth += 1;
        self
    }
}

pub trait BoundTerm {
    /// Alpha equivalence in a term context
    fn term_eq(&self, other: &Self) -> bool;

    #[allow(unused_variables)]
    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {}

    #[allow(unused_variables)]
    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {}

    #[allow(unused_variables)]
    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {}

    #[allow(unused_variables)]
    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {}

    fn free_vars(&self) -> HashSet<FreeVar> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_, _) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

/// Asserts that two expressions are alpha equivalent to each other (using
/// `BoundTerm::term_eq`).
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
                if !::moniker::BoundTerm::term_eq(left_val, right_val) {
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
                if !::moniker::BoundTerm::term_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::term_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}

impl BoundTerm for FreeVar {
    fn term_eq(&self, other: &FreeVar) -> bool {
        match (self, other) {
            (&FreeVar::User(ref lhs), &FreeVar::User(ref rhs)) => lhs == rhs,
            (&FreeVar::Gen(ref lhs, _), &FreeVar::Gen(ref rhs, _)) => lhs == rhs,
            _ => false,
        }
    }
}

impl BoundTerm for Var {
    fn term_eq(&self, other: &Var) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => FreeVar::term_eq(lhs, rhs),
            (&Var::Bound(ref lhs, _), &Var::Bound(ref rhs, _)) => lhs == rhs,
            (_, _) => false,
        }
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        *self = match *self {
            Var::Bound(_, _) => return,
            Var::Free(ref name) => match pattern.on_free(state, name) {
                Some(bound) => Var::Bound(bound, name.ident().cloned()),
                None => return,
            },
        };
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        *self = match *self {
            Var::Free(_) => return,
            Var::Bound(bound, _) => match pattern.on_bound(state, bound) {
                Some(name) => Var::Free(name),
                None => return,
            },
        };
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        on_var(self);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        on_var(self);
    }
}

// Implementations for common types

macro_rules! impl_bound_term {
    ($T:ty) => {
        impl BoundTerm for $T {
            fn term_eq(&self, other: &$T) -> bool {
                self == other
            }
        }
    };
}

impl_bound_term!(());
impl_bound_term!(String);
impl_bound_term!(str);
impl_bound_term!(char);
impl_bound_term!(bool);
impl_bound_term!(u8);
impl_bound_term!(u16);
impl_bound_term!(u32);
impl_bound_term!(u64);
impl_bound_term!(usize);
impl_bound_term!(i8);
impl_bound_term!(i16);
impl_bound_term!(i32);
impl_bound_term!(i64);
impl_bound_term!(isize);
impl_bound_term!(f32);
impl_bound_term!(f64);

impl<T: BoundTerm> BoundTerm for Option<T> {
    fn term_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::term_eq(lhs, rhs),
            (_, _) => false,
        }
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        if let Some(ref mut inner) = *self {
            inner.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        if let Some(ref mut inner) = *self {
            inner.open_term(state, pattern);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        if let Some(ref inner) = *self {
            inner.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        if let Some(ref mut inner) = *self {
            inner.visit_mut_vars(on_var);
        }
    }
}

impl<T: BoundTerm> BoundTerm for Box<T> {
    fn term_eq(&self, other: &Box<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        (**self).close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        (**self).open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        (**self).visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        (**self).visit_mut_vars(on_var);
    }
}

impl<T: BoundTerm + Clone> BoundTerm for Rc<T> {
    fn term_eq(&self, other: &Rc<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        Rc::make_mut(self).close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        Rc::make_mut(self).open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        (**self).visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        Rc::make_mut(self).visit_mut_vars(on_var);
    }
}

impl<T: BoundTerm, U: BoundTerm> BoundTerm for (T, U) {
    fn term_eq(&self, other: &(T, U)) -> bool {
        T::term_eq(&self.0, &other.0) && U::term_eq(&self.1, &other.1)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
    }
}

impl<T: BoundTerm, U: BoundTerm, V: BoundTerm> BoundTerm for (T, U, V) {
    fn term_eq(&self, other: &(T, U, V)) -> bool {
        T::term_eq(&self.0, &other.0)
            && U::term_eq(&self.1, &other.1)
            && V::term_eq(&self.2, &other.2)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
        self.2.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
        self.2.open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
        self.2.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
        self.2.visit_mut_vars(on_var);
    }
}

impl<T: BoundTerm + Clone> BoundTerm for [T] {
    fn term_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        for elem in self {
            elem.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        for elem in self {
            elem.open_term(state, pattern);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        for elem in self {
            elem.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        for elem in self {
            elem.visit_mut_vars(on_var);
        }
    }
}

impl<T: BoundTerm + Clone> BoundTerm for Vec<T> {
    fn term_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        <[T]>::close_term(self, state, pattern)
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        <[T]>::open_term(self, state, pattern)
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        <[T]>::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        <[T]>::visit_mut_vars(self, on_var);
    }
}

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
                if !::moniker::BoundPattern::pattern_eq(left_val, right_val) {
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
                if !::moniker::BoundPattern::pattern_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::pattern_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}

impl BoundPattern for FreeVar {
    fn pattern_eq(&self, _other: &FreeVar) -> bool {
        true
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        *self = match *self {
            FreeVar::User(ref name) => FreeVar::Gen(GenId::fresh(), Some(name.clone())),
            FreeVar::Gen(_, _) => return PatternSubsts::new(vec![self.clone()]),
        };
        PatternSubsts::new(vec![self.clone()])
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>) {
        assert_eq!(perm.len(), 1); // FIXME: assert
        *self = perm.lookup(PatternIndex(0)).unwrap().clone(); // FIXME: double clone
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar> {
        match name == self {
            true => Some(BoundVar {
                scope: state.depth(),
                pattern: PatternIndex(0),
            }),
            false => None,
        }
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        match name.scope == state.depth() {
            true => {
                assert_eq!(name.pattern, PatternIndex(0));
                Some(self.clone())
            },
            false => None,
        }
    }
}

// Implementations for common types

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
