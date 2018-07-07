use std::collections::HashSet;
use std::hash::Hash;
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

pub trait BoundTerm<Ident> {
    /// Alpha equivalence in a term context
    fn term_eq(&self, other: &Self) -> bool;

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>);

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>);

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>));

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>));

    fn free_vars(&self) -> HashSet<FreeVar<Ident>>
    where
        Ident: Eq + Hash + Clone,
    {
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

impl<Ident: PartialEq> BoundTerm<Ident> for FreeVar<Ident> {
    fn term_eq(&self, other: &FreeVar<Ident>) -> bool {
        match (self, other) {
            (&FreeVar::User(ref lhs), &FreeVar::User(ref rhs)) => lhs == rhs,
            (&FreeVar::Gen(ref lhs, _), &FreeVar::Gen(ref rhs, _)) => lhs == rhs,
            _ => false,
        }
    }

    fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<Ident>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<Ident>)) {}
}

impl<Ident: PartialEq + Clone> BoundTerm<Ident> for Var<Ident> {
    fn term_eq(&self, other: &Var<Ident>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => FreeVar::term_eq(lhs, rhs),
            (&Var::Bound(ref lhs, _), &Var::Bound(ref rhs, _)) => lhs == rhs,
            (_, _) => false,
        }
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        *self = match *self {
            Var::Bound(_, _) => return,
            Var::Free(ref name) => match pattern.on_free(state, name) {
                Some(bound) => Var::Bound(bound, name.ident().cloned()),
                None => return,
            },
        };
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        *self = match *self {
            Var::Free(_) => return,
            Var::Bound(bound, _) => match pattern.on_bound(state, bound) {
                Some(name) => Var::Free(name),
                None => return,
            },
        };
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        on_var(self);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        on_var(self);
    }
}

// Implementations for common types

macro_rules! impl_bound_term {
    ($T:ty) => {
        impl<Ident> BoundTerm<Ident> for $T {
            fn term_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<Ident>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<Ident>)) {}
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

impl<Ident, T> BoundTerm<Ident> for Option<T>
where
    T: BoundTerm<Ident>,
{
    fn term_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::term_eq(lhs, rhs),
            (&None, &None) => true,
            (_, _) => false,
        }
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        if let Some(ref mut inner) = *self {
            inner.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        if let Some(ref mut inner) = *self {
            inner.open_term(state, pattern);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        if let Some(ref inner) = *self {
            inner.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        if let Some(ref mut inner) = *self {
            inner.visit_mut_vars(on_var);
        }
    }
}

impl<Ident, T> BoundTerm<Ident> for Box<T>
where
    T: BoundTerm<Ident>,
{
    fn term_eq(&self, other: &Box<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        T::close_term(self, state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        T::open_term(self, state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        T::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        T::visit_mut_vars(self, on_var);
    }
}

impl<Ident, T> BoundTerm<Ident> for Rc<T>
where
    T: BoundTerm<Ident> + Clone,
{
    fn term_eq(&self, other: &Rc<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        T::close_term(Rc::make_mut(self), state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        T::open_term(Rc::make_mut(self), state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        T::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        T::visit_mut_vars(Rc::make_mut(self), on_var);
    }
}

impl<Ident, T, U> BoundTerm<Ident> for (T, U)
where
    T: BoundTerm<Ident>,
    U: BoundTerm<Ident>,
{
    fn term_eq(&self, other: &(T, U)) -> bool {
        T::term_eq(&self.0, &other.0) && U::term_eq(&self.1, &other.1)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
    }
}

impl<Ident, T, U, V> BoundTerm<Ident> for (T, U, V)
where
    T: BoundTerm<Ident>,
    U: BoundTerm<Ident>,
    V: BoundTerm<Ident>,
{
    fn term_eq(&self, other: &(T, U, V)) -> bool {
        T::term_eq(&self.0, &other.0)
            && U::term_eq(&self.1, &other.1)
            && V::term_eq(&self.2, &other.2)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
        self.2.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
        self.2.open_term(state, pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
        self.2.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
        self.2.visit_mut_vars(on_var);
    }
}

impl<Ident, T> BoundTerm<Ident> for [T]
where
    T: BoundTerm<Ident> + Clone,
{
    fn term_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        for elem in self {
            elem.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        for elem in self {
            elem.open_term(state, pattern);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        for elem in self {
            elem.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        for elem in self {
            elem.visit_mut_vars(on_var);
        }
    }
}

impl<Ident, T> BoundTerm<Ident> for Vec<T>
where
    T: BoundTerm<Ident> + Clone,
{
    fn term_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        <[T]>::close_term(self, state, pattern)
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        <[T]>::open_term(self, state, pattern)
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        <[T]>::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
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

pub trait BoundPattern<Ident> {
    /// Alpha equivalence in a pattern context
    fn pattern_eq(&self, other: &Self) -> bool;

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>>;

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>);

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>);

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>);

    /// A callback that is used when `unbind`ing `Bind`s to replace free names
    /// with bound names based on the contents of the pattern
    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar>;

    /// A callback that is used when `bind`ing `Bind`s to replace bound names
    /// with free names based on the contents of the pattern
    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>>;
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

impl<Ident: Clone + PartialEq> BoundPattern<Ident> for FreeVar<Ident> {
    fn pattern_eq(&self, _other: &FreeVar<Ident>) -> bool {
        true
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        *self = match *self {
            FreeVar::User(ref name) => FreeVar::Gen(GenId::fresh(), Some(name.clone())),
            FreeVar::Gen(_, _) => return PatternSubsts::new(vec![self.clone()]),
        };
        PatternSubsts::new(vec![self.clone()])
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        assert_eq!(perm.len(), 1); // FIXME: assert
        *self = perm.lookup(PatternIndex(0)).unwrap().clone(); // FIXME: double clone
    }

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        match FreeVar::term_eq(name, self) {
            true => Some(BoundVar {
                scope: state.depth(),
                pattern: PatternIndex(0),
            }),
            false => None,
        }
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
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

macro_rules! impl_bound_pattern {
    ($T:ty) => {
        impl<Ident> BoundPattern<Ident> for $T {
            fn pattern_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
                PatternSubsts::new(vec![])
            }

            fn rename(&mut self, _: &PatternSubsts<FreeVar<Ident>>) {}

            fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

            fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<Ident>) {}

            fn on_free(&self, _: ScopeState, _: &FreeVar<Ident>) -> Option<BoundVar> {
                None
            }

            fn on_bound(&self, _: ScopeState, _: BoundVar) -> Option<FreeVar<Ident>> {
                None
            }
        }
    };
}

impl_bound_pattern!(());
impl_bound_pattern!(String);
impl_bound_pattern!(str);
impl_bound_pattern!(char);
impl_bound_pattern!(bool);
impl_bound_pattern!(u8);
impl_bound_pattern!(u16);
impl_bound_pattern!(u32);
impl_bound_pattern!(u64);
impl_bound_pattern!(usize);
impl_bound_pattern!(i8);
impl_bound_pattern!(i16);
impl_bound_pattern!(i32);
impl_bound_pattern!(i64);
impl_bound_pattern!(isize);
impl_bound_pattern!(f32);
impl_bound_pattern!(f64);

impl<Ident, P> BoundPattern<Ident> for Option<P>
where
    P: BoundPattern<Ident>,
{
    fn pattern_eq(&self, other: &Option<P>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => P::pattern_eq(lhs, rhs),
            (&None, &None) => true,
            (_, _) => false,
        }
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        match *self {
            Some(ref mut inner) => inner.freshen(),
            None => PatternSubsts::new(vec![]),
        }
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        if let Some(ref mut inner) = *self {
            inner.rename(perm);
        }
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        if let Some(ref mut inner) = *self {
            inner.close_pattern(state, pattern);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        if let Some(ref mut inner) = *self {
            inner.open_pattern(state, pattern);
        }
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        self.as_ref().and_then(|inner| inner.on_free(state, name))
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
        self.as_ref().and_then(|inner| inner.on_bound(state, name))
    }
}

impl<Ident, P1, P2> BoundPattern<Ident> for (P1, P2)
where
    P1: BoundPattern<Ident>,
    P2: BoundPattern<Ident>,
{
    fn pattern_eq(&self, other: &(P1, P2)) -> bool {
        P1::pattern_eq(&self.0, &other.0) && P2::pattern_eq(&self.1, &other.1)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        let mut perm = self.0.freshen();
        perm.extend(self.1.freshen());
        perm
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        self.0.rename(perm);
        self.1.rename(perm);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.close_pattern(state, pattern);
        self.1.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.0.open_pattern(state, pattern);
        self.1.open_pattern(state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        self.0
            .on_free(state, name)
            .or_else(|| self.1.on_free(state, name))
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
        self.0
            .on_bound(state, name)
            .or_else(|| self.1.on_bound(state, name))
    }
}

impl<Ident, P> BoundPattern<Ident> for Box<P>
where
    P: BoundPattern<Ident>,
{
    fn pattern_eq(&self, other: &Box<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        P::freshen(self)
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        P::rename(self, perm);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        P::close_pattern(self, state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        P::open_pattern(self, state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        P::on_free(self, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
        P::on_bound(self, state, name)
    }
}

impl<Ident, P> BoundPattern<Ident> for Rc<P>
where
    P: BoundPattern<Ident> + Clone,
{
    fn pattern_eq(&self, other: &Rc<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar<Ident>> {
        P::freshen(Rc::make_mut(self))
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar<Ident>>) {
        P::rename(Rc::make_mut(self), perm);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        P::close_pattern(Rc::make_mut(self), state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        P::open_pattern(Rc::make_mut(self), state, pattern);
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar<Ident>) -> Option<BoundVar> {
        P::on_free(self, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar<Ident>> {
        P::on_bound(self, state, name)
    }
}
