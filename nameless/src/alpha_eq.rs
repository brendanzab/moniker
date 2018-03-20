use std::rc::Rc;

/// Types that may be compared for alpha equality
///
/// Alpha equality ignores the specific _name_ given to a binding, instead
/// looking at the structure of the bindings. For example, the following two
/// terms should be equal despite the fact that they use different names:
///
/// ```text
/// \x y -> x
/// \a b -> a
/// ```
pub trait AlphaEq<Other: ?Sized = Self> {
    fn alpha_eq(&self, other: &Other) -> bool;
}

macro_rules! impl_alpha_eq_eq {
    ($T: ty) => {
        impl AlphaEq for $T {
            fn alpha_eq(&self, other: &$T) -> bool {
                self == other
            }
        }
    };
}

impl_alpha_eq_eq!(u8);
impl_alpha_eq_eq!(u16);
impl_alpha_eq_eq!(u32);
impl_alpha_eq_eq!(u64);
impl_alpha_eq_eq!(usize);
impl_alpha_eq_eq!(i8);
impl_alpha_eq_eq!(i16);
impl_alpha_eq_eq!(i32);
impl_alpha_eq_eq!(i64);
impl_alpha_eq_eq!(isize);
impl_alpha_eq_eq!(f32);
impl_alpha_eq_eq!(f64);
impl_alpha_eq_eq!(char);
impl_alpha_eq_eq!(String);
impl_alpha_eq_eq!(());

impl<T: AlphaEq> AlphaEq for Option<T> {
    fn alpha_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::alpha_eq(lhs, rhs),
            (_, _) => false,
        }
    }
}

impl<T: AlphaEq> AlphaEq for Box<T> {
    fn alpha_eq(&self, other: &Box<T>) -> bool {
        T::alpha_eq(self, other)
    }
}

impl<T: AlphaEq> AlphaEq for Rc<T> {
    fn alpha_eq(&self, other: &Rc<T>) -> bool {
        T::alpha_eq(self, other)
    }
}

impl<T: AlphaEq> AlphaEq for [T] {
    fn alpha_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::alpha_eq(lhs, rhs))
    }
}

impl<T: AlphaEq> AlphaEq for Vec<T> {
    fn alpha_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::alpha_eq(self, other)
    }
}

/// Asserts that two expressions are alpha equalent to each other (using [`AlphaEq`]).
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations.
///
/// Like [`assert!`], this macro has a second form, where a custom
/// panic message can be provided.
#[macro_export]
macro_rules! assert_alpha_eq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !::nameless::AlphaEq::alpha_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::alpha_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`"#, left_val, right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_alpha_eq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !::nameless::AlphaEq::alpha_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `<_>::alpha_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}
