#[cfg(feature = "codespan")]
use codespan::{
    ByteIndex, ByteOffset, ColumnIndex, ColumnNumber, ColumnOffset, LineIndex, LineNumber,
    LineOffset, Span,
};
use var::{Binder, FreeVar, Var};

// TODO: implement a deriving syntax extension for this!
pub trait Subst<Ident, T>: Sized {
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &T);

    // FIXME: use a data structure with better lookup performance?
    fn substs(&mut self, mappings: &[(FreeVar<Ident>, T)]);
}

macro_rules! impl_subst_ignore {
    ($T:ty) => {
        impl<Ident, T> Subst<Ident, T> for $T {
            fn subst(&mut self, _: &FreeVar<Ident>, _: &T) {}
            fn substs(&mut self, _: &[(FreeVar<Ident>, T)]) {}
        }
    };
}

impl_subst_ignore!(());
impl_subst_ignore!(String);
impl_subst_ignore!(char);
impl_subst_ignore!(bool);
impl_subst_ignore!(u8);
impl_subst_ignore!(u16);
impl_subst_ignore!(u32);
impl_subst_ignore!(u64);
impl_subst_ignore!(usize);
impl_subst_ignore!(i8);
impl_subst_ignore!(i16);
impl_subst_ignore!(i32);
impl_subst_ignore!(i64);
impl_subst_ignore!(isize);
impl_subst_ignore!(f32);
impl_subst_ignore!(f64);

#[cfg(feature = "codespan")]
impl_subst_ignore!(ByteIndex);
#[cfg(feature = "codespan")]
impl_subst_ignore!(ByteOffset);
#[cfg(feature = "codespan")]
impl_subst_ignore!(ColumnIndex);
#[cfg(feature = "codespan")]
impl_subst_ignore!(ColumnNumber);
#[cfg(feature = "codespan")]
impl_subst_ignore!(ColumnOffset);
#[cfg(feature = "codespan")]
impl_subst_ignore!(LineIndex);
#[cfg(feature = "codespan")]
impl_subst_ignore!(LineNumber);
#[cfg(feature = "codespan")]
impl_subst_ignore!(LineOffset);

#[cfg(feature = "codespan")]
impl<Ident, U, T> Subst<Ident, U> for Span<T> {
    fn subst(&mut self, _: &FreeVar<Ident>, _: &U) {}
    fn substs(&mut self, _: &[(FreeVar<Ident>, U)]) {}
}

impl<Ident, T> Subst<Ident, T> for Binder<Ident>
where
    Ident: PartialEq,
{
    fn subst(&mut self, _: &FreeVar<Ident>, _: &T) {}
    fn substs(&mut self, _: &[(FreeVar<Ident>, T)]) {}
}

impl<Ident, T> Subst<Ident, T> for Var<Ident>
where
    Ident: PartialEq,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &T) {
        match *self {
            Var::Free(ref n) => if name == n {
                unimplemented!()
            },
            Var::Bound(_, _, _) => {},
        }
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, T)]) {
        match *self {
            Var::Free(ref n) => {
                if let Some(&(_, ref replacement)) =
                    mappings.iter().find(|&&(ref name, _)| name == n)
                {
                    unimplemented!()
                }
            },
            Var::Bound(_, _, _) => {},
        }
    }
}

impl<Ident, T, U> Subst<Ident, U> for Option<T>
where
    T: Subst<Ident, U>,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &U) {
        if let Some(ref mut inner) = *self {
            inner.subst(name, replacement);
        }
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, U)]) {
        if let Some(ref mut inner) = *self {
            inner.substs(mappings);
        }
    }
}

impl<Ident, T1, T2, U> Subst<Ident, U> for (T1, T2)
where
    T1: Subst<Ident, U>,
    T2: Subst<Ident, U>,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &U) {
        self.0.subst(name, replacement);
        self.1.subst(name, replacement);
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, U)]) {
        self.0.substs(mappings);
        self.1.substs(mappings);
    }
}

impl<Ident, T1, T2, T3, U> Subst<Ident, U> for (T1, T2, T3)
where
    T1: Subst<Ident, U>,
    T2: Subst<Ident, U>,
    T3: Subst<Ident, U>,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &U) {
        self.0.subst(name, replacement);
        self.1.subst(name, replacement);
        self.2.subst(name, replacement);
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, U)]) {
        self.0.substs(mappings);
        self.1.substs(mappings);
        self.2.substs(mappings);
    }
}

impl<Ident, T, U> Subst<Ident, U> for Vec<T>
where
    T: Subst<Ident, U>,
{
    fn subst(&mut self, name: &FreeVar<Ident>, replacement: &U) {
        for elem in self {
            elem.subst(name, replacement);
        }
    }

    fn substs(&mut self, mappings: &[(FreeVar<Ident>, U)]) {
        for elem in self {
            elem.substs(mappings);
        }
    }
}
