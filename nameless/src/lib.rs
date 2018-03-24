//! Automatically derive variable binding and alpha equivalence for abstract
//! syntax trees.

#[macro_use]
extern crate lazy_static;
#[cfg(feature = "nameless-derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate nameless_derive;

#[cfg(feature = "nameless-derive")]
#[doc(hidden)]
pub use nameless_derive::*;

mod bind;
mod embed;
mod name;
#[macro_use]
mod pattern;
#[macro_use]
mod term;
mod var;

pub use self::bind::{bind, unbind, Bind, unbind2};
pub use self::embed::Embed;
pub use self::name::{GenId, Ident, Name};
pub use self::pattern::BoundPattern;
pub use self::term::{BoundTerm, ScopeState};
pub use self::var::{Bound, Debruijn, PatternIndex, Var};
