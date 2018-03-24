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

mod embed;
#[macro_use]
mod pattern;
mod scope;
#[macro_use]
mod term;
mod name;
mod var;

pub use self::embed::Embed;
pub use self::pattern::BoundPattern;
pub use self::scope::{unbind, Scope, unbind2};
pub use self::term::{BoundTerm, ScopeState};
pub use self::name::{GenId, Ident, Name};
pub use self::var::{Bound, Debruijn, PatternIndex, Var};
