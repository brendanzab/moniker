//! Automatically derive variable binding and alpha equivalence for abstract
//! syntax trees.
//!
//! # Example
//!
//! Here is an example of how you might use `nameless` to define the AST for the
//! [simply typed lambda calculus][stlc]:
//!
//! ```rust,ignore
//! #[macro_use]
//! extern crate nameless;
//!
//! use std::rc::Rc;
//! use nameless::{Bind, Embed, Name, Var};
//!
//! #[derive(Debug, Clone, BoundTerm)]
//! pub enum Type {
//!     Base,
//!     Arrow(Rc<Type>, Rc<Type>),
//! }
//!
//! #[derive(Debug, Clone, BoundTerm)]
//! pub enum Expr {
//!     Var(Var),
//!     Lam(Bind<(Name, Embed<Rc<Type>>), Rc<Expr>>),
//!     App(Rc<Expr>, Rc<Expr>),
//! }
//! ```
//!
//! [stlc]: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
//!
//! # Useful data types
//!
//! Data types are separated into patterns and terms - the one exception being
//! `Name`, which can either be used as a term or a pattern.
//!
//! # Terms
//!
//! - `Name`
//! - `Var`: A variable that is either a free `Name` or `Bound`
//! - `Bind<P: BoundPattern, T: BoundPattern>`: bind the term `T` using the pattern `P`
//!
//! # Patterns
//!
//! - `Name`: Capture a name within a term, but ignore for alpha equality
//! - `Embed<T: BoundTerm>`: Embed a term in a pattern

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
