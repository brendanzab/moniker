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
//! use nameless::{Scope, Embed, Name, Var};
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
//!     Lam(Scope<(Name, Embed<Rc<Type>>), Rc<Expr>>),
//!     Let(Scope<Rebind<(Name, Embed<(Rc<Type>, Rc<Expr>>)>, Rc<Expr>),
//!     App(Rc<Expr>, Rc<Expr>),
//! }
//! ```
//!
//! [stlc]: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
//!
//! # Useful data types
//!
//! Data types are separated into patterns and terms:
//!
//! ## Terms
//!
//! - `Var`: A variable that is either a `FreeVar` or `BoundVar`
//! - `Scope<P: BoundPattern, T: BoundTerm>`: bind the term `T` using the pattern `P`
//!
//! ## Patterns
//!
//! - `FreeVar`: Captures a free variable within a term, but is ignored for alpha equality
//! - `Ignore<T>`: Ignores `T` when comparing for alpha equality
//! - `Embed<T: BoundTerm>`: Embed a term in a pattern
//! - `Nest<T: BoundPattern>`: Multiple nested binding patterns
//! - `Rec<T: BoundPattern>`: Recursive binding patterns

#[macro_use]
extern crate lazy_static;
#[cfg(feature = "nameless-derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate nameless_derive;

#[cfg(feature = "nameless-derive")]
#[doc(hidden)]
pub use nameless_derive::*;

mod scope;
#[macro_use]
mod bound_pattern;
#[macro_use]
mod bound_term;
mod embed;
mod ignore;
mod nest;
mod rec;
mod var;

pub use self::bound_pattern::BoundPattern;
pub use self::bound_term::{BoundTerm, ScopeState};
pub use self::embed::Embed;
pub use self::ignore::Ignore;
pub use self::nest::Nest;
pub use self::rec::Rec;
pub use self::scope::Scope;
pub use self::var::{BoundVar, DebruijnIndex, FreeVar, GenId, Ident, PatternIndex, Var};
