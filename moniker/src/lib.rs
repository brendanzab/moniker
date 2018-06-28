//! Automatically derive variable binding and alpha equivalence for abstract
//! syntax trees.
//!
//! # Example
//!
//! Here is an example of how you might use `moniker` to define the AST for the
//! [simply typed lambda calculus][stlc]:
//!
//! ```rust,ignore
//! #[macro_use]
//! extern crate moniker;
//!
//! use std::rc::Rc;
//! use moniker::{Scope, Embed, Name, Var};
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
//! Terms are data types that implement the `BoundTerm` trait.
//!
//! - `Var`: A variable that is either a `FreeVar` or `BoundVar`
//! - `Scope<P: BoundPattern, T: BoundTerm>`: bind the term `T` using the pattern `P`
//!
//! ## Patterns
//!
//! Patterns are data types that implement the `BoundPattern` trait.
//!
//! - `FreeVar`: Captures a free variable within a term, but is ignored for alpha equality
//! - `Ignore<T>`: Ignores `T` when comparing for alpha equality
//! - `Embed<T: BoundTerm>`: Embed a term in a pattern
//! - `Multi<T: BoundPattern>`: Multiple parallel binding patterns
//! - `Nest<T: BoundPattern>`: Multiple nested binding patterns
//! - `Rec<T: BoundPattern>`: Recursive binding patterns

#[macro_use]
extern crate lazy_static;
#[cfg(feature = "moniker-derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate moniker_derive;

#[cfg(feature = "moniker-derive")]
#[doc(hidden)]
pub use moniker_derive::*;

#[macro_use]
mod bound;
mod embed;
mod ignore;
mod multi;
mod nest;
mod rec;
mod scope;
mod var;

pub use self::bound::{BoundPattern, BoundTerm, PatternSubsts, ScopeState};
pub use self::embed::Embed;
pub use self::ignore::Ignore;
pub use self::multi::Multi;
pub use self::nest::Nest;
pub use self::rec::Rec;
pub use self::scope::Scope;
pub use self::var::{BoundVar, DebruijnIndex, FreeVar, GenId, PatternIndex, Var};
