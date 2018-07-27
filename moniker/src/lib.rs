//! Automatically derive variable binding and alpha equivalence for abstract
//! syntax trees.
//!
//! # Example
//!
//! Here is an example of how you might use `moniker` to define the AST for the
//! [simply typed lambda calculus][stlc]:
//!
//! ```rust
//! #[macro_use]
//! extern crate moniker;
//!
//! use std::rc::Rc;
//! use moniker::{Embed, Nest, Binder, Scope, Var};
//!
//! # #[cfg(feature = "moniker-derive")]
//! #[derive(Debug, Clone, BoundTerm)]
//! pub enum Type {
//!     Base,
//!     Arrow(Rc<Type>, Rc<Type>),
//! }
//!
//! # #[cfg(feature = "moniker-derive")]
//! #[derive(Debug, Clone, BoundTerm)]
//! pub enum Expr {
//!     Var(Var<String>),
//!     Lam(Scope<(Binder<String>, Embed<Rc<Type>>), Rc<Expr>>),
//!     Let(Scope<Nest<(Binder<String>, Embed<(Rc<Type>, Rc<Expr>)>)>, Rc<Expr>>),
//!     App(Rc<Expr>, Rc<Expr>),
//! }
//! # fn main() {}
//! ```
//!
//! [stlc]: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
//!
//! # Overview of traits and data types
//!
//! We separate data types into terms and patterns:
//!
//! ## Terms
//!
//! Terms are data types that implement the [`BoundTerm`] trait.
//!
//! - [`Var<N>`]: A variable that is either free or bound
//! - [`Scope<P: BoundPattern<N>, T: BoundTerm<N>>`]: bind the term `T` using the pattern `P`
//! - [`Ignore<T>`]: Ignores `T` when comparing for alpha equality
//!
//! Implementations for tuples, strings, numbers, slices, vectors, and mart pointers
//! are also provided for convenience.
//!
//! [`BoundTerm`]: trait.BoundTerm.html
//! [`Var<N>`]: enum.Var.html
//! [`Scope<P: BoundPattern<N>, T: BoundTerm<N>>`]: struct.Scope.html
//!
//! ## Patterns
//!
//! Patterns are data types that implement the [`BoundPattern`] trait.
//!
//! - [`Binder<N>`]: Captures a free variables within a term, but is ignored for alpha equality
//! - [`Ignore<T>`]: Ignores `T` when comparing for alpha equality
//! - [`Embed<T: BoundTerm<N>>`]: Embed a term `T` in a pattern
//! - [`Nest<P: BoundPattern<N>>`]: Multiple nested binding patterns
//! - [`Rec<P: BoundPattern<N>>`]: Recursively bind a pattern in itself
//!
//! Implementations for tuples, strings, numbers, slices, vectors, and mart pointers
//! are also provided for convenience.
//!
//! [`BoundPattern`]: trait.BoundPattern.html
//! [`Binder<N>`]: enum.Binder.html
//! [`Ignore<T>`]: struct.Ignore.html
//! [`Embed<T: BoundTerm<N>>`]: struct.Embed.html
//! [`Nest<P: BoundPattern<N>>`]: struct.Nest.html
//! [`Rec<P: BoundPattern<N>>`]: struct.Rec.html

#[macro_use]
extern crate lazy_static;
#[cfg(feature = "moniker-derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate moniker_derive;

// Optional impls
#[cfg(feature = "codespan")]
extern crate codespan;
#[cfg(feature = "im")]
extern crate im;
#[cfg(feature = "num-bigint")]
extern crate num_bigint;

#[cfg(feature = "moniker-derive")]
#[doc(hidden)]
pub use moniker_derive::*;

#[macro_use]
#[doc(hidden)]
pub mod macros;

mod binder;
mod bound;
mod embed;
mod free_var;
mod ignore;
mod nest;
mod rec;
mod scope;
mod var;

pub use self::binder::{Binder, BinderIndex};
pub use self::bound::{BoundPattern, BoundTerm, ScopeState};
pub use self::embed::Embed;
pub use self::free_var::{FreeVar, GenId};
pub use self::ignore::Ignore;
pub use self::nest::Nest;
pub use self::rec::Rec;
pub use self::scope::Scope;
pub use self::var::{ScopeOffset, Var};
