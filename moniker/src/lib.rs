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
//! # Useful data types
//!
//! Data types are separated into patterns and terms:
//!
//! ## Terms
//!
//! Terms are data types that implement the [`BoundTerm`] trait.
//!
//! - [`Var<Ident>`]: A variable that is either free or bound
//! - [`Scope<P: BoundPattern<Ident>, T: BoundTerm<Ident>>`]: bind the term `T` using the pattern `P`
//!
//! ## Patterns
//!
//! Patterns are data types that implement the [`BoundPattern`] trait.
//!
//! - [`Binder<Ident>`]: Captures a free variables within a term, but is ignored for alpha equality
//! - [`Ignore<T>`]: Ignores `T` when comparing for alpha equality
//! - [`Embed<T: BoundTerm<Ident>>`]: Embed a term `T` in a pattern
//! - [`Nest<P: BoundPattern<Ident>>`]: Multiple nested binding patterns
//! - [`Rec<P: BoundPattern<Ident>>`]: Recursively bind a pattern in itself
//!
//! [`Var<Ident>`]: enum.Var.html
//! [`Scope<P: BoundPattern<Ident>, T: BoundTerm<Ident>>`]: struct.Scope.html
//! [`Binder<Ident>`]: enum.Binder.html
//! [`Ignore<T>`]: struct.Ignore.html
//! [`Embed<T: BoundTerm<Ident>>`]: struct.Embed.html
//! [`Nest<P: BoundPattern<Ident>>`]: struct.Nest.html
//! [`Rec<P: BoundPattern<Ident>>`]: struct.Rec.html
//! [`BoundTerm`]: trait.BoundTerm.html
//! [`BoundPattern`]: trait.BoundPattern.html

#[cfg(feature = "codespan")]
extern crate codespan;
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
#[doc(hidden)]
pub mod macros;

mod bound;
mod embed;
mod ignore;
mod nest;
mod rec;
mod scope;
mod var;

pub use self::bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
pub use self::embed::Embed;
pub use self::ignore::Ignore;
pub use self::nest::Nest;
pub use self::rec::Rec;
pub use self::scope::Scope;
pub use self::var::{Binder, BinderIndex, BinderOffset, FreeVar, GenId, ScopeOffset, Var};
