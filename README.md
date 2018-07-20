# Moniker

[![Build Status][travis-badge]][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/brendanzab/moniker.svg?branch=master
[travis-url]: https://travis-ci.org/brendanzab/moniker
[gitter-badge]: https://badges.gitter.im/moniker-rs/moniker.svg
[gitter-lobby]: https://gitter.im/moniker-rs/Lobby

Keeping track of bound variables across nested scopes is actually a surprisingly
hard and error-prone thing to implement when building new programming languages.
Moniker aims to alleviate this error-prone boilerplate by providing a set of
generic types and traits for describing how variables are bound, that can then
be used to automatically derive the corresponding name-handling code.

## Example

Here is how we would use Moniker to describe a very small functional language,
with variables, anonymous functions, applications, and let bindings:

```rust
#[macro_use]
extern crate moniker;

use moniker::{Embed, Binder, Scope, Var};
use std::rc::Rc;

/// Types
///
/// Although we do not bind any variables here, we will want to use it later on
/// in our expression syntax, so we derive `BoundTerm` for it.
///
/// ```text
/// t ::= b             base types
///     | t -> t        function types
/// ````
#[derive(Debug, Clone, BoundTerm)]
pub enum Type {
    /// Some base type
    Base,
    /// Function types
    Arrow(RcType, RcType),
}

pub type RcType = Rc<Type>;

/// Patterns
///
/// We'll be using this to bind variables in our expressions, so we'll derive
/// `BoundPattern` for this type.
///
/// ```text
/// p ::= _             wildcard patterns
///     | x             pattern variables
///     | p : t         patterns annotated with types
/// ````
#[derive(Debug, Clone, BoundPattern)]
pub enum Pattern {
    /// Patterns that bind no variables
    Wildcard,
    /// Patterns that bind variables
    Binder(Binder<String>),
    /// Patterns annotated with types
    ///
    /// `Type` does not implement the `BoundPattern` trait, but we can use
    /// `Embed` to embed it patterns.
    Ann(RcPattern, Embed<RcType>),
}

pub type RcPattern = Rc<Pattern>;

/// Expressions
///
/// ```text
/// e ::= x                             variables
///     | e : t                         expressions annotated with types
///     | \p => e                       anonymous functions
///     | e₁ e₂                         function application
///     | let p₁=e₁, ..., pₙ=eₙ in e    mutually recursive let bindings
/// ````
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Expressions annotated with types
    Ann(RcExpr, RcType),
    /// Anonymous functions (ie. lambda expressions)
    ///
    /// We use the `Scope` type to say that variables in the pattern bind
    /// variables in the body expression
    Lam(Scope<RcPattern, RcExpr>),
    /// Function applications
    App(RcExpr, RcExpr),
    /// Mutually recursive let bindings
    ///
    /// We're getting more complex here, combining `Scope` with `Rec`, `Vec`,
    /// and pairs - check out the examples (under the `/moniker/examples`
    /// directory) to see how we use this in an evaluator or type checker.
    Let(Scope<Rec<Vec<(RcPattern, Embed<RcExpr>)>>, RcExpr>),
}

pub type RcExpr = Rc<Expr>;
```

We can now construct lambda expressions by doing the following:

```rust
// \f : (Base -> Base) => \x : Base => f x
Rc::new(Expr::Lam(Scope::new(
    Rc::new(Pattern::Ann(
        Rc::new(Pattern::Binder(Binder::user("f"))),
        Rc::new(Type::Arrow(
            Rc::new(Type::Base),
            Rc::new(Type::Base),
        )),
    )),
    Rc::new(Expr::Lam(Scope::new(
        Rc::new(Pattern::Ann(
            Rc::new(Pattern::Binder(Binder::user("x"))),
            Rc::new(Type::Base),
        )),
        Rc::new(Expr::App(
            Rc::new(Expr::Var(Var::user("f"))),
            Rc::new(Expr::Var(Var::user("x"))),
        )),
    )))
)))
```

More complete examples, including evaluators and type checkers, can be found in
the [`moniker/examples`](/moniker/examples) directory.

## Usage examples

Moniker is currently used on the following Rust language projects:

- [Pikelet](https://github.com/pikelet-lang/pikelet): A dependently typed
  systems programming language

## Roadmap

Moniker is currently good enough to use for initial language prototypes, but
there is more work that we'd like to do to make it a more fully-featured
name binding and scope handling toolkit.

- [ ] Initial implementation using a locally nameless representation
    - [x] Implement basic type combinators
        - [x] `Embed`
        - [x] `Ignore`
        - [x] `Nest`
        - [x] `Rec`
        - [x] `Scope`
    - [ ] Automatically derive traits
        - [x] `BoundTerm`
        - [x] `BoundPattern`
        - [ ] `Subst`
    - [ ] Allow derives to use identifier types other than `String`
    - [ ] Performance optimizations
        - [ ] Cache max-depth of terms
        - [ ] Cache free variables of terms
        - [ ] Perform multiple-opening/closing
        - [ ] Use visitors
- [ ] Explore implementing other name-binding schemes
    - [ ] Named with unique indices
    - [ ] Scope Graphs
    - [ ] ...?

## References

Here are some interesting references that were helpful in understanding the
locally nameless representation that is used by this crate:

- [How I learned to stop worrying and love de Bruijn indices](http://disciple-devel.blogspot.com.au/2011/08/how-i-learned-to-stop-worrying-and-love.html)
- [The Locally Nameless Representation](https://www.chargueraud.org/research/2009/ln/main.pdf)
- [Locally nameless representation with cofinite quantification](http://www.chargueraud.org/softs/ln/)
- [A Locally-nameless Backend for Ott](http://www.di.ens.fr/~zappa/projects/ln_ott/)

## Inspiration

The API was mainly inspired by the Unbound and Unbound-Generics libraries for
Haskell, with some differences. The main change that we make is to have two
separate traits (`BoundTerm` and `BoundPattern`) in place of Unbound's single
`Alpha` type class. We've found that this better captures the semantics of the
library, and greatly cuts down on the potential for accidental misuse.

Other auto-binding libraries exist for a number of different languages:

- DBLib: Facilities for working with de Bruijn indices in Coq
    - [Blog Post](http://gallium.inria.fr/blog/announcing-dblib/)
    - [Github](https://github.com/coq-contribs/dblib)
- Unbound: Specify the binding structure of your data type with an
  expressive set of type combinators, and Unbound handles the rest!
  Automatically derives alpha-equivalence, free variable calculation,
  capture-avoiding substitution, and more.
    - [Github](https://github.com/sweirich/replib)
    - [Hackage](https://hackage.haskell.org/package/unbound)
- Unbound-Generics: an independent re-implementation of Unbound but using
  GHC.Generics instead of RepLib.
    - [Github](http://github.com/lambdageek/unbound-generics)
    - [Hackage](https://hackage.haskell.org/package/unbound-generics)
- Bound: Bruijn indices for Haskell
    - [Blog Post](https://www.schoolofhaskell.com/user/edwardk/bound)
    - [Github](https://github.com/ekmett/bound/)
    - [Hackage](https://hackage.haskell.org/package/bound)
- The Penn Locally Nameless Metatheory Library
    - [Github](https://github.com/plclub/metalib)
