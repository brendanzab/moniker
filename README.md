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

## Motivation

It's interesting to note that the idea of a 'scope' comes up quite often in
programming:

- anonymous functions
- case match arms
- let bindings
- recursive functions and data types
- type parameters
- type definitions
- ...and more!

'Pattern's introduce 'binders', that may then refer to 'variables' referred to
in a 'term'. We refer to the combination of a pattern and the term it binds as a
'scope'.

For example, let's take a Rust implementation of the identity function:

```rust
fn ident<T>(value : T) -> T {
    value
}

ident(23)
```

Here we have three 'binders', and four 'variable' usages:

```rust
// binder
//   |
//   | binder      var   var
//   |   |          |     |
//   |   | binder   |     |
//   |   |   |      |     |
//   v   v   v      v     v
fn ident<T>(value : T) -> T {
    value
//    ^
//    |
//   var
}
// var
// |
// v
ident(23)
```

Here's how it would look if we drew arrows from the binders to the
corresponding variables:

```rust
//   .--------------------------.
//   |   .----------*-----.     |
//   |   |          |     |     |
//   |   |          v     v     |
fn ident<T>(value : T) -> T {// |
//            |                 |
//    .-------'                 |
//    |                         |
//    v                         |
    value //                    |
} //                            |
// .----------------------------'
// |
// v
ident(23)
```

Here's a more complex example:

```rust
type Count = u32;

fn foo<T>((count, data): (Count, T)) -> T {
    match count {
        0 => data,
        count => foo((count - 1, data)),
    }
}
```

And here is a graph of the binders and variables at play:

```rust
//            ?
//            |
//            v
type Count = u32;
//     |
//     '--------------------.
//  .-----------------------|------------------.
//  |  .--------------------|----*------.      |
//  |  |                    |    |      |      |
//  |  |                    v    v      v      |
fn foo<T>((count, data): (Count, T)) -> T { // |
//          |       |                          |
//          |       |                          |
//          |       *--------------.           |
//          v       |              |           |
    match count { //|              |           |
//             .----'              |           |
//             |                   |           |
//             v                   |           |
        0 => data, //              |           |
//         .------------.          |           |
//         |            |          |           |
//         |            v          v           |
        count => foo((count - 1, data)), //    |
//                ^                            |
//                |                            |
//                '----------------------------'
    }
}
```

Keeping track of the relationships between these variables can be a pain, and
can become especially error-prone when then going on to implement evaluators and
type checkers. Moniker aims to support all of these binding structures, with
minimal pain!

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
    - [ ] Implement namespaced variables and binders
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

Here is a list of interesting references and prior art that was helpful when
building Moniker. Note that it isn't necessary to read and understand these to
use the library, but they _might_ be useful if you would like to contribute!

### Papers

- [The Locally Nameless Representation](https://www.chargueraud.org/research/2009/ln/main.pdf)
- [Binders Unbound](http://www.seas.upenn.edu/~sweirich/papers/icfp11.pdf)
- [An overview of Cαml](http://pauillac.inria.fr/~fpottier/publis/fpottier-alphacaml.pdf)
- [Visitors Unchained](http://gallium.inria.fr/~fpottier/publis/fpottier-visitors-unchained.pdf)
- [Engineering Formal Metatheory](http://www.chargueraud.org/research/2007/binders/binders_popl_08.pdf)

### Blog Posts

- [How I learned to stop worrying and love de Bruijn indices](http://disciple-devel.blogspot.com.au/2011/08/how-i-learned-to-stop-worrying-and-love.html)
- [Announcing DBLib](http://gallium.inria.fr/blog/announcing-dblib/)
- [Bound](https://www.schoolofhaskell.com/user/edwardk/bound)

### Other Libraries

The API was mainly inspired by the [Unbound][unbound] and
[Unbound-Generics][unbound-generics] libraries for Haskell, with some
differences. The main change that we make is to have two separate traits
(`BoundTerm` and `BoundPattern`) in place of Unbound's single `Alpha` type
class. We've found that this better captures the semantics of the library, and
greatly cuts down on the potential for accidental misuse.

Other auto-binding libraries exist for a number of different languages:

- [Unbound][unbound]: Specify the binding structure of your data type with an
  expressive set of type combinators, and Unbound handles the rest!
  Automatically derives alpha-equivalence, free variable calculation,
  capture-avoiding substitution, and more.
- [Unbound-Generics][unbound-generics]: an independent re-implementation of
  Unbound but using GHC.Generics instead of RepLib.
- [Cαml][alphaCaml]: a tool that turns a so-called "binding specification" into
  an OCaml compilation unit.
- [alphaLib][alphaLib]: An OCaml library that helps deal with binding constructs
  in abstract syntax trees.
- [DBLib][dblib]: Facilities for working with de Bruijn indices in Coq
- [Bound](https://github.com/ekmett/bound/): DeBruijn indices for Haskell
- [Metalib][metalib]: The Penn Locally Nameless Metatheory Library
- [LibLN][ln]: Locally nameless representation with cofinite quantification

[unbound]: https://github.com/sweirich/replib
[unbound-generics]: https://github.com/lambdageek/unbound-generics
[alphaCaml]: http://pauillac.inria.fr/~fpottier/alphaCaml/alphacaml.html.en
[alphaLib]: https://gitlab.inria.fr/fpottier/alphaLib
[dblib]: https://github.com/coq-contribs/dblib
[metalib]: https://github.com/plclub/metalib
[ln]: http://www.chargueraud.org/softs/ln/
