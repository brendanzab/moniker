# Moniker

[![Build Status][travis-badge]][travis-url]
[![Crates.io][crate-badge]][crate-url]
[![Docs.rs][docs-badge]][docs-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/brendanzab/moniker.svg?branch=master
[travis-url]: https://travis-ci.org/brendanzab/moniker
[crate-url]: https://crates.io/crates/moniker
[crate-badge]: https://img.shields.io/crates/v/moniker.svg
[docs-url]: https://docs.rs/moniker
[docs-badge]: https://docs.rs/moniker/badge.svg
[gitter-badge]: https://badges.gitter.im/brendanzab/moniker.svg
[gitter-lobby]: https://gitter.im/brendanzab/moniker

Moniker aims to alleviate the error-prone boilerplate required to keep track of
bound variables across nested scopes, making it easier to implement new
programming languages and domain-specific languages in Rust.

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
minimal pain! We do this by providing a set of generic types and traits for
describing how variables are bound, that can then be used to automatically
derive the corresponding name-handling code.


## Example

Here is how we would use Moniker to describe a very small functional language,
with variables, anonymous functions, applications, and let bindings:

```rust
#[macro_use]
extern crate moniker;

use moniker::{Embed, Binder, Rec, Scope, Var};
use std::rc::Rc;

/// Expressions
///
/// ```text
/// e ::= x                             variables
///     | \x => e                       anonymous functions
///     | e₁ e₂                         function application
/// ````
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Anonymous functions (ie. lambda expressions)
    ///
    /// We use the `Scope` type to say that variables in the pattern bind
    /// variables in the body expression
    Lam(Scope<Binder<String>, RcExpr>),
    /// Function applications
    App(RcExpr, RcExpr),
}

pub type RcExpr = Rc<Expr>;
```

We can now construct lambda expressions by doing the following:

```rust
// \f => \x => f x
Rc::new(Expr::Lam(Scope::new(
    Binder::user("f"),
    Rc::new(Expr::Lam(Scope::new(
        Binder::user("x"),
        Rc::new(Expr::App(
            Rc::new(Expr::Var(Var::user("f"))),
            Rc::new(Expr::Var(Var::user("x"))),
        )),
    )))
)))
```

### More Complete Examples

More complete examples, including evaluators and type checkers, can be found
under the [`moniker/examples`](/moniker/examples) directory.

| Example Name          | Description                 |
| --------------------- | --------------------------- |
| [`lc`]                | untyped lambda calculus |
| [`lc_let`]            | untyped lambda calculus with nested let bindings |
| [`lc_letrec`]         | untyped lambda calculus with mutually recursive bindings |
| [`lc_multi`]          | untyped lambda calculus with multi-binders |
| [`stlc`]              | simply typed lambda calculus with literals |
| [`stlc_data`]         | simply typed lambda calculus with records, variants, literals, and pattern matching |
| [`stlc_data_isorec`]  | simply typed lambda calculus with records, variants, literals, pattern matching, and iso-recursive types |

[`lc`]: /moniker/examples/lc.rs
[`lc_let`]: /moniker/examples/lc_let.rs
[`lc_letrec`]: /moniker/examples/lc_letrec.rs
[`lc_multi`]: /moniker/examples/lc_multi.rs
[`stlc`]: /moniker/examples/stlc.rs
[`stlc_data`]: /moniker/examples/stlc_data.rs
[`stlc_data_isorec`]: /moniker/examples/stlc_data_isorec.rs

### Projects using Moniker

Moniker is currently used in the following Rust projects:

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
- [abbot][abbot]: Generation of abstract binding trees for SML
- [rabbot][rabbot]: A port of SML's Abbot to Rust
- [DBLib][dblib]: Facilities for working with de Bruijn indices in Coq
- [Bound](https://github.com/ekmett/bound/): DeBruijn indices for Haskell
- [Metalib][metalib]: The Penn Locally Nameless Metatheory Library
- [LibLN][ln]: Locally nameless representation with cofinite quantification

[unbound]: https://github.com/sweirich/replib
[unbound-generics]: https://github.com/lambdageek/unbound-generics
[alphaCaml]: http://pauillac.inria.fr/~fpottier/alphaCaml/alphacaml.html.en
[alphaLib]: https://gitlab.inria.fr/fpottier/alphaLib
[abbot]: https://github.com/robsimmons/abbot
[rabbot]: https://github.com/willcrichton/rabbot
[dblib]: https://github.com/coq-contribs/dblib
[metalib]: https://github.com/plclub/metalib
[ln]: http://www.chargueraud.org/softs/ln/

## Contributing

We really want to encourage new contributors to help out! Please come chat with
us [on our Gitter channel][gitter-lobby] - if you have any questions about the
project, or just want to say hi!

## Acknowledgments

[![YesLogic Logo][yeslogic-logo]][yeslogic]

This work was done in part with the generous support of [YesLogic][yeslogic].

[yeslogic]: http://yeslogic.com/
[yeslogic-logo]: assets/yeslogic-logo.png
