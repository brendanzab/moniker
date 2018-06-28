# Moniker

[![Build Status][travis-badge]][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/brendanzab/moniker.svg?branch=master
[travis-url]: https://travis-ci.org/brendanzab/moniker
[gitter-badge]: https://badges.gitter.im/moniker-rs/moniker.svg
[gitter-lobby]: https://gitter.im/moniker-rs/Lobby

This crate automatically derives variable binding and alpha equivalence for
abstract syntax trees. This is really handy for eliminating error-prone
boilerplate code for name binding, and make it easier to implement new languages
in Rust.

## Example

```rust
#[macro_use]
extern crate moniker;

use moniker::{Scope, Embed, Name, Var};
use std::rc::Rc;

#[derive(Debug, Clone, BoundTerm)]
pub enum Type {
    Base,
    Arrow(Rc<Type>, Rc<Type>),
}

#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    Var(Var),
    Lam(Scope<(Name, Embed<Rc<Type>>), Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}
```

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
