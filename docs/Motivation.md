---
title: "Motivation"
order: "3"
---

## Where are all the functions

Ocamls standard library is a sparse and moves very slowly.

## Modules, Modules, Modules

Ocaml has a great module system, but the standard library doesn't make great use of it.
Almost everything in Standard lives in module which means functions like 
`string_of_int` or `float_to_int` are now available in `Int.toString` or `Float.toInt`. 

## Bucklescript lives in the browser

We discovered that it was impossible to share code between the Bucklescript
frontend and the native OCaml backend, as the types in the standard libraries were
very different.

## How does it work

Both bucklescript, belt and ocamls standard library suffer in the documentation 
department.


## Friction

- While Ocaml is clearly missing a trick using `snake_case` (OSnakeml?) most native libraries use snake case. 
  Bucklescripts standard library, [Belt](https://bucklescript.github.io/bucklescript/api/index.html), uses `camelCase` by default.
  while most native libraries, including Core and the OCaml standard library, use `snake_case`. 

- The libraries in Belt have different names and function signatures than native OCaml and Base/Core.
- Many OCaml libraries have APIs optimized for pipelast (`|>`), while Belt aims
  for pipefirst (`->`).
- Core does not work with Bucklescript, while Belt is optimized for the JS
  platform.
- Belt is incomplete relative to Core, or to other languages' standard
  libraries
- Belt is inconsistent

Standard solves this by providing an identical 'core' API for Bucklescript and
OCaml. It utilises existing standard libraries on those platforms, and so is fast
and memory efficient.

Standard provides separate libraries for OCaml native and Bucklescript.

The libraries have the same API, but different implementations, and are installed as different packages.

## Motivation

Ocamls standard library sucks:

- It has poor documentation
- It has some legacy baggage
- Its T last (TODO is it always?)
- Its inconsistent TODO is it?
- It can only change slowly

Belt is OK:

- It has OK documentation
- It has some legacy baggage
- Its inconsistent TODO is it
- Its not available for native code\* (It is when you are using bucklescript-native)

- High quality documentation and examples
- Consistency
- Well-documented and consistent edge-case behaviour,
- No name mangling
- Standard functions should not throw any exceptions
- Batteries included
- Functions are data-first TODO explain what this means
- Takes inspiration from the standard libraries of Rust, Elm and Go
- Make extensive use of labelled arguments
- use labelled arguments so that can be used with both pipefirst and pipelast,
- Performance
- have both snake_case and camelCase versions of all functions and types,
- are backed by [Jane Street Base](https://opensource.janestreet.com/base/) for native OCaml
- are backed by Belt and the `Js` library for Bucklescript/ReasonML,


- Can it easily be made from existing functions -> Don't include it
- Are people always making that custom function -> Do include it

TODO nail this

- Amazing documentation & examples
- Easy to use and learn
- Consistency
- Find a balance between batteries included and bare essentials
- Providing the same basic API for both ocaml and bucklescript
- Provide platoform specific features where it makes sense
- Strike a balance between JS and Ocaml conventions
- Safety. Exceptions that can throw are not the default, well documented
- Pragmatism over purity. While Foldable Magmas are pretty,
- Portability

Draw inspiration from

- [ImmutableJS](https://immutable-js.github.io/immutable-js/)
- [Lodash](https://lodash.com/docs)
- [Base](https://ocaml.janestreet.com/ocaml-core/latest/doc/base/index.html)
- [Batteries](http://ocaml-batteries-team.github.io/batteries-included/hdoc2/)
- [Containers](https://c-cube.github.io/ocaml-containers/)
- [Go](https://golang.org/pkg/#stdlib)
- [Rust](https://doc.rust-lang.org/std/)
- [Elm](https://package.elm-lang.org/packages/elm/core/latest/)
- [Elixir](https://hexdocs.pm/elixir/Kernel.html)

