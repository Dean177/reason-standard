# standard

> A standard library for use with OCaml and ReasonML.

Standard provides an easy-to-use, comprehensive and performant standard
library, that has the same API for the OCaml and Bucklescript compilers.

**Standard is alpha-quality software.**

**The API will change**

TODO documentation site
TODO put the docs in the readme!

## Installation

### Bucklescript

Install via npm by:

`npm install reason-standard --save`

Then add to your bsconfig.json file:

`"bs-dependencies" : ["reason-standard"]`

### OCaml native

Install via opam:

`opam install reason-standard`

Then add to your dune file:

`(libraries (standard ...))`

## Usage

The recommended way to use Standard is with a top level open. This will ensure all of the built-in modules are reaplced.

```reason
open Standard;

String.toList("somestring")
->List.map(~f=Char.toCode)
->List.map(~f=x => x + 1)
->List.map(~f=Char.fromCode)
->String.fromList
```

Or use the fully qualified names:

```reason
Standard.String.toList("somestring")
->Standard.List.map(~f=Char.toCode)
->Standard.List.map(~f=x => x + 1)
->Standard.List.map(~f=Char.fromCode)
->Standard.String.fromList
```

Since `standard` is still in the early stages and relatively incomplete, you might find it useful to create
a module wrapping standard, that allows you to add new modules or add new functions to existing modules
upstreaming them.

```reason
// Standard.re
include Standard;

module Array = {
  include Standard.Array;

  let functionYouWantToBeInStandard = ...
}
```

## Motivation

We discovered that it was impossible to share code between the Bucklescript
frontend and the native OCaml backend, as the types in the standard libraries were
very different.

The following also contributed additional friction:

- Bucklescripts standard library, [Belt](https://bucklescript.github.io/bucklescript/api/index.html), uses `camelCase` by default, while most native libraries,
  including Core and the OCaml standard library, use snake_case.
- The libraries in Belt have different names and function signatures than native OCaml and Base/Core.
- Many OCaml libraries have APIs optimized for pipelast (`|>`), while Belt aims
  for pipefirst (`->`).
- Core does not work with Bucklescript, while Belt is optimized for the JS
  platform.
- Belt does not work in native OCaml, while Core is optimized for the native
  OCaml runtime.
- Belt is incomplete relative to Core, or to other languages' standard
  libraries (such as [Elm's](https://package.elm-lang.org/packages/elm/core/1.0.2/)).
- Betl is inconsistent

Standard solves this by providing a identical 'core' API for Bucklescript and
OCaml. It utilises existing standard libraries on those platforms, and so is fast
and memory efficient.

Standard provides separate libraries for OCaml native and Bucklescript.

The libraries have the same API, but different implementations, and are installed as different packages.

### Design philosophy

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

TODO nail this

## Contributing

Standard is an ideal library to contribute to, even if you're new to OCaml / Reason.
The maintainers are warm and friendly, and the project abides by a [Code of Conduct](./CODE_OF_CONDUCT.md).
There are many small tasks to be done and even a small change to a single functions documentation is extremely helpful.

Here are some ways to contribute:

- Point out inconsistencies between different functions in the library
- Point out an inelegant function signature which could be improved
- Point out a way in which the library or any of its parts are confusing
- Report an edge-case or performance problem in one of the functions
- Add test cases for a function
- Add documentation to a function
- Improve a function's documentation by discussing an edge-case,
- Check that a function cannot throw exceptions (and add a note to the function documentation to that effect)
- Optimize a function
- Write a benchmark for a function, or a set of functions

If you'd like to contribute but don't know where to start, [open an
issue](https://github.com/Dean177/Standard/issues/new) with your thoughts,
or reach out on [Twitter](https://twitter.com/Dean177) or by
[email](mailto:deanmerchant@gmail.com).

## Developing

If you are new to [OCaml](https://ocaml.org) there are a few prerequisites you will
need to get started:

- Install OCaml and OPAM [based on your OS](https://ocaml.org/docs/install.html)
- Install a current version of [Node](https://nodejs.org/en/) and [yarn](TODO)

Please refer to the `Makefile` for a complete list of supported actions. Here is
a handful of useful, supported commands:

- `make dependencies`: Install dependencies.
- `make build`: Build the project.
- `make test`: Run the test suite. You may need to `make build` first.
- `make documentation`: Build the documentation to browse offline.

## License

Standard uses the [MIT](./LICENSE) license.

```

```
