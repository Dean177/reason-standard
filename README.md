# Standard

[![CircleCI](https://circleci.com/gh/Dean177/reason-standard.svg?style=shield)](https://circleci.com/gh/Dean177/reason-standard)
[![Npm](https://badge.fury.io/js/reason-standard.svg)](https://www.npmjs.com/package/reason-standard)
![Opam](https://img.shields.io/badge/opam_package-unpublished-yellow)

> A standard library for use with OCaml and ReasonML.

Standard provides an easy-to-use, comprehensive and performant standard library, that has the same API for the OCaml and Bucklescript compilers.

**Standard is alpha-quality software. The API will change.**

## Installation

### Bucklescript

Install via npm by

```sh
npm install reason-standard --save
```

Then add to your bsconfig.json file:

`"bs-dependencies" : ["reason-standard"]`

### OCaml native

#### Using opam

Install via opam:

```sh
opam install reason-standard
```

Then add to your dune file:

`(libraries (standard ...))`

#### Using esy

```sh
esy install reason-standard
```

Then add to your dune file:

`(libraries (standard ...))`

## Usage

The recommended way to use Standard is with a top level open at the beginning of a file. This will ensure all of the built-in modules are replaced.

```reason
open Standard;

String.toList("somestring")
->List.map(~f=Char.toCode)
->List.map(~f=x => x + 1)
->List.map(~f=Char.fromCode)
->String.fromList
```

If you want to avoid a top level `open` you can use the qualified name instead:

```reason
Standard.String.toList("somestring")
->Standard.List.map(~f=Char.toCode)
->Standard.List.map(~f=x => x + 1)
->Standard.List.map(~f=Char.fromCode)
->Standard.String.fromList
```

## Documentation

Refer the the interface file for either [bucklescript](./bucklescript/src/Standard.rei) or the [native](./native/src/Standard.rei) targets

If you have cloned the repository

```sh
esy doc
open ${esy doc-path}
```

## Contributing

Standard is an ideal library to contribute to, even if you're new to OCaml / Reason.

The maintainers are warm and friendly, and the project abides by a [Code of Conduct](./CODE_OF_CONDUCT.md).

There are many small tasks to be done and even a small change to a single functions documentation is extremely helpful.

Here are some ways to contribute:

- Fix a typo or gramatical error
- Point out a way in which the library or any of its parts are confusing
- Point out inconsistencies between different functions in the library
- Report an edge-case or performance problem in one of the functions
- Add test cases for a function
- Add documentation to a function
- Improve a function's documentation by discussing an edge-case,
- Check that a function cannot throw exceptions (and add a note to the function documentation to that effect)
- Optimize a function
- Suggest a new function by [creating an issue](https://github.com/Dean177/Standard/issues/new). You might find it helpful to create a module wrapping `Standard`, that allows you to experiment with new modules or adding new functions to existing modules to see if your new function
- Propose a strategy for benchmarking

```reason
// Standard.re
include Standard;

module Array = {
  include Standard.Array;

  let functionYouWantToBeInStandard = ...
}
```

If you'd like to contribute but don't know where to start, [open an
issue](https://github.com/Dean177/reason-standard/issues/new) with your thoughts
or reach out on [Twitter](https://twitter.com/Dean177) or by
[email](mailto:deanmerchant@gmail.com).

## Developing

There are a few prerequisites you will need to get started:

Install a current version of:

- [Node](https://nodejs.org/en/)
- [yarn](http://yarnpkg.com/)
- [esy](http://esy.sh)

## License

Standard uses the [MIT](./LICENSE) license.
