---
title: "Motivation"
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
