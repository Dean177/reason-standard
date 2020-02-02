---
title: "Motivation"
---

We discovered that it was impossible to share code between the Bucklescript
frontend and the native OCaml backend, as the types in the standard libraries were
very different.

The following also contributed additional friction:

- Bucklescripts standard library, [Belt](https://bucklescript.github.io/bucklescript/api/index.html), uses `camelCase` by default, while most native libraries, including Core and the OCaml standard library, use `snake_case`.
- The libraries in Belt have different names and function signatures than native OCaml and Base/Core.
- Many OCaml libraries have APIs optimized for pipelast (`|>`), while Belt aims
  for pipefirst (`->`).
- Core does not work with Bucklescript, while Belt is optimized for the JS
  platform.
- Belt is incomplete relative to Core, or to other languages' standard
  libraries
- Belt is inconsistent

Standard solves this by providing a identical 'core' API for Bucklescript and
OCaml. It utilises existing standard libraries on those platforms, and so is fast
and memory efficient.

Standard provides separate libraries for OCaml native and Bucklescript.

The libraries have the same API, but different implementations, and are installed as different packages.

## Motivation

Ocamls standard library sucks:

- Its spartan
- It has poor documentation
- It has some legacy baggage
- Its T last (TODO is it always?)
- It can only change slowly
- Its inconsistent TODO is it?

Belt is OK:

- It has OK documentation
- It has some legacy baggage
- Its inconsistent TODO is it
- Its not available for native code\* (It is when you are using bucklescript-native)
