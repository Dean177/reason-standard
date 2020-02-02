---
title: "Design philosophy"
---

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