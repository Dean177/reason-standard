---
title: "Usage"
order: "1"
---

The recommended way to use Standard is with a top level open at the beginning of a file. 

This will ensure all of the built-in modules are replaced.

```reason
open Standard;

String.toList("somestring")
->List.filterMap(~f=character => 
  Char.toCode(character)->Int.add(1)->Char.ofCode
)
->String.ofList
```

## Automatic opening

To avoid having to write `open Standard` at the top of every file, you can pass a compiler flag to do this automatically for you. How this is configured depends on your build system.

### With Bucklescript

In `bsconfig.json` edit the `bsc-flags` array to look like the following:

```json
"bsc-flags": [
  "-open", 
  "Standard",
  // ...
]
```

### With Esy

In `package.json` / `esy.json` edit the `esy.flags` array to look like the following:

```json
"esy": {
  "flags": [
    "-open",
    "Standard"
  ],
}
```

### With Dune

https://dune.readthedocs.io/en/stable/concepts.html#ocaml-flags

```clojure
(library
 (name example-library) 
 (libraries standard zarith)
 (flags (:open Standard)))
 ```
 
