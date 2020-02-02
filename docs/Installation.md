---
title: "Installation"
---

## Bucklescript

Install via npm by

```sh
npm install reason-standard --save
```

Then add to your bsconfig.json file:

`"bs-dependencies" : ["reason-standard"]`

## OCaml native

### Using opam

Install via opam:

```sh
opam install reason-standard
```

Then add to your dune file:

`(libraries (standard ...))`

### Using esy

```sh
esy install reason-standard
```

Then add to your dune file:

`(libraries (standard ...))`
