---
title: "Installation"
metaTitle: "Installation"
metaDescription: "Installing Standard"
order: "0"
---

## Bucklescript

Install via npm by

```sh 11
npm install reason-standard --save
```

Then add to your `bsconfig.json` file:

```json
  "bs-dependencies" : ["reason-standard"]
```

## OCaml native

### Using Opam

```sh
opam install reason-standard
```

Then update the libraries section in your `dune` file:

```clj
(libraries (standard))
```

### Using Esy

```sh
esy add @opam/reason-standard
```

Then update the libraries section in your `dune` file:

```clj
(libraries (standard))
```
