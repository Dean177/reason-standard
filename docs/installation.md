---
title: "Installation"
metaTitle: "Installation"
metaDescription: "Installing Standard"
order: "0"
---

## Bucklescript

Install via npm by

```sh
npm install reason-standard --save
```

Then add to your bsconfig.json file:

```json
  "bs-dependencies" : ["reason-standard"]
```

## OCaml native

### Using opam

```sh
opam install reason-standard
```

Then update the libraries section in your `dune` file:

```clj
(libraries (standard))
```

### Using esy

```sh
esy install @opam/reason-standard
```

Then add to your dune file:

```
(libraries (standard ...))
```
