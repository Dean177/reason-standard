---
title: "Developing"
order: "6"
---

There are a few prerequisites you will need to get started:

Install a current version of:

- [Node](https://nodejs.org/en/)
- [yarn](http://yarnpkg.com/)
- [esy](http://esy.sh)


## bucklescript

In the `bucklescript` directory run 

```sh
yarn install
```

Then see `scripts` in `bucklescript/package.json`

## native

In the `native` directory run 

```sh
esy install
```

Then see `scripts` in `native/package.json`

## website

In the `website` directory run 

```sh
yarn install
```

Then see `scripts` in `website/package.json`


## Running tests

The tests files are located in `bucklescript/test` and **run against both platforms**.

They are run against the native codebase via some dune rules in `native/test/dune`.

