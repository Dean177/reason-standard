---
title: "Conventions"
order: "2"
---

## t is the main type of a module

In Reason / Ocaml its a convention for the primary type of a module to be named `t`. 

This means that when you want to refer to a type without `open`ing a module you don't end up repeating yourself:

```reason
let food: String.string = /* ... */

/* compared to */ 

let email: String.t = /* ... */
```

Since this is pervasive it also means your module can be used with [Functors](http://dev.realworldocaml.org/functors.html), as they almost always adhere to this convention.

## f is for function

Functions which take a function as an argument will almost always be a labelled argument named `f`.

Take [`Array.map`](/api#Array.map) as an example:

```reason
let map: (array('a), ~f:('a => 'b)) => array('b);
```

This means that its easy to use these functions either applying all of the arguments:

```reason
Array.map([|1,2,3|], ~f=(number) => number * 3);
```

or by chaining functions together using [`|>`](/api#Fun.pipe)

```reason
Array.filter([|1,2,3|], ~f=Int.isOdd)
|> Array.map(~f=(number) => number * 3);
```

## exn is for exception 

The type for exceptions in Ocaml is actually called `exn`.

You can read more about exceptions in [Real World Ocaml](http://dev.realworldocaml.org/error-handling.html#scrollNav-2) or in the [Ocaml manual](https://caml.inria.fr/pub/docs/manual-ocaml/coreexamples.html#s:exceptions).

## sep is for separator

[`Array.intersperse`](/api#Array.intersperse), [`Array.join`](/api#Array.join) and their [`List`](/api#List) counterparts all take an element that will be used as a seperator. 

Since this is a bit of a mouthful for a pretty commonly used function it gets shortened to `sep`.

## Function suffixes

Some functions come in multiple flavors. 

Some of these flavors are so common they are distinguished by a really short suffix.

### ___2 is an alternative behaviour

When a function could behave in slightly different ways, but we want to provide the functionality of both, one of the implementations gets a two stuck on the end. 

The best example of this is [`Float.atan2`](/api#Float.atan2)

### ___I is for "with index"

[`Array.map`](/api#Array.map) and [`Array.mapI`](/api#Array.mapI),  are almost exactly the same, except `mapI`, in addition to being called with each element, also _gets called with the elements index_. 

### ___Unsafe means "could raise an exception" 

Some functions have 'unsafe' versions which instead of returning an [`Option`](/api#Option) or a [`Result`](/api#Result) could raise an exception. 

Sometimes this can be for performance, and sometimes you just need an escape hatch.

See [`Option.get`](/api#Option.get) and [`Option.getUnsafe`](/api#Option.getUnsafe)

## Modules

### S is for signature

Functions which accept first class modules or [Functors]() need a way to label their arguments. 

In a similar way to modules primary type being [named `t`](#t-is-the-main-type-of-n-module) 

## Data comes first

In allmost all of the functions that `Standard` provides, the datastructure the 
function is operating on will be the first positional argument.

This is the opposite of the way the standard library, or other related languages 
like [elm](https://package.elm-lang.org/help/design-guidelines#the-data-structure-is-always-the-last-argument) 
or [haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.13.0.0/GHC-List.html) 
tend to do things, but for some good reasons:

### Better error messages

The type of the

```reason
let words = ["hi"]
let res = List.map(n => n + 1, words)
//                             ^^^^^
// This expression has type string list
//  but an expression was expected of type Base.Int.t list
//  Type string is not compatible with type Base.Int.t = int
```

```reason
let words = ["hi"]
let res = Standard.List.map(~f:n => n + 1, words)
//                                  ^
// This expression has type string but an expression was expected of type int
```

### Better type inference

Say we have a module `Book`
```reason
module Book = {
  type t = {
    isbn: string,
    title: string,
  };

  let classics = [
    { isbn: "9788460767923", title: "Moby Dick or The Whale" }
  ];
}
```

With a data-last approach we end up needing to provide additional annotations when we use [List.map](/api#List.map):

```reason
let isbns = List.map(book => book.isbn, Book.classics);
/*
  The record field isbn can't be found.
  If it's defined in another module or file, bring it into scope by:
    - Annotating it with said module name:
    - Or specifying its type:
*/
```
But with data-first

```reason
open Standard;

let isbns = List.map(books, ~f=book => book.isbn, Book.classics);
/* 👍 */
```


### More intuitive, consistent APIs

Consider appending one array to another.

With data last

```reason
let append = (arrayToAppend, data) => /* ... */;

append([|1, 2, 3|], [|4, 5, 6|]) = [|4, 5, 6, 1, 2, 3|]

/* The standard library diverges from its usual data-last
   convention and implements this function data-first */
```

With data first

```reason
open Standard

Array.append([|1, 2, 3|], [|4, 5, 6|]) = [|1, 2, 3, 4, 5, 6|]
```

This section was heavily inspired by [Javier Chavarri's excellent blog post](https://www.javierchavarri.com/data-first-and-data-last-a-comparison/) and the discussions on the [Bucklescipt](https://github.com/BuckleScript/bucklescript/issues/2625) and [Reason](https://github.com/facebook/reason/issues/1452#issuecomment-350424873) GitHub issue trackers.
