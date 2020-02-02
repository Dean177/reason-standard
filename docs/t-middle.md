---
title: "Alternatives"
---
https://www.javierchavarri.com/data-first-and-data-last-a-comparison/
https://github.com/BuckleScript/bucklescript/issues/2625

## Better error messages

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

## Better type inference

```reason
module Book = {
  type t = {
    isbn: string,
    title: string,

  }

  let classics = [{ isbn: "9788460767923", title: "Moby Dick or The Whale" }];
}
```

```reason
// With t-last
let isbns = List.map(book => book.isbn, Book.classics);
```

```
The record field isbn can't be found.
If it's defined in another module or file, bring it into scope by:
  - Annotating it with said module name:
  - Or specifying its type:
```

```reason
// With t-first
let isbns = Belt.List.map(books, book => book.isbn, Book.classics);
```

```reason
// With t-first and a labelled argument
let isbns = Standard.List.map(~f:book => book.isbn, Book.classics);
```

## Consistency

- Its easier to be consistent

## More intuitive APIs for left-to-right readers

Array.append

```
append(a, as) => [a, ...as]
```

```
append(as, a) => [|...as, a|]
```

# Cons

- Composition
- Mitigated by named arguments
- Integration with optional parameters
