---
title: "Contributing"
---

## Contributing

Standard is an ideal library to contribute to, even if you're new to OCaml / Reason.

The maintainers are warm and friendly, and the project abides by a [Code of Conduct](./CODE_OF_CONDUCT.md).

There are many small tasks to be done and even a small change to a single functions documentation is extremely helpful.

Here are some ways to contribute:

- Fix a typo or gramatical error
- Point out a way in which the library or any of its parts are confusing
- Point out inconsistencies between different functions in the library
- Report an edge-case or performance problem in one of the functions
- Add test cases for a function
- Add documentation to a function
- Improve a function's documentation by discussing an edge-case,
- Check that a function cannot throw exceptions (and add a note to the function documentation to that effect)
- Optimize a function
- Suggest a new function by [creating an issue](https://github.com/Dean177/Standard/issues/new). You might find it helpful to create a module wrapping `Standard`, that allows you to experiment with new modules or adding new functions to existing modules to see if your new function
- Propose a strategy for benchmarking

```reason
// Standard.re
include Standard;

module Array = {
  include Standard.Array;

  let functionYouWantToBeInStandard = ...
}
```

If you'd like to contribute but don't know where to start, [open an
issue](https://github.com/Dean177/reason-standard/issues/new) with your thoughts
or reach out on [Twitter](https://twitter.com/Dean177) or by
[email](mailto:deanmerchant@gmail.com).

## Guiding principles

### Over-communicate! Create an issue first

If you are planning on removing, changing or adding new features to `Standard`, please open an issue first

### Many small PR's are better than one big one

Don't save up small changes for one big PR if you can avoid it
Less likely to cause conflicts,
Easier to review

### Try not to assume any background knowledge

TODO

### Provide documentation for all levels, beginner to expert

TODO

## Names

- [Don't use abbreviations](TODO)
- If its a super common abbreviation, it needs to be justified and explained in [Conventions.md]
- [Don't be cute](). A name that doesn't help you understand what a function does isn't a very good name. Needing to understand the function to understand the name is the opposite of how things should work.
- Prefer shorter names
- Prefer long names for functions that do something dangerous (that can raise an exception)
- Try to use names that Javascripters / Ocamlers already know.

### Maybe

- Don't be esoteric. (see Don't be cute). TODO What does this actually mean in practice ???
- Use existing names, even if they aren't a perfect fit for the specific scenario (e.g. Array.length, List.length, Map.length, Set.length)???
- Use specific names when the added context makes sense???

## Documentation

Avoid the following:

- simple
- easy
- just

They don't help anyone understand how anything works, but they can make someone who is struggling with a new concept or module feel inadequate.

### Avoid this style of documentation

> `lfindi ?pos t ~f` returns the smallest `i >= pos` such that `f i t.[i]`, if there is such an `i`. By default, `pos = 0`.

TODO Because
