---
title: "Documentation"
order: "9"
---

The documentation in Standard strives for simplicity and regularity. 

It should be easy for readers of all ability levels to glance through a file and find the information they need.

### Documenting a module

Here is the module documentation for the Maybe library:

module Maybe exposing (Maybe(Just,Nothing), andThen, map, withDefault, oneOf)

```
{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}
```
This represents the text that actually gets displayed as the documentation for a module. Notice that:

The module lists what values are exported. Maybes are not an opaque type, as the tags are exported as well.
The module documentation comes after the module declaration, but before the imports. This is so the first thing in the file is the module name and the second is how to use it.
The first line starts after a single space, and all subsequent lines are aligned all the way to the left.
The @docs keyword precedes a comma-separated list of values that are inlined in the resulting documentation.
Functions are grouped into related units with titles, declared in Markdown. Sometimes it's appropriate to put a little text under the title associated with the group but not any one function.
Although documentation for each function should be self-contained, things are ordered intelligently. Assume people will read through linearly, so try to make the document structure ideal for learning the API. You need to understand the Maybe data type to understand anything else, so it appears first. withDefault is an important function so it appears early on. And so forth.
Again, the goal is to have consistency, so readers can glance through easily and writers do not need to argue about style.

Finally, modules need to be listed as exposed in elm.json. Some of the compiler's rules about the documentation format will only be enforced for these modules.

### Documenting a value

Here is an example from the String library:

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    ofList ['e','l','m'] == "elm"
-}
ofList : List Char -> String
ofList = ...
Notice that:

A documentation comment starts {-| and ends with -}. The vertical bar indicates that it is a special comment.
The text begins after a single space, and all subsequent lines are aligned all the way to the left.
There is an example that shows a typical use of the function.
This example is indented four spaces.
The comment is closed on its own line.
There is an explicit type annotation.
For publicly exposed functions, type annotations and comments are required, and examples are best practice. The goal is to have consistency across all codebases, so readers can glance through easily and writers do not need to argue about style.

## Design guidelines 

These guidelines are meant to promote consistency and quality across all Elm packages. 
It is a collection of best practices that will help you write better APIs and your users write better code. Here is the overview, but it is important to read through and see why these recommendations matter.

### Design for a concrete use case

Before you start writing a library, think about what your goal is.

What is the concrete problem you want to solve?
What would it mean for your API to be a success?
Who has this problem? What do they want from an API?
What specific things are needed to solve that problem?
Have other people worked on this problem? What lessons can be learned from them? Are there specific weaknesses you want to avoid?
Actually think about these things. Write ideas on paper. Ask for advice from someone you trust. It will make your library better. If you are doing this right, you will have example code and a tentative API before you start implementing anything.

### Avoid gratuitous abstraction

Some functional programmers like to make their API as general as possible. This will reliably make your API harder to understand. Is that worth it? What concrete benefits are users gaining? Does that align with the concrete use case you have in mind for your library?

Abstraction is a tool, not a design goal. Unless abstraction is making someone’s life easier, it is not a good idea. If you cannot demonstrate why your abstraction is helpful, there is a problem with your API.

### Write helpful documentation with examples

This document describes how documentation works in Elm, and you can preview your docs here.

Providing examples of common uses is extremely helpful. Do it! The standard libraries all make a point to have examples that show how one should be using those functions.

Also, make the documentation for the library itself helpful. Perhaps have an example that shows how to use many functions together, showcasing the API.

Finally, think hard about the order that the functions appear in and what kind of title each section gets. People will read documentation linearly when learning a library, so give them some structure!

### The data structure is always the first argument

Function composition works better when the data structure is the last argument:

getCombinedHeight people =
    people
      |> map .height
      |> foldl (+) 0
Folding also works better when the data structure is the last argument of the accumulator function. All foldl and foldr functions work this way:

-- Good API
remove : String -> Dict String a -> Dict String a

filteredPeople =
    foldr remove people ["Steve","Tom","Sally"]


-- Bad API
without : Dict String a -> String -> Dict String a

filteredPeople =
    foldr (flip without) people ["Steve","Tom","Sally"]
The order of arguments in fold is specifically intended to make this very straight-forward: the data structure is always the last argument.

### Naming

#### Use human readable names

Use human readable names

Abbreviations are generally a silly idea for an API. Having an API that is clear is more important than saving three or four characters by dropping letters from a name.

#### Module names should not reappear in function names

A function called State.runState is redundant and silly. More importantly, it encourages people to use import State exposing (..) which does not scale well. In files with many so-called "unqualified" dependencies, it is essentially impossible to figure out where functions are coming from. This can make large code bases impossible to understand, especially if custom infix operators are used as well. Repeating the module name actively encourages this kind of unreadable code.

With a name like State.run the user is encouraged to disambiguate functions with namespacing, leading to a codebase that will be clearer to people reading the project for the first time.
