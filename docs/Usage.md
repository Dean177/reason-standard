---
title: "Usage"
---


The recommended way to use Standard is with a top level open at the beginning of a file. This will ensure all of the built-in modules are replaced.

```reason
open Standard;

String.toList("somestring")
->List.map(~f=Char.toCode)
->List.map(~f=x => x + 1)
->List.map(~f=Char.fromCode)
->String.fromList
```

If you want to avoid a top level `open` you can use the qualified name instead:

```reason
Standard.String.toList("somestring")
->Standard.List.map(~f=Char.toCode)
->Standard.List.map(~f=x => x + 1)
->Standard.List.map(~f=Char.fromCode)
->Standard.String.fromList
```



With Esy

```json
"esy": {
  "flags": [
    "-open",
    "Base"
  ],
}
```

With Dune


https://dune.readthedocs.io/en/stable/concepts.html#ocaml-flags

```clojure
(flags (:open Standard))
```

```clojure
(library
 (name example-library) 
 (libraries standard zarith)
 (flags (:open Standard)))
 ```

To get started using Batteries at the toplevel, copy the ocamlinit file to ~/.ocamlinit:

\$ cp ocamlinit ~/.ocamlinit
If you already have findlib in your ~/.ocamlinit, you only need the last line in our ocamlinit to load batteries.

More usage help available on the batteries-included wiki.

```ocaml
(* This script starts loading batteries into the ocaml toplevel.
 *
 * To install, copy to your ~/.ocamlinit.  If you already have an
 * ocamlinit file that initializes findlib, just add the last
 * phrase to your ocamlinit.
 *)

(* Pretend to be in non-interactive mode to hide topfind
initialization message *)

let interactive = !Sys.interactive;;
Sys.interactive := false;;
#use "topfind";;
Sys.interactive := interactive;;

(* run battop.ml in toplevel *)

Toploop.use_silently
             Format.err_formatter (Filename.concat (Findlib.package_directory
             "batteries") "battop.ml");;
```
