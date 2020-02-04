---
title: "Standard"
---

![](./is_it_worth_the_time.png)

Standard provides an easy-to-use, comprehensive and performant standard library, that has the same API for the OCaml and Bucklescript compilers.

```reason
open Standard;

String.toList("somestring")
->List.filterMap(~f=character => 
  Char.toCode(character)->Int.add(1)->Char.ofCode
)
->String.ofList
```

