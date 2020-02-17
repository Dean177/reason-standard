open Standard
open AlcoJest

let suite = suite "Set - Ocaml Syntax"  (fun () -> (  
  describe ".?[]" (fun () -> (
    let animals = Set.String.ofList ["Bear"; "Wolf"] in

    test "custom index operators can be used in the ocaml syntax"  (fun () -> (
      expect (animals.Set.?{"Bear"})  |> (toEqual Eq.(bool) true)
    ))
  ))
))
