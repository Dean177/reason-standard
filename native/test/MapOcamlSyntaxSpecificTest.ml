open Standard
open AlcoJest

let suite = suite "Map - Ocaml Syntax"  (fun () -> (  
  let animals = Map.String.ofList [("Bears", 2)] in

  describe ".?{}" (fun () -> (
    test "custom index operators can be used in the ocaml syntax"  (fun () -> (
      expect (animals.Map.?{"Bears"}) |> (toEqual Eq.(option(int)) (Some 2))
    ))
  ));

  describe ".?{}<-" (fun () -> (
    test "custom index operators can be used in the ocaml syntax"  (fun () -> (
      let withWolves = animals.Map.?{"Wolves"} <- 15 in
      expect (Map.toList withWolves) 
      |> (toEqual Eq.(list(pair string int)) [
        ("Bears", 2);
        ("Wolves", 15)
      ])
    ))
  ));
))
