open Standard
open AlcoJest

let suite = suite "String - Ocaml Syntax"  (fun () -> (  
  let animal = "Salmon" in
  
  describe ".[]" (fun () -> (
    test "regular string syntax is the equivalent to String.get"  (fun () -> (
      expect animal.[0] |> toEqual Eq.char 'S'
    ))
  ));

  describe ".?[]" (fun () -> (
    test "in bounds index returns Some"  (fun () -> (
      expect animal.String.?[1] |> toEqual Eq.(option(char)) (Some 'a')
    ));

    test "out of bounds index returns None"  (fun () -> (
      expect animal.String.?[9] |> toEqual Eq.(option(char)) None
    ))
  ));
))
