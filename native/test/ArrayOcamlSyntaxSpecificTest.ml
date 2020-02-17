open Standard
open AlcoJest

let suite = suite "Array - Ocaml Syntax"  (fun () -> (  
  let animals = [|"Bear"; "Wolf"|] in
  describe ".()" (fun () -> (
    test "regular array syntax is the equivalent to Array.get"  (fun () -> (
      expect animals.(0) |> toEqual Eq.string "Bear"
    ))
  ));
  describe ".?()" (fun () -> (
    test "in bounds index returns Some"  (fun () -> (
      expect animals.Array.?(1) |> toEqual Eq.(option(string)) (Some "Wolf")
    ));

    test "out of bounds index returns None"  (fun () -> (
      expect animals.Array.?(3) |> toEqual Eq.(option(string)) None
    ))
  ));
))
