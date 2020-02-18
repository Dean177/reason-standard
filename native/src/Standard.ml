include Core

module Array = struct
  include Core.Array

  let (.?()) (array: 'element t) (index: int) : 'element option = 
    Array.getAt array ~index
end

(* This requires 4.06, but only the Ocaml syntax supports this *)
(* TODO this can be included in the bucklescript branch *)
module Map = struct
  include Core.Map

  let (.?{}) (map: ('key, 'value, _) t) (key: 'key) : 'value option = 
    Core.Map.get map key  

  let (.?{}<-) (map: ('key, 'value, 'id) t) (key: 'key) (value: 'value): ('key, 'value, 'id) t = 
    Core.Map.add map ~key ~value
end

(* This requires 4.06, but only the Ocaml syntax supports this *)
(* TODO this can be included in the bucklescript branch *)
module Set = struct
  include Core.Set

  let (.?{}) (set: ('element, _) t) (element: 'element) : bool = 
    includes set element
end

(* This requires 4.06, but only the Ocaml syntax supports this *)
(* TODO this can be included in the bucklescript branch *)
module String = struct
  include Core.String

  let (.?[]) (string: string) (index: int) : char option = 
    String.getAt string ~index
end

(* This requires 4.08 *)
module Option = struct
  include Core.Option

  let (let+) t f = map t ~f 
  let (and+) t t' = both t t'

  let (let*) t f = bind t ~f 
  let (and*) t t' = bind t ~f:(fun a -> map t' ~f:(fun b -> (a, b)))
end

(* This requires 4.08 *)
module Result = struct
  include Core.Result

  let (let+) t f = map t ~f 
  let (and+) t t' = both t t'

  let (let*) t f = bind t ~f 
  let (and*) t t' = bind t ~f:(fun a -> 
    map t' ~f:(fun b -> (a, b))
  ) 
end

(** The standard library modules shadowed by [Standard] *)
(* module Caml = struct
  (** This module provides access to Ocamls original standard library.

      It means you can still use standard library functions in files which you 
      have done

      {[open Standard]}
  *)
  module Array = Array
  module Char = Char
  module Float = Float
  module Int = Int
  module List = List
  module Option = Option
  (* TODO This requires 4.06 *)
  module Result = Result
  module String = Result
end *)
