include Core

module Array = struct
  include Core.Array

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Core.Array.get} 

      {b Note} Currently this is only supported by the Ocaml syntax.
  
      {2 Examples}

      {[Array.([||].?(3)) = Some 'g']}

      {[Array.([||].?(9)) = None]}
   *)
  let (.?()) (array: 'element t) (index: int) : 'element option = 
    Array.getAt array ~index
end

module Map = struct
  include Core.Map

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Core.Map.get} 

      {b Note} Currently this is only supported by the Ocaml syntax.
  
      {2 Examples}

      {[
        let indexToAnimal = Map.Int.fromList [(1, "Ant");(2, "Bat");(3, "Cat")] in
        indexToAnimal.Map.?{3} = Some "Cat"
      ]}
   *)
  let (.?{}) (map: ('key, 'value, _) t) (key: 'key) : 'value option = 
    Core.Map.get map key  

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Core.Map.add} 
  
      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {2 Examples}

      {[
        let indexToAnimal = Map.Int.fromList [(1, "Ant");(2, "Bat");(3, "Cat")] in
        let indexToAnimal = numbers.Map.?{4} <- "Dog" in
        indexToAnimal.Map.?{4} = Some "Dog"
      ]}
   *)
  let (.?{}<-) (map: ('key, 'value, 'id) t) (key: 'key) (value: 'value): ('key, 'value, 'id) t = 
    Core.Map.add map ~key ~value
end

module Set = struct
  include Core.Set

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Core.Array.includes} 
  
      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {2 Examples}

      {[
        let animals = Set.String.fromList ["Ant"; "Bat"; "Cat"] in
        numbers.Set.?{"Emu"} = false
      ]}
   *)
  let (.?{}) (set: ('element, _) t) (element: 'element) : bool = 
    includes set element
end

module String = struct
  include Core.String

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Core.String.get} 

      {b Note} Currently this is only supported by the Ocaml syntax.
  
      {2 Examples}

      {[("Doggie".String.?[3]) = Some 'g']}

      {[String.("Doggie".?[9]) = None]}
   *)
  let (.?[]) (string: string) (index: int) : char option = 
    String.getAt string ~index
end

module Option = struct
  include Core.Option

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Option.map}

      {b Note} Currently this is only supported by the Ocaml syntax.

      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (let+) (t : 'a t) (f : 'a -> 'b) : 'b t = map t ~f 

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Option.both}

      {b Note} Currently this is only supported by the Ocaml syntax.

      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (and+) (t : 'a t) (t' : 'b t) : ('a * 'b) t = both t t'

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Option.bind}

      {b Note} Currently this is only supported by the Ocaml syntax.

      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (let*) (t : 'a t) (f : 'a -> 'b t) : 'b t =  
    bind t ~f 


  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Option.both}

      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (and*) (t : 'a t) (t' : 'b t) : ('a * 'b) t =
    both t t'
end

(* This requires 4.08 *)
module Result = struct
  include Core.Result

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Result.map}

      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (let+) (t : ('a, 'error) t) (f : 'a -> 'b) : ('b, 'error) t = 
    map t ~f 

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Result.both}

      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (and+) (t : ('a, 'error) t) (t' : ('b, 'error) t) : (('a * 'b), 'error) t =
    both t t'

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Result.bind}

      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (let*) (t : ('a, 'error) t) (f : 'a -> ('b, 'error) t) : ('b, 'error) t = 
    bind t ~f 

  (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html } binding operator} 
      for {!Core.Result.both}

      {b Note} Currently this is only supported by the Ocaml syntax.
      
      {b Note} This requires at least Ocaml 4.08 which means currently this is 
      only supported by the native compiler.
  *)
  let (and*) (t : ('a, 'error) t) (t' : ('b, 'error) t) : (('a * 'b), 'error) t = 
    both t t'  
end