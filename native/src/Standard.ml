include Core

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