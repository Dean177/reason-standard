(** Module doc for core

    This shows how it works

    {[
      open Standard
      let coreExample = List.map [] ~f:(fun element -> element + 1) in
      coreExample = []
    ]}

    Here is a link to {!Fourier.}
 *)

 type 'a t = 'a list

 type 'a undocumented = 'a list

(** This is a doc for a function.

    It links to another value in the same module {!constant}.

    @exception Invalid_argument If [index] is out of bound

    {2 Examples}

    {[
      List.bind [1] ~f:(fun n -> [n;n]) = [1; 1]
    ]}

    @alias flatMap, andThen, concatMap
*)
val bind: 'a list -> ('a -> 'b list) -> 'b list

(** {1 Section Heading} *)

(** Documentation for a constant 

    It refers to a value in another module {!Fourier.pi}
*)
val constant: float

(** Documentation for a another constant 

    It refers to another module {!Fourier}
*)
val another_constant: float

(** {1 Section Heading two} *)

(** Brief explanation of the [Sub] module *)
module Sub : sig  
  (** Detailed explanation of the [Sub] module.

      Here is how you use it

      {[
        Sub.ofList []
      ]}
  
   *)

  (** Doc for a submodule type *)
   type nonrec t = int t

  (** Doc for a submodule function *)
   val ofList: 'a list -> t
end