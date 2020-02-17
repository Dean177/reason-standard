(** Module doc for the entry point *)

module Modu = struct
  (** A module for working with {{: https://en.wikipedia.org/wiki/Floating-point_arithmetic } floating-point numbers}. 
  
      Valid syntax for [float]s includes:
      
      {[
        0.
        0.0
        42.
        42.0
        3.14
        0.1234
        123_456.123_456
        6.022e23   (* = (6.022 * 10^23) *)
        6.022e+23  (* = (6.022 * 10^23) *)
        1.602e-19  (* = (1.602 * 10^-19) *)
        1e3        (* = (1 * 10 ** 3) = 1000. *)
      ]}

      Without opening this module you can use the [.] suffixed operators:

      {[ 1.0 +. 2.0 /. 0.25 *. 2.0 = 17.0 ]}

      But by opening this module locally you can use the un-suffixed operators

      {[Float.((10.0 - 1.5 / 0.5) ** 3.0) = 2401.0]}

      {b Historical Note: } The particular details of floats (e.g. [NaN]) are
      specified by {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } which is literally hard-coded into almost all
      CPUs in the world.
  *)

  type 'a t = 'a list

  (** Module Doc *)

  (** A documented function

      With a code sample

      @example {[
        Modu.func [1;2;3] ~initial:0, ~f:(+)
      ]}

      @param list listlist
      @param initial initialinitial
      @param f ff
   *)
  let func (l: 'element t) ~(initial: 'result) ~(f : 'result -> 'element -> 'result) : 'result = 
    List.fold_left f initial l

  (** {1 Deconstruct} 

      A section heading with a little more detail

      {2 Sub-section heading}

      With even more details 
  
  *)

  let head = List.hd

  (** {1 Convert} *)

  let len = List.length
end

module Core = Core

include Included

module ListExtensions = struct
  include Included.ListExtensions

  let (let*) list f = List.map f list |> List.concat
end

module AliasedModule = Fourier

(** Doc *)
let id = fun a -> a