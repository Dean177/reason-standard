  (**
    Arbitrary precision integers.

    Backed by {{: https://github.com/ocaml/Zarith } Zarith } when targetting native and uses the {{: https://github.com/GoogleChromeLabs/babel-plugin-transform-jsbi-to-bigint } JSBI polyfill } when targetting Javascript.

    For Javascript platforms {{: https://caniuse.com/#feat=bigint } which support } {{: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt } BigInt } there is a {{: https://github.com/GoogleChromeLabs/babel-plugin-transform-jsbi-to-bigint } babel plugin } which removes the polyfill.
  *)
  type t
  
  (** {1 Create} *)

  (** Create an {!Integer} from an {!Int} *)
  val ofInt : int -> t
  
  (** Create an {!Integer} from an Int64 *)
  val ofInt64 : Int64.t -> t
  
  (** Create an {!Integer} from an Int64.

      Returns [None] when called with {!Float.nan}, {!Float.infinity} or {!Float.negativeInfinity}
  *)
  val ofFloat : float -> t option
  
  (** Attempt to parse a {!String} into a {!Integer}.

      {2 Examples}

      {[Integer.(ofString "0" = Some (ofInt 0))]}

      {[Integer.(ofString "42" = Some (ofInt 42))]}

      {[Integer.(ofString "-3" = Some (ofInt -3))]}

      {[Integer.(ofString "123_456" = Some (ofInt 123_456))]}

      {[Integer.(ofString "0xFF" = Some (ofInt 255))]}

      {[Integer.(ofString "0x00A" = Some (ofInt 10))]}

      {[Integer.(ofString "Infinity" = None)]}

      {[Integer.(ofString "NaN" = None)]}
  *)
  val ofString : string -> t option
  
  (** {1 Constants } *)

  (** The literal [0] as a named value *)
  val zero : t
  
  (** The literal [1] as a named value *)
  val one : t
  
  (** {1 Operators } *)

  (** Add two {!Integer}s.

      {2 Examples}

      {[Integer.(add (ofInt 3002) (ofInt 4004) = ofInt 7006)]}

      Or using the operator:

      {[Integer.((ofInt 3002) + (ofInt 4004) = (ofInt 7006))]}
  *)
  val add : t -> t -> t
  
  (** See {!Integer.add} *)
  val (+) : t -> t -> t
  
  (** Subtract numbers

      {2 Examples}

      {[Integer.(subtract one one = zero)]}

      Alternatively the operator can be used:

      {[Integer.((ofInt 4) - (ofInt 3) = one)]}
  *)
  val subtract : t -> t -> t
  
  (** See {!Integer.subtract} *)
  val (-) : t -> t -> t
  
  (** Multiply two integers

      {2 Examples}

      {[Integer.(multiply (ofInt 2) (ofInt 7) = (ofInt 14))]}

      Alternatively the operator can be used:

      {[Integer.((ofInt 2) * (ofInt 7) = ofInt 14)]}
  *)
  val multiply : t -> t -> t
  
  (** See {!Integer.multiply} *)
  val ( * ) : t -> t -> t
  
  (** Integer division

      Notice that the remainder is discarded.

      {3 Exceptions}

      Throws [Division_by_zero] when the divisor is [zero].

      {2 Examples}

      {[
        Integer.(divide (ofInt 3) ~by:(ofInt 2) = (ofInt 1))
      ]}

      {[
        Integer.((ofInt 27) / (ofInt 5) = (ofInt 5))
      ]}
  *)
  val divide : t -> by:t -> t
  
  (** See {!Integer.divide} *)
  val (/) : t -> t -> t
  
  (** Exponentiation, takes the base first, then the exponent.

      Alternatively the [**] operator can be used.

      {2 Examples}

      {[
        Integer.(
          power ~base:(ofInt 7) ~exponent:3 ~modulo:(ofInt 300) = ofInt 43
        )
      ]}

      {[
        Integer.(
          (ofInt 7) ** 4 = ofInt 2401
        )
      ]}
  *)
  val power : ?modulo:t -> base:t -> exponent:int -> t
  
  (** See {!Integer.power} *)
  val ( ** ) : t -> int -> t
  
  (** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

      {2 Examples}

      {[
        Integer.(
          assert (negate (ofInt 8) = ofInt -8);
          assert (negate (ofInt -7) = ofInt 7);
          assert (negate zero = zero)
        )
      ]}
  *)
  val negate : t -> t
  
  (** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

      {2 Examples}

      {[
        Integer.(
          assert (absolute 8 = 8);
          assert (absolute (-7) = 7);
          assert (absolute 0 = 0);
        )
      ]}
  *)
  val absolute : t -> t
  
  (** Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

      If you intend to use [modulo] to detect even and odd numbers consider using {!Integer.isEven} or {!Integer.isOdd}.

      Our [modulo] function works in the typical mathematical way when you run into negative numbers

      Use {!Integer.remainder} for a different treatment of negative numbers.

      {2 Examples}

      {[
        Integer.(

          let three = ofInt 3 in
          let two = ofInt 2 in

          assert (modulo three ~by:three = zero);
          assert (modulo two ~by:three = two);
          assert (modulo one ~by:three = one);
          assert (modulo zero ~by:three = zero);
          assert (modulo (negate one) ~by:three = one);
          assert (modulo (negate two) ~by:three = two);
          assert (modulo (negate three) ~by:three = zero)
        )
      ]}
  *)
  val modulo : t -> by:t -> t

  (** See {!Integer.modulo} *)
  val (mod) : t -> t -> t
  
  (** Get the remainder after division. Here are bunch of examples of dividing by four:

      Use {!Integer.modulo} for a different treatment of negative numbers.

      {2 Examples}

      {[
        Integer.(
          let three = ofInt 3 in
          let two = ofInt 2 in

          assert (remainder three ~by:three = zero);
          assert (remainder two ~by:three = two);
          assert (remainder one ~by:three = one);
          assert (remainder zero ~by:three = zero);
          assert (remainder (negate one) ~by:three = (negate one));
          assert (remainder (negate two) ~by:three = (negate two));
          assert (remainder (negate three) ~by:three = zero)
        )
      ]}
  *)
  val remainder : t -> by:t -> t
  
  (** Returns the larger of two [Integers]s

      {2 Examples}

      {[Integer.(maximum (ofInt 7) (ofInt 9) = (ofInt 9))]}

      {[Integer.(maximum (ofInt -4) (ofInt -1) = (ofInt -1))]}
  *)
  val maximum : t -> t -> t
  
  (** Returns the smaller of two [Integers]s

      {2 Examples}

      {[Integer.(minimum (ofInt 7) (ofInt 9) = (ofInt 7))]}

      {[Integer.(minimum (ofInt -4) (ofInt -1) = (ofInt -4))]}
  *)
  val minimum : t -> t -> t

  (** {1 Query} *)

  (** Check if an [int] is even

      {2 Examples}

      {[Integer.(isEven (ofInt 8)) = true]}

      {[Integer.(isEven (ofInt 7)) = false]}

      {[Integer.(isEven (ofInt 0)) = true]}
  *)
  val isEven : t -> bool
  
  (** Check if an [int] is odd

      {2 Examples}

      {[Integer.(isOdd (ofInt 7) = true)]}

      {[Integer.(isOdd (ofInt 8) = false)]}

      {[Integer.(isOdd (ofInt 0) = false)]}
  *)
  val isOdd : t -> bool
  
  (** Clamps an integer within the inclusive [lower] and [upper] bounds.

      {3 Exceptions}

      Throws an [Invalid_argument] exception if [lower > upper]

      {2 Examples}

      {[Integer.(clamp ~lower:zero ~upper:(ofInt 8) (ofInt 5) = (ofInt 5))]}

      {[Integer.(clamp ~lower:zero ~upper:(ofInt 8) (ofInt 9) = (ofInt 8))]}

      {[Integer.(clamp ~lower:(ofInt -10) ~upper:(ofInt -5) (ofInt 5) = (ofInt -5))]}
  *)
  val clamp : t -> lower:t -> upper:t -> t
  
  (** Checks if an integer is between [lower] and up to, but not including, [upper].

      {3 Exceptions}

      Throws an [Invalid_argument] exception if [lower > upper]

      {2 Examples}

      {[Integer.(inRange ~lower:(ofInt 2) ~upper:(ofInt 4) (ofInt 3) = true)]}

      {[Integer.(inRange ~lower:(ofInt 5) ~upper:(ofInt 8) (ofInt 4) = false)]}

      {[Integer.(inRange ~lower:(ofInt -6) ~upper:(ofInt -2) (ofInt -3) = true)]}
  *)
  val inRange : t -> lower:t -> upper:t -> bool
  
  
  (** {1 Convert} *)

  (** Convert an {!Integer} to an {!Int}

      Returns [None] when greater than [Int.maximumValue] or less than [Int.minimumValue]

      {2 Examples}

      {[Integer.(ofInt 4 |> toInt) = Some 4]}

      {[
        String.repeat "9" ~times:10_000 
        |> Integer.ofString 
        |> Option.flatMap ~f:Integer.toString
           = None
      ]}
  *)
  val toInt : t -> int option
  
  (** Convert an {!Integer} to an [Int64.t]

      Returns [None] when greater than [Int64.max_int] or less than [Int64.min_int]

      {2 Examples}

      {[Integer.ofInt 1 |> Integer.toInt64 = Some Int64.one]}

      {[
        String.repeat "9" ~times:10_000 
        |> Integer.ofString 
        |> Option.flatMap ~f:Integer.toString
           = None
      ]}
  *)
  val toInt64 : t -> Int64.t option
  
  (** Convert an {!Integer} to a {!Float}

      Returns {!Float.infinity} when greater than {!Float.largestValue}.

      {2 Examples}

      {[Integer.ofString "8" |> Integer.toFloat = 8.0]}

      {[
        String.repeat "9" ~times:10_000 
        |> Integer.ofString
        |> Option.map ~f:Integer.toFloat
          = Some Float.infinity
      ]}
  *)
  val toFloat : t -> float
  
  (** Gives a human-readable, decimal string representation *)
  val toString : t -> string
  
  (** {1 Compare} *)

  (** Test two {!Integer}s for equality *)
  val equal : t -> t -> bool
  
  (** Compare two {!Integer}s *)
  val compare : t -> t -> int

  (** The unique identity for [ints] {!Comparator} *)
  type identity

  val comparator: (t, identity) Comparator.t