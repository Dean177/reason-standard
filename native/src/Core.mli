(** *)

(** Functions for working with boolean ([true] or [false]) values. *)
module Bool : sig
  (** Functions for working with boolean values.

      Booleans in OCaml / Reason are represented by the [true] and [false] literals.

      Whilst a bool isnt a variant, you will get warnings if you haven't
      exhaustively pattern match on them:

      {[
        let bool = false
        let string = 
          match bool with
          | false -> "false"        
        (* 
          Warning 8: this pattern-matching is not exhaustive.
          Here is an example of a case that is not matched:
          true
        *)
      ]}
  *)

  type t = bool

  (** {1 Creation} *)

  (** Convert an {!Int} into a {!Bool}.

      {2 Examples}

      {[Bool.ofInt 0 = Some false]}

      {[Bool.ofInt 1 = Some true]}

      {[Bool.ofInt 8 = None]}

      {[Bool.ofInt (-3) = None]}
  *)
  val ofInt : int -> t option

  (** Convert a {!String} into a {!Bool}.

      {2 Examples}

      {[Bool.ofString "true" = Some true]}

      {[Bool.ofString "false" = Some false]}

      {[Bool.ofString "True" = None]}

      {[Bool.ofString "False" = None]}

      {[Bool.ofString "0" = None]}

      {[Bool.ofString "1" = None]}

      {[Bool.ofString "Not even close" = None]}
  *)
  val ofString : string -> t option

  (** The lazy logical AND operator.

      Returns [true] if both of its operands evaluate to [true].

      If the 'left' operand evaluates to [false], the 'right' operand is not evaluated.

      {2 Examples}

      {[Bool.(true && true) = true]}

      {[Bool.(true && false) = false]}

      {[Bool.(false && true) = false]}

      {[Bool.(false && false) = false]}
  *)
  external (&&) : bool -> bool -> bool = "%sequand"

  (** The lazy logical OR operator.

      Returns [true] if one of its operands evaluates to [true].

      If the 'left' operand evaluates to [true], the 'right' operand is not evaluated.

      {2 Examples}

      {[Bool.(true || true) = true]}

      {[Bool.(true || false) = true]}

      {[Bool.(false || true) = true]}

      {[Bool.(false || false) = false]}
  *)
  external (||) : bool -> bool -> bool = "%sequor"

  (** The exclusive or operator.

      Returns [true] if {b exactly one} of its operands is [true].

      {2 Examples}

      {[Bool.xor true true  = false]}

      {[Bool.xor true false = true]}

      {[Bool.xor false true  = true]}

      {[Bool.xor false false = false]}
  *)
  val xor : t -> t -> t

  (** Negate a [bool].

      {2 Examples}

      {[Bool.not false = true]}

      {[Bool.not true = false]}
  *)
  val not : t -> t
  
  (** Negate a function.

      This can be useful in combination with {!List.filter} / {!Array.filter} or {!List.find} / {!Array.find}

      {2 Examples}

      {[
        let isLessThanTwelve = Bool.negate (fun n -> n >= 12) in
        isLessThanTwelve 12 = false
      ]}
  *)
  val negate : ('a -> bool) -> 'a -> bool
  
  (** {1 Conversion} *)

  (** Convert a [bool] to a {!String}

      {2 Examples}

      {[Bool.toString true = "true"]}

      {[Bool.toString false = "false"]}
  *)
  val toString : t -> string
  
  (** Convert a [bool] to an {!Int}.

      {2 Examples}

      {[Bool.toInt true = 1]}

      {[Bool.toInt false = 0]}
  *)  
  val toInt : t -> int

  (** {1 Comparison} *)

  (** Test for the equality of two [bool] values.

      {2 Examples}

      {[Bool.equal true true = true]}

      {[Bool.equal false false = true]}

      {[Bool.equal false true = false]}
  *)
  val equal : t -> t -> t

  (** Compare two boolean values

      {2 Examples}

      {[Bool.compare true false = 1]}

      {[Bool.compare false true = -1]}

      {[Bool.compare true true = 0]}

      {[Bool.compare false false = 0]}
  *)
  val compare : t -> t -> int
end

(** Functions for working with single characters. *)
module Char : sig
  (** Functions for working with single characters.

      Character literals are enclosed in ['a'] pair of single quotes.

      {[let digit = '7']}

      The functions in this module work on ASCII characters (range 0-255) only,
      {b not Unicode}.

      Since character 128 through 255 have varying values depending on what
      standard you are using (ISO 8859-1 or Windows 1252), you are advised to
      stick to the 0-127 range.
  *)

  type t = char

  (** {1 Create}

      You can also create a {!Char} using single quotes:

      {[let char = 'c']}
  *)

  (** Convert an ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point } to a character.

      Returns [None] if the codepoint is outside the range of 0 to 255 inclusive.

      {2 Examples}

      {[Char.ofCode 65 = Some 'A']}

      {[Char.ofCode 66 = Some 'B']}

      {[Char.ofCode 3000 = None]}

      {[Char.ofCode (-1) = None]}

      The full range of extended ASCII is from [0] to [255]. For numbers outside that range, you get [None].
  *)
  val ofCode : int -> char option
  
  (** Converts a string to character. Returns None when the string isn't of length one.

      {2 Examples}

      {[Char.ofString "A" = Some 'A']}

      {[Char.ofString " " = Some ' ']}

      {[Char.ofString "" = None]}

      {[Char.ofString "abc" = None]}

      {[Char.ofString " a" = None]}
  *)
  val ofString : string -> char option
  
  (** Converts an ASCII character to lower case, preserving non alphabetic ASCII characters.

      {2 Examples}

      {[Char.toLowercase 'A' = 'a']}

      {[Char.toLowercase 'B' = 'b']}

      {[Char.toLowercase '7' = '7']} *)
  val toLowercase : char -> char
  
  (** Convert an ASCII character to upper case, preserving non alphabetic ASCII characters.

      {2 Examples}

      {[toUppercase 'a' = 'A']}

      {[toUppercase 'b' = 'B']}

      {[toUppercase '7' = '7']} 
  *)
  val toUppercase : char -> char
  
  (** Detect lower case ASCII characters.

      {2 Examples}

      {[Char.isLowercase 'a' = true]}

      {[Char.isLowercase 'b' = true]}

      {[Char.isLowercase 'z' = true]}

      {[Char.isLowercase '0' = false]}

      {[Char.isLowercase 'A' = false]}

      {[Char.isLowercase '-' = false]}
  *)
  val isLowercase : char -> bool

  (** Detect upper case ASCII characters.

      {2 Examples}

      {[Char.isUppercase 'A' = true]}

      {[Char.isUppercase 'B' = true]}

      {[Char.isUppercase 'Z' = true]}

      {[Char.isUppercase 'h' = false]}

      {[Char.isUppercase '0' = false]}

      {[Char.isUppercase '-' = false]} 
  *)
  val isUppercase : char -> bool

  (** Detect upper and lower case ASCII alphabetic characters.

      {2 Examples}

      {[Char.isLetter 'a' = true]}

      {[Char.isLetter 'b' = true]}

      {[Char.isLetter 'E' = true]}

      {[Char.isLetter 'Y' = true]}

      {[Char.isLetter '0' = false]}

      {[Char.isLetter '-' = false]} 
  *)
  val isLetter : char -> bool

  (** Detect when a character is a number

      {2 Examples}

      {[Char.isDigit '0' = true]}

      {[Char.isDigit '1' = true]}

      {[Char.isDigit '9' = true]}

      {[Char.isDigit 'a' = false]}

      {[Char.isDigit 'b' = false]}
  *)
  val isDigit : char -> bool


  (** Detect upper case, lower case and digit ASCII characters.

      {2 Examples}

      {[Char.isAlphanumeric 'a' = true]}

      {[Char.isAlphanumeric 'b' = true]}

      {[Char.isAlphanumeric 'E' = true]}

      {[Char.isAlphanumeric 'Y' = true]}

      {[Char.isAlphanumeric '0' = true]}

      {[Char.isAlphanumeric '7' = true]}

      {[Char.isAlphanumeric '-' = false]}
  *)
  val isAlphanumeric : char -> bool

  (** Detect if a character is a {{: https://en.wikipedia.org/wiki/ASCII#Printable_characters } printable } character

      A Printable character has a {!Char.toCode} in the range 32 to 127, inclusive ([' '] to ['~']).

      {2 Examples}

      {[Char.isPrintable 'G' = true]}

      {[Char.isPrintable '%' = true]}

      {[Char.isPrintable ' ' = true]}

      {[Char.isPrintable '\t' = false]}

      {[Char.isPrintable '\007' = false]}
  *)
  val isPrintable : char -> bool

  (** Detect one of the following characters:
      - ['\t'] (tab)
      - ['\n'] (newline)
      - ['\011'] (vertical tab)
      - ['\012'] (form feed)
      - ['\r'] (carriage return)
      - [' '] (space)

      {2 Examples}

      {[Char.isWhitespace '\t' = true]}

      {[Char.isWhitespace ' ' = true]}

      {[Char.isWhitespace '?' = false]}

      {[Char.isWhitespace 'G' = false]}
  *)
  val isWhitespace : char -> bool

  (** {1 Conversion} *)

  (** Convert to the corresponding ASCII [code point][cp].

      [cp]: https://en.wikipedia.org/wiki/Code_point

      {2 Examples}

      {[Char.toCode 'A' = 65]}

      {[Char.toCode 'B' = 66]}
  *)
  val toCode : char -> int

  (** Convert a character into a string.

      {2 Examples}

      {[Char.toString 'A' = "A"]}

      {[Char.toString '{' = "{"]}

      {[Char.toString '7' = "7"]}
  *)
  val toString : char -> string

  (** Converts a digit character to its corresponding {!Int}.

      Returns [None] when the character isn't a digit.

      {2 Examples}

      {[Char.toDigit "7" = Some 7]}

      {[Char.toDigit "0" = Some 0]}

      {[Char.toDigit "A" = None]}

      {[Char.toDigit "" = None]}
  *)
  val toDigit : char -> int option
  
  (** {1 Comparison} *)

  (** Test two {!Char}s for equality *)
  val equal : t -> t -> bool

  (** Compare two {!Char}s *)
  val compare : t -> t -> int
end

(** Functions for working with floating point numbers. *)
module Float : sig
  (** A module for working with {{: https://en.wikipedia.org/wiki/Floating-point_arithmetic } floating-point numbers}.
  
      Valid syntax for [float]s includes:
      {[
        0.
        42.
        42.0
        3.14
        -0.1234
        123_456.123_456
        6.022e23   (* = (6.022 * 10^23) *)
        6.022e+23  (* = (6.022 * 10^23) *)
        1.602e-19  (* = (1.602 * 10^-19) *)
        1e3        (* = (1 * 10 ** 3) = 1000. *)
      ]}

      Without opening this module you can use the [.] suffixed operators e.g

      {[ 1. +. 2. /. 0.25 *. 2. = 17. ]}

      But by opening this module locally you can use the un-suffixed operators

      {[Float.((10.0 - 1.5 / 0.5) ** 3.0) = 2401.0]}

      {b Historical Note: } The particular details of floats (e.g. [NaN]) are
      specified by {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } which is literally hard-coded into almost all
      CPUs in the world.
  *)

  type t = float
  
  
  (** {1 Constants} *)

  (** The literal [0.0] as a named value *)
  val zero : t
  
  (** The literal [1.0] as a named value *)
  val one : t

  (** [NaN] as a named value. NaN stands for {{: https://en.wikipedia.org/wiki/NaN } not a number}.

      {b Note } comparing values with {!Float.nan} will {b always return } [false] even if the value you are comparing against is also [NaN].

      e.g

      {[
        let isNotANumber x = Float.(x = nan) in

        isNotANumber nan = false
      ]}

      For detecting [Nan] you should use {!Float.isNaN}
  *)
  val nan : t

  (** Positive {{: https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity } infinity }

      {[Float.log ~base:10.0 0.0 = Float.infinity]}
  *)
  val infinity : t

  (** Negative infinity, see {!Float.infinity} *)
  val negativeInfinity : t
  
  (** An approximation of {{: https://en.wikipedia.org/wiki/E_(mathematical_constant) } Euler's number }. *)
  val e : t

  (** An approximation of {{: https://en.wikipedia.org/wiki/Pi } pi }. *)
  val pi : t

  (** The smallest interval between two representable numbers. *)
  val epsilon : t
  
  (** The largest (furthest from zero) representable positive [float] *)
  val largestValue : t
  
  (** The smallest representable positive [float]. The closest to zero without actually being zero. *)
  val smallestValue : t
  
  (** For floats greater than [maximumSafeInteger], it no longer holds that [Float.(n + 1.) > n]  *)
  val maximumSafeInteger : t
  
  (** For floats less than [minimumSafeInteger], it no longer holds that [Float.(n - 1.) < n]  *)
  val minimumSafeInteger : t
  
  
  (** {1 Basic arithmetic and operators} *)

  (** Addition for floating point numbers.

      Although [int]s and [float]s support many of the same basic operations such as
      addition and subtraction you {b cannot} [add] an [int] and a [float] directly which
      means you need to use functions like {!Int.toFloat} to convert both values to the same type.

      So if you needed to add a {!List.length} to a [float] for some reason, you
      could:

      {[Float.add 3.14 (Int.toFloat (List.length [1,2,3])) = 6.14]}

      or

      {[Float.roundToInt 3.14 + List.length [1,2,3] = 6]}

      Languages like Java and JavaScript automatically convert [int] values
      to [float] values when you mix and match. This can make it difficult to be sure
      exactly what type of number you are dealing with and cause unexpected behavior.

      OCaml has opted for a design that makes all conversions explicit.

      {2 Examples}

      {[
        Float.add 3.14 3.14 = 6.28
        Float.(3.14 + 3.14 = 6.28)
      ]}
  *)
  val add : t -> t -> t
  
  (** See {!Float.add} *)
  val (+) : t -> t -> t
  
  (** Subtract numbers

      Alternatively the [-] operator can be used

      {2 Examples}

      {[Float.subtract 4.0 3.0 = 1.0]}

      {[Float.(4.0 - 3.0) = 1.0]}
  *)
  val subtract : t -> t -> t
  
  (** See {!Float.subtract} *)
  val (-) : t -> t -> t
  
  (** Multiply numbers

      Alternatively the [*] operator can be used

      {2 Examples}

      {[Float.multiply 2.0 7.0 = 14.0]}

      {[Float.(2.0 * 7.0) = 14.0]}
  *)
  val multiply : t -> t -> t
  
  (** See {!Float.multiply} *)
  val ( * ) : t -> t -> t
  
  (** Floating-point division:

      Alternatively the [/] operator can be used

      {2 Examples}

      {[Float.divide 3.14 ~by:2.0 = 1.57]}

      {[Float.(3.14 / 2.0) = 1.57]}
  *)
  val divide : t -> by:t -> t
  
  (** See {!Float.divide} *)
  val (/) : t -> t -> t
  
  (** Exponentiation, takes the base first, then the exponent.

      Alternatively the [**] operator can be used

      {2 Examples}

      {[Float.power ~base:7.0 ~exponent:3.0 = 343.0]}

      {[Float.(7.0 ** 3.0) = 343.0]}
  *)
  val power : base:t -> exponent:t -> t
  
  (** See {!Float.power} *)
  val ( ** ) : t -> t -> t
  
  (** Flips the 'sign' of a [float] so that positive floats become negative and negative integers become positive. Zero stays as it is.

      Alternatively an operator is available

      {2 Examples}

      {[Float.(~- 4.0) = (-4.0)]}

      {[
        Float.negate 8 = (-8)
        Float.negate (-7) = 7
        Float.negate 0 = 0
      ]}
  *)
  val negate : t -> t
  
  (** See {!Float.negate} *)
  val (~-) : t -> t
  
  (** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value} of a number.

      {2 Examples}

      {[
        Float.absolute 8. = 8.
        Float.absolute (-7) = 7
        Float.absolute 0 = 0
      ]}
  *)
  val absolute : t -> t
  
  (** Returns the larger of two [float]s, if both arguments are equal, returns the first argument

      If either (or both) of the arguments are [NaN], returns [NaN]

      {2 Examples}

      {[Float.maximum 7. 9. = 9.]}

      {[Float.maximum (-4.) (-1.) = (-1.)]}

      {[Float.(isNaN (maximum 7. nan)) = true]}
  *)
  val maximum : t -> t -> t
  
  (** Returns the smaller of two [float]s, if both arguments are equal, returns the first argument

      If either (or both) of the arguments are [NaN], returns [NaN]

      {2 Examples}

      {[Float.minimum 7.0 9.0 = 7.0]}

      {[Float.minimum (-4.0) (-1.0) = (-4.0)]}

      {[Float.(isNaN (minimum 7. nan)) = true]}
  *)
  val minimum : t -> t -> t

  (** Clamps [n] within the inclusive [lower] and [upper] bounds.

      {3 Exceptions}

      Throws an [Invalid_argument] exception if [lower > upper]

      {2 Examples}

      {[Float.clamp ~lower:0. ~upper:8. 5. = 5.]}

      {[Float.clamp ~lower:0. ~upper:8. 9. = 8.]}

      {[Float.clamp ~lower:(-10.) ~upper:(-5.) 5. = -5.]}
  *)
  val clamp : t -> lower:t -> upper:t -> t
  
  (** {1 Fancier math} *)

  (** Take the square root of a number.

      [squareRoot] returns [NaN] when its argument is negative. See {!Float.nan} for more.

      {2 Examples}

      {[Float.squareRoot 4.0 = 2.0]}

      {[Float.squareRoot 9.0 = 3.0]}
  *)
  val squareRoot : t -> t
  
  (** Calculate the logarithm of a number with a given base.

      {2 Examples}

      {[Float.log ~base:10. 100. = 2.]}

      {[Float.log ~base:2. 256. = 8.]}
  *)
  val log : t -> base:t -> t
  
  (** {1 Query} *)

  (** Determine whether a float is an undefined or unrepresentable number.

      {b Note } this function is more useful than it might seem since [NaN] {b does not } equal [Nan]:

      {[Float.(nan = nan) = false]}

      {2 Examples}

      {[Float.is_nan (0.0 / 0.0) = true]}

      {[Float.(is_nan (squareRoot (-1.0))) = true]}

      {[Float.is_nan (1.0 / 0.0) = false  (* Float.infinity {b is} a number *)]}

      {[Float.is_nan 1. = false]}
  *)
  val isNaN : t -> bool
  
  (** Determine whether a float is finite number. True for any float except [Infinity], [-Infinity] or [NaN]

      Notice that [NaN] is not finite!

      {2 Examples}

      {[Float.isFinite (0. / 0.) = false]}

      {[Float.(isFinite (squareRoot (-1.))) = false]}

      {[Float.isFinite (1. / 0.) = false]}

      {[Float.isFinite 1. = true]}

      {[Float.(isFinite nan) = false]}
  *)
  val isFinite : t -> bool
  
  (** Determine whether a float is positive or negative infinity.

      {2 Examples}

      {[Float.isInfinite (0. / 0.) = false]}

      {[Float.(isInfinite (squareRoot (-1.))) = false]}

      {[Float.isInfinite (1. / 0.) = true]}

      {[Float.isInfinite 1. = false]}

      {[Float.(isInfinite nan) = false]}
  *)
  val isInfinite : t -> bool
  
  (** Determine whether the passed value is an integer.

      {2 Examples}

      {[Float.isInteger 4.0 = true]}

      {[Float.isInteger Float.pi = false]}
  *)
  val isInteger : t -> bool
  
  (** Determine whether the passed value is a safe integer (number between -(2**53 - 1) and 2**53 - 1).

      {2 Examples}

      {[Float.isSafeInteger 4.0 = true]}

      {[Float.isSafeInteger Float.pi = false]}

      {[Float.(isSafeInteger (maximumSafeInteger + 1.)) = false]}
  *)
  val isSafeInteger : t -> bool
  
  
  (** Checks if a float is between [lower] and up to, but not including, [upper].

      If [lower] is not specified, it's set to to [0.0].

      {3 Exceptions}

      Throws an [Invalid_argument] exception if [lower > upper]

      {2 Examples}

      {[Float.inRange ~lower:2. ~upper:4. 3. = true]}

      {[Float.inRange ~lower:1. ~upper:2. 2. = false]}

      {[Float.inRange ~lower:5.2 ~upper:7.9 9.6 = false]}
  *)
  val inRange : t -> lower:t -> upper:t -> bool
  
  (** {1 Angles} *)

  (** This type is just an alias for [float]. 
      
      Its purpose is to make understanding the signatures of the following 
      functions a little easier.
  *)
  type radians = float

  (** [hypotenuse x y] returns the length of the hypotenuse of a right-angled triangle with sides of length [x] and [y], or, equivalently, the distance of the point [(x, y)] to [(0, 0)].

      {2 Examples}

      {[Float.hypotenuse 3. 4. = 5.]}
  *)
  val hypotenuse : t -> t -> t
  
  (** Converts an angle in {{: https://en.wikipedia.org/wiki/Degree_(angle) } degrees} to {!Float.radians}.

      {2 Examples}

      {[Float.degrees 180. = Float.pi]}

      {[Float.degrees 360. = Float.pi * 2.]}

      {[Float.degrees 90. = Float.pi /. 2.]}
  *)
  val degrees : t -> radians
  
  (** Convert a {!Float.t} to {{: https://en.wikipedia.org/wiki/Radian } radians }.

      {b Note } This function doesn't actually do anything to its argument, but can be useful to indicate intent when inter-mixing angles of different units within the same function.

      {2 Examples}

      {[Float.(radians pi) = 3.141592653589793]}
  *)
  val radians : t -> radians
  
  (** Convert an angle in {{: https://en.wikipedia.org/wiki/Turn_(geometry) } turns} into {!Float.radians}.

      One turn is equal to 360 degrees.

      {2 Examples}

      {[Float.(turns (1. / 2.)) = pi]}

      {[Float.(turns 1. = degrees 360.)]}
  *)
  val turns : t -> radians
  
  (** {1 Polar coordinates} *)

  (** Convert {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } (radius, radians) to {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } (x,y).

      {2 Examples}

      {[Float.(ofPolar (squareRoot 2., degrees 45.)) = (1., 1.)]}
  *)
  val ofPolar : (float * radians) -> (float * float)

  (** Convert {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } [(x, y)] to {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } [(radius, radians)].

      {2 Examples}

      {[Float.toPolar (-1.0, 0.0) = (1.0, Float.pi)]}

      {[Float.toPolar (3.0, 4.0) = (5.0, 0.9272952180016122)]}

      {[Float.toPolar (5.0, 12.0) = (13.0, 1.1760052070951352)]}
  *)
  val toPolar : (float * float) -> (float * radians)
  
  (** Figure out the cosine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

      {2 Examples}

      {[Float.(cos (degrees 60.)) = 0.5000000000000001]}

      {[Float.(cos (radians (pi / 3.))) = 0.5000000000000001]}
  *)
  val cos : radians -> t
  
  (** Figure out the arccosine for [adjacent / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

      {2 Examples}

      {[Float.(acos (radians 1.0 / 2.0)) = Float.radians 1.0471975511965979 (* 60 degrees or pi/3 radians *)]}
  *)
  val acos : radians -> t
  
  (** Figure out the sine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

      {2 Examples}

      {[Float.(sin (degrees 30.)) = 0.49999999999999994]}

      {[Float.(sin (radians (pi / 6.))) = 0.49999999999999994]}
  *)
  val sin : radians -> t
  
  (** Figure out the arcsine for [opposite / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

      {2 Examples}

      {[Float.(asin (1.0 / 2.0)) = 0.5235987755982989 (* 30 degrees or pi / 6 radians *)]}
  *)
  val asin : radians -> t
  
  (** Figure out the tangent given an angle in radians.

      {2 Examples}

      {[Float.(tan (degrees 45.)) = 0.9999999999999999]}

      {[Float.(tan (radians (pi / 4.))) = 0.9999999999999999]}

      {[Float.(tan (pi / 4.)) = 0.9999999999999999]}
  *)
  val tan : radians -> t
  
  (** This helps you find the angle (in radians) to an [(x, y)] coordinate, but
      in a way that is rarely useful in programming.

      {b You probably want} {!atan2} instead!

      This version takes [y / x] as its argument, so there is no way to know whether
      the negative signs comes from the [y] or [x] value. So as we go counter-clockwise
      around the origin from point [(1, 1)] to [(1, -1)] to [(-1,-1)] to [(-1,1)] we do
      not get angles that go in the full circle:

      Notice that everything is between [pi / 2] and [-pi/2]. That is pretty useless
      for figuring out angles in any sort of visualization, so again, check out
      {!Float.atan2} instead!

      {2 Examples}

      {[Float.atan (1. /. 1.) = 0.7853981633974483  (* 45 degrees or pi/4 radians *)]}

      {[Float.atan (1. /. -1.) = -0.7853981633974483  (* 315 degrees or 7 * pi / 4 radians *)]}

      {[Float.atan (-1. /. -1.) = 0.7853981633974483 (* 45 degrees or pi/4 radians *)]}

      {[Float.atan (-1. /.  1.) = -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)]}
  *)
  val atan : t -> radians
  
  (** This helps you find the angle (in radians) to an [(x, y)] coordinate.

      So rather than [Float.(atan (y / x))] you can [Float.atan2 ~y ~x] and you can get a full range of angles:

      {2 Examples}

      {[Float.atan2 ~y:1. ~x:1. = 0.7853981633974483  (* 45 degrees or pi/4 radians *)]}

      {[Float.atan2 ~y:1. ~x:(-1.) = 2.3561944901923449  (* 135 degrees or 3 * pi/4 radians *)]}

      {[Float.atan2 ~y:(-1.) ~x:(-1.) = -(2.3561944901923449) (* 225 degrees or 5 * pi/4 radians *)]}

      {[Float.atan2 ~y:(-1.) ~x:1. = -(0.7853981633974483) (* 315 degrees or 7 * pi/4 radians *)]}
  *)
  val atan2 : y:t -> x:t -> radians

  (** {1 Conversion} *)

  (** The possible [direction]s availible when doing {!Float.round}.

      See {!Float.round} for what each variant represents.
   *)
  type direction = [ 
    | `Zero  
    | `AwayFromZero  
    | `Up  
    | `Down
    | `Closest of [ `Zero  | `AwayFromZero  | `Up  | `Down  | `ToEven ] 
  ]
  
  (** Round a number, by default to the to the closest [int] with halves rounded [`Up] (towards positive infinity)

      Other rounding strategies are available by using the optional [~direction] labelelled.

      {2 Examples}

      {[
        Float.round 1.2 = 1.0
        Float.round 1.5 = 2.0
        Float.round 1.8 = 2.0
        Float.round -1.2 = -1.0
        Float.round -1.5 = -1.0
        Float.round -1.8 = -2.0
      ]}

      {3 Towards zero}

      {[
        Float.round ~direction:`Zero 1.2 = 1.0
        Float.round ~direction:`Zero 1.5 = 1.0
        Float.round ~direction:`Zero 1.8 = 1.0
        Float.round ~direction:`Zero (-1.2) = -1.0
        Float.round ~direction:`Zero (-1.5) = -1.0
        Float.round ~direction:`Zero (-1.8) = -1.0
      ]}

      {3 Away from zero}

      {[
        Float.round ~direction:`AwayFromZero 1.2 = 1.0
        Float.round ~direction:`AwayFromZero 1.5 = 1.0
        Float.round ~direction:`AwayFromZero 1.8 = 1.0
        Float.round ~direction:`AwayFromZero (-1.2) = -1.0
        Float.round ~direction:`AwayFromZero (-1.5) = -1.0
        Float.round ~direction:`AwayFromZero (-1.8) = -1.0
      ]}

      {3 Towards infinity}

      This is also known as {!Float.ceiling}

      {[
        Float.round ~direction:`Up 1.2 = 1.0
        Float.round ~direction:`Up 1.5 = 1.0
        Float.round ~direction:`Up 1.8 = 1.0
        Float.round ~direction:`Up (-1.2) = -1.0
        Float.round ~direction:`Up (-1.5) = -1.0
        Float.round ~direction:`Up (-1.8) = -1.0
      ]}

      {3 Towards negative infinity}

      This is also known as {!Float.floor}

      {[List.map  ~f:(Float.round ~direction:`Down) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -2.0; 1.0 1.0 1.0]]}

      {3 To the closest integer}

      Rounding a number [x] to the closest integer requires some tie-breaking for when the [fraction] part of [x] is exactly [0.5].

      {4 Halves rounded towards zero}

      {[List.map  ~f:(Float.round ~direction:(`Closest `AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -1.0; -1.0; 1.0 1.0 2.0]]}

      {4 Halves rounded away from zero}

      This method is often known as {b commercial rounding }

      {[List.map  ~f:(Float.round ~direction:(`Closest `AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 2.0 2.0]]}

      {4 Halves rounded down}

      {[List.map  ~f:(Float.round ~direction:(`Closest `Down)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 1.0 2.0]]}

      {4 Halves rounded up}

      This is the default.

      [Float.round 1.5] is the same as [Float.round ~direction:(`Closest `Up) 1.5]

      {4 Halves rounded towards the closest even number}

      {[
        Float.round ~direction:(`Closest `ToEven) -1.5 = -2.0
        Float.round ~direction:(`Closest `ToEven) -2.5 = -2.0
      ]}
  *)
  val round : ?direction:direction -> t -> t
  
  (** Floor function, equivalent to [Float.round ~direction:`Down].

      {2 Examples}

      {[
        Float.floor 1.2 = 1.0
        Float.floor 1.5 = 1.0
        Float.floor 1.8 = 1.0
        Float.floor -1.2 = -2.0
        Float.floor -1.5 = -2.0
        Float.floor -1.8 = -2.0
      ]}
  *)
  val floor : t -> t
  
  (** Ceiling function, equivalent to [Float.round ~direction:`Up].

      {2 Examples}

      {[
        Float.ceiling 1.2 = 2.0
        Float.ceiling 1.5 = 2.0
        Float.ceiling 1.8 = 2.0
        Float.ceiling -1.2 = (-1.0)
        Float.ceiling -1.5 = (-1.0)
        Float.ceiling -1.8 = (-1.0)
      ]}
  *)
  val ceiling : t -> t
  
  
  (** Ceiling function, equivalent to [Float.round ~direction:`Zero].

      {2 Examples}

      {[
        Float.truncate 1.0 = 1.
        Float.truncate 1.2 = 1.
        Float.truncate 1.5 = 1.
        Float.truncate 1.8 = 1.
        Float.truncate (-1.2) = -1.
        Float.truncate (-1.5) = -1.
        Float.truncate (-1.8) = -1.
      ]}
  *)
  val truncate : t -> t
  
  
  (** Convert an {!Int} to a [float]

      {2 Examples}

      {[
        Float.ofInt 5 = 5.0
        Float.ofInt 0 = 0.0
        Float.ofInt -7 = -7.0
      ]}
  *)
  val ofInt : int -> t
  
  (** Convert a {!String} to a [float].

      Parses [nan] and [infinity] case-insensitive.

      {2 Examples}

      {[Float.ofString "4.667" = Some 4.667]}

      {[Float.ofString "-4.667" = Some (-4.667)]}

      {[Float.ofString "Hamster" = None]}

      {[Float.ofString "NaN" = Some Float.nan]}

      {[Float.ofString "nan" = Some Float.nan]}

      {[Float.ofString "Infinity" = Some Float.infinity]}
  *)
  val ofString : string -> t option
  
  (** {1 Conversion} *)

  (** Converts a [float] to an {!Int} by {b ignoring the decimal portion}. See {!Float.truncate} for examples.

      Returns [None] when trying to round a [float] which can't be represented as an [int] such as {!Float.nan} or {!Float.infinity} or numbers which are too large or small.

      You probably want to use some form of {!Float.round} prior to using this function.

      {2 Examples}

      {[Float.(toInt 1.6) = (Some 1)]}

      {[Float.(toInt 2.0) = (Some 2)]}

      {[Float.(toInt 5.683) = (Some 5)]}

      {[Float.(toInt nan) = None]}

      {[Float.(toInt infinity) = None]}

      {[Float.(round 1.6 |> toInt) = Some 2]}
  *)
  val toInt : t -> int option
  
  (** Convert a [float] to a {!String}

      {2 Examples}

      TODO
  *)
  val toString : t -> string
  
  (** {1 Comparison} *)

  (** Test two floats for equality *)
  val equal : t -> t -> bool
  
  (** Compare two floats *)
  val compare : t -> t -> int
end

(** Fixed precision integers *)
module Int : sig
  (** The platform-dependant {{: https://en.wikipedia.org/wiki/Signed_number_representations } signed } {{: https://en.wikipedia.org/wiki/Integer } integer} type.

      An [int] is a whole number.

      [int]s are subject to {{: https://en.wikipedia.org/wiki/Integer_overflow } overflow }, meaning that [Int.maximumValue + 1 = Int.minimumValue].

      If you need to work with integers larger than {!maximumValue} (or smaller than {!minimumValue} you can use the {!Integer} module.

      Valid syntax for [int]s includes:
      {[
        0
        42
        9000
        1_000_000
        1_000_000
        0xFF (* 255 in hexadecimal *)
        0x000A (* 10 in hexadecimal *)
      ]}

      {b Note:} The number of bits used for an [int] is platform dependent.

      When targeting Bucklescript {{: Ints are 32 bits} https://bucklescript.github.io/docs/en/common-data-types.html#int }.

      When targeting native OCaml uses 31-bits on 32-bit platforms and 63-bits on 64-bit platforms
      which means that [int] math is well-defined in the range [-2 ** 30] to [2 ** 30 - 1] for 32bit platforms [-2 ** 62] to [2 ** 62 - 1] for 64bit platforms.

      Outside of that range, the behavior is determined by the compilation target.

      You can read about the reasons for OCamls unusual integer sizes {{: https://v1.realworldocaml.org/v1/en/html/memory-representation-of-values.html} here }.

      {e Historical Note: } The name [int] comes from the term {{: https://en.wikipedia.org/wiki/Integer } integer}). It appears
      that the [int] abbreviation was introduced in the programming language ALGOL 68.

      Today, almost all programming languages use this abbreviation.
  *)
  
  type t = int

  type identity

  (** {1 Creation} *)

  (** Attempt to parse a [string] into a [int].

      {2 Examples}

      {[Int.ofString "0" = Some 0.]}

      {[Int.ofString "42" = Some 42.]}

      {[Int.ofString "-3" = Some (-3)]}

      {[Int.ofString "123_456" = Some 123_456]}

      {[Int.ofString "0xFF" = Some 255]}

      {[Int.ofString "0x00A" = Some 10]}

      {[Int.ofString "Infinity" = None]}

      {[Int.ofString "NaN" = None]}
  *)
  val ofString : string -> t option
  
  (** {1 Constants } *)

  (** The literal [0] as a named value *)
  val zero : t
  
  (** The literal [1] as a named value *)
  val one : t
  
  (** The maximum representable [int] on the current platform *)
  val maximumValue : t
  
  (** The minimum representable [int] on the current platform *)
  val minimumValue : t
  
  (** {1 Operators }

      {b Note } You do not need to open the {!Int} module to use the
      {!( + )}, {!( - )}, {!( * )}, {!( ** )}, {! (mod)} or {!( / )} operators, these are
      available as soon as you [open Standard]
  *)

  (** Add two {!Int} numbers.

    {[Int.add 3002 4004 = 7006]}

    Or using the globally available operator:

    {[3002 + 4004 = 7006]}

    You {e cannot } add an [int] and a [float] directly though.

    See {!Float.add} for why, and how to overcome this limitation.
  *)
  val add : t -> t -> t
  
  (** See {!Int.add} *)
  val (+) : t -> t -> t
  
  (** Subtract numbers

      {[Int.subtract 4 3 = 1]}

      Alternatively the operator can be used:

      {[4 - 3 = 1]}
  *)
  val subtract : t -> t -> t
  
  (** See {!Int.subtract} *)
  val (-) : t -> t -> t
  
  (** Multiply [int]s like

      {[Int.multiply 2 7 = 14]}

      Alternatively the operator can be used:

      {[(2 * 7) = 14]}
  *)
  val multiply : t -> t -> t
  
  (** See {!Int.multiply} *)
  val ( * ) : t -> t -> t
  
  (** Integer division

      Notice that the remainder is discarded.

      {3 Exceptions}

      Throws [Division_by_zero] when the divisor is [0].

      {2 Examples}

      {[Int.divide 3 ~by:2 = 1]}

      {[27 / 5 = 5]}
  *)
  val divide : t -> by:t -> t
  
  (** See {!Int.divide} *)
  val (/) : t -> t -> t
  
  (** Floating point division

      {2 Examples}

      {[Int.(3 % 2) = 1.5]}

      {[Int.(27 % 5) = 5.25]}

      {[Int.(8 % 4) = 2.0]}
  *)
  val (%) : t -> t -> float
  
  (** Exponentiation, takes the base first, then the exponent.

      {2 Examples}

      {[Int.power ~base:7 ~exponent:3 = 343]}

      Alternatively the [**] operator can be used:

      {[7 ** 3 = 343]}
  *)
  val power : base:t -> exponent:t -> t
  
  (** See {!Int.power} *)
  val ( ** ) : t -> t -> t
  
  (** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

      {2 Examples}

      {[Int.negate 8 = (-8)]}

      {[Int.negate (-7) = 7]}

      {[Int.negate 0 = 0]}

      Alternatively the [~-] operator can be used:

      {[~-(7) = (-7)]}
  *)
  val negate : t -> t
  
  (** See {!Int.negate} *)
  val (~-) : t -> t
  
  (** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

      {2 Examples}

      {[Int.absolute 8 = 8]}

      {[Int.absolute (-7) = 7]}

      {[Int.absolute 0 = 0]}
  *)
  val absolute : t -> t
  
  (** Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

      If you intend to use [modulo] to detect even and odd numbers consider using {!Int.isEven} or {!Int.isOdd}.

      The [modulo] function works in the typical mathematical way when you run into negative numbers

      Use {!Int.remainder} for a different treatment of negative numbers.

      {2 Examples}

      {[Int.modulo ~by:3 (-4) = 1]}

      {[Int.modulo ~by:3 (-3 )= 0]}

      {[Int.modulo ~by:3 (-2) = 2]}

      {[Int.modulo ~by:3 (-1) = 1]}

      {[Int.modulo ~by:3 0 = 0]}

      {[Int.modulo ~by:3 1 = 1]}

      {[Int.modulo ~by:3 2 = 2]}

      {[Int.modulo ~by:3 3 = 0]}

      {[Int.modulo ~by:3 4 = 1]}
  *)
  val modulo : t -> by:t -> t
  
  (** See {!Int.modulo} *)
  val (mod) : t -> t -> t

  (** Get the remainder after division. Here are bunch of examples of dividing by four:

      Use {!Int.modulo} for a different treatment of negative numbers.

      {2 Examples}

      {[
        List.map
          ~f:(Int.remainder ~by:4)
          [(-5); (-4); (-3); (-2); (-1); 0; 1; 2; 3; 4; 5] =
            [(-1); 0; (-3); (-2); (-1); 0; 1; 2; 3; 0; 1]
      ]}
  *)
  val remainder : t -> by:t -> t
  
  (** Returns the larger of two [int]s

      {2 Examples}

      {[Int.maximum 7 9 = 9]}

      {[Int.maximum (-4) (-1) = (-1)]}
  *)
  val maximum : t -> t -> t
  
  (** Returns the smaller of two [int]s

      {2 Examples}

      {[Int.minimum 7 9 = 7]}

      {[Int.minimum (-4) (-1) = (-4)]}      
  *)
  val minimum : t -> t -> t

  (** {1 Query} *)

  (** Check if an [int] is even

      {2 Examples}

      {[Int.isEven 8 = true]}

      {[Int.isEven 7 = false]}

      {[Int.isEven 0 = true]}
  *)
  val isEven : t -> bool
  
  (** Check if an [int] is odd

    {2 Examples}

    {[Int.isOdd 7 = true]}

    {[Int.isOdd 8 = false]}

    {[Int.isOdd 0 = false]}
  *)
  val isOdd : t -> bool
  
  (** Clamps [n] within the inclusive [lower] and [upper] bounds.

    {3 Exceptions}

    Throws an [Invalid_argument] exception if [lower > upper]

    {2 Examples}

    {[Int.clamp ~lower:0 ~upper:8 5 = 5]}

    {[Int.clamp ~lower:0 ~upper:8 9 = 8]}

    {[Int.clamp ~lower:(-10) ~upper:(-5) 5 = (-5)]}
  *)
  val clamp : t -> lower:t -> upper:t -> t
  
  (** Checks if [n] is between [lower] and up to, but not including, [upper].

      {3 Exceptions}

      Throws an [Invalid_argument] exception if [lower > upper]

      {2 Examples}

      {[Int.inRange ~lower:2 ~upper:4 3 = true]}

      {[Int.inRange ~lower:5 ~upper:8 4 = false]}

      {[Int.inRange ~lower:(-6) ~upper:(-2) (-3) = true]}

  *)
  val inRange : t -> lower:t -> upper:t -> bool
  
  (** {1 Conversion } *)

  (** Convert an integer into a float. Useful when mixing {!Int} and {!Float} values like this:

      {2 Examples}

      {[
        let halfOf (number : int) : float =
          Float.((Int.toFloat number) / 2)
          (* Note that locally opening the {!Float} module here allows us to use the floating point division operator *) 
        in
        halfOf 7 = 3.5
      ]}
  *)
  val toFloat : t -> float
  
  (** Convert an [int] into a [string] representation.

      Guarantees that

      {[Int.(ofString (toString n)) = Some n ]}

      {2 Examples}

      {[Int.to_string 3 = "3"]}

      {[Int.to_string (-3) = "-3"]}

      {[Int.to_sString 0 = "0"]}
  *)
  val toString : t -> string
  
  (** {1 Comparison} *)

  (** Test two [int]s for equality *)
  val equal : t -> t -> bool

  (** Compare two [int]s *)
  val compare : t -> t -> int
end

(** Arbitrary precision integers.  *)
module Integer : sig
  (**
    Arbitrary precision integers.

    Backed by {{: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt } BigInt }
    when targeting Javascript and {{: https://github.com/ocaml/Zarith } Zarith } when targetting native.
  *)
  type t
  
  (** {1 Creation} *)

  (** Create an {!Integer} from an {!Int} *)
  val ofInt : int -> t
  
  (** Create an {!Integer} from an Int64 *)
  val ofInt64 : Int64.t -> t
  
  (** Create an {!Integer} from an Int64.

      Returns [None] when called with {!Float.nan}, {!Float.infinity} or {!Float.negativeInfinity}

      {2 Examples}

      TODO
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
  
  
  (** {1 Conversion } *)

  (** Convert an {!Integer} to an {!Int}

      Returns [None] when greater than [Int.maximumValue] or less than [Int.minimumValue]

      {2 Examples}

      {[Integer.(ofInt 4 |> toInt) = Some 4]}

      {[
        String.repeat "9" ~times:10_000 
        |> Integer.ofString 
        |> Option.bind ~f:Integer.toString
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
        |> Option.bind ~f:Integer.toString
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
  
  (** {1 Comparison} *)

  (** Test two {!Integer}s for equality *)
  val equal : t -> t -> bool
  
  (** Compare two {!Integer}s *)
  val compare : t -> t -> int
end

(** Functions for working with ["strings"] *)
module String : sig
  (** Functions for working with ["strings"] *)

  type t = string

  (** {1 Create} *)

  (** Converts the given character to an equivalent string of length one. *)
  val ofChar : char -> string
  
  (** Create a string from an {!Array} of characters.

      Note that these must be individual characters in single quotes, not strings of length one.

      {2 Examples}

      {[String.ofList [] = ""]}

      {[String.ofList ['a'; 'b'; 'c'] = "abc"]}
  *)
  val ofArray : char array -> string
  
  (** Create a string from a {!List} of characters.

      Note that these must be individual characters in single quotes, not strings of length one.

      {2 Examples}

      {[String.ofList [] = ""]}

      {[String.ofList ['a'; 'b'; 'c'] = "abc"]}
  *)
  val ofList : char list -> string
  
  (** Create a string by repeating a string [count] time.

      {3 Exceptions}

      If [count] is negative, [String.repeat] throws a [RangeError] exception.

      {2 Examples}

      {[String.repeat ~count:3 "ok" = "okokok"]}

      {[String.repeat ~count:3 "" = ""]}

      {[String.repeat ~count:0 "ok" = ""]}
  *)
  val repeat : string -> count:int -> string
  
  (** Create a string by providing a length and a function to choose characters.

      Returns an empty string if the length is negative.

      {2 Examples}

      {[String.initialize 8 ~f:(Fun.constant '9') = "999999999"]}
  *)
  val initialize : int -> f:(int -> char) -> string
  
  (** Check if a string is empty *)
  val isEmpty : string -> bool
  
  (** Returns the length of the given string.

      {b Warning} if the string contains non-ASCII characters then {!length} will
      not equal the number of characters

      {2 Examples}

      {[String.length "abc" = 3]}
  *)
  val length : string -> int
  
  (** Get the character at the specified index *)
  val get : string -> int -> char
  
  (** Get the character at [~index] *)
  val getAt : string -> index:int -> char option
  
  (** Returns, as an {!Option}, a tuple containing the first {!Char} and the remaining String.

      If given an empty string, returns [None].

      {2 Examples}

      {[String.uncons "abcde" = Some ('a', "bcde")]}

      {[String.uncons "a" = Some ('a', "")]}

      {[String.uncons "" = None]}
  *)
  val uncons : string -> (char * string) option
  
  (** Drop [count] characters from the left side of a string.

      {2 Examples}

      {[
        String.dropLeft ~count:3 "abcdefg" = "defg"
        String.dropLeft ~count:0 "abcdefg" = "abcdefg"
        String.dropLeft ~count:7 "abcdefg" = ""
        String.dropLeft ~count:(-2) "abcdefg" = "fg"
        String.dropLeft ~count:8 "abcdefg" = ""
      ]}
  *)
  val dropLeft : string -> count:int -> string
  
  (** Drop [count] characters from the right side of a string.

      {2 Examples}

      {[
        String.dropRight ~count:3 "abcdefg" = "abcd"
        String.dropRight ~count:0 "abcdefg" = "abcdefg"
        String.dropRight ~count:7 "abcdefg" = ""
        String.dropRight ~count:(-2) "abcdefg" = "abcdefg"
        String.dropRight ~count:8 "abcdefg" = ""
      ]}
  *)
  val dropRight : string -> count:int -> string
  
  (** Divide a string into a list of strings, splitting whenever [on] is encountered.

      {2 Examples}

      {[
        String.split ~on:"/" "a/b/c" = ["a"; "b"; "c"]
        String.split ~on:"--" "a--b--c" = ["a"; "b"; "c"]
        String.split ~on:"/" "abc" = ["abc"]
        String.split ~on:"/" "" = [""]
        String.split ~on:"" "abc" = ["a"; "b"; "c"]
      ]}
  *)
  val split : string -> on:string -> string list
  
  (** See if the second string starts with [prefix]

      {2 Examples}

      {[String.startsWith ~prefix:"the" "theory" = true]}

      {[String.startsWith ~prefix:"ory" "theory" = false]}
  *)
  val startsWith : string -> prefix:string -> bool
  
  (** See if the second string ends with [suffix].

      {2 Examples}

      {[String.endsWith ~suffix:"the" "theory" = false]}

      {[String.endsWith ~suffix:"ory" "theory" = true]}
  *)
  val endsWith : string -> suffix:string -> bool
  
  (** Converts all upper case letters to lower case.

      {b Note} This function works only with ASCII characters, not Unicode.

      {2 Examples}

      {[String.toLowercase "AaBbCc123" = "aabbcc123"]}
  *)
  val toLowercase : string -> string
  
  (** Converts all lower case letters to upper case.

      {b Note} This function works only with ASCII characters, not Unicode.

      {2 Examples}

      {[String.toUppercase "AaBbCc123" = "AABBCC123"]}
  *)
  val toUppercase : string -> string
  
  (** Converts the first letter to lower case if it is upper case.

      {b Note} This function works only with ASCII characters, not Unicode.

      {2 Examples}

      {[String.uncapitalize "Anastasia" = "anastasia"]}
  *)
  val uncapitalize : string -> string
  
  (** Converts the first letter of [s] to lowercase if it is upper case.

      {b Note} This function works only with ASCII characters, not Unicode.

      {2 Examples}

      {[String.uncapitalize "den" = "Den"]}
  *)
  val capitalize : string -> string
  
  (** Test if the first letter of a string is upper case.

      {b Note} This function works only with ASCII characters, not Unicode.

      {2 Examples}

      {[String.isCapitalized "Anastasia" = true]}

      {[String.isCapitalized "" = false]}
  *)
  val isCapitalized : string -> bool
  
  (** Check if one string appears within another

      {2 Examples}

      {[String.includes "team" ~substring:"tea" = true]}

      {[String.includes "team" ~substring:"i" = false]}

      {[String.includes "ABC" ~substring:"" = true]}
  *)
  val includes : string -> substring:string -> bool
  
  (** Reverse a string

      {b Note} This function does not work with Unicode characters.

      {2 Examples}

      {[String.reverse "stressed" = "desserts"]}
  *)
  val reverse : string -> string
  
  (** Extract a substring from the specified indicies. 

      See {!Array.slice}.
  *)
  val slice : ?to_:int -> string -> from:int -> string
  
  (** Removes leading and trailing {{!Char.isWhitespace} whitespace} from a string

      {2 Examples}

      {[String.trim "  abc  " = "abc"]}

      {[String.trim "  abc def  " = "abc def"]}

      {[String.trim "\r\n\t abc \n\n" = "abc"]}
  *)
  val trim : string -> string
  
  (** Like {!trim} but only drops characters from the beginning of the string. *)
  val trimLeft : string -> string
  
  (** Like {!trim} but only drops characters from the end of the string. *)
  val trimRight : string -> string
  
  (** Insert a string at [index]. 
  
      The character previously at index will now follow the inserted string.

      {2 Examples}

      {[String.insertAt ~insert:"**" ~index:2 "abcde" = "ab**cde"]}

      {[String.insertAt ~insert:"**" ~index:0 "abcde" = "**abcde"]}

      {[String.insertAt ~insert:"**" ~index:5 "abcde" = "abcde**"]}

      {[String.insertAt ~insert:"**" ~index:(-2) "abcde" = "abc**de"]}

      {[String.insertAt ~insert:"**" ~index:(-9) "abcde" = "**abcde"]}

      {[String.insertAt ~insert:"**" ~index:9 "abcde" = "abcde**"]}
  *)
  val insertAt : string -> index:int -> value:t -> string
  
  (** Run [f] on each character in a string. *)
  val forEach : string -> f:(char -> unit) -> unit
  
  (** Like {!Array.fold} but the elements are {!Char}s  *)
  val fold : string -> initial:'a -> f:('a -> char -> 'a) -> 'a
  
  (** {1 Conversion} *)

  (** Returns an {!Array} of the individual characters in the given string.

      {2 Examples}

      {[String.toArray "" = [||]]}

      {[String.toArray "abc" = [|'a'; 'b'; 'c'|]]}
  *)
  val toArray : string -> char array
  
  (** Returns a {!List} of the individual characters in the given string.

      {2 Examples}

      {[String.toList "" = []]}

      {[String.toList "abc" = ['a'; 'b'; 'c']]}
  *)
  val toList : string -> char list
  
  (** {1 Comparison} *)

  type identity

  (** Test two string for equality *)
  val equal : string -> string -> bool

  (** Test two string for equality *)
  val compare : string -> string -> int
end

(** Interfaces for use with container types like {!Array} or {!List} *)
module Container : sig
  (** This module contains module signatures which are used in functions which 
      accept first class modules. 
  *)

  module type Sum  = sig
    (** Modules which conform to this signature can be used with functions like
        {!Array.sum} or {!List.sum}
    *)

    type t
    val zero : t
    val add : t -> t -> t
  end
end

(** Functions for working with optional values. *)
module Option : sig
    (** {!Option} represents a value which may not be present.

      It is a variant containing the [(Some 'a)] and [None] constructors

      {[
        type 'a t =
          | Some of 'a
          | None
      ]}

      Many other languages use [null] or [nil] to represent something similar.

      {!Option} values are very common and they are used in a number of ways:
      - Initial values
      - Optional function arguments
      - Optional record fields
      - Return values for functions that are not defined over their entire input range (partial functions).
      - Return value for otherwise reporting simple errors, where None is returned on error.

      Lots of functions in [Standard] return options, one you have one you can
      work with the value it might contain by:

      - Pattern matching
      - Using {!map} or {!bind} (or their operators in {!Infix})
      - Unwrapping it using {!get}, or its operator {!Infix.(|?)}
      - Converting a [None] into an exception using{!getUnsafe}

      If the function you are writing can fail in a variety of ways, use a {!Result} instead to
      better communicate with the caller.

      If a function only fails in unexpected, unrecoverable ways, maybe you want raise exception.
  *)

  type 'a t = 'a option

  (** A function version of the [Some] constructor.

      In most situations you just want to use the [Some] constructor directly.

      However OCaml doesn't support piping to variant constructors.

      Note that when using the Reason syntax you {b can} use fast pipe ([->]) with variant constructors, so you don't need this function.

      See the {{: https://reasonml.github.io/docs/en/pipe-first#pipe-into-variants} Reason docs } for more.

      {2 Examples}

      {[String.reverse("desserts") |> Option.some = Some "desserts" ]}
   *)
  val some : 'a -> 'a option

  (** Returns [None] if the first argument is [None], otherwise return the second argument.

    Unlike the built in [&&] operator, the [and_] function does not short-circuit.

    When you call [and_], both arguments are evaluated before being passed to the function.

    {2 Examples}

    {[Option.and_ (Some 11) (Some 22) = Some 22]}

    {[Option.and_ None (Some 22) = None]}

    {[Option.and_ (Some 11) None = None]}

    {[Option.and_ None None = None]}
  *)
  val and_ : 'a t -> 'a t -> 'a t
  
  (** Return the first argument if it {!isSome}, otherwise return the second.

    Unlike the built in [||] operator, the [or_] function does not short-circuit.
    When you call [or_], both arguments are evaluated before being passed to the function.

    {2 Examples}

    {[Option.or_ (Some 11) (Some 22) = Some 11]}

    {[Option.or_ None (Some 22) = Some 22]}

    {[Option.or_ (Some 11) None = Some 11]}

    {[Option.or_ None None = None]}
  *)
  val or_ : 'a t -> 'a t -> 'a t
  
  (** Transform two options into an option of a {!Tuple}.

      Returns None if either of the aguments is None.

      {2 Examples}

      {[Option.both (Some 3004) (Some "Ant") = Some (3004, "Ant")]}

      {[Option.both (Some 3004) None = None]}

      {[Option.both None (Some "Ant") = None]}

      {[Option.both None None = None]}
  *)
  val both : 'a t -> 'b t -> ('a * 'b) t
  
  (** Flatten two optional layers into a single optional layer.

      {2 Examples}

      {[Option.join (Some (Some 4)) = Some 4]}

      {[Option.join (Some None) = None]}

      {[Option.join (None) = None]}
  *)
  val join : 'a t t -> 'a t

  (** Transform the value inside an option.

      Leaves [None] untouched.

      See {!Infix.(>>|)} for an operator version of this function.

      {2 Examples}

      {[Option.map ~f:(fun x -> x * x) (Some 9) = Some 81]}

      {[Option.map ~f:Int.toString (Some 9) = Some "9"]}

      {[Option.map ~f:(fun x -> x * x) None = None]}
  *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
  
  (** Combine two {!Option}s

      If both options are [Some] returns, as [Some] the result of running [f] on both values.

      If either value is [None], returns [None]

      {2 Examples}

      {[Option.map2 (Some 3) (Some 4) ~f:Int.add = Some 7]}

      {[Option.map2 (Some 3) (Some 4) ~f:Tuple.make = Some (3, 4)]}

      {[Option.map2 (Some 3) None ~f:Int.add = None]}

      {[Option.map2 None (Some 4) ~f:Int.add = None]}
  *)
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  
  (** Chain together many computations that may not return a value.

      It is helpful to see its definition:
      {[
        let bind t ~f =
          match t with
          | Some x -> f x
          | None -> None
      ]}

      This means we only continue with the callback if we have a value.

      For example, say you need to parse some user input as a month:

      {[
        let toValidMonth (month: int) : (int option) =
          if (1 <= month && month <= 12) then
            Some month
          else
            None
        in

        let userInput = "5" in

        Int.ofString userInput
        |> Option.bind ~f:toValidMonth
      ]}

      If [String.toInt] produces [None] (because the [userInput] was not an 
      integer) this entire chain of operations will short-circuit and result in 
      [None]. If [toValidMonth] results in [None], again the chain of 
      computations will result in [None].

      See {!Infix.(>>=)} for an operator version of this function.

      {2 Examples}

      {[Option.bind (Some [1, 2, 3]) ~f:List.head = Some 1]}

      {[Option.bind (Some []) ~f:List.head = None]}
  *)
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  
  
  (** Unwrap an [option('a)] returning [default] if called with [None].

      This comes in handy when paired with functions like {!Map.get} or {!List.head} which return an {!Option}.

      See {!Infix.(|?)} for an operator version of this function.

      {b Note} This can be overused! Many cases are better handled using pattern matching, {!map} or {!bind}.

      {2 Examples}

      {[Option.get ~default:99 (Some 42) = 42]}

      {[Option.get ~default:99 None = 99]}

      {[Option.get ~default:"unknown" (Map.get Map.String.empty "Tom") = "unknown"]}
  *)
  val get : 'a t -> default:'a -> 'a
  
  (** Unwrap an [option('a)] returning the enclosed ['a].

      {b Note} in most situations it is better to use pattern matching, {!get}, {!map} or {!bind}.
      Can you structure your code slightly differently to avoid potentially raising an exception?

      {3 Exceptions}

      Raises an [Invalid_argument] exception if called with [None]

      {2 Examples}

      {[List.head [1;2;3] |> Option.getUnsafe = 1]}

      {[List.head [] |> Option.getUnsafe]}
  *)
  val getUnsafe : 'a t -> 'a
  
  (** Check if an {!Option} is a [Some].

      In most situtations you should just use pattern matching instead.

      {2 Examples}

      {[Option.isSome (Some 3004) = true]}

      {[Option.isSome None = false]}
  *)
  val isSome : 'a t -> bool
  
  (** Check if an {!Option} is a [None].

      In most situtations you should just use pattern matching instead.

      {2 Examples}

      {[Option.isNone (Some 3004) = false]}

      {[Option.isNone None = true]}
  *)
  val isNone : 'a t -> bool

  (** Run a function against a value, if it is present. *)
  val forEach : 'a t -> f:('a -> unit) -> unit
  
  (** TODO *)
  val fold : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b
  
  (** Convert an option to a {!Array}.

      [None] is represented as an empty list and [Some] is represented as a list of one element.

      {2 Examples}

      {[Option.toArray (Some 3004) = [|3004|]]}

      {[Option.toArray (None) = [||]]}
  *)
  val toArray : 'a t -> 'a array
  
  (** Convert an option to a {!List}.

      [None] is represented as an empty list and [Some] is represented as a list of one element.

      {2 Examples}

      {[Option.toList (Some 3004) = [3004]]}

      {[Option.toList (None) = []]}
  *)
  val toList : 'a t -> 'a list
  
  (** {1 Comparison} *)

  (** Test two optional values for equality using the provided function

      {2 Examples}

      {[Option.equal Int.equal (Some 1) (Some 1) = true]}

      {[Option.equal Int.equal (Some 1) (Some 3) = false]}

      {[Option.equal Int.equal (Some 1) None = false]}

      {[Option.equal Int.equal None None = true]}
  *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  
  
  (** Compare two optional values using the provided function.

      A [None] is "less" than a [Some]

      {2 Examples}

      {[Option.compare Int.compare (Some 1) (Some 3) = -1]}

      {[Option.compare Int.compare (Some 1) None = 1]}

      {[Option.compare Int.compare None None = 0]}
  *)
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  module Infix : sig
    (** Operators for code that works extensively with {!Option}s.

        This module is intended to be [open]ed at the top of a block of code (or module) that uses
        its operators extensively.

        {[           
          let nameToAge = Map.String.ofArray [|
            ("Ant", 1);
            ("Bat", 5);
            ("Cat", 19);
          |] in

          let catAge = Map.get nameToAge "Cat" |? 8 in
          (* 19 *)

          Option.Infix.(
            Map.get nameToAge "Ant" >>= (fun antAge -> 
              Map.get nameToAge "Bat" >>| (fun batAge -> 
                Int.absolute(batAge - antAge)
              )
            )
          )
          (* Some (4) *)
        ]}
    *)

    (** The operator version of {!get}

       {2 Examples}

       {[Some 3004 |? 8 = 3004]}

       {[None |? 8 = 8]}
    *)
    val (|?) : 'a t -> 'a -> 'a

    (** The operator version of {!map}

        {2 Examples}

        {[Some "desserts" >>| String.reverse = Some "stressed"]}

        {[None >>| String.reverse = None]}
    *)
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    (** The operator version of {!bind}

        {2 Examples}

        {[Some [1, 2, 3] >>= List.head = Some 1]}

        {[Some [] >>= List.head = None]}
    *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

(** Functions for working with computations which may fail. *)
module Result : sig
  (** A {!Result} is used to represent a computation which may fail.

      A [Result] is a variant, which has a constructor for successful results 
      [(Ok 'ok)], and one for unsuccessful results ([(Error 'error)]).

      {[
        type ('ok, 'error) t =
          | Ok of 'ok
          | Error of 'error
      ]}

      Here is how you would annotate a [Result] variable whose [Ok]
      variant is an integer and whose [Error] variant is a string:

      {[let ok: (int, string) Result.t = Ok 3]}

      {[let error: (int, string) Result.t = Error "This computation failed!"]}

      {b Note} The ['error] case can be of {b any} type and while [string] is very common you could also use:
      - [string List.t] to allow errors to be accumulated
      - [exn], in which case the result type just makes exceptions explicit in the return type
      - A variant or polymorphic variant, with one case per possible error. This is means each error can be dealt with explicitly. See {{: https://keleshev.com/composable-error-handling-in-ocaml } this excellent article} for mnore information on this approach.

      If the function you are writing can only fail in a single obvious way, maybe you want an {!Option} instead.
  *)

  type ('ok, 'error) t = ('ok, 'error) Result.t
  
  
  (** {1 Creation} *)

  (** A function alternative to the [Ok] constructor which can be used in places where
      the constructor isn't permitted such as at the of a {!Fun.(|>)} or functions like {!List.map}.

      {2 Examples}

      {[String.reverse "desserts" |> Result.ok = Ok "stressed"]}

      {[List.map [1; 2; 3] ~f:Result.ok = [Ok 1; Ok 2; Ok 3]]}
  *)
  val ok : 'ok -> ('ok, 'error) t
  
  (** A function alternative to the [Error] constructor which can be used in places where
      the constructor isn't permitted such as at the of a {!Fun.pipe} or functions like {!List.map}.

      {b Note}

      When targetting the Bucklescript compiler you {b can} use constructors with the fast pipe.

      {[5 |. Ok = (Ok 5)]}

      See the {{: https://reasonml.github.io/docs/en/pipe-first#pipe-into-variants} Reason docs } for more.

      {2 Examples}

      {[Int.negate 3 |> Result.error 3 = Error (-3)]}

      {[List.map [1; 2; 3] ~f:Result.error = [Error 1; Error 2; Error 3]]}
  *)
  val error : 'error -> ('ok, 'error) t
  
  (** Run the provided function and wrap the returned value in a {!Result}, catching any exceptions raised.

      {2 Examples}

      {[Result.attempt (fun () -> 5 / 0) = Error Division_by_zero]}

      {[
        let numbers = [|1,2,3|] in
        Result.attempt (fun () -> numbers.(3)) = 
          Error (Invalid_argument "index out of bounds")
      ]}
  *)
  val attempt : (unit -> 'ok) -> ('ok, exn) t
  
  
  (** Convert an {!Option} to a {!Result} where a [(Some value)] becomes [(Ok value)] and a [None] becomes [(Error error)].

      {2 Examples}

      {[Result.ofOption (Some 84) ~error:"Greater than 100" = Ok 8]}

      {[
        Result.ofOption None ~error:"Greater than 100" = 
          Error "Greater than 100"
      ]}
  *)
  val ofOption : 'ok option -> error:'error -> ('ok, 'error) t
  
  (** Check if a {!Result} is an [Ok].

      Useful when you want to perform some side affect based on the presence of
      an [Ok] like logging.

      {b Note} if you need access to the contained value rather than doing
      [Result.isOk] followed by {!Result.getUnsafe} its safer and just as
      convenient to use pattern matching directly or use one of {!Result.bind}
      or {!Result.map}

      {2 Examples}

      {[Result.isOk (Ok 3) = true]}

      {[Result.isOk (Error 3) = false]}
  *)
  val isOk : (_, _) t -> bool
  
  (** Check if a {!Result} is an [Error].

      Useful when you want to perform some side affect based on the presence of
      an [Error] like logging.

      {b Note} if you need access to the contained value rather than doing
      {!Result.isOk} followed by {!Result.getUnsafe} its safer and just as
      convenient to use pattern matching directly or use one of {!Result.bind}
      or {!Result.map}

      {2 Examples}

      {[Result.isError (Ok 3) = false]}

      {[Result.isError (Error 3) = true]}
  *)
  val isError : (_, _) t -> bool
  
  (** Returns the first argument if it {!isError}, otherwise return the second argument.

      Unlike the {!Bool.(&&)} operator, the [and_] function does not short-circuit.
      When you call [and_], both arguments are evaluated before being passed to the function.

      {2 Examples}

      {[Result.and_ (Ok "Antelope") (Ok "Salmon") = Ok "Salmon"]}

      {[
        Result.and_ 
          (Error (`UnexpectedBird "Finch")) 
          (Ok "Salmon") 
          = Error (`UnexpectedBird "Finch")
      ]}

      {[
        Result.and_ 
          (Ok "Antelope") 
          (Error (`UnexpectedBird "Finch")) 
            = Error (`UnexpectedBird "Finch")
      ]}

      {[
        Result.and_ 
          (Error (`UnexpectedInvertabrate "Honey bee")) 
          (Error (`UnexpectedBird "Finch")) 
            = Error (`UnexpectedBird "Honey Bee")
      ]}
  *)
  val and_ : ('ok, 'error) t -> ('ok, 'error) t -> ('ok, 'error) t
  
  (** Return the first argument if it {!isOk}, otherwise return the second.

    Unlike the built in [||] operator, the [or_] function does not short-circuit.
    When you call [or_], both arguments are evaluated before being passed to the function.

    {2 Examples}

    {[Result.or_ (Ok "Boar") (Ok "Gecko") = (Ok "Boar")]}

    {[Result.or_ (Error (`UnexpectedInvertabrate "Periwinkle")) (Ok "Gecko") = (Ok "Gecko")]}

    {[Result.or_ (Ok "Boar") (Error (`UnexpectedInvertabrate "Periwinkle")) = (Ok "Boar") ]}

    {[Result.or_ (Error (`UnexpectedInvertabrate "Periwinkle")) (Error (`UnexpectedBird "Robin")) = (Error (`UnexpectedBird "Robin"))]}
  *)
  val or_ : ('ok, 'error) t -> ('ok, 'error) t -> ('ok, 'error) t
  
  (** Combine two results, if both are [Ok] returns an [Ok] containing a {!Tuple} of the values.

      If either is an [Error], returns the [Error].

      The same as writing [Result.map2 ~f:Tuple.make]

      {2 Examples}

      {[Result.both (Ok "Badger") (Ok "Rhino") = Ok ("Dog", "Rhino")]}

      {[
        Result.both (Error (`UnexpectedBird "Flamingo")) (Ok "Rhino") = 
          (Error (`UnexpectedBird "Flamingo"))
      ]}

      {[
        Result.both 
          (Ok "Badger") 
          (Error (`UnexpectedInvertabrate "Blue ringed octopus")) = 
            (Error (`UnexpectedInvertabrate "Blue ringed octopus"))
      ]}

      {[
        Result.both 
          (Error (`UnexpectedBird "Flamingo")) 
          (Error (`UnexpectedInvertabrate "Blue ringed octopus")) = 
            (Error (`UnexpectedBird "Flamingo"))
      ]}
  *)
  val both : ('a, 'error) t -> ('b, 'error) t -> (('a * 'b), 'error) t
  
  (** Collapse a nested result, removing one layer of nesting.

      {2 Examples}

      {[Result.join (Ok (Ok 2)) = Ok 2]}

      {[
        Result.join (Ok (Error (`UnexpectedBird "Peregrin falcon"))) = 
          (Error (`UnexpectedBird "Peregrin falcon"))
      ]}

      {[
        Result.join (Error (`UnexpectedInvertabrate "Woodlouse")) = 
          (Error (`UnexpectedInvertabrate "Woodlouse"))
      ]}
  *)
  val join : (('ok, 'error) t, 'error) t -> ('ok, 'error) t
  
  (** Unwrap a Result using the [~default] value in case of an [Error]

      {2 Examples}

      {[Result.get ~default:0 (Ok 12) = 12]}

      {[Result.get ~default:0 ((Error (`UnexpectedBird "Ostrich"))) = 0]}
  *)
  val get : ('ok, 'error) t -> default:'ok -> 'ok
  
  (** Unwrap a Result, raising an exception in case of an [Error]

      {e Exceptions}

      Raises an [Invalid_argument "Result.getUnsafe called with an Error"] exception.

      {2 Examples}

      {[Result.getUnsafe (Ok 12) = 12]}

      {[Result.getUnsafe (Error "bad") ]}
  *)
  val getUnsafe : ('ok, _) t -> 'ok
  
  (** Like {!Result.get} but unwraps an [Error] value instead

      {2 Examples}

      {[
        Result.getError
          (Error (`UnexpectedBird "Swallow")) 
          ~default:(`UnexpectedInvertabrate "Ladybird") = 
            `UnexpectedBird "Swallow"
      ]}

      {[
        Result.getError 
          (Ok 5) 
          ~default:(`UnexpectedInvertabrate "Ladybird") = 
            `UnexpectedInvertabrate "Ladybird"
      ]}
  *)
  val getError : ('ok, 'error) t -> default:'error -> 'error
  
  (** Combine two results

      If one of the results is an [Error], that becomes the return result.

      If both are [Error] values, returns its first.

      {2 Examples}

      {[Result.map2 (Ok 7) (Ok 3) ~f:Int.add = Ok 10]}

      {[Result.map2 (Error "A") (Ok 3) ~f:Int.add = Error "A"]}

      {[Result.map2 (Ok 7) (Error "B") ~f:Int.add = Error "B"]}

      {[Result.map2 (Error "A") (Error "B") ~f:Int.add = Error "A"]}
  *)
  val map2 :
    ('a, 'error) t -> ('b, 'error) t -> f:('a -> 'b -> 'c) -> ('c, 'error) t
  
  (** If all of the elements of a list are [Ok], returns an [Ok] of the the list of unwrapped values.

      If {b any} of the elements are an [Error], the first one encountered is returned.

      TODO This has the same name as Map.combine, but is very different
      {2 Examples}

      {[Result.combine [Ok 1; Ok 2; Ok 3; Ok 4] = Ok [1; 2; 3; 4]]}

      {[Result.combine [Ok 1; Error "two"; Ok 3; Error "four"] = Error "two"]}
  *)
  val combine : ('ok, 'error) t list -> ('ok list, 'error) t
  
  (** Transforms the ['ok] in a result using [f]. Leaves the ['error] untouched.

      {2 Examples}

      {[Result.map (Ok 3) ~f:(Int.add 1) = Ok 9]}

      {[Result.map (Error "three") ~f:(Int.add 1) = Error "three"]}
  *)
  val map : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t
  
  (** Transforms the value in an [Error] using [f]. Leaves an [Ok] untouched.

      {2 Examples}

      {[Result.mapError (Ok 3) ~f:String.reverse = Ok 3]}

      {[Result.mapError (Error "bad") ~f:(Int.add 1)  = Error "bad"]}
  *)
  val mapError : ('ok, 'a) t -> f:('a -> 'b) -> ('ok, 'b) t
  
  (** Converts an [Result.t('error, Option.t('ok)] into a [Option.t(Result.t('ok, 'error))]

      {2 Examples}

      {[Result.transpose (Ok (Some 5)) = Some (Ok 5)]}

      {[Result.transpose (Ok (None)) = None]}

      {[Result.transpose (Error "fail") = (Some (Error "fail"))]}
  *)
  val transpose : ('ok option, 'error) t -> ('ok, 'error) t option
  
  (** Run a function which may fail on a result.

      Short-circuits of called with an [Error].

      {2 Examples}

      {[
        let reciprical (x:float) : (string, float) Standard.Result.t = (
          if (x = 0.0) then
            Error "Divide by zero"
          else
            Ok (1.0 /. x)
        )

        let root (x:float) : (string, float) Standard.Result.t = (
          if (x < 0.0) then
            Error "Cannot be negative"
          else
            Ok (Float.squareRoot x)
        )
      ]}

      {2 Examples}

      {[Result.bind ~f:reciprical (Ok 4.0) = Ok 0.25]}

      {[Result.bind ~f:reciprical (Error "Missing number!") = Error "Missing number!"]}

      {[Result.bind ~f:reciprical (Ok 0.0) = Error "Divide by zero"]}

      {[Result.bind (Ok 4.0) ~f:root  |> Result.bind ~f:reciprical = Ok 0.5]}

      {[Result.bind (Ok -2.0) ~f:root |> Result.bind ~f:reciprical = Error "Cannot be negative"]}

      {[Result.bind (Ok 0.0) ~f:root |> Result.bind ~f:reciprical = Error "Divide by zero"]}
  *)
  val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t
  
  (** TODO Seriously what would you use this for *)
  val fold : ('ok, _) t -> initial:'b -> f:('b -> 'ok -> 'b) -> 'b
  
  (** Run a function against an [(Ok value)], ignores [Error]s.

      {2 Examples}

      {[
        Result.forEach (Ok "Dog") ~f:print_endline
        (* prints "Dog" *)
      ]}
   *)
  val forEach : ('ok, _) t -> f:('ok -> unit) -> unit
  
  (** {1 Conversion} *)

  (** Convert a {!Result} to an {!Option}.

      An [Ok x] becomes [Some x]

      An [Error _] becomes [None]

      {2 Examples}

      {[Result.toOption (Ok 42) = Some 42]}

      {[Result.toOption (Error "Missing number!") = None]}
  *)
  val toOption : ('ok, _) t -> 'ok option
  
  
  (** {1 Comparison} *)

  (** Test two results for equality using the provided functions.

      {2 Examples}

      {[Result.equal String.equal Int.equal (Ok 3) (Ok 3) = true]}

      {[Result.equal String.equal Int.equal (Ok 3) (Ok 4) = false]}

      {[Result.equal String.equal Int.equal (Error "Fail") (Error "Fail") = true]}

      {[Result.equal String.equal Int.equal (Error "Expected error") (Error "Unexpected error") = false]}

      {[Result.equal String.equal Int.equal (Error "Fail") (Ok 4) = false]}
  *)
  val equal :
    ('ok -> 'ok -> bool) ->
      ('error -> 'error -> bool) ->
        ('ok, 'error) t -> ('ok, 'error) t -> bool
  
  (** Compare results for using the provided functions.

      In the case when one of the results is an [Error] and one is [Ok], [Error]s  are considered 'less' then [Ok]s

      {2 Examples}

      {[Result.compare String.compare Int.compare (Ok 3) (Ok 3) = 0]}

      {[Result.compare String.compare Int.compare (Ok 3) (Ok 4) = -1]}

      {[Result.compare String.compare Int.compare (Error "Fail") (Error "Fail") = 0]}

      {[Result.compare String.compare Int.compare (Error "Fail") (Ok 4) = -1]}

      {[Result.compare String.compare Int.compare (Ok 4) (Error "Fail") = 1]}

      {[Result.compare String.compare Int.compare (Error "Expected error") (Error "Unexpected error") = -1]}
  *)
  val compare :
    ('ok -> 'ok -> int) ->
      ('error -> 'error -> int) -> ('ok, 'error) t -> ('ok, 'error) t -> int
  
  
  module Infix : sig
    (** In functions that make heavy use of {!Result}s placing an

        {[open Result.Infix]}

        Can make code significantly more concise at the expense of placing a greater cognitive burden on future readers.
    *)


    (** An operator version of {!Result.get} where the [default] value goes to the right of the operator.

        {2 Examples}

        The following eamples assume [open Result.Infix] is in scope.

        {[Ok 4 |? 8 = 4]}

        {[Error "Missing number!" |? 8 = 8]}
    *)
    val (|?) : ('a, 'error) t -> 'a -> 'a
    

    (** An operator version of {!bind}

        {2 Examples}

        The following examples assume

        {[
          open Result.Infix

          let reciprical (x:float) : (string, float) Standard.Result.t =
            if (x = 0.0) then
              Error "Divide by zero"
            else
              Ok (1.0 /. x)
        ]}

        Is in scope.

        {[Ok 4. >>= reciprical = Ok 0.25]}

        {[Error "Missing number!" >>= reciprical = Error "Missing number!"]}

        {[Ok 0. >>= reciprical = Error "Divide by zero"]}
    *)
    val (>>=) : ('ok, 'error) t -> ('ok -> ('b, 'error) t) -> ('b, 'error) t
    
    
    (** An operator version of {!map}

        {2 Examples}

        The following examples assume [open Result.Infix] is in scope.

        {[Ok 4 >>| Int.add(1) = Ok 5]}

        {[Error "Its gone bad" >>| Int.add(1) = Error "Its gone bad"]}
    *)
    val (>>|) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
  end
end

(** A fixed lenfth collection of values *)
module Array : sig
  (** A mutable vector of elements which must have the same type.

      Has constant time (O(1)) {!get}, {!set} and {!length} operations.

      Arrays have a fixed length, if you want to be able to add an arbitrary number of elements maybe you want a {!List}.
  *)

  type 'a t = 'a array
  
  (** {1 Create}

      You can create an [array] in OCaml with the [[|1; 2; 3|]] syntax.
  *)

  (** Create an array with only one element.

      {2 Examples}

      {[Array.singleton 1234 = [|1234|]]}

      {[Array.singleton "hi" = [|"hi"|]]} 
  *)
  val singleton : 'a -> 'a t
  
  (** Creates an array of length [length] with the value [x] populated at each index.

      {2 Examples}

      {[Array.repeat ~length:5 'a' = [|'a'; 'a'; 'a'; 'a'; 'a'|]]}

      {[Array.repeat ~length:0 7 = [||]]}

      {[Array.repeat ~length:(-1) "Why?" = [||]]} 
  *)
  val repeat : 'a -> length:int -> 'a t
  
  (** Creates an array containing all of the integers from [from] if it is provided or [0] if not, up to but not including [to]

      {2 Examples}

      {[Array.range 5 = [|0; 1; 2; 3; 4|] ]}

      {[Array.range ~from:2 5 = [|2; 3; 4|] ]}

      {[Array.range ~from:(-2) 3 = [|-2; -1; 0; 1; 2|] ]}
  *)
  val range : ?from:int -> int -> int t

  (** Initialize an array. [Array.initialize n ~f] creates an array of length [n] with
      the element at index [i] initialized to the result of [(f i)].

      {2 Examples}

      {[Array.initialize 4 ~f:identity = [|0; 1; 2; 3|]]}

      {[Array.initialize 4 ~f:(fun n -> n * n) = [|0; 1; 4; 9|]]} *)
  val initialize : int -> f:(int -> 'a) -> 'a t

  (** Create an array from a {!List}.

      {2 Examples}

      {[Array.ofList [1;2;3] = [|1;2;3|]]}
  *)
  val ofList : 'a list -> 'a t
  
  (** Create a shallow copy of an array.

      {2 Examples}

      {[
        let numbers = [|1;2;3|] in
        let otherNumbers = Array.copy numbers in
        numbers.(1) <- 9;
        numbers = [|1;9;3|];
        otherNumbers = [|1;2;3|];
      ]}

      {[
        let numberGrid = [|
          [|1;2;3|];
          [|4;5;6|];
          [|7;8;9|];
        |] in

        let numberGridCopy = Array.copy numberGrid in

        numberGrid.(1).(1) <- 0;

        numberGridCopy.(1).(1) = 9;
      ]}
  *)
  val clone : 'a t -> 'a t

  (** {1 Basic operations} *)

  (** Get the element at the specified index.

      The first element has index number 0.

      The last element has index number [Array.length a - 1].

      You should prefer using the dedicated literal syntax;

      {[array.(n)]}

      Or using the safer {!Array.getAt} function.

      {3 Exceptions}

      Raises [Invalid_argument "index out of bounds"] for indexes outside of the range [0] to [(Array.length a - 1)].

      {2 Examples}

      {[[|1; 2; 3; 2; 1|].(3) = 2]}

      {[
        let animals = [|"cat"; "dog"; "eel"|] in
        animals.(2) = "eel"
      ]}
  *)
  val get : 'a t -> int -> 'a
  
  (** Returns, as an {!Option}, the element at index number [n] of array [a].

      Returns [None] if [n] is outside the range [0] to [(Array.length a - 1)].

      {2 Examples}

      {[Array.getAt [|0; 1; 2|] ~index:5 = None]}

      {[Array.getAt [||] ~index:0 = None]}
  *)
  val getAt : 'a t -> index:int -> 'a option

  (** Modifies an array in place, replacing the element at [index] with [value].

      You should prefer either to write

      {[array.(index) <- value]}

      Or use the {!setAt} function instead.

      {3 Exceptions}

      Raises [Invalid_argument "index out of bounds"] if [n] is outside the range [0] to [Array.length a - 1].

      {2 Examples}

      {[
        let numbers = [|1;2;3|] in
        Array.set numbers 1 1;
        numbers.(2) <- 0;

        numbers = [|1;0;0|]
      ]}
  *)
  val set : 'a t -> int -> 'a -> unit
  
  (** Like {!set} but with labelled arguments *)
  val setAt : 'a t -> index:int -> value:'a -> unit
  
  (** Get the first element of an array.

      Returns [None] if the array is empty.

      {2 Examples}

      {[Array.first [1;2;3] = Some 1]}

      {[Array.first [1] = Some 1]}

      {[Array.first [] = None]}
  *)
  val first : 'a t -> 'a option
  
  (** Get the last element of an array.

      Returns [None] if the array is empty.

      {2 Examples}

      {[Array.last [1;2;3] = Some 3]}

      {[Array.last [1] = Some 1]}

      {[Array.last [] = None]}
  *)
  val last : 'a t -> 'a option

  (** Get a sub-section of a list. [from] is a zero-based index where we will start our slice.

      The [to_] is a zero-based index that indicates the end of the slice.

      The slice extracts up to but not including [to_].

      Both the [from] and [to_] indexes can be negative, indicating an offset from the end of the list.

      {2 Examples}

      {[Array.slice ~from:0 ~to_:3 [0; 1; 2; 3; 4] = [0; 1; 2]]}

      {[Array.slice ~from:1 ~to_:4 [0; 1; 2; 3; 4] = [1; 2; 3]]}

      {[Array.slice ~from:5 ~to_:3 [0; 1; 2; 3; 4] = []]}

      {[Array.slice ~from:1 ~to_:(-1) [0; 1; 2; 3; 4] = [1; 2; 3]]}

      {[Array.slice ~from:(-2) ~to_:5 [0; 1; 2; 3; 4] = [3; 4]]}

      {[Array.slice ~from:(-2) ~to_:(-1) [0; 1; 2; 3; 4] = [3]]}
  *)
  val slice : ?to_:int -> 'a t -> from:int -> 'a t
  
  (** Swaps the values at the provided indicies.

      {3 Exceptions}

      Raises an [Invalid_argument] exception of either index is out of bounds for the array.

      {2 Examples}

      {[Array.swap [|1; 2; 3|] 1 2 = [|1; 3; 2|]]}
  *)
  val swap : 'a t -> int -> int -> unit

  (** Reverses an array {b in place}, mutating the existing array.

      {2 Examples}

      {[
        let numbers = [|1; 2; 3|] in
        Array.reverse numbers
        numbers = [|3; 2; 1|];
      ]}
  *)
  val reverse : 'a t -> unit
  
  (** Sort in place, modifying the existing array, using the provided [compare] function to determine order.

      On native it uses {{: https://en.wikipedia.org/wiki/Merge_sort } merge sort} which means the sort is stable,
      runs in constant heap space, logarithmic stack space and [n * log (n)] time.

      When targeting javascript the time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

      {2 Examples}

      {[Array.sortInPlace [|5;6;8;3;6|] ~compare:compare = [|3;5;6;6;8|]]}
  *)
  val sort : 'a t -> compare:('a -> 'a -> int) -> unit
  
  (** {1 Query} *)
  
  (** Check if an array is empty

      {2 Examples}

      {[Array.isEmpty [|1; 2, 3|] = false]}

      {[Array.isEmpty [||] = true]} 
  *)
  val isEmpty : 'a t -> bool

  (** Return the length of an array.

      {2 Examples}

      {[Array.length [|1; 2, 3|] = 3]}

      {[Array.length [||] = 0]} 
  *)
  val length : 'a t -> int
  
  (** Determine if [f] returns true for [any] values in an array.

      Iteration is stopped as soon as [f] returns [true]

      {2 Examples}

      {[Array.any ~f:Int.isEven [|1;2;3;5|] = true]}

      {[Array.any ~f:Int.isEven [|1;3;5;7|] = false]}

      {[Array.any ~f:Int.isEven [||] = false]}
  *)
  val any : 'a t -> f:('a -> bool) -> bool
  
  (** Determine if [f] returns true for [all] values in an array.

      Iteration is stopped as soon as [f] returns [false]

      {2 Examples}

      {[Array.all ~f:Int.isEven [|2;4|] = true]}

      {[Array.all ~f:Int.isEven [|2;3|] = false]}

      {[Array.all ~f:Int.isEven [||] = true]}
  *)
  val all : 'a t -> f:('a -> bool) -> bool
   
  (** Count the number of elements which [f] returns [true] for

      {2 Examples}

      {[Array.count [|7; 5; 8; 6|] ~f:Int.isEven = 2]}
  *)
  val count : 'a t -> f:('a -> bool) -> int
  
  (** Returns, as an {!Option}, the first element for which [f] evaluates to [true].

      If [f] doesn't return [true] for any of the elements [find] will return [None]

      {2 Examples}

      {[Array.find ~f:Int.isEven [|1; 3; 4; 8;|] = Some 4]}

      {[Array.find ~f:Int.isOdd [|0; 2; 4; 8;|] = None]}

      {[Array.find ~f:Int.isEven [||] = None]}
  *)
  val find : 'a t -> f:('a -> bool) -> 'a option
  
  (** Similar to {!Array.find} but [f] is also called with the current index, and the return value will be a tuple of the index the passing value was found at and the passing value.

      {2 Examples}

      {[Array.findIndex [|1; 3; 4; 8;|] ~f:(fun index number -> index > 2 && Int.isEven number) = Some (3, 8)]}
  *)
  val findIndex : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

  (** Test if an array contains the specified element using the provided [equal] to test for equality.

      {2 Examples}

      {[Array.contains [1; 2; 3]  2 ~equal:(=) = true]}
  *)
  val includes : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

  (** Find the smallest element using the provided [compare] function.

      Returns [None] if called on an empty array.

      {2 Examples}

      {[Array.minimum [|7;5;8;6|] ~compare:Int.compare = Some 5]}

      {[Array.minimum [||] ~compare:Int.compare = None]}
  *)
  val minimum : 'a t -> compare:('a -> 'a -> int) -> 'a option
  
  (** Find the largest element using the provided [compare] function.

      Returns [None] if called on an empty array.

      {2 Examples}

      {[Array.maximum [|7;5;8;6|] ~compare:Int.compare = Some 8]}

      {[Array.maximum [||] ~compare:Int.compare = None]}
  *)
  val maximum : 'a t -> compare:('a -> 'a -> int) -> 'a option
  
  (** Find a {!Tuple} of the {!minimum} and {!maximum} in a single pass

      Returns [None] if called on an empty array.

      {2 Examples}

      {[Array.extent [|7;5;8;6|] ~compare:Int.compare = Some (5, 8)]}

      {[Array.extent [|7|] ~compare:Int.compare = Some (7, 7)]}

      {[Array.extent [||] ~compare:Int.compare = None]}
  *)
  val extent : 'a t -> compare:('a -> 'a -> int) -> ('a * 'a) option

  (** Calculate the sum of a list using the provided modules [zero] value and [add] function.

      {2 Examples}

      {[Array.sum [|1; 2; 3|] (module Int) = 6]}

      {[Array.sum [|4.0; 4.5; 5.0|] (module Float) = 13.5]}

      {[
        Array.sum 
          [|"a"; "b"; "c"|] 
          (
            module struct
              type t = string
              let zero = ""
              let add = (^)
            end
          ) 
          = "abc"
      ]}
  *)
  val sum : 'a t -> (module Container.Sum with type t = 'a) -> 'a

  (** {1 Transform} *)

  (** Create a new array which is the result of applying a function [f] to every element.

      {2 Examples}

      {[Array.map ~f:Float.squareRoot [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]}
  *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
  
  (** Apply a function [f] to every element with its index as the first argument.

      {2 Examples}

      {[Array.mapWithIndex ~f:( * ) [|5; 5; 5|] = [|0; 5; 10|]]}
  *)
  val mapI : 'a t -> f:(int -> 'a -> 'b) -> 'b t
    
  (** Keep elements that [f] returns [true] for.

      {2 Examples}

      {[Array.filter ~f:Int.isEven [|1; 2; 3; 4; 5; 6|] = [|2; 4; 6|]]}
  *)
  val filter : 'a t -> f:('a -> bool) -> 'a t

  (** Allows you to combine {!map} and {!filter} into a single pass.

      The output array only contains elements for which [f] returns [Some].

      Why [filterMap] and not just {!filter} then {!map}?

      {!filterMap} removes the {!Option} layer automatically.

      If your mapping is already returning an {!Option} and you want to skip over [None]s, then [filterMap] is much nicer to use.

      {2 Examples}

      {[
        let characters = [|'a'; '9'; '6'; ' '; '2'; 'z' |] in
        Array.filterMap characters ~f:Char.toDigit = [|9; 6; 2|]
      ]}

      {[
        Array.filterMap [|3; 4; 5; 6|] ~f:(fun number ->
          if Int.isEven number then
            Some (number * number)
          else
            None
        ) = [16; 36]
      ]}
  *)
  val filterMap : 'a t -> f:('a -> 'b option) -> 'b t

  (** {!map} [f] onto an array and {!concatenate} the resulting arrays

      {2 Examples}

      {[Array.bind ~f:(fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]}
  *)
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  
  (** Produce a new value from an array.

      [fold] takes two arguments, an [initial] 'accumulator' value and a function [f].

      For each element of the array [f] will be called with two arguments; the current accumulator and an element.

      [f] returns the value that the accumulator should have for the next iteration.

      The [initial] value is the value the accumulator will have on the first call to [f].

      After applying [f] to every element of the array, [fold] returns the accumulator.

      [fold] iterates over the elements of the array from first to last.

      Folding is useful whenever you have a collection of something, and want to produce a single value from it.

      For examples if we have:

      {[
        let numbers = [|1, 2, 3|] in
        let sum =
          Array.fold numbers ~initial:0 ~f:(fun accumulator element -> accumulator +element)
        in
        sum = 6
      ]}

      Walking though each iteration step by step:

      + [accumulator: 0, element: 1, result: 1]
      + [accumulator: 1, element: 2, result: 3]
      + [accumulator: 3, element: 3, result: 6]

      And so the final result is [6]. (Note that in reality you probably want to use {!Array.sum})

      {2 Examples}

      {[Array.fold [|1; 2; 3|] ~initial:[] ~f:(List.cons) = [3; 2; 1]]}

      {[
        Array.fold [|1; 1; 2; 2; 3|] ~initial:Set.Int.empty ~f:Set.add |> Set.toArray = [|1; 2; 3|]
      ]}

      {[
        let lastEven integers =
          Array.fold integers ~initial:None ~f:(fun last int ->
            if Int.isEven then
              Some int
            else
              last
          )
        in
        lastEven [|1;2;3;4;5|] = Some 4
      ]}
  *)
  val fold : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b
  
  (** This method is like {!fold} except that it iterates over the elements of the array from last to first.

      {2 Examples}

      {[Array.foldRight ~f:(+) ~initial:0 (Array.repeat ~length:3 5) = 15]}

      {[Array.foldRight ~f:List.cons ~initial:[] [|1; 2; 3|] = [1; 2; 3]]}
  *)
  val foldRight : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b

  (** {1 Combine} *)

  (** Creates a new array which is the result of appending the second array onto the end of the first.

      {2 Examples}

      {[
        let fortyTwos = Array.repeat ~length:2 42 in
        let eightyOnes = Array.repeat ~length:3 81 in
        Array.append fourtyTwos eightyOnes = [|42; 42; 81; 81; 81|];
      ]}
  *)
  val append : 'a t -> 'a t -> 'a t
  
  (** Concatenate an array of arrays into a single array:

      {2 Examples}

      {[Array.concatenate [|[|1; 2|]; [|3|]; [|4; 5|]|] = [|1; 2; 3; 4; 5|]]}
  *)
  val concatenate : 'a t t -> 'a t

  (** Combine two arrays by merging each pair of elements into a {!Tuple}

      If one array is longer, the extra elements are dropped.

      The same as [Array.map2 ~f:Tuple.make]

      {2 Examples}

      {[Array.zip [|1;2;3;4;5|] [|"Dog"; "Eagle"; "Ferret"|] = [|(1, "Dog"); (2, "Eagle"); (3, "Ferret")|]]}
  *)
  val zip : 'a t -> 'b t -> ('a * 'b) t
    
  (** Combine two arrays, using [f] to combine each pair of elements.

      If one array is longer, the extra elements are dropped.

      {2 Examples}

      {[
        let totals (xs : int array) (ys : int array) : int array =
          Array.map2 ~f:(+) xs ys in

        totals [|1;2;3|] [|4;5;6|] = [|5;7;9|]
      ]}

      {[
        Array.map2
          ~f:Tuple.create
          [|"alice"; "bob"; "chuck"|]
          [|2; 5; 7; 8|] =
            [|("alice",2); ("bob",5); ("chuck",7)|]
      ]}
  *)
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  
  (** Combine three arrays, using [f] to combine each trio of elements.

      If one array is longer, the extra elements are dropped.

      {2 Examples}

      {[
        Array.map3
          ~f:Tuple3.create
          [|"alice"; "bob"; "chuck"|]
          [|2; 5; 7; 8;|]
          [|true; false; true; false|] =
            [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
      ]}
  *)
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

  (** {1 Deconstruct} *)

  (** Split an array into a {!Tuple} of arrays. Values which [f] returns true for will end up in {!Tuple.first}.

      {2 Examples}

      {[Array.partition [|1;2;3;4;5;6|] ~f:Int.isOdd = ([|1;3;5|], [|2;4;6|])]}
  *)
  val partition : 'a t -> f:('a -> bool) -> ('a t * 'a t)
  
  (** Divides an array into a {!Tuple} of arrays.

      Elements which have index upto (but not including) [index] will be in the first component of the tuple.

      Elements with an index greater than or equal to [index] will be in the second.

      {3 Exceptions}

      Raises an [Invalid_argument] exception if [index] is less than zero

      {2 Examples}

      {[Array.splitAt [|1;2;3;4;5|] ~index:2 = ([|1;2|], [|3;4;5|])]}

      {[Array.splitAt [|1;2;3;4;5|] ~index:10 = ([|1;2;3;4;5|], [||])]}

      {[Array.splitAt [|1;2;3;4;5|] ~index:0 = ([||], [|1;2;3;4;5|])]}
  *)
  val splitAt : 'a t -> index:int -> ('a t * 'a t)
  
  (** Divides an array at the first element [f] returns [true] for.

      Returns a {!Tuple}, the first component contains the elements [f] returned false for,
      the second component includes the element that [f] retutned [true] for an all the remaining elements.

      {2 Examples}

      {[
        Array.splitWhen 
          [|5; 7; 8; 6; 4;|]
          ~f:Int.isEven = 
          ([|5; 7|], [|8; 6; 4|])
      ]}

      {[
        Array.splitWhen 
          [|"Ant"; "Bat"; "Cat"|] 
          ~f:(fun animal -> String.length animal > 3) =
            ([|"Ant"; "Bat"; "Cat"|], [||])
      ]}

      {[
        Array.splitWhen [|2.; Float.pi; 1.111|] ~f:Float.isInteger =
          ([||], [|2.; Float.pi; 1.111|])
      ]}
  *)
  val splitWhen : 'a t -> f:('a -> bool) -> ('a t * 'a t)
  
  (** Decompose an array of {!Tuple}s into a {!Tuple} of arrays.

      {2 Examples}

      {[Array.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  *)
  val unzip : ('a * 'b) t -> ('a t * 'b t)

  (** {1 Iterate} *)

  (** Iterates over the elements of invokes [f] for each element.

      {2 Examples}

      {[Array.forEach [|1; 2; 3|] ~f:(fun int -> print (Int.toString int))]}
  *)
  val forEach : 'a t -> f:('a -> unit) -> unit
  
  (** Iterates over the elements of invokes [f] for each element.

      {2 Examples}

      {[
        Array.forEachI [|1; 2; 3|] ~f:(fun index int -> printf "%d: %d" index int)
        (*
          0: 1
          1: 2
          2: 3
        *)
      ]}
  *)
  val forEachI : 'a t -> f:(int -> 'a -> unit) -> unit
  
  (** Return all of the [Some] values from an array of options

      {2 Examples}

      {[Array.values [|(Some "Ant"); None; (Some "Cat")|] = [|"Ant"; "Cat"|]]}

      {[Array.values [|None; None; None|] = [||]]}
  *)
  val values : 'a option t -> 'a t

  (** Places [sep] between all the elements of the given array.

      {2 Examples}

      {[
        Array.intersperse ~sep:"on" [|"turtles"; "turtles"; "turtles"|] =
        [|"turtles"; "on"; "turtles"; "on"; "turtles"|]
      ]}

      {[Array.intersperse ~sep:0 [||] = [||]]}
  *)
  val intersperse : 'a t -> sep:'a -> 'a t
    
  (** Split an array into equally sized chunks.

      If there aren't enough elements to make the last 'chunk', those elements are ignored.

      {2 Examples}

      {[
        Array.chunksOf ~size:2 [|"#FFBA49"; "#9984D4"; "#20A39E"; "#EF5B5B"; "#23001E"|] =  [|
          [|"#FFBA49"; "#9984D4"|];
          [|"#20A39E"; "#EF5B5B"|];
        |]
      ]}
   *)
  val chunksOf : 'a t -> size:int -> 'a t t
  
  (** Provides a sliding 'window' of sub-arrays over an array.

      The first sub-array starts at index [0] of the array and takes the first [size] elements.

      The sub-array then advances the index [step] (which defaults to 1) positions before taking the next [size] elements.

      The sub-arrays are guaranteed to always be of length [size] and iteration stops once a sub-array would extend beyond the end of the array.

      {2 Examples}

      {[Array.sliding [|1;2;3;4;5|] ~size:1 = [|[|1|]; [|2|]; [|3|]; [|4|]; [|5|]|] ]}

      {[Array.sliding [|1;2;3;4;5|] ~size:2 = [|[|1;2|]; [|2;3|]; [|3;4|]; [|4;5|]|] ]}

      {[Array.sliding [|1;2;3;4;5|] ~size:3 = [|[|1;2;3|]; [|2;3;4|]; [|3;4;5|]|] ]}

      {[Array.sliding [|1;2;3;4;5|] ~size:2 ~step:2 = [|[|1;2|]; [|3;4|]|] ]}

      {[Array.sliding [|1;2;3;4;5|] ~size:1 ~step:3 = [|[|1|]; [|4|]|] ]}
  *)
  val sliding : ?step:int -> 'a t -> size:int -> 'a t t
  
  (** {1 Convert} *)

  (** Converts a list of strings into a {!String}, placing [sep] between each string in the result.

      {2 Examples}

      {[Array.join [|"Ant"; "Bat"; "Cat"|] ~sep:", " = "Ant, Bat, Cat"]}
   *)
  val join : string t -> sep:string -> string
  
  (** Create a {!List} of elements from an array.

      {2 Examples}

      {[Array.toList [|1;2;3|] = [1;2;3]]}

      {[Array.toList (Array.ofList [3; 5; 8]) = [3; 5; 8]]}
  *)
  val toList : 'a t -> 'a list
  
  (** Create an indexed {!List} from an array. Each element of the array will be paired with its index as a {!Tuple}.

      {2 Examples}

      {[Array.toIndexedList [|"cat"; "dog"|] = [(0, "cat"); (1, "dog")]]}
  *)
  val toIndexedList : 'a t -> (int * 'a) list
  
  (** {1 Comparison} *)

  (** Test two arrays for equality using the provided function to test pairs of elements. *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  
  (** Compare two arrays using the provided function to compare pairs of elements.

      A shorter array is 'less' than a longer one.

      {2 Examples}

      {[Array.compare Int.compare [|1;2;3|] [|1;2;3;4|] = -1]}

      {[Array.compare Int.compare [|1;2;3|] [|1;2;3|] = 0]}

      {[Array.compare Int.compare [|1;2;5|] [|1;2;3|] = 1]}
  *)
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

(** Arbitrary length, singly linked lists *)
module List : sig
    (** Immutable singly-linked list of elements which must have the same type.

      Lists can have any number of elements.

      They are fast (O(1)) when:
      - Getting the first element using {!head}
      - Getting the {!tail}
      - Creating a new list by adding an element to the front using {!cons}

      They also support exhaustive pattern matching

      {[
        match aList with 
        | [] -> "Empty"
        | [a] -> "Exactly one element"
        | [a, b] -> "Exactly two elements"
        | a :: b :: cs -> "More than two elements"
      ]}

      Lists are slow when:
      - You need to access an element that isn't at the front of the list
      - Counting how many elements are in the list

      As they have inefficent ([O(n)]) {!getAt} and {!length} operations.

      If those are important to your use-case, perhaps you need an {!Array}.
  *)
  
  type 'a t = 'a list

  (** {1 Create}

      You can create a [list] with the [[1;2;3]] syntax.
  *)

  (** An empty list.

      {2 Examples}

      {[List.empty = []]}

      {[List.length List.empty = 0]}
  *)
  val empty : 'a t
  
  (** Create a list with only one element.

      {2 Examples}

      {[List.singleton 1234 = [1234]]}

      {[List.singleton "hi" = ["hi"]]}
  *)
  val singleton : 'a -> 'a t
  
  (** Creates a list of length [times] with the value [x] populated at each index.

      {2 Examples}

      {[List.repeat ~times:5 'a' = ['a'; 'a'; 'a'; 'a'; 'a']]}

      {[List.repeat ~times:0 7 = []]}

      {[List.repeat ~times:(-1) "Why?" = []]}
  *)
  val repeat : 'a -> times:int -> 'a t
  
  (** Creates a list containing all of the integers from [from] if it is provided or [0] if not, up to but not including [to]

      {2 Examples}

      {[List.range 5 = [0; 1; 2; 3; 4] ]}

      {[List.range ~from:2 5 = [2; 3; 4] ]}

      {[List.range ~from:(-2) 3 = [-2; -1; 0; 1; 2] ]}
  *)
  val range : ?from:int -> int -> int t
  
  (** Initialize a list.

      [List.initialize n ~f] creates a list of length [n] by setting the element at position [index] to be [f(index)].

      {2 Examples}

      {[List.initialize 4 ~f:identity = [0; 1; 2; 3]]}

      {[List.initialize 4 ~f:(fun index -> index * index) = [0; 1; 4; 9]]}
  *)
  val initialize : int -> f:(int -> 'a) -> 'a t
  
  (** Create a list from an {!Array}.

      {2 Examples}

      {[List.ofArray [|1;2;3|] = [1;2;3]]}
  *)
  val ofArray : 'a array -> 'a t
  
  (** {1 Basic operations} *)

  (** Returns, as an {!Option}, the first element of a list.

      If the list is empty, returns [None]

      {2 Examples}

      {[List.head [1;2;3] = Some 1]}

      {[List.head [] = None]}
  *)
  val head : 'a t -> 'a option
  
  (** Returns, as an {!Option}, a list without its first element.

      If the list is empty, returns [None]

      {2 Examples}

      {[List.tail [1;2;3] = Some [2;3]]}

      {[List.tail [1] = Some []]}

      {[List.tail [] = None]}
  *)
  val tail : 'a t -> 'a t option
    
  (** Prepend a value to the front of a list.

      The [::] operator can also be used, in Reason you use the spread syntax
      instead.

      {2 Examples}

      {[List.cons [2;3;4] 1 = [1;2;3;4]]}

      {[1 :: [2;3;4] = [1;2;3;4]]}
  *)
  val cons : 'a t -> 'a -> 'a t

  (** Attempt to take the first [count] elements of a list.

     If the list has fewer than [count] elements, returns [None].

     {2 Examples}

     {[List.take [1;2;3] ~count:2 = Some [1;2]]}

     {[List.take [] ~count:2 = None]}

     {[List.take [1;2;3;4] ~count:8 = None]}
  *)
  val take : 'a t -> count:int -> 'a t

  (** Take elements from a list until [f] returns [false]

      {2 Examples}

      {[
        List.takeWhile ~f:Int.isEven [2; 4; 6; 7; 8; 9] = [2; 4; 6]
        List.takeWhile ~f:Int.isEven [2; 4; 6] = [2; 4; 6]
        List.takeWhile ~f:Int.isEven [1; 2; 3] = []
      ]}
  *)
  val takeWhile : 'a t -> f:('a -> bool) -> 'a t
  
  (** Drop the first [count] elements from the front of a list.

      {2 Examples}

      {[List.drop [1;2;3;4] ~count:2 = [3;4]]}

      {[List.drop [1;2;3;4] ~count:6 = []]}
  *)
  val drop : 'a t -> count:int -> 'a t
  
  (** Drop elements from a list until [f] returns [false]

      {2 Examples}

      {[List.dropWhile ~f:Int.isEven [2; 4; 6; 7; 8; 9] = [7; 8; 9]]}
    
      {[List.dropWhile ~f:Int.isEven [2; 4; 6; 8] = []]}
    
      {[List.dropWhile ~f:Int.isEven [1; 2; 3] = [1; 2; 3]]}
  *)
  val dropWhile : 'a t -> f:('a -> bool) -> 'a t

  (** As an {!Option} get of all of the elements of a list except the last one.

      Returns [None] if the list is empty.

      {2 Examples}

      {[List.initial [1;2;3] = Some [1;2]]}

      {[List.initial [1] = Some []]}

      {[List.initial [] = None]}
  *)
  val initial : 'a t -> 'a t option
    
  (** Get the last element of a list.

      Returns [None] if the list is empty.

      {b Warning} This will iterate through the entire list.

      {2 Examples}

      {[List.last [1;2;3] = Some 3]}

      {[List.last [1] = Some 1]}

      {[List.last [] = None]}
  *)
  val last : 'a t -> 'a option
  
  (** Returns the element at position [index] in the list.

      Returns [None] if [index] is outside of the bounds of the list.

      {2 Examples}

      {[List.getAt [1;2;3] ~index:1 = Some 2]}

      {[List.getAt [] ~index:2 = None]}

      {[List.getAt [1;2;3] ~index:100 = None]}
  *)
  val getAt : 'a t -> index:int -> 'a option

  (** Insert a new element at the specified index.

      The element previously occupying [index] will now be at [index + 1]

      If [index] is greater than then length of the list, it will be appended:

      {e Exceptions}

      Raises an [Invalid_argument] exception if [index] is negative

      {2 Examples}

      {[
        List.insertAt 
          ~index:2 
          ~value:999 
          [100; 101; 102; 103] =  
            [100; 101; 999; 102; 103]
      ]}

      {[List.insertAt ~index:0 ~value:999 [100; 101; 102; 103] = [999; 100; 101; 102; 103]]}

      {[List.insertAt ~index:4 ~value:999 [100; 101; 102; 103] = [100; 101; 102; 103; 999]]}

      {[List.insertAt ~index:(-1) ~value:999 [100; 101; 102; 103] = [999]]}

      {[List.insertAt ~index:5 ~value:999 [100; 101; 102; 103] = [999]]}
  *)
  val insertAt : 'a t -> index:int -> value:'a -> 'a t
  
  (** Returns a new list with the value at [index] updated to be the result of applying [f].

      If [index] is outside of the bounds of the list, returns the list as-is.

      {2 Examples}

      {[List.updateAt [1; 2; 3] ~index:1 ~f:(Int.add 3) = [1; 5; 3]]}

      {[
        let animals = ["Ant"; "Bat"; "Cat"] in
        animals = List.updateAt animals ~index:4 ~f:String.reverse
      ]}
  *)
  val updateAt : 'a t -> index:int -> f:('a -> 'a) -> 'a t
  
  (** Creates a new list without the element at [index].

      If [index] is outside of the bounds of the list, returns the list as-is.

      {2 Examples}

      {[List.removeAt [1; 2; 3] ~index:2 = [1; 2]]}

      {[
        let animals = ["Ant"; "Bat"; "Cat"] in
        List.equal String.equal animals (List.removeAt animals ~index:4) = true
      ]}
  *)
  val removeAt : 'a t -> index:int -> 'a t

  (** Reverse the elements in a list

      {2 Examples}

      {[List.reverse [1; 2; 3] = [3; 2; 1]]}
   *)
  val reverse : 'a t -> 'a t

  (** Sort using the provided [compare] function.

      On native it uses {{: https://en.wikipedia.org/wiki/Merge_sort } merge sort} which means the sort is stable,
      runs in linear heap space, logarithmic stack space and n * log (n) time.

      When targeting javascript the time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

      {2 Examples}

      {[List.sort [5;6;8;3;6] ~compare:Int.compare = [3;5;6;6;8]]}
  *)
  val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
      
  (** {1 Query} *)

  (** Determine if a list is empty.

      {2 Examples}

      {[List.isEmpty List.empty = true]}

      {[List.isEmpty [||] = true]}

      {[List.isEmpty [|1; 2; 3|] = false]}
  *)
  val isEmpty : _ t -> bool

  (** Return the number of elements in a list.

      {b Warning} [List.length] needs to access the {b entire} list in order to calculate its result.

      If you need fast access to the length, perhaps you need an {!Array}.

      A common mistake is to have something like the following:

      {[
        if (List.length someList) = 0 then (
          () (* It will take longer than you think to reach here *)
        ) else (
          () (* But it doesn't need to *)
        )
      ]}

      instead you should do

      {[
        if (List.isEmpty someList) then (
          () (* This happens instantly *)
        ) else (
          () (* Since List.isEmpty takes the same amount of time for all lists *)
        )
      ]}

      Or

      {[
        match someList with
        | [] -> () (* Spoilers *)
        | _ -> () (* This is how isEmptu is implemented *)
      ]}


      {2 Examples}

      {[List.length [] = 0]}

      {[List.length [7; 8; 9] = 3]}
  *)
  val length : 'a t -> int

  (** Determine if [f] returns true for [any] values in a list.

      Stops iteration as soon as [f] returns true.

      {2 Examples}

      {[List.any ~f:isEven [|2;3|] = true]}

      {[List.any ~f:isEven [|1;3|] = false]}

      {[List.any ~f:isEven [||] = false]}
  *)
  val any : 'a t -> f:('a -> bool) -> bool
  
  (** Determine if [f] returns true for [all] values in a list.

      Stops iteration as soon as [f] returns false.

      {2 Examples}

      {[List.all ~f:Int.isEven [|2;4|] = true]}

      {[List.all ~f:Int.isEven [|2;3|] = false]}

      {[List.all ~f:Int.isEven [||] = true]}
  *)
  val all : 'a t -> f:('a -> bool) -> bool
  
  (** Count the number of elements which [f] returns [true] for

      {2 Examples}

      {[List.count [7;5;8;6] ~f:Int.isEven = 2]}
   *)
  val count : 'a t -> f:('a -> bool) -> int
    
  (** Returns, as an option, the first element for which [f] evaluates to true.

    If [f] doesn't return [true] for any of the elements [find] will return [None]

    {2 Examples}

    {[List.find ~f:Int.isEven [|1; 3; 4; 8;|] = Some 4]}

    {[List.find ~f:Int.isOdd [|0; 2; 4; 8;|] = None]}

    {[List.find ~f:Int.isEven [||] = None]}
  *)
  val find : 'a t -> f:('a -> bool) -> 'a option
  
  (** Returns, as an option, a tuple of the first element and its index for which [f] evaluates to true.

      If [f] doesnt return [true] for any [(index, element)] pair, returns [None].

      {2 Examples}

      {[List.findIndex ~f:(fun index number -> index > 2 && Int.isEven number) [|1; 3; 4; 8;|] = Some (3, 8)]}
  *)
  val findIndex : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
  
  (** Test if a list contains the specified element using the provided [equal] to test for equality.

      This function may iterate the entire list, so if your code needs to
      repeatedly perform this check, maybe you want a {!Set} instead.

      {2 Examples}

      {[List.includes [1; 3; 5; 7] 3 ~equal:Int.equal = true]}

      {[List.includes [1; 3; 5; 7] 4 ~equal:Int.equal = false]}

      {[List.includes [] 5 ~equal:Int.equal = false]}
  *)
  val includes : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  
  (** Find the smallest element using the provided [compare] function.

      Returns [None] if called on an empty array.

      {2 Examples}

      {[List.minimum [|7; 5; 8; 6|] ~compare:Int.compare = Some 5]}
  *)
  val minimum : 'a t -> compare:('a -> 'a -> int) -> 'a option
  
  (** Find the largest element using the provided [compare] function.

      Returns [None] if called on an empty array.

      {2 Examples}

      {[List.maximum [|7; 5; 8; 6|] ~compare:compare = Some 8]}
  *)
  val maximum : 'a t -> compare:('a -> 'a -> int) -> 'a option
  
  (** Find a {!Tuple} of the [(minimum, maximum)] elements using the provided [compare] function.

      Returns [None] if called on an empty array.

      {2 Examples}

      {[List.extent [|7; 5; 8; 6|] ~compare:compare = Some (5, 8)]}
  *)
  val extent : 'a t -> compare:('a -> 'a -> int) -> ('a * 'a) option
  
  (** Calculate the sum of a list using the provided modules [zero] value and [add] function.

      {2 Examples}

      {[List.sum [1;2;3] (module Int) = 6]}

      {[List.sum [4.0;4.5;5.0] (module Float) = 13.5]}

      {[
        List.sum 
          ["a"; "b"; "c"] 
          (
            module struct
              type t = string
              let zero = ""
              let add = (^)
            end
          ) 
          = "abc"
      ]}
  *)
  val sum : 'a t -> (module Container.Sum with type t = 'a) -> 'a
  
  (** {1 Transform} *)

  (** Create a new list which is the result of applying a function [f] to every element.

      {2 Examples}

      {[List.map ~f:Float.squareRoot [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]}
  *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
  
  (** Apply a function [f] to every element and its index.

      {2 Examples}

      {[
        List.mapI
          ["zero"; "one"; "two"]
          ~f:(fun index element ->
            (Int.toString index) ^ ": " ^ element) 
          = ["0: zero"; "1: one"; "2: two"]
      ]}
  *)
  val mapI : 'a t -> f:(int -> 'a -> 'b) -> 'b t  
  
  (** Keep elements that [f] returns [true] for.

      {2 Examples}

      {[List.filter ~f:Int.isEven [1; 2; 3; 4; 5; 6] = [2; 4; 6]]}
  *)
  val filter : 'a t -> f:('a -> bool) -> 'a t
  
  (** Like {!filter} but [f] is also called with each elements index. *)
  val filterI : 'a t -> f:(int -> 'a -> bool) -> 'a t
    
  (** Allows you to combine {!map} and {!filter} into a single pass.

      The output list only contains elements for which [f] returns [Some].

      Why [filterMap] and not just {!filter} then {!map}?

      {!filterMap} removes the {!Option} layer automatically.
      If your mapping is already returning an {!Option} and you want to skip over Nones, then [filterMap] is much nicer to use.

      {2 Examples}

      {[
        let characters = ['a'; '9'; '6'; ' '; '2'; 'z'] in
        List.filterMap characters ~f:Char.toDigit = [9; 6; 2]
      ]}

      {[
        List.filterMap [3; 4; 5; 6] ~f:(fun number ->
          if Int.isEven number then
            Some (number * number)
          else
            None
        ) = [16; 36]
      ]}
  *)
  val filterMap : 'a t -> f:('a -> 'b option) -> 'b t
  
  (** Apply a function [f] onto a list and {!concatenate} the resulting list of lists.

      {2 Examples}

      {[List.bind ~f xs = List.map ~f xs |> List.concatenate]}

      {[List.bind ~f:(fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]}
  *)
  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  (** Transform a list into a value

      After applying [f] to every element of the list, [fold] returns the accumulator.

      [fold] iterates over the elements of the list from first to last.

      For examples if we have:

      {[
        let numbers = [1, 2, 3] in
        let sum =
          List.fold numbers ~initial:0 ~f:(fun accumulator element -> accumulator + element)
        in
        sum = 6
      ]}

      Walking though each iteration step by step:

      + [accumulator: 0, element: 1, result: 1]
      + [accumulator: 1, element: 2, result: 3]
      + [accumulator: 3, element: 3, result: 6]

      And so the final result is [6]. (Note that in this case you probably want to use {!List.sum})

      {b Examples continued}

      {[List.fold [|1; 2; 3|] ~initial:[] ~f:(List.cons) = [3; 2; 1]]}

      {[
        let unique integers =
          List.fold integers ~initial:Set.Int.empty ~f:Set.add |> Set.toList
        in
        unique [|1; 1; 2; 3; 2|] = [|1; 2; 3|]
      ]}

      {[
        let lastEven integers =
          List.fold integers ~initial:None ~f:(fun last int ->
            if Int.isEven then
              Some int
            else
              last
          )
        in
        lastEven [|1;2;3;4;5|] = Some 4
      ]}
  *)
  val fold : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b

  (** This method is like {!fold} except that it iterates over the elements of the list from last to first. *)
  val foldRight : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b

  (** {1 Combine} *)

  (** Creates a new list which is the result of appending the second list onto the end of the first.

      {2 Examples}

      {[
        let fortyTwos = List.repeat ~length:2 42 in
        let eightyOnes = List.repeat ~length:3 81 in
        List.append fourtyTwos eightyOnes = [42; 42; 81; 81; 81];
      ]}
  *)
  val append : 'a t -> 'a t -> 'a t
  
  (** Concatenate a list of lists into a single list:

      {2 Examples}

      {[List.concatenate [[1; 2]; [3]; [4; 5]] = [1; 2; 3; 4; 5]]}
  *)
  val concatenate : 'a t t -> 'a t

  (** Combine two lists by merging each pair of elements into a {!Tuple}

      If one list is longer, the extra elements are dropped.

      The same as [List.map2 ~f:Tuple.make]

      {2 Examples}

      {[List.zip [|1;2;3;4;5|] [|"Dog"; "Eagle"; "Ferret"|] = [|(1, "Dog"); (2, "Eagle"); (3, "Ferret")|]]}
  *)
  val zip : 'a t -> 'b t -> ('a * 'b) t

  (** Combine two lists, using [f] to combine each pair of elements.

      If one list is longer, the extra elements are dropped.

      {2 Examples}

      {[List.map2 [|1;2;3|] [|4;5;6|] ~f:(+) = [|5;7;9|]]}

      {[
        List.map2
          [|"alice"; "bob"; "chuck"|]
          [|3; 5; 7; 9; 11; 13; 15; 17; 19|]
          ~f:Tuple.create
            = [|("alice", 3); ("bob", 5); ("chuck", 7)|]
      ]}
  *)
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  
  (** Combine three lists, using [f] to combine each trio of elements.

      If one list is longer, the extra elements are dropped.

      {2 Examples}

      {[
        List.map3
          ~f:Tuple3.create
          [|"alice"; "bob"; "chuck"|]
          [|2; 5; 7; 8;|]
          [|true; false; true; false|] =
            [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
      ]}
  *)
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

  (** {1 Deconstruct} *)

  (** Split a list into a {!Tuple} of lists. Values which [f] returns true for will end up in {!Tuple.first}.

      {2 Examples}

      {[List.partition [1;2;3;4;5;6] ~f:Int.isOdd = ([1;3;5], [2;4;6])]}
  *)
  val partition : 'a t -> f:('a -> bool) -> ('a t * 'a t)
    
  (** Divides a list into a {!Tuple} of lists.

      Elements which have index upto (but not including) [index] will be in the first component of the tuple.

      Elements with an index greater than or equal to [index] will be in the second.

      If [index] is outside of the bounds of the list, all elements will be in the first component of the tuple.

      {2 Examples}

      {[List.splitAt [1;2;3;4;5] ~index:2 = ([1;2], [3;4;5])]}
  *)
  val splitAt : 'a t -> index:int -> ('a t * 'a t)
  
  (** Divides a list into a {!Tuple} at the first element [f] returns [true] for.

      Elements up to (but not including) the first element [f] returns [true] for
      will be in the first component of the tuple, the remaining elements will be
      in the second

      {2 Examples}

      {[List.splitWhen [2; 4; 5; 6; 7] ~f:Int.isEven = ([2; 4], [5; 6; 7])]}

      {[List.splitWhen [2; 4; 5; 6; 7] ~f:(Fun.constant false) = ([2; 4; 5; 6; 7], [])]}
  *)
  val splitWhen : 'a t -> f:('a -> bool) -> ('a t * 'a t)
  
  (** Decompose a list of {!Tuple} into a {!Tuple} of lists.

      {2 Examples}

      {[List.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  *)
  val unzip : ('a * 'b) t -> ('a t * 'b t)

  (** {1 Iterate} *)

  (** Iterates over the elements of invokes [f] for each element.

      The function you provide must return [unit], and the [forEach] call itself also returns [unit].

      You use [List.forEach] when you want to process a list only for side effects.


      {2 Examples}

      {[
        List.forEach [|1; 2; 3|] ~f:(fun int -> print (Int.toString int))
        (* 
          Prints
          1
          2
          3 
        *)
      ]}
  *)
  val forEach : 'a t -> f:('a -> unit) -> unit
  
  (** Like {!forEach} but [f] is also called with the elements index.

      {2 Examples}

      {[
        List.forEachI [1; 2; 3] ~f:(fun index int -> printf "%d: %d" index int)
        (*
          Prints
          0: 1
          1: 2
          2: 3
        *)
      ]}
  *)
  val forEachI : 'a t -> f:(int -> 'a -> unit) -> unit
    
  (** Places [sep] between all the elements of the given list.

      {2 Examples}

      {[List.intersperse ~sep:"on" [|"turtles"; "turtles"; "turtles"|] = [|"turtles"; "on"; "turtles"; "on"; "turtles"|]]}

      {[List.intersperse ~sep:0 [||] = [||]]}
  *)
  val intersperse : 'a t -> sep:'a -> 'a t

  (** Split a list into equally sized chunks.

      If there aren't enough elements to make the last 'chunk', those elements are ignored.

      {2 Examples}

      {[
        List.chunksOf ~size:2 ["#FFBA49"; "#9984D4"; "#20A39E"; "#EF5B5B"; "#23001E"] =  [
          ["#FFBA49"; "#9984D4"];
          ["#20A39E"; "#EF5B5B"];
        ]
      ]}
   *)
  val chunksOf : 'a t -> size:int -> 'a t t

  (** Provides a sliding 'window' of sub-lists over a list.

      The first sub-list starts at the head of the list and takes the first [size] elements.

      The sub-list then advances [step] (which defaults to 1) positions before taking the next [size] elements.

      The sub-lists are guaranteed to always be of length [size] and iteration stops once a sub-list would extend beyond the end of the list.

      {2 Examples}

      {[List.sliding [1;2;3;4;5] ~size:1 = [[1]; [2]; [3]; [4]; [5]] ]}

      {[List.sliding [1;2;3;4;5] ~size:2 = [[1;2]; [2;3]; [3;4]; [4;5]] ]}

      {[List.sliding [1;2;3;4;5] ~size:3 = [[1;2;3]; [2;3;4]; [3;4;5]] ]}

      {[List.sliding [1;2;3;4;5] ~size:2 ~step:2 = [[1;2]; [3;4]] ]}

      {[List.sliding [1;2;3;4;5] ~size:1 ~step:3 = [[1]; [4]] ]}

      {[List.sliding [1;2;3;4;5] ~size:2 ~step:3 = [[1; 2]; [4; 5]]]}

      {[List.sliding [1;2;3;4;5] ~size:7 = []]}
  *)
  val sliding : ?step:int -> 'a t -> size:int -> 'a t t
  
  (** Divide a list into groups.

      [f] is called with consecutive elements, when [f] returns [false] a new group is started.

      {2 Examples}

      {[
        List.groupWhile [1; 2; 3;] ~f:(Fun.constant false) = [[1]; [2]; [3]]
      ]}

      {[
        List.groupWhile [1; 2; 3;] ~f:(Fun.constant true) = [[1; 2; 3]]
      ]}

      {[
        List.groupWhile 
          ~f:String.equal
          ["a"; "b"; "b"; "a"; "a"; "a"; "b"; "a"] = 
            [["a"]; ["b"; "b"]; ["a"; "a"; "a";] ["b"]; ["a"]]
      ]}

      {[
        List.groupWhile 
          ~f:(fun x y -> x mod 2 = y mod 2)
          [2; 4; 6; 5; 3; 1; 8; 7; 9] = 
            [[2; 4; 6]; [5; 3; 1]; [8]; [7; 9]]
      ]}
  *)
  val groupWhile : 'a t -> f:('a -> 'a -> bool) -> 'a t t

  (** {1 Convert} *)

  (** Converts a list of strings into a {!String}, placing [sep] between each string in the result.

      {2 Examples}

      {[List.join ["Ant"; "Bat"; "Cat"] ~sep:", " = "Ant, Bat, Cat"]}
   *)
  val join : string t -> sep:string -> string
  
  (** Converts a list to an {!Array}. *)
  val toArray : 'a t -> 'a array
  
  (** {1 Comparison} *)

  (** Test two lists for equality using the provided function to test elements. *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  
  (** Compare two lists using the provided function to compare elements.

      A shorter list is 'less' than a longer one.

      {2 Examples}

      {[List.compare Int.compare [1;2;3] [1;2;3;4] = -1]}

      {[List.compare Int.compare [1;2;3] [1;2;3] = 0]}

      {[List.compare Int.compare [1;2;5] [1;2;3] = 1]}
  *)
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

(** Functions for manipulating pairs of values *)
module Tuple : sig
  (** Functions for manipulating pairs of values *)

  type ('a, 'b) t = ('a * 'b)
  
  (** {1 Create} *)

  (** Create a two-tuple with the given values.

      The values do not have to be of the same type.

      {2 Examples}

      {[Tuple.make 3 "Clementine" = (3, "Clementine")]}
  *)
  val make : 'a -> 'b -> ('a * 'b)
  
  (** Create a tuple from the first two elements of an {!Array}.

      If the array is longer than two elements, the extra elements are ignored.

      If the array is less than two elements, returns [None]

      {2 Examples}

      {[Tuple.ofArray [|1; 2|] = Some (1, 2)]}

      {[Tuple.ofArray [|1|] = None]}

      {[Tuple.ofArray [|4; 5; 6|] = Some (4, 5)]}
  *)
  val ofArray : 'a array -> ('a * 'a) option
  
  (** Create a tuple from the first two elements of a {!List}.

      If the list is longer than two elements, the extra elements are ignored.

      If the list is less than two elements, returns [None]

      {2 Examples}

      {[Tuple.ofList [1; 2] = Some (1, 2)]}

      {[Tuple.ofList [1] = None]}

      {[Tuple.ofList [4; 5; 6] = Some (4, 5)]}
  *)
  val ofList : 'a list -> ('a * 'a) option
  
  (** Extract the first value from a tuple.

      {2 Examples}

      {[Tuple.first (3, 4) = 3]}

      {[Tuple.first ("john", "doe") = "john"]}
  *)
  val first : ('a * 'b) -> 'a
  
  (** Extract the second value from a tuple.

      {2 Examples}

      {[Tuple.second (3, 4) = 4]}

      {[Tuple.second ("john", "doe") = "doe"]}
  *)
  val second : ('a * 'b) -> 'b
  
  (** {1 Transform} *)

  (** Transform the {!first} value in a tuple.

      {2 Examples}

      {[Tuple.mapFirst ~f:String.reverse ("stressed", 16) = ("desserts", 16)]}

      {[Tuple.mapFirst ~f:String.length ("stressed", 16) = (8, 16)]}
  *)
  val mapFirst : ('a * 'b) -> f:('a -> 'x) -> ('x * 'b)
  
  
  (** Transform the second value in a tuple.

      {2 Examples}

      {[Tuple.mapSecond ~f:Float.squareRoot ("stressed", 16.) = ("stressed", 4.)]}

      {[Tuple.mapSecond ~f:(~-) ("stressed", 16) = ("stressed", -16)]}
  *)
  val mapSecond : ('a * 'b) -> f:('b -> 'c) -> ('a * 'c)
  
  (** Transform both values of a tuple, using [f] for the first value and [g] for the second.

      {2 Examples}

      {[Tuple.mapEach ~f:String.reverse ~g:Float.squareRoot ("stressed", 16.) = ("desserts", 4.)]}

      {[Tuple.mapEach ~f:String.length ~g:(~-) ("stressed", 16) = (8, -16)]}
  *)
  val mapEach : ('a * 'b) -> f:('a -> 'x) -> g:('b -> 'y) -> ('x * 'y)
  
  
  (** Transform both of the values of a tuple using the same function.

      [mapAll] can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple.mapAll ~f:(Int.add 1) (3, 4, 5) = (4, 5, 6)]}

      {[Tuple.mapAll ~f:String.length ("was", "stressed") = (3, 8)]}
  *)
  val mapAll : ('a * 'a) -> f:('a -> 'b) -> ('b * 'b)
  
  (** Switches the first and second values of a tuple.

      {2 Examples}

      {[Tuple.swap (3, 4) = (4, 3)]}

      {[Tuple.swap ("stressed", 16) = (16, "stressed")]}
  *)
  val swap : ('a * 'b) -> ('b * 'a)
  
  (** {1 Conversion} *)

  (** Turns a tuple into an {!Array} of length two.

      This function can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple.toArray (3, 4) = [|3; 4|]]}

      {[Tuple.toArray ("was", "stressed") = [|"was"; "stressed"|]]}
  *)
  val toArray : ('a * 'a) -> 'a array
  
  
  (** Turns a tuple into a list of length two. This function can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple.toList (3, 4) = [3; 4]]}

      {[Tuple.toList ("was", "stressed") = ["was"; "stressed"]]}
  *)
  val toList : ('a * 'a) -> 'a list
  
  (** {1 Comparison} *)

  (** Test two {!Tuple}s for equality, using the provided functions to test the
      first and second components.

      {2 Examples}

      {[Tuple.equal Int.equal String.equal (1, "Fox") (1, "Fox") = true]}

      {[Tuple.equal Int.equal String.equal (1, "Fox") (2, "Hen") = false]}
  *)
  val equal :
    ('a -> 'a -> bool) ->
      ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  
  (** Compare two {!Tuple}s, using the provided functions to compare the first
      components then, if the first components are equal, the second components.

      {2 Examples}

      {[Tuple.compare Int.compare String.compare (1, "Fox") (1, "Fox") = 0]}

      {[Tuple.compare Int.compare String.compare (1, "Fox") (1, "Eel") = 1]}

      {[Tuple.compare Int.compare String.compare (1, "Fox") (2, "Hen") = -1]}
  *)
  val compare :
    ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  
end
(** Functions for manipulating trios of values *)

module Tuple3 : sig
  (** Functions for manipulating trios of values *)

  type ('a, 'b, 'c) t = ('a * 'b * 'c)
  
  (** {1 Create} *)

  (** Create a {!Tuple3}.

      {2 Examples}

      {[Tuple3.create 3 "cat" false = (3, "cat", false)]}

      {[
        List.map3 ~f:Tuple3.create [1;2;3] ['a'; 'b'; 'c'] [4.; 5.; 6.] =
          [(1, 'a', 4.), (2, 'b', 5.), (3, 'c', 6.)]
      ]}
  *)
  val make : 'a -> 'b -> 'c -> ('a * 'b * 'c)
  
  (** Create a tuple from the first two elements of an {!Array}.

      If the array is longer than two elements, the extra elements are ignored.

      If the array is less than two elements, returns [None]

      {2 Examples}

      {[Tuple3.ofArray [|1; 2;3 |] = Some (1, 2, 3)]}

      {[Tuple3.ofArray [|1; 2|] = None]}

      {[Tuple3.ofArray [|4;5;6;7|] = Some (4, 5, 6)]}
  *)
  val ofArray : 'a array -> ('a * 'a * 'a) option
  
  (** Create a tuple from the first two elements of a {!List}.

      If the list is longer than two elements, the extra elements are ignored.

      If the list is less than two elements, returns [None]

      {2 Examples}

      {[Tuple3.ofList [1; 2; 3] = Some (1, 2, 3)]}

      {[Tuple3.ofList [1; 2] = None]}

      {[Tuple3.ofList [4; 5; 6; 7] = Some (4, 5, 6)]}
  *)
  val ofList : 'a list -> ('a * 'a * 'a) option
  
  (** {1 Create} *)

  (** Extract the first value from a tuple.

      {2 Examples}

      {[Tuple3.first (3, 4, 5) = 3]}

      {[Tuple3.first ("john", "danger", "doe") = "john"]}
  *)
  val first : ('a * 'b * 'c) -> 'a
  
  (** Extract the second value from a tuple.

      {2 Examples}

      {[Tuple.second (3, 4, 5) = 4]}

      {[Tuple.second ("john", "danger", "doe") = "danger"]}
  *)
  val second : ('a * 'b * 'c) -> 'b
  
  (** Extract the third value from a tuple.

      {2 Examples}

      {[Tuple.third (3, 4, 5) = 5]}

      {[Tuple.third ("john", "danger", "doe") = "doe"]}
  *)
  val third : ('a * 'b * 'c) -> 'c
  
  (** Extract the first and second values of a {!Tuple3} as a {!Tuple}.

      {2 Examples}

      {[Tuple3.initial (3, "stressed", false) = (3, "stressed")]}

      {[Tuple3.initial ("john", 16, true) = ("john", 16)]}
  *)
  val initial : ('a * 'b * 'c) -> ('a * 'b)
  
  (** Extract the second and third values of a {!Tuple3} as a {!Tuple}.

      {2 Examples}

      {[Tuple3.tail (3, "stressed", false) = ("stressed", false)]}

      {[Tuple3.tail ("john", 16, true) = (16, true)]}
  *)
  val tail : ('a * 'b * 'c) -> ('b * 'c)
  
  (** Transform the first value in a tuple.

      {2 Examples}

      {[Tuple3.mapFirst ~f:String.reverse ("stressed", 16, false) = ("desserts", 16, false)]}

      {[Tuple3.mapFirst ~f:String.length ("stressed", 16, false) = (8, 16, false)]}
  *)
  val mapFirst : ('a * 'b * 'c) -> f:('a -> 'x) -> ('x * 'b * 'c)
  
  (** Transform the second value in a tuple.

      {2 Examples}

      {[Tuple3.mapSecond ~f:Float.squareRoot ("stressed", 16., false) = ("stressed", 4., false)]}

      {[Tuple3.mapSecond ~f:(~-) ("stressed", 16, false) = ("stressed", -16, false)]}
  *)
  val mapSecond : ('a * 'b * 'c) -> f:('b -> 'y) -> ('a * 'y * 'c)
  
  (** Transform the third value in a tuple.

      {2 Examples}

      {[Tuple3.mapThird ~f:not ("stressed", 16, false) ("stressed", 16, true)]}
  *)
  val mapThird : ('a * 'b * 'c) -> f:('c -> 'z) -> ('a * 'b * 'z)
  
  (** Transform each value in a tuple by applying [f] to the {!first} value, [g] to the {!second} value and [h] to the {!third} value.

      {2 Examples}

      {[
        Tuple3.mapEach
          ~f:String.reverse
          ~g:Float.squareRoot
          ~h:Bool.not
          ("stressed", 16., false) = ("desserts", 4., true)
      ]}
  *)
  val mapEach :
    ('a * 'b * 'c) ->
      f:('a -> 'x) -> g:('b -> 'y) -> h:('c -> 'z) -> ('x * 'y * 'z)
  
  (** Transform all the values of a tuple using the same function.

      [mapAll] can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple.mapAll ~f:Float.squareRoot (9., 16., 25.) = (3., 4., 5.)]}

      {[Tuple.mapAll ~f:String.length ("was", "stressed", "then") = (3, 8, 4)]}
  *)
  val mapAll : ('a * 'a * 'a) -> f:('a -> 'b) -> ('b * 'b * 'b)
  
  (** Move each value in the tuple one position to the left, moving the value in the first position into the last position.

      {2 Examples}

      {[Tuple.rotateLeft (3, 4, 5) = (4, 5, 3)]}

      {[Tuple.rotateLeft ("was", "stressed", "then") = ("stressed", "then", "was")]}
  *)
  val rotateLeft : ('a * 'b * 'c) -> ('b * 'c * 'a)
  
  (** Move each value in the tuple one position to the right, moving the value in the last position into the first position.

      {2 Examples}

      {[Tuple.rotateRight (3, 4, 5) = (5, 3, 4)]}

      {[Tuple.rotateRight ("was", "stressed", "then") = ("then", "was", "stressed")]}
  *)
  val rotateRight : ('a * 'b * 'c) -> ('c * 'a * 'b)

  (** {1 Conversion} *)

  (** Turns a tuple into a {!List} of length three.

      This function can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple3.toArray (3, 4, 5) = [3; 4; 5]]}

      {[Tuple3.toArray ("was", "stressed", "then") = ["was"; "stressed"; "then"]]}
  *)
  val toArray : ('a * 'a * 'a) -> 'a array
  
  
  (** Turns a tuple into a {!List} of length three.

      This function can only be used on tuples which have the same type for each value.

      {2 Examples}

      {[Tuple3.toList (3, 4, 5) = [3; 4; 5]]}

      {[Tuple3.toList ("was", "stressed", "then") = ["was"; "stressed"; "then"]]}
  *)
  val toList : ('a * 'a * 'a) -> 'a list
  
  (** {1 Comparison} *)

  (** Test two {!Tuple3}s for equality, using the provided functions to test the
      first, second and third components.

      {2 Examples}

      {[Tuple.equal Int.equal String.equal Char.equal (1, "Fox", 'j') (1, "Fox", 'k') = true]}

      {[Tuple.equal Int.equal String.equal Char.equal (1, "Fox", 'j') (2, "Hen", 'j') = false]}
   *)
  val equal :
    ('a -> 'a -> bool) ->
      ('b -> 'b -> bool) ->
        ('c -> 'c -> bool) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool
  
  (** Compare two {!Tuple3}s, using the provided functions to compare the first
      components then, if the first components are equal, the second components,
      then the third components

      {2 Examples}

      {[Tuple.compare Int.compare String.compare Char.compare (1, "Fox", 'j') (1, "Fox", 'j') = 0]}

      {[Tuple.compare Int.compare String.compare Char.compare (1, "Fox", 'j') (1, "Eel", 'j') = 1]}

      {[Tuple.compare Int.compare String.compare Char.compare (1, "Fox", 'j') (2, "Fox", 'm') = -1]}
   *)
  val compare :
    ('a -> 'a -> int) ->
      ('b -> 'b -> int) ->
        ('c -> 'c -> int) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
end

(** A collection of unique values *)
module Set : sig
   (** A {!Set} represents a unique collection of values.

      [Set] is an immutable data structure which means operations like {!Set.add} and {!Set.remove} do not modify the data structure, but return a new set with the desired changes.

      Since the usage is so common the {!Set.Int} and {!Set.String} modules are available, offering a convenient way to construct new sets.

      For other data types you can use {!Set.Poly} which uses OCaml's polymorphic [compare] function.

      The specialized modules {!Set.Int}, {!Set.String} are in general more efficient.
  *)

  type ('a, 'id) t


  (** {1 Construction} *)

  (** A [Set] can be constructed using one of the functions available in the {!Set.Int}, {!Set.String} or {!Set.Poly} sub-modules. *)

  (** {1 Basic operations} *)

  (** Insert a value into a set.

      {2 Examples}

      {[Set.add (Set.Int.ofList [1; 2]) 3 |> Set.toList = [1; 2; 3]]}

      {[Set.add (Set.Int.ofList [1; 2]) 2 |> Set.toList = [1; 2]]}
  *)
  val add : ('a, 'id) t -> 'a -> ('a, 'id) t
  
  
  (** Remove a value from a set, if the set doesn't contain the value anyway, returns the original set

      {2 Examples}

      {[Set.remove (Set.Int.ofList [1; 2]) 2 |> Set.toList = [1]]}

      {[
        let originalSet = Set.Int.ofList [1; 2] in
        let newSet = Set.remove orignalSet 3 in
        originalSet = newSet
      ]}
  *)
  val remove : ('a, 'id) t -> 'a -> ('a, 'id) t
  
  (** Determine if a value is in a set

      {2 Examples}

     {[Set.includes (Set.String.ofList ["Ant"; "Bat"; "Cat"]) "Bat" = true]}
  *)
  val includes : ('a, _) t -> 'a -> bool
  
  (** Determine the number of elements in a set.

      {2 Examples}

      {[Set.length (Set.Int.ofList [1; 2; 3]) = 3]}
  *)
  val length : (_, _) t -> int
  
  (** Returns, as an {!Option}, the first element for which [f] evaluates to [true]. If [f] doesn't return [true] for any of the elements [find] will return [None].

      {2 Examples}

      {[Set.find ~f:Int.isEven (Set.Int.ofList [1; 3; 4; 8]) = Some 4]}

      {[Set.find ~f:Int.isOdd (Set.Int.ofList [0; 2; 4; 8]) = None]}

      {[Set.find ~f:Int.isEven Set.Int.empty = None]}
  *)
  val find : ('value, _) t -> f:('value -> bool) -> 'value option
  
  (** {1 Query} *)

  (** Check if a set is empty.

      {2 Examples}

      {[Set.isEmpty (Set.Int.empty) = true]}

      {[Set.isEmpty (Set.Int.singleton 4) = false]}
  *)
  val isEmpty : (_, _) t -> bool
  
  (** Determine if [f] returns true for [any] values in a set.

      {2 Examples}

      {[Set.any (Set.Int.ofArray [|2;3|]) ~f:Int.isEven = true]}

      {[Set.any (Set.Int.ofList [1;3]) ~f:Int.isEven = false]}

      {[Set.any (Set.Int.ofList []) ~f:Int.isEven = false]}
  *)
  val any : ('value, _) t -> f:('value -> bool) -> bool
  
  (** Determine if [f] returns true for [all] values in a set.

      {2 Examples}

      {[Set.all ~f:Int.isEven (Set.Int.ofArray [|2;4|]) = true]}

      {[Set.all ~f:Int.isEven (Set.Int.ofLis [2;3]) = false]}

      {[Set.all ~f:Int.isEven Set.Int.empty = true]}
  *)
  val all : ('value, _) t -> f:('value -> bool) -> bool
  
  (** {1 Combine} *)

  (** Returns a new set with the values from the first set which are not in the second set.

      {2 Examples}

      {[Set.difference (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList = [1;5]]}

      {[Set.difference (Set.Int.ofList [2;3;4]) (Set.Int.ofList [1;2;5]) |> Set.toList = [3;4]]}
  *)
  val difference : ('a, 'id) t -> ('a, 'id) t -> ('a, 'id) t
  
  (** Get the intersection of two sets. Keeps values that appear in both sets.

      {2 Examples}

      {[Set.intersection (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList= [2]]}
  *)
  val intersection : ('a, 'id) t -> ('a, 'id) t -> ('a, 'id) t
  
  (** Get the union of two sets. Keep all values.

      {2 Examples}

      {[Set.union (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList = [1;2;3;4;5]]}
  *)
  val union : ('a, 'id) t -> ('a, 'id) t -> ('a, 'id) t
  
  (** {1 Transform} *)

  (** Keep elements that [f] returns [true] for.

      {2 Examples}

      {[Set.filter (Set.Int.ofList [1;2;3]) ~f:Int.isEven |> Set.toList = [2]]}
  *)
  val filter : ('a, 'id) t -> f:('a -> bool) -> ('a, 'id) t
  
  (** Divide a set into two according to [f]. The first set will contain the values that [f] returns [true] for, values that [f] returns [false] for will end up in the second.

      {2 Examples}

      {[
        let numbers = Set.Int.ofList [1; 1; 5; 6; 5; 7; 9; 8] in
        let (evens, odds) = Set.partition numbers ~f:Int.isEven in
        Set.toList evens = [6; 8]
        Set.toList odds = [1; 5; 7; 9]
      ]}
  *)
  val partition :
    ('a, 'id) t -> f:('a -> bool) -> (('a, 'id) t * ('a, 'id) t)
  
  (** Transform a set into a value which is result of running each element in the set through [f], where each successive invocation is supplied the return value of the previous.

    See {!Array.fold} for a more in-depth explanation.

    {2 Examples}

    {[Set.fold ~f:( * ) ~initial:1 (Set.Int.ofList [1;2;3;4]) = 24]}
  *)
  val fold : ('a, _) t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b
  
  (** Runs a function [f] against each element of the set. *)
  val forEach : ('a, _) t -> f:('a -> unit) -> unit
  
  (** {1 Conversion} *)

  (** Converts a set into an {!Array} *)
  val toArray : ('a, _) t -> 'a array
  
  (** Converts a set into a {!List}. *)
  val toList : ('a, _) t -> 'a list
  
  (** Construct sets which can hold any data type using the polymorphic [compare] function. *)
  module Poly : sig
    type identity
    type nonrec 'a t = ('a, identity) t

    (** The empty set.

        A great starting point.
     *)
    val empty : unit -> 'a t
    
    (** Create a set of a single value

        {2 Examples}

        {[Set.Int.singleton (5, "Emu") |> Set.toList = [(5, "Emu")]]}
    *)
    val singleton : 'a -> 'a t
    
    
    (** Create a set from an {!Array}

        {2 Examples}

        {[Set.Poly.ofArray [(1, "Ant");(2, "Bat");(2, "Bat")] |> Set.toList = [(1, "Ant"); (2, "Bat")]]}
    *)
    val ofArray : 'a array -> 'a t
    
    (** Create a set from a {!List}

      {2 Examples}

      {[Set.Poly.ofList [(1, "Ant");(2, "Bat");(2, "Bat")] |> Set.toList = [(1, "Ant"); (2, "Bat")]]}
    *)
    val ofList : 'a list -> 'a t
  end

  (** Construct sets of {!Int}s *)
  module Int : sig
    type nonrec t = (Int.t, Int.identity) t

    (** A set with nothing in it. *)
    val empty : t


    (** Create a set from a single {!Int}

      {2 Examples}

      {[Set.Int.singleton 5 |> Set.toList = [5]]}
    *)
    val singleton : int -> t
    
    
    (** Create a set from an {!Array}

        {2 Examples}

        {[Set.Int.ofArray [|1;2;3;3;2;1;7|] |> Set.toArray = [|1;2;3;7|]]}
    *)
    val ofArray : int array -> t
    
    (** Create a set from a {!List}

        {2 Examples}

        {[Set.Int.ofList [1;2;3;3;2;1;7] |> Set.toList = [1;2;3;7]]}
    *)
    val ofList : int list -> t
  end

  (** Construct sets of {!String}s *)
  module String : sig
    type nonrec t = (String.t, String.identity) t

    (** A set with nothing in it. *)
    val empty : t
    
    (** Create a set of a single {!String}

        {2 Examples}

        {[Set.String.singleton "Bat" |> Set.toList = ["Bat"]]}
    *)
    val singleton : String.t -> t
    
    (** Create a set from an {!Array}

        {2 Examples}

        {[Set.String.ofArray [|"a";"b";"g";"b";"g";"a";"a"|] |> Set.toArray = [|"a";"b";"g"|]]}
    *)
    val ofArray : String.t array -> t
    
    (** Create a set from a {!List}

        {2 Examples}

        {[Set.String.ofList [|"a";"b";"g";"b";"g";"a";"a"|] |> Set.toList = ["a";"b";"g"]]}
    *)
    val ofList : String.t list -> t
  end
end

(** A collection of key-value pairs *)
module Map : sig
  (** A [Map] represents a unique mapping from keys to values.

      [Map] is an immutable data structure which means operations like {!Map.add} and {!Map.remove} do not modify the data structure, but return a new map with the desired changes.

      Since the usage is so common the {!Map.Int} and {!Map.String} modules are available, offering a convenient way to construct new Maps.

      For other data types you can use {!Map.Poly} which internally uses OCaml's polymorphic [compare] function on the keys.

      The specialized modules {!Map.Int}, {!Map.String} are in general more efficient.
  *)

  type ('key, 'value, 'id) t


  (** {1 Construction}

      A [Map] can be constructed using one of the functions available in {!Map.Int}, {!Map.String} or {!Map.Poly}
  *)

  (** {1 Basic operations} *)

  (** Adds a new entry to a map. If [key] is allready present, its previous value is replaced with [value].

      {2 Examples}

      {[
        Map.add 
          (Map.Int.ofList [(1, "Ant"); (2, "Bat")]) 
          ~key:3 
          ~value:"Cat" 
        |> Map.toList = [(1, "Ant"); (2, "Bat"); (3, "Cat")]
      ]}

      {[Map.add (Map.Int.ofList [(1, "Ant"); (2, "Bat")]) ~key:2 ~value:"Bug" |> Map.toList = [(1, "Ant"); (2, "Bug")]]}
  *)
  val add : ('key, 'value, 'id) t -> key:'key -> value:'value -> ('key, 'value, 'id) t
  
  (** Removes a key-value pair from a map based on they provided key.

      {2 Examples}
      {[
        let animalPopulations = Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ] in
        Map.remove animalPopulations "Mosquito" |> Map.toList = [
          ("Elephant", 3_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
      ]}
  *)
  val remove : ('key, 'value, 'id) t -> 'key -> ('key, 'value, 'id) t
  
  (** Get the value associated with a key. If the key is not present in the map, returns [None].

      {2 Examples}

      let animalPopulations = Map.String.ofList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ] in
      Map.get animalPopulations "Shrew" = Some 56_423;
  *)
  val get : ('key, 'value, 'id) t -> 'key -> 'value option
  
  (** Returns, as an {!Option} the first key-value pair for which [f] evaluates to true.

      If [f] doesn't return [true] for any of the elements [find] will return [None].

      Searches starting from the smallest {b key}

      {2 Examples}

      {[
        Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
        |> Map.find ~f:(fun ~key ~value -> value > 10_000)
          = Some ("Mosquito", 56_123_156)
      ]}
  *)
  val find : ('key, 'value, _) t -> f:(key:'key -> value:'value -> bool) -> ('key * 'value) option
  
  
  (** Update the value for a specific key using [f]. If [key] is not present in the map [f] will be called with [None].

      {2 Examples}

      {[
        let animalPopulations = Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ] in

        Map.update animalPopulations ~key:"Hedgehog" ~f:(fun population ->
          match population with
          | None -> Some 1
          | Some count -> Some (count + 1)
        )
        |> Map.toList = [
          ("Elephant", 3_156);
          ("Hedgehog", 1);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
      ]}
  *)
  val update :
    ('key, 'value, 'id) t ->
      key:'key -> f:('value option -> 'value option) -> ('key, 'value, 'id) t
  
  (** Returns the number of key-value pairs present in the map.

      {2 Examples}

      {[
        Map.Int.ofList [(1, "Hornet"); (3, "Marmot")]
        |> Map.length = 2
      ]}
  *)
  val length : (_, _, _) t -> int
  
  (** Returns, as an {!Option}, the smallest {b key } in the map.

      Returns [None] if the map is empty.

      {2 Examples}

      {[
        Map.Int.ofList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")]
        |> Map.minimum = Some 1
      ]}
  *)
  val minimum : ('key, _, _) t -> 'key option
  
  (** Returns the largest {b key } in the map.

      Returns [None] if the map is empty.

      {2 Examples}

      {[
        Map.Int.ofList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")] 
        |> Map.maximum = Some 8
      ]}
  *)
  val maximum : ('key, _, _) t -> 'key option
  
  (** Returns, as an {!Option}, a {!Tuple} of the [(minimum, maximum)] {b key}s in the map.

      Returns [None] if the map is empty.

      {2 Examples}

      {[
        Map.Int.ofList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")]
        |> Map.extent = Some (1, 8)
      ]}
  *)
  val extent : ('key, _, _) t -> ('key * 'key) option
  
  (** {1 Query} *)

  (** Determine if a map is empty. *)
  val isEmpty : (_, _, _) t -> bool
  
  (** Determine if a map includes [key].  *)
  val includes : ('key, _, _) t -> 'key -> bool
  
  (** Determine if [f] returns [true] for [any] values in a map. *)
  val any : (_, 'value, _) t -> f:('value -> bool) -> bool
  
  (** Determine if [f] returns [true] for [all] values in a map. *)
  val all : (_, 'value, _) t -> f:('value -> bool) -> bool
  
  (** {1 Combine} *)

  (** Combine two maps.

      You provide a function [f] which is provided the key and the optional
      value from each map and needs to account for the three possibilities:

      1. Only the 'left' map includes a value for the key.
      2. Both maps contain a value for the key.
      3. Only the 'right' map includes a value for the key.

      You then traverse all the keys, building up whatever you want.

      {2 Examples}

      {[
        let animalToPopulation = 
          Map.String.ofList [
            ("Elephant", 3_156);
            ("Shrew", 56_423);
          ] 
        in
        let animalToPopulationGrowthRate = Map.String.ofList [
          ("Elephant", 0.88);
          ("Squirrel", 1.2);
          ("Python", 4.0);
        ] in

        Map.merge 
          animalToPopulation 
          animalToPopulationGrowthRate 
          ~f:(fun _animal population growth ->
            match (Option.both population growth) with
            | Some (population, growth) -> 
                Some Float.((ofInt population) * growth)
            | None -> None
          )
        |> Map.toList
          = [("Elephant", 2777.28)]
      ]}
  *)
  val merge :
    ('key, 'v1, 'id) t ->
      ('key, 'v2, 'id) t ->
        f:('key -> 'v1 option -> 'v2 option -> 'v3 option) -> ('key, 'v3, 'id) t
  
  (** {1 Transform} *)

  (** Apply a function to all values in a dictionary.

      {2 Examples}

      {[
        Map.String.ofList [
          ("Elephant", 3_156);
          ("Shrew", 56_423);
        ]
        |> Map.map ~f:Int.toString
        |> Map.toList
          = [
          ("Elephant", "3156");
          ("Shrew", "56423");
        ]
      ]}
  *)
  val map : ('key, 'value, 'id) t -> f:('value -> 'b) -> ('key, 'b, 'id) t
  
  (** Like {!map} but [f] is also called with each values corresponding key *)
  val mapI : ('key, 'va, 'i) t -> f:('key -> 'va -> 'vb) -> ('key, 'vb, 'i) t
  
  (** Keep elements that [f] returns [true] for.

      {2 Examples}

      {[
        Map.String.ofList [
          ("Elephant", 3_156);
          ("Shrew", 56_423);
        ]
        |> Map.map ~f:(fun population -> population > 10_000)
        |> Map.toList
          = [
          ("Shrew", "56423");
        ]
      ]}
  *)
  val filter : ('key, 'value, 'id) t -> f:('value -> bool) -> ('key, 'value, 'id) t

  (** Divide a map into two, the first map will contain the key-value pairs that [f] returns [true] for, pairs that [f] returns [false] for will end up in the second.

      {2 Examples}

      {[
        let (endangered, notEndangered) = Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
        |> Map.partition ~f:(fun population -> population < 10_000)
        in

        Map.toList endangered = [
          ("Elephant", 3_156);
          ("Rhino", 3);
        ];

        Map.toList notEndangered = [
          ("Mosquito", 56_123_156);
          ("Shrew", 56_423);
        ];
      ]}
  *)
  val partition :
    ('key, 'value, 'id) t ->
      f:(key:'key -> value:'value -> bool) -> (('key, 'value, 'id) t * ('key, 'value, 'id) t)
  
  (** Like {!Array.fold} but [f] is also called with both the [key] and [value] 

      {2 Examples}

      TODO
  *)
  val fold :
    ('key, 'value, _) t -> initial:'a -> f:('a -> key:'key -> value:'value -> 'a) -> 'a
  

  (** {1 Iterate} *)

  (** Runs a function [f] against each {b value} in the map.
  

      {2 Examples}

      TODO
  *)
  val forEach : (_, 'value, _) t -> f:('value -> unit) -> unit
  
  
  (** {1 Conversion} *)

  (** Get a {!List} of all of the keys in a map.

      {2 Examples}

      {[
        Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
        |> Map.keys = [
          "Elephant";
          "Mosquito";
          "Rhino";
          "Shrew";
        ]
      ]}
  *)
  val keys : ('key, _, _) t -> 'key list
  
  (** Get a {!List} of all of the values in a map.

      {2 Examples}

      {[
        Map.String.ofList [
          ("Elephant", 3_156);
          ("Mosquito", 56_123_156);
          ("Rhino", 3);
          ("Shrew", 56_423);
        ]
        |> Map.values = [
          3_156;
          56_123_156;
          3;
          56_423;
        ]
      ]}
  *)
  val values : (_, 'value, _) t -> 'value list

  (** Get an {!Array} of all of the key-value pairs in a map. *)
  val toArray : ('key, 'value, _) t -> ('key * 'value) array

  (** Get a {!List} of all of the key-value pairs in a map. *)
  val toList : ('key, 'value, _) t -> ('key * 'value) list

  (** Construct a Map which can be keyed by any data type using the polymorphic [compare] function. *)
  module Poly : sig
    type identity
    type nonrec ('key, 'value) t = ('key, 'value, identity) t
    
    (** A map with nothing in it. *)
    val empty : unit -> ('key, 'value) t

    (** Create a map from a key and value

        {2 Examples}
    
        {[Map.Poly.singleton ~key:false ~value:1 |> Map.toList = [(false, 1)]]}
    *)
    val singleton : key:'key -> value:'value -> ('key, 'value) t

    (** Create a map from an {!Array} of key-value tuples *)
    val ofArray : ('key * 'value) array -> ('key, 'value) t
    
    (** Create a map from a {!List} of key-value tuples *)
    val ofList : ('key * 'value) list -> ('key, 'value) t
  end

  (** Construct a Map with {!Int}s for keys. *)
  module Int : sig
    type nonrec 'value t = (Int.t, 'value, Int.identity) t
    
    (** A map with nothing in it. *)
    val empty : 'value t
    
    (** Create a map from a key and value

        {2 Examples}
        
        {[Map.Int.singleton ~key:1 ~value:"Ant" |> Map.toList = [(1, "Ant")]]}
    *)
    val singleton : key:int -> value:'value -> 'value t
    
    (** Create a map from an {!Array} of key-value tuples *)
    val ofArray : (int * 'value) array -> 'value t
    
    (** Create a map of a {!List} of key-value tuples *)
    val ofList : (int * 'value) list -> 'value t
  end

  (** Construct a Map with {!String}s for keys. *)

  module String : sig
    type nonrec 'value t = (String.t, 'value, String.identity) t
    
    (** A map with nothing in it. *)
    val empty : 'value t
    
    (** Create a map from a key and value

        {2 Examples}
        
        {[Map.String.singleton ~key:"Ant" ~value:1 |> Map.toList = [("Ant", 1)]]}
    *)
    val singleton : key:string -> value:'value -> 'value t
    
    (** Create a map from an {!Array} of key-value tuples *)
    val ofArray : (string * 'value) array -> 'value t

    (** Create a map from a {!List} of key-value tuples *)
    val ofList : (string * 'value) list -> 'value t
  end
end

(** Functions for working with functions. *)
module Fun : sig
  (** Functions for working with functions.

      While the functions in this module can often make code more concise, this
      often imposes a readability burden on future readers.
  *) 
  
  (** Given a value, returns exactly the same value. This may seem pointless at first glance but it can often be useful when an api offers you more control than you actually need.

      Perhaps you want to create an array of integers

      {[Array.initialize 6 ~f:Fun.identity = [|0; 1; 2; 3; 4; 5|]]}

      (In this particular case you probably want to use {!Array.range}.)

      Or maybe you need to register a callback, but dont want to do anything:

      {[
        let httpMiddleware = 
          HttpLibrary.createMiddleWare
            ~onEventYouDoCareAbout:transformAndReturn
            ~onEventYouDontCareAbout:Fun.identity        
      ]}
  *)
  external identity : 'a -> 'a = "%identity"
  
  (** Discards the value it is given and returns [()]

      This is primarily useful when working with imperative side-effecting code
      or to avoid [unused value] compiler warnings when you really meant it,
      and haven't just made a mistake.

      {2 Examples}

      {[
        (* Pretend we have a module with the following signature:
            module PretendMutableQueue : sig
              type 'a t

              (** Adds an element to the queue, returning the new length of the queue *)
              val pushReturningLength : 'a t -> 'a -> int
            end
        *)

        let addListToQueue queue list =
          List.forEach list ~f:(fun element ->
            ignore (MutableQueue.pushReturningLength queue element)
          )
        in ()
      ]}
  *)
  external ignore : _ -> unit = "%ignore"
  
  (** Create a function that {b always} returns the same value.

      Useful with functions like {!List.map} or {!Array.initialize}

      {2 Examples}

      {[List.map ~f:(Fun.constant 0) [1;2;3;4;5] = [0;0;0;0;0]]}

      {[Array.initialize 6 ~f:(Fun.constant 0) = [|0;0;0;0;0;0|]]}
  *)
  val constant : 'a -> 'b -> 'a

  (** A function which always returns its second argument. *)
  val sequence : 'a -> 'b -> 'b
  
  (** Reverses the argument order of a function.

      For any arguments [x] and [y], [(flip f) x y] is the same as [f y x].

      Perhaps you want to [fold] something, but the arguments of a function you
      already have access to are in the wrong order.

      {2 Examples}

      TODO
  *)
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

  (** See {!Fun.(<|)} *)
  val apply : ('a -> 'b) -> 'a -> 'b

  (** Like {!(|>)} but in the opposite direction.

      [f <| x] is exactly the same as [f x].

      Maybe you want to apply a function to a [match] expression? That sort of thing.

      {2 Examples}

      TODO
  *)
  val (<|) : ('a -> 'b) -> 'a -> 'b

  (** See {!Fun.(|>)} *) 
  external pipe : 'a -> ('a -> 'b) -> 'b = "%revapply"

  (** Saying [x |> f] is exactly the same as [f x], just a bit longer.

      It is called the "pipe" operator because it lets you write "pipelined" code.

      It can make nested function calls more readable.

      For example, say we have a [sanitize] function for turning user input into
      integers:

      {[
        (* Before *)
        let sanitize (input: string) : int option =
          Int.ofString (String.trim input)
      ]}

      We can rewrite it like this:

      {[
        (* After *)
        let sanitize (input: string) : int option =
          input
          |> String.trim
          |> Int.ofString
      ]}

      This can be overused! When you have three or four steps, the code often gets clearer if you break things out into
      some smaller piplines assigned to variables. Now the transformation has a name, maybe it could have a type annotation.

      It can often be more self-documenting that way!
  *)
  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
  
  (** Function composition, passing results along in the suggested direction.

      For example, the following code (in a very roundabout way) checks if a number divided by two is odd:

      {[let isHalfOdd = Fun.(not << Int.isEven << Int.divide ~by:2)]}

      You can think of this operator as equivalent to the following:

      {[(g << f) = (fun x -> g (f x))]}

      So our example expands out to something like this:

      {[let isHalfOdd = fun n -> not (Int.isEven (Int.divide ~by:2 n))]}
  *)
  val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  
  (** See {!Fun.compose} *)
  val (<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

  (** Function composition, passing results along in the suggested direction.

      For example, the following code checks if the square root of a number is odd:

      {[Int.squareRoot >> Int.isEven >> not]}
  *)
  val composeRight : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

  (** See {!Fun.composeRight} *)
  val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

  (** Useful for performing some side affect in {!Fun.pipe}-lined code.

      Most commonly used to log a value in the middle of a pipeline of function calls.

      {2 Examples}

      {[
        let sanitize (input: string) : int option =
          input
          |> String.trim
          |> Fun.tap ~f:(fun trimmedString -> print_endline trimmedString)
          |> Int.ofString
      ]}

      {[
        Array.filter [|1;3;2;5;4;|] ~f:Int.isEven
        |> Fun.tap ~f:(fun numbers -> numbers.(0) <- 0)
        |> Fun.tap ~f:Array.reverseInPlace
        = [|4;0|]
      ]}
  *)
  val tap : 'a -> f:('a -> unit) -> 'a  

  (* TODO a better type than unit for the return value? *)
  (** Runs the provided function, forever. *)
  val forever : (unit -> unit) -> unit

  (** Runs a function repeatedly.

      {2 Examples}

      {[
        let count = ref 0
        times(10, fun () -> (count <- !count + 1))
        !count = 10
      ]}
  *)
  val times : int -> f:(unit -> unit) -> unit

  (** Takes a function [f] which takes a single argument of a tuple ['a * 'b] and returns a function which takes two arguments that can be partially applied.

      {2 Examples}

      {[
        let squareArea (width, height) = width * height in
        let curriedArea : float -> float -> float = curry squareArea in
        let sizes = [3, 4, 5] in
        List.map sizes ~f:(curriedArea 4) = [12; 16; 20]
      ]}
  *)
  val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
  
  (** Takes a function which takes two arguments and returns a function which takes a single argument of a tuple.

      {2 Examples}

      {[
        let sum (a : int) (b: int) : int = a + b in
        let uncurriedSum : (int * int) -> int = uncurry add in
        uncurriedSum (3, 4) = 7
      ]}
  *)
  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
    
  (** Like {!curry} but for a {!Tuple3} *)
  val curry3 : (('a * 'b * 'c) -> 'd) -> 'a -> 'b -> 'c -> 'd
  
  (** Like {!uncurry} but for a {!Tuple3} *)
  val uncurry3 : ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd
end