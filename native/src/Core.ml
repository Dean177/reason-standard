module Comparator = struct
  type ('a, 'identity) t = ('a, 'identity) Base.Comparator.t

  type ('a, 'identity) comparator = ('a, 'identity) t


  module type T = sig 
    type nonrec t    
    val compare : t -> t -> int
  end

  module type S = sig
    type nonrec t
    type identity
    val comparator : (t, identity) comparator
  end 

  type ('a, 'identity) s = (module S with type identity = 'identity and type t = 'a)

  (* let ofBaseComparator (baseComparator : ('a, 'id) Base.Map.comparator) : ('a, 'id) s = Obj.magic baseComparator *)

  let toBaseComparator (comparator : ('a, 'id) s) : ('a, 'id) Base.Map.comparator = Obj.magic comparator

  let opaque _ = Base.Sexp.Atom "<opaque>"

  let make ~compare =
    Obj.magic (Base.Comparator.make ~compare ~sexp_of_t:opaque)    

  module Make (M : T) = struct
    module BaseComparator = Base.Comparator.Make(struct
      include M
      let compare = M.compare
      let sexp_of_t = opaque
    end)
    include BaseComparator
    type identity = BaseComparator.comparator_witness
    let comparator = BaseComparator.comparator
  end  
end

module Bool = struct
  type t = bool

  external ( && ) : bool -> bool -> bool = "%sequand"

  external ( || ) : bool -> bool -> bool = "%sequor"

  let xor a b = (a && not b) || ((not a) && b)

  let not = not

  let negate f t = not (f t)

  let equal = ( = )

  let compare = compare

  let ofInt i = match i with 0 -> Some false | 1 -> Some true | _ -> None

  let ofString string =
    match string with "false" -> Some false | "true" -> Some true | _ -> None

  let toString = function true -> "true" | false -> "false"

  let toInt t = match t with true -> 1 | false -> 0
end

module Char = struct
  type t = char

  let toCode (c : char) = (Base.Char.to_int c : int)

  let ofCode (i : int) =
    (if 0 <= i && i <= 255 then Some (Char.chr i) else None : char option)

  let toString = Base.Char.to_string

  let ofString (str : string) =
    (match String.length str with 1 -> Some str.[0] | _ -> None : char option)

  let toDigit char =
    match char with '0' .. '9' -> Some (toCode char - toCode '0') | _ -> None

  let toLowercase = Base.Char.lowercase

  let toUppercase = Base.Char.uppercase

  let isLowercase = Base.Char.is_lowercase

  let isUppercase = Base.Char.is_uppercase

  let isLetter = Base.Char.is_alpha

  let isDigit = Base.Char.is_digit

  let isAlphanumeric = Base.Char.is_alphanum

  let isPrintable = Base.Char.is_print

  let isWhitespace = Base.Char.is_whitespace

  let equal: char -> char -> bool = ( = )

  let compare = compare

  type identity = Base.Char.comparator_witness

  let comparator = Base.Char.comparator
end

module Fun = struct
  external identity : 'a -> 'a = "%identity"

  external ignore : _ -> unit = "%ignore"

  let constant a _ = a

  let sequence a b = ignore a ; b

  let flip f a b = f b a

  let apply f a = f a

  let ( <| ) f a = f a

  external pipe : 'a -> ('a -> 'b) -> 'b = "%revapply"

  external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

  let compose f g a = f (g a)

  let ( << ) = compose

  let composeRight f g a = g (f a)

  let ( >> ) = composeRight

  let tap value ~f = f value ; value

  let rec times n ~f =
    if n <= 0 then ()
    else (
      f () ;
      times (n - 1) ~f )

  let forever f =
    try
      while true do
        f ()
      done;
      failwith "[while true] managed to return, you are in trouble now."
    with exn -> exn

  let curry (f : 'a * 'b -> 'c) (a : 'a) (b : 'b) = (f (a, b) : 'c)

  let uncurry (f : 'a -> 'b -> 'c) ((a, b) : 'a * 'b) = (f a b : 'c)

  let curry3 (f : 'a * 'b * 'c -> 'd) (a : 'a) (b : 'b) (c : 'c) =
    (f (a, b, c) : 'd)

  let uncurry3 (f : 'a -> 'b -> 'c -> 'd) ((a, b, c) : 'a * 'b * 'c) =
    (f a b c : 'd)
end

module Container = struct
  module type Sum = sig
    type t

    val zero : t

    val add : t -> t -> t
  end
end

module Tuple = struct
  type ('a, 'b) t = 'a * 'b

  let make a b = (a, b)

  let ofArray = function [|a; b|] -> Some (a, b) | _ -> None

  let ofList = function a :: b :: _ -> Some (a, b) | _ -> None

  let first (a, _) = a

  let second (_, b) = b

  let mapFirst (a, b) ~f = (f a, b)

  let mapSecond (a, b) ~f = (a, f b)

  let mapEach (a, b) ~f ~g = (f a, g b)

  let mapAll (a, b) ~f = (f a, f b)

  let swap (a, b) = (b, a)

  let toArray (a, b) = [|a; b|]

  let toList (a, b) = [a; b]

  let equal equalFirst equalSecond (a, b) (a', b') =
    equalFirst a a' && equalSecond b b'

  let compare compareFirst compareSecond (a, b) (a', b') =
    match compareFirst a a' with 0 -> compareSecond b b' | result -> result
end

module Tuple3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let make a b c = (a, b, c)

  let ofArray = function [|a; b; c|] -> Some (a, b, c) | _ -> None

  let ofList = function a :: b :: c :: _ -> Some (a, b, c) | _ -> None

  let first ((a, _, _) : 'a * 'b * 'c) = (a : 'a)

  let second ((_, b, _) : 'a * 'b * 'c) = (b : 'b)

  let third ((_, _, c) : 'a * 'b * 'c) = (c : 'c)

  let initial ((a, b, _) : 'a * 'b * 'c) = ((a, b) : 'a * 'b)

  let tail ((_, b, c) : 'a * 'b * 'c) = ((b, c) : 'b * 'c)

  let mapFirst (a, b, c) ~f = (f a, b, c)

  let mapSecond (a, b, c) ~f = (a, f b, c)

  let mapThird (a, b, c) ~f = (a, b, f c)

  let mapEach (a, b, c) ~f ~g ~h = (f a, g b, h c)

  let mapAll (a1, a2, a3) ~f = (f a1, f a2, f a3)

  let rotateLeft ((a, b, c) : 'a * 'b * 'c) = ((b, c, a) : 'b * 'c * 'a)

  let rotateRight ((a, b, c) : 'a * 'b * 'c) = ((c, a, b) : 'c * 'a * 'b)

  let toArray (a, b, c) = [|a; b; c|]

  let toList ((a, b, c) : 'a * 'a * 'a) = ([a; b; c] : 'a list)

  let equal equalFirst equalSecond equalThird (a, b, c) (a', b', c') =
    equalFirst a a' && equalSecond b b' && equalThird c c'

  let compare compareFirst compareSecond compareThird (a, b, c) (a', b', c') =
    match compareFirst a a' with
    | 0 -> (
      match compareSecond b b' with 0 -> compareThird c c' | result -> result )
    | result ->
        result
end

module Option = struct
  type 'a t = 'a option

  let some a = Some a

  let isSome = Option.is_some

  let isNone = Option.is_none

  let and_ ta tb = match isSome ta with true -> tb | false -> None

  let or_ ta tb = match isSome ta with true -> ta | false -> tb

  let flatMap t ~f = match t with Some x -> f x | None -> None

  let flatten = Option.join

  let both a b =
    match (a, b) with (Some a, Some b) -> Some (a, b) | _ -> None

  let map t ~f = Option.map f t

  let map2 (ta : 'a t) (tb : 'b t) ~(f : 'a -> 'b -> 'c) =
    (match (ta, tb) with (Some a, Some b) -> Some (f a b) | _ -> None : 'c t)

  let get t ~default = match t with None -> default | Some value -> value

  let ( |? ) t default = get t ~default

  let getUnsafe x =
    match x with
    | None ->
        raise (Invalid_argument "Option.getUnsafe called with None")
    | Some x ->
        x

  let forEach t ~f = Option.iter f t

  let toArray t = match t with None -> [||] | Some value -> [|value|]

  let toList t = match t with None -> [] | Some value -> [value]

  module Infix = struct    
    let ( >>= ) t f = flatMap t ~f

    let ( >>| ) t f = map t ~f
  end

  let equal equal a b =
    match (a, b) with
    | (None, None) ->
        true
    | (Some a', Some b') ->
        equal a' b'
    | _ ->
        false

  let compare compare a b =
    match (a, b) with
    | (None, None) ->
        0
    | (Some a', Some b') ->
        compare a' b'
    | (None, Some _) ->
        -1
    | (Some _, None) ->
        1
end

module Result = struct
  type ('ok, 'error) t = ('ok, 'error) Result.t

  let ok = Result.ok

  let error = Result.error

  let ofOption ma ~error =
    match ma with None -> Result.Error error | Some right -> Result.Ok right

  let isOk = Result.is_ok

  let isError = Result.is_error

  let both a b =
    match (a, b) with
    | (Ok a', Ok b') ->
        Ok (a', b')
    | (Error a', _) ->
        Error a'
    | (_, Error b') ->
        Error b'

  let flatten = Result.join

  let or_ a b = match a with Ok _ -> a | _ -> b

  let and_ a b = match a with Ok _ -> b | _ -> a

  let get = Result.value

  let ( |? ) t default = get t ~default

  let getUnsafe = Result.get_ok

  let getError t ~default =
    match t with Ok _ -> default | Error error -> error

  let map t ~f = Result.map f t

  let map2 a b ~f =
    match (a, b) with
    | (Ok a, Ok b) ->
        Ok (f a b)
    | (Error a, _) ->
        Error a
    | (_, Error b) ->
        Error b

  let mapError t ~f =
    match t with Error error -> Error (f error) | Ok value -> Ok value

  let values t =
    Base.List.fold_right t ~f:(map2 ~f:(fun a b -> a :: b)) ~init:(Ok []) 

  let toOption r = match r with Ok v -> Some v | Error _ -> None

  let flatMap t ~f = Result.bind t f

  let attempt f =
    match f () with value -> Ok value | exception error -> Error error

  let transpose t =
    match t with
    | Error error ->
        Some (Error error)
    | Ok None ->
        None
    | Ok (Some value) ->
        Some (Ok value)

  let forEach t ~f = match t with Ok a -> f a | _ -> ()

  let equal equalOk equalError a b =
    match (a, b) with
    | (Error a', Error b') ->
        equalError a' b'
    | (Ok a', Ok b') ->
        equalOk a' b'
    | _ ->
        false

  let compare (compareOk : 'ok -> 'ok -> int)
      (compareError : 'error -> 'error -> int) (a : ('ok, 'error) t)
      (b : ('ok, 'error) t) =
    ( match (a, b) with
      | (Error a', Error b') ->
          compareError a' b'
      | (Ok a', Ok b') ->
          compareOk a' b'
      | (Error _, Ok _) ->
          -1
      | (Ok _, Error _) ->
          1
      : int )

  module Infix = struct    
    let ( >>= ) t f = flatMap t ~f

    let ( >>| ) t f = map t ~f
  end
end

module Float = struct
  type t = float

  type radians = t

  let ofInt = Base.Float.of_int

  let ofString string =
    try Some (Base.Float.of_string string) with Invalid_argument _ -> None

  let zero = 0.0

  let one = 1.0

  let nan = Base.Float.nan

  let infinity = Base.Float.infinity

  let negativeInfinity = Base.Float.neg_infinity

  let e = Base.Float.euler

  let pi = Base.Float.pi

  let epsilon = Base.Float.epsilon_float

  let maximumSafeInteger = (2. ** 52.) -. 1.

  let minimumSafeInteger = (-2. ** 52.) -. 1.

  let largestValue = Base.Float.max_finite_value

  let smallestValue = Base.Float.min_positive_normal_value

  let add = ( +. )

  let ( + ) = ( +. )

  let subtract = ( -. )

  let ( - ) = ( -. )

  let multiply = ( *. )

  let ( * ) = ( *. )

  let divide n ~by = n /. by

  let ( / ) = ( /. )

  let power ~base ~exponent = base ** exponent

  let ( ** ) = ( ** )

  let negate = Base.Float.neg

  let ( ~- ) = negate

  let absolute = Base.Float.abs

  let isInteger t = t = Base.Float.round t

  let isSafeInteger t = isInteger t && t <= maximumSafeInteger

  let clamp n ~lower ~upper =
    if upper < lower then
      raise
        (Invalid_argument
           ( "~lower:" ^ Base.Float.to_string lower
           ^ " must be less than or equal to ~upper:"
           ^ Base.Float.to_string upper ))
    else if
      Base.Float.is_nan lower || Base.Float.is_nan upper || Base.Float.is_nan n
    then Base.Float.nan
    else max lower (min upper n)

  let inRange n ~lower ~upper =
    if
      let open Base.Float in
      upper < lower
    then
      raise
        (Invalid_argument
           ( "~lower:" ^ Base.Float.to_string lower
           ^ " must be less than or equal to ~upper:"
           ^ Base.Float.to_string upper ))
    else n >= lower && n < upper

  let squareRoot = sqrt

  let log n ~base =
    let open Base.Float in
    log10 n / log10 base

  let isNaN = Base.Float.is_nan

  let isInfinite = Base.Float.is_inf

  let isFinite n = (not (isInfinite n)) && not (isNaN n)

  let maximum x y = if isNaN x || isNaN y then nan else if y > x then y else x

  let minimum x y = if isNaN x || isNaN y then nan else if y < x then y else x

  let hypotenuse x y = squareRoot ((x * x) + (y * y))

  let degrees n = n * (pi / 180.0)

  let radians = Fun.identity

  let turns n = n * 2. * pi

  let cos = Base.Float.cos

  let acos = Base.Float.acos

  let sin = Base.Float.sin

  let asin = Base.Float.asin

  let tan = Base.Float.tan

  let atan = Base.Float.atan

  let atan2 ~y ~x = Base.Float.atan2 y x

  type direction =
    [ `Zero
    | `AwayFromZero
    | `Up
    | `Down
    | `Closest of [`Zero | `AwayFromZero | `Up | `Down | `ToEven] ]

  let round ?(direction = `Closest `Up) n =
    match direction with
    | (`Up | `Down | `Zero) as dir ->
        Base.Float.round n ~dir
    | `AwayFromZero ->
        if n < 0. then Base.Float.round n ~dir:`Down
        else Base.Float.round n ~dir:`Up
    | `Closest `Zero ->
        if n > 0. then Base.Float.round (n -. 0.5) ~dir:`Up
        else Base.Float.round (n +. 0.5) ~dir:`Down
    | `Closest `AwayFromZero ->
        if n > 0. then Base.Float.round (n +. 0.5) ~dir:`Down
        else Base.Float.round (n -. 0.5) ~dir:`Up
    | `Closest `Down ->
        Base.Float.round (n -. 0.5) ~dir:`Up
    | `Closest `Up ->
        Base.Float.round_nearest n
    | `Closest `ToEven ->
        Base.Float.round_nearest_half_to_even n

  let floor = Base.Float.round_down

  let ceiling = Base.Float.round_up

  let truncate = Base.Float.round_towards_zero

  let ofPolar (r, theta) = (r * cos theta, r * sin theta)

  let toPolar (x, y) = (hypotenuse x y, atan2 ~x ~y)

  let toInt = Base.Float.iround_towards_zero

  let toString = Base.Float.to_string

  let equal = ( = )

  let compare = compare
end

module Int = struct
  type t = int

  let ofString = int_of_string_opt

  let minimumValue = Base.Int.min_value

  let maximumValue = Base.Int.max_value

  let zero = 0

  let one = 1

  let add = ( + )

  let ( + ) = ( + )

  let subtract = ( - )

  let ( - ) = ( - )

  let multiply = ( * )

  let ( * ) = multiply

  let divide n ~by = n / by

  let ( / ) = ( / )

  let ( /. ) = Base.Int.( // )

  let power ~base ~exponent =
    let open Base.Int in
    base ** exponent

  let ( ** ) = Base.Int.( ** )

  let negate = ( ~- )

  let ( ~- ) = ( ~- )

  let remainder n ~by = Stdlib.(n mod by)

  let modulo n ~by =     
    ((if n < 0 then 2 * (abs n) else n) mod by)

  let ( mod ) n by = modulo n ~by

  let maximum = Base.Int.max

  let minimum = Base.Int.min

  let absolute n = Base.Int.abs n

  let isEven n = remainder n ~by:2 = 0

  let isOdd n = remainder n ~by:2 <> 0

  let clamp n ~lower ~upper =
    if upper < lower then
      raise
        (Invalid_argument
           ( "~lower:" ^ Base.Int.to_string lower
           ^ " must be less than or equal to ~upper:"
           ^ Base.Int.to_string upper ))
    else max lower (min upper n)

  let inRange n ~lower ~upper =
    if upper < lower then
      raise
        (Invalid_argument
           ( "~lower:" ^ Base.Int.to_string lower
           ^ " must be less than or equal to ~upper:"
           ^ Base.Int.to_string upper ))
    else n >= lower && n < upper

  let toFloat = Base.Int.to_float

  let toString = Base.Int.to_string

  let equal = ( = )

  let compare = compare

  type identity = Base.Int.comparator_witness

  let comparator = Base.Int.comparator
end

module Integer = struct  
  type t = Z.t
  include Comparator.Make(struct 
    type nonrec t = t
    let compare = Z.compare
  end)

  let ofInt = Z.of_int

  let ofInt64 = Z.of_int64

  let ofFloat float =
    match Z.of_float float with
    | integer ->
        Some integer
    | exception Z.Overflow ->
        None

  let ofString string =
    match Z.of_string string with value -> Some value | exception _ -> None

  let zero = Z.zero

  let one = Z.one

  let isEven t =
    let open Z in
    t mod ~$2 = zero

  let isOdd t =
    let open Z in
    t mod ~$2 <> zero

  let add = Z.add

  let ( + ) = Z.( + )

  let subtract = Z.sub

  let ( - ) = subtract

  let multiply = Z.mul

  let ( * ) = multiply

  let divide = Z.div

  let ( / ) = divide

  let divide n ~by = divide n by

  let negate = Z.neg

  let modulo (n : t) ~(by : t) : t = (Z.rem n by)

  let (mod) (n : t) (by : t) : t = (Z.rem n by)

  let remainder (n : t) ~(by : t) = (Z.rem n by)

  let ( ** ) = Z.( ** )

  let power ?modulo ~(base : t) ~(exponent : int) =
    match modulo with
    | Some modulus ->
        Z.powm base (ofInt exponent) modulus
    | None ->
        Z.pow base exponent

  let maximum a b = if a < b then b else a

  let minimum a b = if a > b then b else a

  let absolute n = if n < zero then negate n else n

  let clamp n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else max lower (min upper n)

  let inRange n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else n >= lower && n < upper

  let toInt t = if t > ofInt Int.maximumValue then None else Some (Z.to_int t)

  let toInt64 t =
    if t > ofInt64 Int64.max_int then None else Some (Z.to_int64 t)

  let toFloat = Z.to_float

  let toString = Z.to_string

  let equal = Z.equal

  let compare = Z.compare
end

module String = struct
  type t = string

  let initialize length ~f =
    Base.List.init length ~f |> Base.String.of_char_list

  let repeat t ~count =
    Base.List.init count ~f:(fun _ -> t) |> Base.String.concat

  let ofArray characters =
    let open Base in
    Array.to_list characters |> String.of_char_list

  let ofList = Base.String.of_char_list

  let length = String.length

  let isEmpty t = length t = 0

  let get = Base.String.get

  let getAt a ~index =
    if index >= 0 && index < length a then Some (Base.String.get a index)
    else None

  let (.?[]) (string: string) (index: int) : char option = 
    getAt string ~index

  let uncons (s : string) =
    ( match s with
      | "" ->
          None
      | s ->
          Some (s.[0], String.sub s 1 (String.length s - 1))
      : (char * string) option )

  let dropLeft (s : string) ~(count : int) =
    (Base.String.drop_prefix s count : string)

  let dropRight (s : string) ~(count : int) =
    (Base.String.drop_suffix s count : string)

  let split t ~(on : string) =
    (Str.split (Str.regexp_string on) t : string list)

  let startsWith t ~prefix = Base.String.is_prefix ~prefix t

  let endsWith t ~suffix = Base.String.is_suffix ~suffix t

  let toLowercase (s : string) = (String.lowercase_ascii s : string)

  let toUppercase (s : string) = (String.uppercase_ascii s : string)

  let uncapitalize (s : string) = (String.uncapitalize_ascii s : string)

  let capitalize (s : string) = (String.capitalize_ascii s : string)

  let isCapitalized (s : string) = (s = String.capitalize_ascii s : bool)

  let includes t ~substring = (Base.String.is_substring t ~substring : bool)

  let reverse = Base.String.rev

  let ofChar = Base.String.of_char

  let slice ?(to_ = 0) str ~from = String.sub str from (to_ - from)

  let insertAt t ~(index : int) ~(value : string) =
    ( let length = length t in
      let startCount = index in
      let endCount = length - index in
      let start = dropRight ~count:endCount t in
      let end_ = dropLeft ~count:startCount t in
      String.concat "" [start; value; end_]
      : string )

  let toArray string = Base.String.to_list string |> Array.of_list

  let toList = Base.String.to_list

  let trim string = Base.String.strip string

  let trimLeft string = Base.String.lstrip string

  let trimRight string = Base.String.rstrip string

  let padLeft string targetLength ~with_ = 
    if (length(string) >= targetLength) then 
      string 
    else (
      let paddingLength = targetLength - length(string) in
      let count = paddingLength / length(with_) in
      let padding = slice (repeat with_ ~count) ~from:0 ~to_:paddingLength in
      padding ^ string
    )

  let padRight string targetLength ~with_ = 
    if (length(string) >= targetLength) then 
      string 
    else (
      let paddingLength = targetLength - length(string) in
      let count = paddingLength / length(with_) in
      let padding = slice (repeat with_ ~count) ~from:0 ~to_:paddingLength in
      string ^ padding
    )

  let forEach = Base.String.iter

  let fold s ~initial ~f = Base.String.fold s ~init:initial ~f

  let equal = Base.String.equal

  let compare = Base.String.compare

  type identity = Base.String.comparator_witness

  let comparator = Base.String.comparator
end

module Set = struct
  type ('a, 'id) t = ('a, 'id) Base.Set.t

  module Of (M : Comparator.S) = struct
    type nonrec 'value t = (M.t, M.identity) t
  end

  let empty comparator = 
    (Base.Set.empty (Comparator.toBaseComparator comparator))    

  let singleton (comparator: ('a, 'identity) Comparator.s) (element: 'a) : ('a, 'identity) t = 
    (Base.Set.of_list (Comparator.toBaseComparator comparator) [element])

  let ofArray (comparator: ('a, 'identity) Comparator.s) (elements: 'a array) : ('a, 'identity) t  = 
    (Base.Set.of_list (Comparator.toBaseComparator comparator) (Array.to_list elements))

  let ofList (comparator: ('a, 'identity) Comparator.s) (elements: 'a list) : ('a, 'identity) t  = 
    (Base.Set.of_list (Comparator.toBaseComparator comparator) elements)

  let length = Base.Set.length

  let isEmpty = Base.Set.is_empty

  let includes = Base.Set.mem

  let (.?{}) (set: ('element, _) t) (element: 'element) : bool = 
    includes set element

  let add = Base.Set.add

  let remove = Base.Set.remove

  let difference = Base.Set.diff

  let intersection = Base.Set.inter

  let union = Base.Set.union

  let filter = Base.Set.filter

  let partition = Base.Set.partition_tf

  let find = Base.Set.find

  let all = Base.Set.for_all

  let any = Base.Set.exists

  let forEach = Base.Set.iter

  let fold s ~initial ~f = Base.Set.fold s ~init:initial ~f

  let toArray = Base.Set.to_array

  let toList = Base.Set.to_list

  module Poly = struct
    type identity = Base.Comparator.Poly.comparator_witness

    type nonrec 'a t = ('a, identity) t

    let empty () = Base.Set.Poly.empty

    let singleton = Base.Set.Poly.singleton

    let ofArray = Base.Set.Poly.of_array

    let ofList = Base.Set.Poly.of_list
  end

  module Int = struct
    type nonrec t = (Base.Int.t, Base.Int.comparator_witness) t

    let empty = Base.Set.empty (module Base.Int)

    let singleton = Base.Set.singleton (module Base.Int)

    let ofArray = Base.Set.of_array (module Base.Int)

    let ofList = Base.Set.of_list (module Base.Int)
  end

  module String = struct
    type nonrec t = (String.t, Base.String.comparator_witness) t

    let empty = Base.Set.empty (module Base.String)

    let singleton = Base.Set.singleton (module Base.String)

    let ofArray = Base.Set.of_array (module Base.String)

    let ofList = Base.Set.of_list (module Base.String)
  end
end

module Map = struct
  type ('key, 'value, 'id) t = ('key, 'value, 'id) Base.Map.t

  module Of (M : Comparator.S) = struct
    type nonrec 'value t = (M.t, 'value, M.identity) t
  end

  let keepLatestOnly = fun _ latest -> latest

  let empty (comparator : ('key, 'identity) Comparator.s) : ('key, 'value, 'identity) t = 
    (Base.Map.empty (Comparator.toBaseComparator comparator))    

  let singleton (comparator: ('key, 'identity) Comparator.s) ~key ~value : ('key, 'value, 'identity) t = 
    Base.Map.of_alist_reduce (Comparator.toBaseComparator comparator) [(key, value)] ~f:keepLatestOnly

  let ofArray (comparator: ('key, 'identity) Comparator.s) (elements: ('key * 'value) array) : ('key, 'value, 'identity) t  = 
    Base.Map.of_alist_reduce (Comparator.toBaseComparator comparator) (Array.to_list elements) ~f:keepLatestOnly

  let ofList (comparator: ('key, 'identity) Comparator.s) (elements: ('key * 'value) list) : ('key, 'value, 'identity) t  = 
    Base.Map.of_alist_reduce (Comparator.toBaseComparator comparator) elements ~f:keepLatestOnly

  let isEmpty = Base.Map.is_empty

  let includes = Base.Map.mem

  let length = Base.Map.length

  let minimum t = Base.Map.min_elt t |> Option.map ~f:Tuple.first

  let maximum t = Base.Map.max_elt t |> Option.map ~f:Tuple.first

  let extent t = Option.both (minimum t) (maximum t)

  let add m ~key ~value = Base.Map.set m ~key ~data:value

  let (.?{}<-) (map: ('key, 'value, 'id) t) (key: 'key) (value: 'value): ('key, 'value, 'id) t = 
    add map ~key ~value

  let remove = Base.Map.remove

  let get = Base.Map.find

  let (.?{}) (map: ('key, 'value, _) t) (key: 'key) : 'value option = 
    get map key  

  let update m ~key ~f = Base.Map.change m key ~f

  let merge m1 m2 ~f =
    Base.Map.merge m1 m2 ~f:(fun ~key desc ->
        match desc with
        | `Left v1 ->
            f key (Some v1) None
        | `Right v2 ->
            f key None (Some v2)
        | `Both (v1, v2) ->
            f key (Some v1) (Some v2))

  let map = Base.Map.map

  let mapI t ~f = Base.Map.mapi t ~f:(fun ~key ~data -> f key data)

  let filter = Base.Map.filter

  let partition m ~f =
    Base.Map.partition_mapi m ~f:(fun ~key ~data ->
        if f ~key ~value:data then `Fst data else `Snd data)

  let find m ~f =
    Base.Map.fold m ~init:None ~f:(fun ~key ~data matching ->
        match matching with
        | Some _ ->
            matching
        | None ->
            if f ~key ~value:data then Some (key, data) else None)

  let any = Base.Map.exists

  let all = Base.Map.for_all

  let forEach = Base.Map.iter

  let forEachI (map : ('key, 'value, _) t) ~(f:key:'key -> value:'value -> unit) : unit = 
    Base.Map.iteri map ~f:(fun ~key ~data -> f ~key ~value:data)

  let fold m ~initial ~f =
    Base.Map.fold m ~init:initial ~f:(fun ~key ~data acc ->
        f acc ~key ~value:data)

  let keys = Base.Map.keys

  let values = Base.Map.data

  let toArray m = Base.Map.to_alist m |> Base.List.to_array

  let toList m = Base.Map.to_alist m

  module Poly = struct
    type identity = Base.Comparator.Poly.comparator_witness

    type nonrec ('k, 'v) t = ('k, 'v, identity) t

    let empty () = Base.Map.Poly.empty

    let singleton ~key ~value = Base.Map.Poly.singleton key value

    let ofList l = Base.Map.Poly.of_alist_reduce l ~f:(fun _ curr -> curr)

    let ofArray a = Base.Array.to_list a |> ofList
  end

  module Int = struct
    type nonrec 'v t = (Int.t, 'v, Int.identity) t

    let empty = Base.Map.empty (module Base.Int)

    let singleton ~key ~value = Base.Map.singleton (module Base.Int) key value

    let ofList l =
      Base.Map.of_alist_reduce (module Base.Int) l ~f:(fun _ curr -> curr)

    let ofArray a = Base.Array.to_list a |> ofList
  end

  module String = struct
    type nonrec 'v t = (String.t, 'v, Base.String.comparator_witness) t

    let empty = Base.Map.empty (module Base.String)

    let singleton ~key ~value =
      Base.Map.singleton (module Base.String) key value

    let ofList l =
      Base.Map.of_alist_reduce (module Base.String) l ~f:(fun _ curr -> curr)

    let ofArray a = Base.Array.to_list a |> ofList
  end
end

module Array = struct
  type 'a t = 'a array

  let singleton (a : 'a) = ([|a|] : 'a array)

  let clone = Base.Array.copy

  let initialize (length : int) ~(f : int -> 'a) =
    if length <= 0 then [||] else Base.Array.init length ~f

  let repeat element ~length = initialize length ~f:(Fun.constant element)

  let range ?(from = 0) (to_ : int) =
    (Base.Array.init (max 0 (to_ - from)) ~f:(fun i -> i + from) : int array)

  let ofList = Base.List.to_array

  let length (a : 'a array) = (Base.Array.length a : int)

  let isEmpty (a : 'a array) = (length a = 0 : bool)

  let first t = if length t < 1 then None else Some t.(0)

  let last t = if length t < 1 then None else Some t.(length t - 1)

  let get = Base.Array.get

  let getAt a ~index =
    if index >= 0 && index < length a then Some (Base.Array.get a index)
    else None

  let (.?()) (array: 'element t) (index: int) : 'element option = 
    getAt array ~index

  let set = Base.Array.set

  let setAt t ~index ~value = set t index value

  let filter = Base.Array.filter

  let sum (type a) t (module M : Container.Sum with type t = a) =
    Base.Array.fold t ~init:M.zero ~f:M.add

  let filterMap = Base.Array.filter_map

  let flatMap = Base.Array.concat_map

  let fold a ~initial ~f = Base.Array.fold a ~init:initial ~f

  let foldRight a ~initial ~f =
    Base.Array.fold_right a ~init:initial ~f:(Fun.flip f)

  let count t ~f =
    fold t ~initial:0 ~f:(fun total element ->
        total + match f element with true -> 1 | false -> 0)

  let swap = Base.Array.swap

  let find = Base.Array.find

  let findIndex = Base.Array.findi

  let map = Base.Array.map

  let mapI = Base.Array.mapi

  let map2 (a : 'a array) (b : 'b array) ~(f : 'a -> 'b -> 'c) =
    ( let minLength = min (length a) (length b) in
      Base.Array.init minLength ~f:(fun i -> f a.(i) b.(i))
      : 'c array )

  let zip = map2 ~f:Tuple.make

  let map3 (arrayA : 'a array) (arrayB : 'b array) (arrayC : 'c array)
      ~(f : 'a -> 'b -> 'c -> 'd) =
    let minLength =
      Base.min (length arrayA) (Base.min (length arrayC) (length arrayB))
    in
    Base.Array.init minLength ~f:(fun i -> f arrayA.(i) arrayB.(i) arrayC.(i))

  let partition = Base.Array.partition_tf

  let splitAt a ~index =
    ( Base.Array.init index ~f:(fun i -> a.(i))
    , Base.Array.init (length a - 1) ~f:(fun i -> a.(index + i)) )

  let splitWhen a ~f =
    match findIndex a ~f:(fun _index element -> f element) with
    | None ->
        (a, [||])
    | Some (index, _) ->
        splitAt a ~index

  let unzip = Base.Array.unzip

  let append (a : 'a array) (a' : 'a array) =
    (Base.Array.append a a' : 'a array)

  let flatten (al : 'a array array) =
    (Base.Array.concat (Base.Array.to_list al) : 'a array)

  let intersperse array ~sep =
    Base.Array.init
      (max 0 ((Array.length array * 2) - 1))
      ~f:(fun i -> if i mod 2 <> 0 then sep else array.(i / 2))

  let any = Base.Array.exists

  let all = Base.Array.for_all

  let includes = Base.Array.mem

  let values t =
    fold t ~initial:[] ~f:(fun results element ->
        match element with None -> results | Some value -> value :: results)
    |> ofList

  let join t ~sep = Stdlib.String.concat sep (Array.to_list t)

  let slice ?to_ array ~from =
    let defaultTo = match to_ with None -> length array | Some i -> i in
    let sliceFrom =
      if from >= 0 then min (length array) from
      else max 0 (min (length array) (length array + from))
    in
    let sliceTo =
      if defaultTo >= 0 then min (length array) defaultTo
      else max 0 (min (length array) (length array + defaultTo))
    in
    if sliceFrom >= sliceTo then [||]
    else
      Base.Array.init (sliceTo - sliceFrom) ~f:(fun i -> array.(i + sliceFrom))

  let sliding ?(step = 1) a ~size =
    let n = Array.length a in
    if size > n then [||]
    else
      initialize
        (1 + ((n - size) / step))
        ~f:(fun i -> initialize size ~f:(fun j -> a.((i * step) + j)))

  let chunksOf t ~size = sliding t ~step:size ~size

  let maximum = Base.Array.max_elt

  let minimum = Base.Array.min_elt

  let extent t ~compare =
    fold t ~initial:None ~f:(fun range element ->
        match range with
        | None ->
            Some (element, element)
        | Some (min, max) ->
            Some
              ( ( match compare element min < 0 with
                | true ->
                    element
                | false ->
                    min )
              , match compare element max > 0 with
                | true ->
                    element
                | false ->
                    max ))

  let sort t = Base.Array.sort t

  let reverse = Base.Array.rev_inplace

  let forEach a ~f = Base.Array.iter a ~f

  let forEachI a ~f = Base.Array.iteri a ~f

  let groupBy t comparator ~f =
    fold t ~initial:(Map.empty comparator) ~f:(fun map element -> (
      let key = f element in
      Map.update map ~key ~f:(function
          | None -> Some [element]
          | Some elements -> Some (element :: elements)
      )
    ))

  let toList (a : 'a array) = (Base.Array.to_list a : 'a list)

  let toIndexedList a =
    Base.Array.fold_right a
      ~init:(length a - 1, [])
      ~f:(fun x (i, acc) -> (i - 1, (i, x) :: acc))
    |> Base.snd

  let equal equal a b =
    if length a <> length b then false
    else if length a = 0 then true
    else (
      let rec loop index =
        if index = length a then true
        else equal a.(index) b.(index) && loop (index + 1)
      in
      loop 0 )

  let compare compare a b =
    match Int.compare (length a) (length b) with
    | 0 ->
        if length a == 0 then 0
        else (
          let rec loop index =
            if index = length a then 0
            else (
              match compare a.(index) b.(index) with
              | 0 ->
                  loop (index + 1)
              | result ->
                  result )
          in
          loop 0 )
    | result ->
        result
end

module List = struct
  type 'a t = 'a list

  let empty = []

  let singleton = Base.List.return

  let repeat element ~times = Base.List.init times ~f:(fun _ -> element)

  let rec range ?(from = 0) to_ =
    if from >= to_ then [] else from :: range ~from:(from + 1) to_

  let initialize = Base.List.init

  let sum (type a) (a : a t) (module M : Container.Sum with type t = a) =
    (Base.List.fold a ~init:M.zero ~f:M.add : a)

  let ofArray = Base.Array.to_list

  let isEmpty (l : 'a list) = (l = [] : bool)

  let head = Base.List.hd

  let tail = Base.List.tl

  let cons list element = element :: list

  let take t ~count = Base.List.take t count

  let takeWhile (l : 'a list) ~(f : 'a -> bool) =
    ( let rec takeWhileHelper acc l' =
        match l' with
        | [] ->
            Base.List.rev acc
        | x :: rest ->
            if f x then takeWhileHelper (x :: acc) rest else Base.List.rev acc
      in
      takeWhileHelper [] l
      : 'a list )

  let drop t ~count = Base.List.drop t count

  let rec dropWhile (l : 'a list) ~(f : 'a -> bool) =
    ( match l with
      | [] ->
          []
      | x :: rest ->
          if f x then dropWhile ~f rest else l
      : 'a list )

  let initial (l : 'a list) =
    ( match Base.List.rev l with
      | [] ->
          None
      | _ :: rest ->
          Some (Base.List.rev rest)
      : 'a list option )

  let rec last (l : 'a list) =
    ( match l with [] -> None | [a] -> Some a | _ :: tail -> last tail
      : 'a option )

  let append (l1 : 'a list) (l2 : 'a list) = (Base.List.append l1 l2 : 'a list)

  let flatten = Base.List.concat

  let map2 = Base.List.map2_exn

  let map3 = Base.List.map3_exn

  let reverse (l : 'a list) = (Base.List.rev l : 'a list)

  let map = Base.List.map

  let mapI = Base.List.mapi

  let flatMap = Base.List.concat_map

  let includes = Base.List.mem

  let find = Base.List.find

  let findIndex = Base.List.findi

  let any = Base.List.exists

  let all = Base.List.for_all

  let getAt (l : 'a list) ~(index : int) = (Base.List.nth l index : 'a option)

  let filterMap = Base.List.filter_map

  let filter t ~f = Base.List.filter t ~f

  let filterI t ~f = Base.List.filteri t ~f

  let partition = Base.List.partition_tf

  let fold t ~initial ~f = Base.List.fold t ~init:initial ~f

  let count = Base.List.count

  let foldRight t ~initial ~f =
    Base.List.fold_right t ~init:initial ~f:(Fun.flip f)

  let splitAt (l : 'a list) ~(index : int) =
    ((take ~count:index l, drop ~count:index l) : 'a list * 'a list)

  let splitWhen (l : 'a list) ~(f : 'a -> bool) =
    ( match findIndex ~f:(fun _ element -> f element) l with
      | Some (index, _) ->
          splitAt ~index l
      | None ->
          (l, [])
      : 'a list * 'a list )

  let updateAt (l : 'a list) ~(index : int) ~(f : 'a -> 'a) =
    ( if index < 0 then l
      else (
        let (front, back) = splitAt ~index l in
        match back with [] -> l | x :: rest -> append front (f x :: rest) )
      : 'a list )

  let length (l : 'a list) = (List.length l : int)

  let removeAt (l : 'a list) ~(index : int) =
    ( if index < 0 then l
      else (
        let (front, back) = splitAt ~index l in
        match tail back with None -> l | Some t -> append front t )
      : 'a list )

  let minimum = Base.List.min_elt

  let maximum = Base.List.max_elt

  let extent t ~compare =
    fold t ~initial:None ~f:(fun current element ->
        match current with
        | None ->
            Some (element, element)
        | Some (min, max) ->
            Some
              ( ( match compare element min < 0 with
                | true ->
                    element
                | false ->
                    min )
              , match compare element max > 0 with
                | true ->
                    element
                | false ->
                    max ))

  let insertAt (t : 'a list) ~(index : int) ~(value : 'a) =
    ( let (front, back) = splitAt t ~index in
      append front (value :: back)
      : 'a list )

  let zip listA listB = 
    let rec loop result xs ys = 
      match (xs, ys) with
      | [], _ -> result
      | _, [] -> result
      | x :: xs, y :: ys -> loop ((x, y) :: result) xs ys
    in
    loop [] listA listB

  let unzip = Base.List.unzip

  let sliding ?(step = 1) (t : 'a t) ~(size : int) =
    ( let rec takeAllOrEmpty t n (current, count) =
        if count = n then reverse current
        else (
          match t with
          | [] ->
              []
          | x :: xs ->
              takeAllOrEmpty xs n (x :: current, count + 1) )
      in
      let rec loop t =
        if isEmpty t then []
        else (
          let sample = takeAllOrEmpty t size ([], 0) in
          if isEmpty sample then [] else sample :: loop (Base.List.drop t step)
          )
      in
      loop t
      : 'a t t )

  let chunksOf t ~size = sliding t ~step:size ~size

  let intersperse (l : 'a list) ~sep =
    ( match l with
      | [] ->
          []
      | [x] ->
          [x]
      | x :: rest ->
          x :: foldRight rest ~initial:[] ~f:(fun acc x -> sep :: x :: acc)
      : 'a list )

  let forEach l ~f = Base.List.iter l ~f

  let forEachI = Base.List.iteri

  let toArray = Base.List.to_array

  let groupWhile t ~f = Base.List.group t ~break:f

  let sort = Base.List.sort

  let join t ~sep = Stdlib.String.concat sep t

  let groupBy t comparator ~f =
    fold t ~initial:(Map.empty comparator) ~f:(fun map element -> (
      let key = f element in
      Map.update map ~key ~f:(function
          | None -> Some [element]
          | Some elements -> Some (element :: elements)
      )
    ))

  let rec equal equalElement a b =
    match (a, b) with
    | ([], []) ->
        true
    | (x :: xs, y :: ys) ->
        equalElement x y && equal equalElement xs ys
    | _ ->
        false

  let rec compare compareElement a b =
    match (a, b) with
    | ([], []) ->
        0
    | ([], _) ->
        -1
    | (_, []) ->
        1
    | (x :: xs, y :: ys) -> (
      match compareElement x y with
      | 0 ->
          compare compareElement xs ys
      | result ->
          result )
end

let () = ()
