module Comparator = struct
  type ('a, 'identity) t = ('a, 'identity) Belt.Id.cmp

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

  module Make (M : T) : S with type t = M.t = struct
    module BeltComparator = Belt.Id.MakeComparable(struct
      type t = M.t
      let cmp = M.compare
    end)
    type t = M.t
    type identity = BeltComparator.identity
    let comparator = BeltComparator.cmp
  end  

  let make (type a) ~(compare:a -> a -> int) : (module S with type t = a) =
    (module Make (struct
      type t = a
      let compare = compare
    end))

  let toBeltComparator (type a) (type id) ((module Comparator) : (module S with type identity = id and type t = a)) : (a, id) Belt.Id.comparable = 
    ((module struct 
      type t = Comparator.t
      type identity = Comparator.identity
      let cmp = Obj.magic Comparator.comparator
    end)) 
end

module Bool = struct
  type t = bool

  let ofInt i = match i with 0 -> Some false | 1 -> Some true | _ -> None

  let ofString string =
    match string with "false" -> Some false | "true" -> Some true | _ -> None

  external ( && ) : bool -> bool -> bool = "%sequand"

  external ( || ) : bool -> bool -> bool = "%sequor"

  let xor a b = (a && not b) || ((not a) && b)

  let not = not

  let negate f t = not (f t)

  external toString : bool -> string = "toString" [@@bs.send]

  let toInt t = match t with true -> 1 | false -> 0

  let compare = compare

  let equal = ( = )
end

module Char = struct
  

  let toCode (c : char) = Char.code c

  let ofCode i =
    (if 0 <= i && i <= 255 then Some (Char.chr i) else None : char option)

  let toString c = String.make 1 c

  let ofString str =
    (match String.length str with 1 -> Some str.[0] | _ -> None : char option)

  let toDigit char =
    match char with '0' .. '9' -> Some (toCode char - toCode '0') | _ -> None

  let toLowercase char =
    match char with
    | 'A' .. 'Z' ->
        Char.chr (toCode 'a' + (toCode char - toCode 'A'))
    | _ ->
        char

  let toUppercase char =
    match char with
    | 'a' .. 'z' ->
        Char.chr (toCode 'A' + (toCode char - toCode 'a'))
    | _ ->
        char

  let isLowercase = function 'a' .. 'z' -> true | _ -> false

  let isUppercase = function 'A' .. 'Z' -> true | _ -> false

  let isLetter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let isDigit = function '0' .. '9' -> true | _ -> false

  let isAlphanumeric = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
        true
    | _ ->
        false

  let isPrintable = function ' ' .. '~' -> true | _ -> false

  let isWhitespace = function
    | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' ->
        true
    | _ ->
        false

  let equal = ( = )

  let compare = compare

  include Comparator.Make(struct
    type t = char
    let compare = compare
  end)
end

module Fun = struct
  external identity : 'a -> 'a = "%identity"

  external ignore : _ -> unit = "%ignore"

  let constant a _ = a

  let sequence _ b = b

  let flip f x y = f y x

  let apply f a = f a

  let ( <| ) a b = a b

  external pipe : 'a -> ('a -> 'b) -> 'b = "%revapply"

  external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

  let compose g f a = g (f a)

  let ( << ) = compose

  let composeRight g f a = f (g a)

  let ( >> ) = composeRight

  let tap a ~f = f a ; a

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


  let curry (f : 'a * 'b -> 'c) a b = (f (a, b) : 'c)

  let uncurry (f : 'a -> 'b -> 'c) ((a, b) : 'a * 'b) = (f a b : 'c)

  let curry3 f a b c = f (a, b, c)

  let uncurry3 f (a, b, c) = f a b c
end

module Container = struct
  module type Sum = sig
    type t

    val zero : t

    val add : t -> t -> t
  end
end

module Result = struct
  type ('ok, 'error) t = ('ok, 'error) Belt.Result.t

  let ok a = Belt.Result.Ok a

  let error e = Belt.Result.Error e

  let ofOption ma ~error =
    match ma with
    | None ->
        Belt.Result.Error error
    | Some right ->
        Belt.Result.Ok right

  let isError = Belt.Result.isError

  let isOk = Belt.Result.isOk

  let both a b =
    match (a, b) with
    | (Ok a', Ok b') ->
        Ok (a', b')
    | (Error a', _) ->
        Error a'
    | (_, Error b') ->
        Error b'

  let flatten a = match a with Ok a' -> a' | Error error -> Error error

  let or_ a b = match a with Ok _ -> a | _ -> b

  let and_ a b = match a with Ok _ -> b | _ -> a

  let get t ~default = Belt.Result.getWithDefault t default

  let ( |? ) t default = get t ~default

  let getUnsafe t = Belt.Result.getExn t

  let getError t ~default =
    match t with Ok _ -> default | Error value -> value

  let map2 a b ~f =
    match (a, b) with
    | (Ok a, Ok b) ->
        Ok (f a b)
    | (Error a, _) ->
        Error a
    | (_, Error b) ->
        Error b

  let values t =
    List.fold_right (map2 ~f:(fun a b -> a :: b)) t (Ok [])

  let map t ~f = Belt.Result.map t f

  let mapError t ~f =
    match t with Error error -> Error (f error) | Ok value -> Ok value

  let toOption r = match r with Ok v -> Some v | Error _ -> None

  let flatMap t ~f = Belt.Result.flatMap t f

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

module Option = struct
  type 'a t = 'a option

  let some a = Some a

  let isSome = Belt.Option.isSome

  let isNone = Belt.Option.isNone

  let or_ ta tb = match isSome ta with true -> ta | false -> tb

  let and_ ta tb = match isSome ta with true -> tb | false -> ta

  let flatMap t ~f = match t with None -> None | Some x -> f x

  let flatten = function Some option -> option | None -> None

  let both a b =
    match (a, b) with (Some a, Some b) -> Some (a, b) | _ -> None

  let map t ~f = Belt.Option.map t f

  let map2 a b ~f =
    match (a, b) with (Some a, Some b) -> Some (f a b) | _ -> None

  let get t ~default = Belt.Option.getWithDefault t default
  
  let ( |? ) t default = get t ~default
    
  let getOrFailWith t ~exn =
    match t with Some value -> value | None -> raise exn

  let getUnsafe =
    getOrFailWith ~exn:(Invalid_argument "Option.getUnsafe called with None")

  let toArray t = match t with None -> [||] | Some value -> [|value|]

  let toList t = match t with None -> [] | Some value -> [value]

  let forEach t ~f = match t with None -> () | Some x -> f x

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

  module Infix = struct
    let ( >>= ) t f = flatMap t ~f

    let ( >>| ) t f = map t ~f
  end
end

module Float = struct
  type t = float

  let ofInt = Js.Int.toFloat

  let ofString string = Some (Js.Float.fromString string)

  let add = ( +. )

  let ( + ) = ( +. )

  let subtract = ( -. )

  let ( - ) = ( -. )

  let multiply = ( *. )

  let ( * ) = ( *. )

  let divide n ~by = n /. by

  let ( / ) = ( /. )

  let power ~base ~exponent = Js.Math.pow_float ~base ~exp:exponent

  let ( ** ) base exponent = power ~base ~exponent

  let negate = ( ~-. )

  let ( ~- ) = ( ~-. )

  let absolute = Js.Math.abs_float

  let clamp n ~lower ~upper =
    if upper < lower then
      raise
        (Invalid_argument
           ( "~lower:" ^ Js.Float.toString lower
           ^ " must be less than or equal to ~upper:" ^ Js.Float.toString upper
           ))
    else if Js.Float.isNaN lower || Js.Float.isNaN upper || Js.Float.isNaN n
    then nan
    else max lower (min upper n)

  let inRange n ~lower ~upper =
    if upper < lower then
      raise
        (Invalid_argument
           ( "~lower:" ^ Js.Float.toString lower
           ^ " must be less than or equal to ~upper:" ^ Js.Float.toString upper
           ))
    else n >= lower && n < upper

  let squareRoot = sqrt

  let log n ~base = Js.Math.log n / Js.Math.log base

  let zero = 0.0

  let one = 1.0

  let nan = Js.Float._NaN

  let infinity = infinity

  let negativeInfinity = neg_infinity

  let e = Js.Math._E

  let pi = Js.Math._PI

  let epsilon = epsilon_float

  external largestValue : t = "MAX_VALUE" [@@bs.scope "Number"] [@@bs.val]

  external smallestValue : t = "MIN_VALUE" [@@bs.scope "Number"] [@@bs.val]

  external maximumSafeInteger : t = "MAX_SAFE_INTEGER"
    [@@bs.scope "Number"] [@@bs.val]

  external minimumSafeInteger : t = "MIN_SAFE_INTEGER"
    [@@bs.scope "Number"] [@@bs.val]

  let isNaN = Js.Float.isNaN

  let isFinite = Js.Float.isFinite

  let isInfinite n = (not (Js.Float.isFinite n)) && not (isNaN n)

  external isInteger : t -> bool = "isInteger" [@@bs.scope "Number"] [@@bs.val]

  external isSafeInteger : t -> bool = "isSafeInteger"
    [@@bs.scope "Number"] [@@bs.val]

  let maximum x y = if isNaN x || isNaN y then nan else if y > x then y else x

  let minimum x y = if isNaN x || isNaN y then nan else if y < x then y else x

  let hypotenuse = Js.Math.hypot

  type radians = float

  let degrees n = n * (pi / 180.0)

  let radians = Fun.identity

  let turns n = n * 2. * pi

  let cos = Js.Math.cos

  let acos = Js.Math.acos

  let sin = Js.Math.sin

  let asin = Js.Math.asin

  let tan = Js.Math.tan

  let atan = Js.Math.atan

  let atan2 ~y ~x = Js.Math.atan2 ~y ~x ()

  type direction =
    [ `Zero
    | `AwayFromZero
    | `Up
    | `Down
    | `Closest of [`Zero | `AwayFromZero | `Up | `Down | `ToEven] ]

  let round ?(direction = `Closest `Up) n =
    match direction with
    | `Up ->
        Js.Math.ceil_float n
    | `Down ->
        Js.Math.floor_float n
    | `Zero ->
        Js.Math.trunc n
    | `AwayFromZero ->
        if n > 0. then Js.Math.ceil_float n else Js.Math.floor_float n
    | `Closest `Zero ->
        if n > 0. then Js.Math.ceil_float (n -. 0.5)
        else Js.Math.floor_float (n +. 0.5)
    | `Closest `AwayFromZero ->
        if n > 0. then Js.Math.floor_float (n +. 0.5)
        else Js.Math.ceil_float (n -. 0.5)
    | `Closest `Down ->
        Js.Math.ceil_float (n -. 0.5)
    | `Closest `Up ->
        Js.Math.round n
    | `Closest `ToEven ->
        let roundNearestLowerBound = -.(2. ** 52.) in
        let roundNearestUpperBound = 2. ** 52. in
        if n <= roundNearestLowerBound || n >= roundNearestUpperBound then
          n +. 0.
        else (
          let floor = floor n in
          let ceil_or_succ = floor +. 1. in
          let diff_floor = n -. floor in
          let diff_ceil = ceil_or_succ -. n in
          if diff_floor < diff_ceil then floor
          else if diff_floor > diff_ceil then ceil_or_succ
          else if mod_float floor 2. = 0. then floor
          else ceil_or_succ )

  let floor = Js.Math.floor_float

  let ceiling = Js.Math.ceil_float

  let truncate = Js.Math.trunc

  let ofPolar (r, theta) = (r * cos theta, r * sin theta)

  let toPolar (x, y) = (hypotenuse x y, atan2 ~x ~y)

  let toInt f =
    if Js.Float.isFinite f then Some (Js.Math.unsafe_trunc f) else None

  let toString = Js.Float.toString

  let equal = ( = )

  let compare = compare
end

module Int = struct
  include Comparator.Make(struct
    type t = int
    let compare = compare
  end)

  let minimumValue = Js.Int.min

  let maximumValue = Js.Int.max

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

  let ( /. ) n by = Js.Int.toFloat n /. Js.Int.toFloat by

  let power ~base ~exponent = Js.Math.pow_int ~base ~exp:exponent

  let ( ** ) base exponent = Js.Math.pow_int ~base ~exp:exponent

  let negate = ( ~- )

  let ( ~- ) = ( ~- )

  let remainder n ~by = (n mod by)

  let ( mod ) n by =  
    (if n <= 0 then abs n * 2 else n) mod by

  let modulo n ~by = n mod by

  let maximum = Js.Math.max_int

  let minimum = Js.Math.min_int

  let absolute = abs

  let isEven n = n mod 2 = 0

  let isOdd n = n mod 2 <> 0

  let clamp n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else max lower (min upper n)

  let inRange n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else n >= lower && n < upper

  let toFloat = Js.Int.toFloat

  let toString = Js.Int.toString

  let ofString s =
    match int_of_string s with i -> Some i | exception Failure _ -> None

  let equal = ( = )

  let compare = compare
end

module Integer = struct
  include Comparator.Make(struct
    type t
    let compare = compare
  end)

  external ofInt : int -> t = "BigInt" [@@bs.val]

  external ofInt64 : Int64.t -> t = "BigInt" [@@bs.val]

  external ofFloatUnsafe : float -> t = "BigInt" [@@bs.val]

  let ofFloat float = Some (ofFloatUnsafe float)

  external ofStringUnsafe : string -> t Js.Nullable.t = "BigInt" [@@bs.val]

  let ofString string =
    match ofStringUnsafe string |> Js.Nullable.toOption with
    | value ->
        value
    | exception _ ->
        None

  let compare = compare

  let equal = ( = )

  let zero = [%raw "BigInt(0)"]

  let one = [%raw "BigInt(1)"]

  let isEven = ([%raw fun n -> "{ return n % 2 === 0 }"] : t -> bool)

  let isOdd = ([%raw fun n -> "{ return n % 2 !== 0 }"] : t -> bool)

  let ( < ) = ([%raw fun a b -> "{return a < b}"] : t -> t -> bool)  

  let ( >= ) = ([%raw fun a b -> "{return a > b}"] : t -> t -> bool)

  let ( > ) = ([%raw fun a b -> "{return a >= b}"] : t -> t -> bool)

  let add = [%raw fun a b -> "{return a + b}"]

  let ( + ) = add

  let subtract = [%raw fun a b -> "{return a - b}"]

  let ( - ) = subtract

  let multiply = ([%raw fun a b -> "{return a * b}"] : t -> t -> t)

  let ( * ) = multiply

  let divide = ([%raw fun a b -> "{return a / b}"] : t -> t -> t)

  let ( / ) = divide

  let divide n ~by = divide n by

  let negate = ([%raw fun a -> "{return a * BigInt(-1)}"] : t -> t)

  let modulo = ([%raw fun a b -> "{return a % b}"] : t -> t -> t)

  let modulo (n : t) ~(by : t) = (modulo n by : t)
  
  let (mod) (n : t) (by : t) = (modulo n ~by : t)

  let remainder (n : t) ~(by : t) = (modulo n ~by : t)

  let power = ([%raw fun a b -> "{return a ** b}"] : t -> t -> t)

  let ( ** ) (base : t) (exponent : int) : t = power base (ofInt exponent)

  let power ?modulo:(modulus : t option) ~(base : t) ~(exponent : int) =
    match modulus with
    | None ->
      base ** exponent 
    | Some modulus -> (
      let rec loop (b : t) (e : int) (result : t) : t =
        if e <= 0 then 
          result
        else
          loop
            (modulo (b * b) ~by:modulus)
            (Int.subtract e 1)
            (if Int.isEven e then result else modulo (result * b) ~by:modulus)
      in
      (loop (base : t) (exponent: int) (one: t) ) : t
    )

  let maximum a b = if a < b then b else a

  let minimum a b = if a > b then b else a

  let absolute n = if n < zero then negate n else n

  let clamp n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else maximum lower (minimum upper n)

  let inRange n ~lower ~upper =
    if upper < lower then
      raise (Invalid_argument "~lower must be less than or equal to ~upper")
    else n >= lower && n < upper

  external asIntN : int -> t -> 'a = "asIntN" [@@bs.val] [@@bs.scope "BigInt"]

  let toInt t =
    if t > ofInt Int.maximumValue || t > ofInt Int.minimumValue then None
    else Some (asIntN 32 t)

  let toInt64 t =
    if t > ofInt64 Int64.max_int || t < ofInt64 Int64.min_int then None
    else Some (asIntN 64 t)

  external toFloat : t -> float = "Number" [@@bs.val]

  external toString : t -> string = "toString" [@@bs.send]
end

module Tuple = struct
  type ('a, 'b) t = 'a * 'b

  let make a b = (a, b)

  let ofArray array =
    match array with
    | [||] | [|_|] ->
        None
    | [|a; b|] ->
        Some (a, b)
    | _ ->
        None

  let ofList list =
    match list with [] | [_] -> None | a :: b :: _rest -> Some (a, b)

  let first (a, _) = a

  let second (_, b) = b

  let mapFirst (a, b) ~f = (f a, b)

  let mapSecond (a, b) ~f = (a, f b)

  let mapEach (a, b) ~f ~g = (f a, g b)

  let mapAll (a1, a2) ~f = (f a1, f a2)

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

  let ofArray array =
    match array with
    | [||] | [|_|] | [|_; _|] ->
        None
    | [|a; b; c|] ->
        Some (a, b, c)
    | _ ->
        None

  let ofList list =
    match list with
    | [] | [_] | [_; _] ->
        None
    | a :: b :: c :: _rest ->
        Some (a, b, c)

  let first (a, _, _) = a

  let second (_, b, _) = b

  let third (_, _, c) = c

  let initial (a, b, _) = (a, b)

  let tail (_, b, c) = (b, c)

  let mapFirst (a, b, c) ~f = (f a, b, c)

  let mapSecond (a, b, c) ~f = (a, f b, c)

  let mapThird (a, b, c) ~f = (a, b, f c)

  let mapEach (a, b, c) ~f ~g ~h = (f a, g b, h c)

  let mapAll (a1, a2, a3) ~f = (f a1, f a2, f a3)

  let rotateLeft (a, b, c) = (b, c, a)

  let rotateRight (a, b, c) = (c, a, b)

  let toArray (a, b, c) = [|a; b; c|]

  let toList (a, b, c) = [a; b; c]

  let equal equalFirst equalSecond equalThird (a, b, c) (a', b', c') =
    equalFirst a a' && equalSecond b b' && equalThird c c'

  let compare compareFirst compareSecond compareThird (a, b, c) (a', b', c') =
    match compareFirst a a' with
    | 0 -> (
      match compareSecond b b' with 0 -> compareThird c c' | result -> result )
    | result ->
        result
end

module String = struct
  include Comparator.Make(struct
    type t = string
    let compare = compare
  end)

  let initialize length ~f =
    Js.Array.joinWith ""
      (Array.init length (fun index -> f index |> Char.toString))

  let get (string : string) (index : int) = string.[index]

  let getAt (string : string) ~(index : int) =
    if index < 0 || index >= String.length string then None
    else Some string.[index]
  
  let (.?[]) (string: string) (index: int) : char option = 
    getAt string ~index

  let ofArray characters =
    Js.Array.joinWith ""
      (Array.map
         (fun character -> Char.toCode character |. Js.String.fromCharCode)
         characters)

  let ofList t =
    Js.Array.joinWith ""
      (Array.map
         (fun character -> Char.toCode character |. Js.String.fromCharCode)
         (Array.of_list t))

  let ofChar c = Char.toCode c |. Js.String.fromCharCode

  let isEmpty t = t = ""

  let length = String.length

  let uncons s =
    match s with
    | "" ->
        None
    | s ->
        Some (s.[0], String.sub s 1 (String.length s - 1))

  let dropLeft s ~count = Js.String.substr ~from:count s

  let dropRight s ~count =
    if count < 1 then s else Js.String.slice ~from:0 ~to_:(-count) s

  let split t ~on = Js.String.split on t |> Array.to_list

  let endsWith t ~suffix = Js.String.endsWith suffix t

  let startsWith t ~prefix = Js.String.startsWith prefix t

  let trim = Js.String.trim

  external trimLeft : string -> string = "trimStart" [@@bs.send]

  external trimRight : string -> string = "trimEnd" [@@bs.send]

  external padLeft : string -> int -> string -> string = "padStart" [@@bs.send]

  let padLeft string count ~with_ = padLeft string count with_

  external padRight : string -> int -> string -> string = "padEnd" [@@bs.send]

  let padRight string count ~with_ = padRight string count with_

  let toLowercase s = String.lowercase_ascii s

  let toUppercase s = String.uppercase_ascii s

  let uncapitalize = String.uncapitalize_ascii

  let capitalize = String.capitalize_ascii

  let isCapitalized s = s = String.capitalize_ascii s

  let includes t ~substring = Js.String.includes substring t

  let repeat s ~count = Js.String.repeat count s

  let reverse s =
    Js.Array.joinWith "" (Js.Array.reverseInPlace (Js.String.split "" s))

  let toArray (t : string) : char array =
    Js.String.castToArrayLike t
    |. Js.Array.from
    |> (Js.Array.map (fun characterString -> Char.ofString characterString |. Option.getUnsafe))
    

  let toList (s : string) : char list =
    toArray s |> Belt.List.fromArray

  let slice ?to_ (t : string) ~from =
    (Js.String.slice ~from ~to_:(Option.get to_ ~default:(length t)) t : string)

  let insertAt t ~index ~value =
    Js.String.slice ~from:0 ~to_:index t
    ^ value
    ^ Js.String.sliceToEnd ~from:index t

  let forEach t ~f = Array.iter f (toArray t)

  let fold t ~initial ~f = Belt.Array.reduce (toArray t) initial f

  let equal = ( = )

  let compare = compare
end

module Set = struct
  type ('a, 'cmp) t = ('a, 'cmp) Belt.Set.t

  module Of (M : Comparator.S) = struct
    type nonrec 'value t = (M.t, M.identity) t
  end

  let empty comparator = 
    (Belt.Set.make ~id:(Comparator.toBeltComparator comparator))    

  let singleton (comparator: ('a, 'identity) Comparator.s) (element: 'a) : ('a, 'identity) t = 
    (Belt.Set.fromArray ~id:(Comparator.toBeltComparator comparator) [|element|])

  let ofArray (comparator: ('a, 'identity) Comparator.s) (elements: 'a array) : ('a, 'identity) t  = 
    (Belt.Set.fromArray ~id:(Comparator.toBeltComparator comparator) elements)

  let ofList (comparator: ('a, 'identity) Comparator.s) (elements: 'a list) : ('a, 'identity) t  = 
    (Belt.Set.fromArray ~id:(Comparator.toBeltComparator comparator) (Array.of_list elements))

  let length = Belt.Set.size

  let isEmpty = Belt.Set.isEmpty

  let includes = Belt.Set.has

  let (.?{}) (set: ('element, _) t) (element: 'element) : bool = 
    includes set element

  let add = Belt.Set.add

  let remove = Belt.Set.remove

  let difference = Belt.Set.diff

  let intersection = Belt.Set.intersect

  let union = Belt.Set.union

  let filter s ~f = Belt.Set.keep s f

  let partition s ~f = Belt.Set.partition s f

  let find s ~f = (Belt.Set.toArray s |. Belt.Array.getBy) f

  let all s ~f = Belt.Set.every s f

  let any s ~f = Belt.Set.some s f

  let forEach s ~f = Belt.Set.forEach s f

  let fold s ~initial ~f = Belt.Set.reduce s initial f

  let toArray = Belt.Set.toArray

  let toList = Belt.Set.toList

  module Poly = struct
    type identity

    type nonrec 'a t = ('a, identity) t

    let ofArray (type a) (a : a array) =
      ( Belt.Set.fromArray a
          ~id:
            ( module struct
              type t = a

              type nonrec identity = identity

              let cmp = Pervasives.compare |. Obj.magic
            end )
        : a t )

    let ofList l = Array.of_list l |. ofArray

    let empty () = ofArray [||]

    let singleton a = ofArray [|a|]
  end

  module Int = struct
    type nonrec t = (Int.t, Int.identity) t

    let ofArray a = Poly.ofArray a |. Obj.magic

    let empty = ofArray [||]

    let singleton a = ofArray [|a|]

    let ofList l = Array.of_list l |. ofArray
  end

  module String = struct
    type nonrec t = (String.t, String.identity) t

    let ofArray a = Poly.ofArray a |. Obj.magic

    let empty = ofArray [||]

    let singleton a = ofArray [|a|]

    let ofList l = Array.of_list l |. ofArray
  end
end

module Map = struct
  type ('key, 'value, 'cmp) t = ('key, 'value, 'cmp) Belt.Map.t

  module Of (M : Comparator.S) = struct
    type nonrec 'value t = (M.t, 'value, M.identity) t
  end

  let ofArray (comparator: ('key, 'id) Comparator.s) (values : ('key * 'v) array) : ('key, 'value, 'id) t  =
    (Belt.Map.fromArray values
        ~id:(Comparator.toBeltComparator comparator)
      )

  let empty comparator = ofArray comparator [||]

  let ofList comparator l = ofArray comparator (Array.of_list l)

  let singleton comparator ~key ~value = ofArray comparator [|(key, value)|]

  let isEmpty = Belt.Map.isEmpty

  let includes = Belt.Map.has

  let length = Belt.Map.size

  let add m ~key ~value = Belt.Map.set m key value

  let (.?{}<-) (map: ('key, 'value, 'id) t) (key: 'key) (value: 'value): ('key, 'value, 'id) t = 
    add map ~key ~value

  let remove = Belt.Map.remove

  let get = Belt.Map.get

  let (.?{}) (map: ('key, 'value, _) t) (key: 'key) : 'value option = 
    get map key  

  let update m ~key ~f = Belt.Map.update m key f

  let merge m1 m2 ~f = Belt.Map.merge m1 m2 f

  let map m ~f = Belt.Map.map m (fun value -> f value)

  let mapI t ~f = Belt.Map.mapWithKey t f

  let filter m ~f = Belt.Map.keep m (fun _ value -> f value)

  let partition m ~f = Belt.Map.partition m (fun key value -> f ~key ~value)

  let find m ~f = Belt.Map.findFirstBy m (fun key value -> f ~key ~value)

  let any m ~f = Belt.Map.some m (fun _ value -> f value)

  let all m ~f = Belt.Map.every m (fun _ value -> f value)

  let forEach m ~f = Belt.Map.forEach m (fun _ value -> f value)

  let forEachI m ~f = Belt.Map.forEach m (fun key value -> f ~key ~value)

  let fold m ~initial ~f =
    Belt.Map.reduce m initial (fun acc key data -> f acc ~key ~value:data)

  let keys m = Belt.Map.keysToArray m |. Array.to_list

  let values m = Belt.Map.valuesToArray m |. Array.to_list

  let maximum = Belt.Map.maxKey

  let minimum = Belt.Map.minKey

  let extent t = Option.both (minimum t) (maximum t)

  let toArray = Belt.Map.toArray

  let toList = Belt.Map.toList

  module Poly = struct
    type identity

    type nonrec ('k, 'v) t = ('k, 'v, identity) t

    let ofArray (type k v) (a : (k * v) array) =
      ( Belt.Map.fromArray a
          ~id:
            ( module struct
              type t = k

              type nonrec identity = identity

              let cmp = Pervasives.compare |. Obj.magic
            end )
        : (k, v) t )

    let empty () = ofArray [||]

    let ofList l = ofArray (Array.of_list l)

    let singleton ~key ~value = ofArray [|(key, value)|]
  end

  module Int = struct
    type nonrec 'v t = (Int.t, 'v, Int.identity) t

    let ofArray a = Poly.ofArray a |. Obj.magic

    let empty = ofArray [||]

    let singleton ~key ~value = ofArray [|(key, value)|]

    let ofList l = ofArray (Array.of_list l)
  end

  module String = struct
    type nonrec 'v t = (String.t, 'v, String.identity) t

    let ofArray a = Poly.ofArray a |. Obj.magic

    let empty = ofArray [||]

    let singleton ~key ~value = ofArray [|(key, value)|]

    let ofList l = ofArray (Array.of_list l)
  end
end

module Array = struct
  type 'a t = 'a array

  let singleton a = [|a|]

  let clone t = Array.map Fun.identity t

  let length = Belt.Array.length

  let isEmpty a = length a = 0

  let initialize length ~f = Belt.Array.makeBy length f

  let range ?(from = 0) to_ =
    Belt.Array.makeBy (to_ - from) (fun i -> i + from)

  let ofList = Belt.List.toArray

  let toList = Belt.List.fromArray

  let toIndexedList array =
    Belt.Array.reduceReverse array
      (length array - 1, [])
      (fun (i, acc) x -> (i - 1, (i, x) :: acc))
    |. snd

  let get = Belt.Array.getExn

  let getAt t ~index = Belt.Array.get t index

  let (.?()) (array: 'element t) (index: int) : 'element option = 
    getAt array ~index

  let first t = getAt t ~index:0

  let last t = getAt t ~index:(Array.length t - 1)

  let set t index value = t.(index) <- value

  let setAt t ~index ~value = t.(index) <- value

  let sum (type a) t (module M : Container.Sum with type t = a) =
    (Array.fold_left M.add M.zero t : a)

  let filter t ~f = Belt.Array.keep t f

  let swap t i j =
    let temp = t.(i) in
    t.(i) <- t.(j) ;
    t.(j) <- temp ;
    ()

  let fold t ~initial ~f = Belt.Array.reduce t initial f

  let foldRight t ~initial ~f = Belt.Array.reduceReverse t initial f

  let maximum t ~compare =
    fold t ~initial:None ~f:(fun max element ->
        match max with
        | None ->
            Some element
        | Some current -> (
          match compare element current > 0 with
          | true ->
              Some element
          | false ->
              max ))

  let minimum t ~compare =
    fold t ~initial:None ~f:(fun min element ->
        match min with
        | None ->
            Some element
        | Some current -> (
          match compare element current < 0 with
          | true ->
              Some element
          | false ->
              min ))

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

  let map t ~f = Belt.Array.map t f

  let mapI t ~f = Belt.Array.mapWithIndex t f

  let map2 a b ~(f : 'a -> 'b -> 'c) = (Belt.Array.zipBy a b f : 'c array)  

  let map3 as_ bs (cs : 'c t) ~f =
    let minLength =
      Belt.Array.reduce [|length bs; length cs|] (length as_) min
    in
    Belt.Array.makeBy minLength (fun i -> f as_.(i) bs.(i) cs.(i))

  let zip = map2 ~f:(fun a b -> (a, b))

  let flatMap t ~f = Belt.Array.map t f |. Belt.Array.concatMany

  let sliding ?(step = 1) a ~size =
    let n = Array.length a in
    if size > n then [||]
    else
      initialize
        (1 + ((n - size) / step))
        ~f:(fun i -> initialize size ~f:(fun j -> a.((i * step) + j)))

  let find t ~f =
    let rec find_loop t ~f ~length i =
      if i >= length then None
      else if f t.(i) then Some t.(i)
      else find_loop t ~f ~length (i + 1)
    in
    find_loop t ~f ~length:(length t) 0

  let findIndex array ~f =
    let rec loop index =
      if index >= length array then None
      else if f index array.(index) then Some (index, array.(index))
      else loop (index + 1)
    in
    loop 0

  let any t ~f = Belt.Array.some t f

  let all t ~f = Belt.Array.every t f

  let includes t v ~equal = any t ~f:(equal v)

  let append a a' = Belt.Array.concat a a'

  let flatten (ars : 'a array array) = Belt.Array.concatMany ars

  let intersperse t ~sep =
    Belt.Array.makeBy
      (max 0 ((length t * 2) - 1))
      (fun i -> if i mod 2 <> 0 then sep else t.(i / 2))

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
      Belt.Array.makeBy (sliceTo - sliceFrom) (fun i -> array.(i + sliceFrom))

  let count t ~f =
    fold t ~initial:0 ~f:(fun total element ->
        total + match f element with true -> 1 | false -> 0)

  let chunksOf t ~size = sliding t ~step:size ~size

  let reverse = Belt.Array.reverseInPlace

  let forEach t ~f = (Belt.Array.forEach t f : unit)

  let forEachI t ~f =
    ( for i = 0 to length t - 1 do
        f i t.(i)
      done
      : unit )

  let partition t ~f =
    ( fold t ~initial:([], []) ~f:(fun result element ->
          let update = if f element then Tuple.mapFirst else Tuple.mapSecond in
          update result ~f:(fun result -> element :: result))
    |. Tuple.mapAll ) ~f:(fun list -> List.rev list |. ofList)

  let splitAt t ~index =
    (slice t ~from:0 ~to_:index, slice t ~from:index ~to_:(length t))

  let splitWhen t ~f =
    match findIndex t ~f:(fun _ e -> f e) with
    | None ->
        (t, [||])
    | Some (index, _) ->
        splitAt t ~index

  let unzip t =
    ( Array.init (length t) (fun i -> Tuple.first t.(i))
    , Array.init (length t) (fun i -> Tuple.second t.(i)) )

  let repeat element ~length = Array.init (max length 0) (fun _ -> element)

  let filterMap t ~f =
    fold t ~initial:[] ~f:(fun results element ->
        match f element with None -> results | Some value -> value :: results)
    |. ofList

  let sort a ~compare = Array.sort compare a

  let values t =
    fold t ~initial:[] ~f:(fun results element ->
        match element with None -> results | Some value -> value :: results)
    |. ofList

  let join t ~sep = Js.Array.joinWith sep t

  let groupBy t comparator ~f =
    fold t ~initial:(Map.empty comparator) ~f:(fun map element -> (
      let key = f element in
      Map.update map ~key ~f:(function
          | None -> Some [element]
          | Some elements -> Some (element :: elements)
      )
    ))

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

  let singleton x = [x]

  let ofArray array = List.init (Array.length array) (fun i -> array.(i))

  let range ?(from = 0) to_ = List.init (to_ - from) (fun i -> i + from)

  let rec repeat element ~times =
    if times <= 0 then [] else element :: repeat element ~times:(times - 1)

  let flatten = Belt.List.flatten

  let reverse = Belt.List.reverse

  let append = Belt.List.concat

  let sum (type a) t (module M : Container.Sum with type t = a) =
    List.fold_left M.add M.zero t

  let map t ~f = Belt.List.map t f

  let flatMap t ~f = flatten (map t ~f)

  let mapI t ~f = Belt.List.mapWithIndex t f

  let map2 a b ~f = Belt.List.zipBy a b f

  let zip = map2 ~f:(fun a b -> (a, b))

  let rec map3 a b c ~f =
    match (a, b, c) with
    | (x :: xs, y :: ys, z :: zs) ->
        f x y z :: map3 xs ys zs ~f
    | _ ->
        []

  let rec last l =
    match l with [] -> None | [x] -> Some x | _ :: rest -> last rest

  let unzip list =
    (List.map (fun (a, _) -> a) list, List.map (fun (_, b) -> b) list)

  let includes t value ~equal = Belt.List.has t value equal

  let find t ~f = Belt.List.getBy t f

  let getAt t ~index = Belt.List.get t index

  let any t ~f = List.exists f t

  let head l = Belt.List.head l

  let drop t ~count = Belt.List.drop t count |. Belt.Option.getWithDefault []

  let take t ~count = Belt.List.take t count |. Belt.Option.getWithDefault []

  let initial l =
    match reverse l with [] -> None | _ :: rest -> Some (reverse rest)

  let filterMap t ~f = Belt.List.keepMap t f

  let filter t ~f = Belt.List.keep t f

  let filterI t ~f = Belt.List.keepWithIndex t (fun e i -> f i e)

  let partition t ~f = Belt.List.partition t f

  let fold t ~initial ~f = Belt.List.reduce t initial f

  let count t ~f =
    fold t ~initial:0 ~f:(fun total element ->
        total + match f element with true -> 1 | false -> 0)

  let foldRight t ~initial ~f = Belt.List.reduceReverse t initial f

  let findIndex list ~f =
    let rec loop i l =
      match l with
      | [] ->
          None
      | x :: rest ->
          if f i x then Some (i, x) else loop (i + 1) rest
    in
    loop 0 list

  let splitAt t ~index =
    if index < 0 then
      raise (Invalid_argument "List.splitAt called with negative index") ;
    let rec loop front back i =
      match back with
      | [] ->
          (t, [])
      | element :: rest ->
          if i = 0 then (reverse front, back)
          else loop (element :: front) rest (i - 1)
    in
    loop [] t index

  let updateAt =
    ( fun t ~index ~f ->
        Belt.List.mapWithIndex t (fun i element ->
            if i = index then f element else element)
      : 'a t -> index:int -> f:('a -> 'a) -> 'a t )

  let length l = Belt.List.length l

  let rec dropWhile t ~f =
    match t with [] -> [] | x :: rest -> if f x then dropWhile rest ~f else t

  let isEmpty t = t = []

  let sliding ?(step = 1) t ~size =
    let rec loop t =
      if isEmpty t then []
      else (
        let sample = Belt.List.take t size in
        let rest = Belt.List.drop t step in
        match (sample, rest) with
        | (None, _) ->
            []
        | (Some x, None) ->
            [x]
        | (Some x, Some xs) ->
            x :: loop xs )
    in
    loop t

  let chunksOf t
   ~size = sliding t ~step:size ~size

  let cons t element = element :: t

  let uncons = function
   | [] -> None
   | head :: tail -> Some (head, tail)

  let takeWhile t ~f =
    let rec takeWhileHelper acc t =
      match t with
      | [] ->
          reverse acc
      | x :: rest ->
          if f x then takeWhileHelper (x :: acc) rest else reverse acc
    in
    takeWhileHelper [] t

  let all t ~f = Belt.List.every t f

  let tail t = match t with [] -> None | _ :: rest -> Some rest

  let removeAt t ~index =
    if index < 0 then t
    else (
      let (front, back) : 'a t * 'a t = splitAt t ~index in
      match tail back with None -> t | Some t -> append front t )

  let minimum t ~compare =
    fold t ~initial:None ~f:(fun min element ->
        match min with
        | None ->
            Some element
        | Some value -> (
          match compare element value < 0 with
          | true ->
              Some element
          | false ->
              min ))

  let maximum t ~compare =
    fold t ~initial:None ~f:(fun max element ->
        match max with
        | None ->
            Some element
        | Some value -> (
          match compare element value > 0 with
          | true ->
              Some element
          | false ->
              max ))

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

  let sort t ~compare = Belt.List.sort t compare

  let span t ~f =
    match t with [] -> ([], []) | _ -> (takeWhile t ~f, dropWhile t ~f)

  let rec groupWhile t ~f =
    match t with
    | [] ->
        []
    | x :: rest ->
        let (ys, zs) = span rest ~f:(f x) in
        (x :: ys) :: groupWhile zs ~f

  let insertAt t ~index ~value =
    if index < 0 then
      raise (Invalid_argument "List.splitAt called with negative index") ;
    let rec loop front back i =
      match back with
      | [] ->
          reverse (value :: front)
      | element :: rest ->
          if i = 0 then append (reverse front) (value :: element :: rest)
          else loop (element :: front) rest (index - 1)
    in
    loop [] t index

  let splitWhen t ~f =
    let rec loop front back =
      match back with
      | [] ->
          (t, [])
      | element :: rest ->
          if f element then (reverse front, back)
          else loop (element :: front) rest
    in
    loop [] t

  let intersperse t ~sep =
    match t with
    | [] ->
        []
    | [x] ->
        [x]
    | x :: rest ->
        x :: foldRight rest ~initial:[] ~f:(fun acc x -> sep :: x :: acc)

  let initialize length ~f = Belt.List.makeBy length f

  let forEach t ~f = (Belt.List.forEach t f : unit)

  let forEachI t ~f = (Belt.List.forEachWithIndex t f : unit)

  let toArray = Array.ofList

  let join strings ~sep = Js.Array.joinWith sep (toArray strings)

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
