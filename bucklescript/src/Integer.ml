
module Comp : Comparator.T = struct
  type t
  let compare = compare
end 

type t = Comp.t


external ofInt : int -> t = "BigInt" [@@bs.module "jsbi"] [@@bs.val]

external ofInt64 : Int64.t -> t = "BigInt" [@@bs.module "jsbi"] [@@bs.val]

external ofFloatUnsafe : float -> t = "BigInt" [@@bs.module "jsbi"] [@@bs.val]

let ofFloat float = Some (ofFloatUnsafe float)

external ofStringUnsafe : string -> t Js.Nullable.t = "BigInt" [@@bs.module "jsbi"] [@@bs.val]

let ofString string =
  match ofStringUnsafe string |> Js.Nullable.toOption with
  | value ->
      value
  | exception _ ->
      None

external equal: t -> t -> bool = "equal" [@@bs.module "jsbi"]

external (==): t -> t -> bool = "equal" [@@bs.module "jsbi"]

external (<>): t -> t -> bool = "notEqual" [@@bs.module "jsbi"]

let zero = ofInt(0)

let one = ofInt(1)

external ( < ) : t -> t -> bool = "lessThan" [@@bs.module "jsbi"]

external ( >= ) : t -> t -> bool = "greaterThanOrEqual" [@@bs.module "jsbi"]

external ( > ) : t -> t -> bool = "greaterThan" [@@bs.module "jsbi"]

let compare a b =
  if a < b then 
    -1
  else if a > b then 
    1
  else 
    0

external add : t -> t -> t = "add" [@@bs.module "jsbi"]

external ( + ) : t -> t -> t= "add" [@@bs.module "jsbi"]

external subtract : t -> t -> t = "subtract" [@@bs.module "jsbi"]

external ( - ) : t -> t -> t = "subtract" [@@bs.module "jsbi"]

external multiply : t -> t -> t = "multiply" [@@bs.module "jsbi"]

external ( * ) : t -> t -> t = "multiply" [@@bs.module "jsbi"]

external divide : t -> t -> t = "divide" [@@bs.module "jsbi"]

external ( / ) : t -> t -> t = "divide" [@@bs.module "jsbi"]

let divide n ~by = divide n by

let negate : t -> t = multiply(ofInt(-1))

external modulo : t -> t -> t = "remainder" [@@bs.module "jsbi"]

let isEven (n : t) : bool = modulo n (ofInt 2) == ofInt(0)

let isOdd (n : t) : bool = modulo n (ofInt 2) <> ofInt(0)

let (mod) : t -> t -> t = modulo

let modulo (n : t) ~(by : t) : t = modulo n by

let remainder (n : t) ~(by : t) : t = modulo n ~by



external power : t -> t -> t = "exponentiate"  [@@bs.module "jsbi"]

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

include (Comparator.Make(Comp) : sig 
  type identity
  val comparator : (t, identity) Comparator.comparator
end
  )