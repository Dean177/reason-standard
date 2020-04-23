type t = int

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