module Fun = {
  external identity: 'a => 'a = "%identity";

  external ignore: _ => unit = "%ignore";

  let constant = (a, _) => a;

  let sequence = (_, b) => b;

  let flip = (f, x, y) => f(y, x);

  let apply = (f, a) => f(a);

  let (<|) = (a, b) => a(b);

  external pipe: ('a, 'a => 'b) => 'b = "%revapply";

  external (|>): ('a, 'a => 'b) => 'b = "%revapply";

  let compose = (g, f, a) => g(f(a));

  let (<<) = compose;

  let composeRight = (g, f, a) => f(g(a));

  let (>>) = composeRight;

  let tap = (a, ~f) => {
    f(a);
    a;
  };

  let rec times = (n, ~f) =>
    if (n <= 0) {
      ();
    } else {
      f();
      times(n - 1, ~f);
    };

  let negate = (f, a) => !f(a);

  let forever = f => {
    while (true) {
      f();
    };
  };
};

module Container = {
  module type Sum = {
    type t;
    let zero: t;
    let add: (t, t) => t;
  };
};

module type Comparable = {
  type t;
  let compare: (t, t) => int;
};

module type Comparator = {
  type t;
  type identity;
  let compare: (t, t) => int;
};

type comparator('k, 'id) = (module Comparator with
                               type identity = 'id and type t = 'k);

module Bool = {
  type t = bool;

  external (&&): (bool, bool) => bool = "%sequand";

  external (||): (bool, bool) => bool = "%sequor";

  let xor = (a, b) => a && !b || !a && b;

  let (!) = (!);

  let negate = (f, t) => !f(t);

  let equal = (==);

  let compare = compare;

  let fromInt = i =>
    switch (i) {
    | 0 => Some(false)
    | 1 => Some(true)
    | _ => None
    };

  let fromString = string =>
    switch (string) {
    | "false" => Some(false)
    | "true" => Some(true)
    | _ => None
    };

  [@bs.send] external toString: bool => string = "toString";

  let toInt = t => t ? 1 : 0;
};

module List = {
  type t('a) = list('a);

  let empty = [];

  let singleton = x => [x];

  let fromArray = array => {
    List.init(Array.length(array), i => array[i]);
  };

  let range = (~from=0, to_) => List.init(to_ - from, i => i + from);

  let rec repeat = (element, ~times) =>
    if (times <= 0) {
      [];
    } else {
      [element, ...repeat(element, ~times=times - 1)];
    };

  let concatenate = Belt.List.flatten;

  let reverse = Belt.List.reverse;

  let append = Belt.List.concat;

  let sum = (type a, t, module M: Container.Sum with type t = a) =>
    List.fold_left(M.add, M.zero, t);

  let map = (t, ~f) => Belt.List.map(t, f);

  let bind = (t, ~f) => concatenate(map(t, ~f));

  let mapI = (t, ~f) => Belt.List.mapWithIndex(t, f);

  let map2 = (a, b, ~f) => Belt.List.zipBy(a, b, f);

  let rec map3 = (a, b, c, ~f) =>
    switch (a, b, c) {
    | ([x, ...xs], [y, ...ys], [z, ...zs]) => [
        f(x, y, z),
        ...map3(xs, ys, zs, ~f),
      ]
    | _ => []
    };

  let rec last = l =>
    switch (l) {
    | [] => None
    | [x] => Some(x)
    | [_, ...rest] => last(rest)
    };

  let unzip = list => (
    List.map(((a, _)) => a, list),
    List.map(((_, b)) => b, list),
  );

  let unzip3 = list => (
    List.map(((a, _, _)) => a, list),
    List.map(((_, b, _)) => b, list),
    List.map(((_, _, c)) => c, list),
  );

  let includes = (t, value, ~equal) => Belt.List.has(t, value, equal);

  let find = (t, ~f) => Belt.List.getBy(t, f);

  let getAt = (t, ~index) => Belt.List.get(t, index);

  let any = (t, ~f) => List.exists(f, t);

  let head = l => Belt.List.head(l);

  let drop = (t, ~count) =>
    Belt.List.drop(t, count)->(Belt.Option.getWithDefault([]));

  let take = (t, ~count) =>
    Belt.List.take(t, count)->Belt.Option.getWithDefault([]);

  let initial = l =>
    switch (reverse(l)) {
    | [] => None
    | [_, ...rest] => Some(reverse(rest))
    };

  let filterMap = (t, ~f) => Belt.List.keepMap(t, f);

  let filter = (t, ~f) => Belt.List.keep(t, f);

  let filterI = (t, ~f) => Belt.List.keepWithIndex(t, (e, i) => f(i, e));

  let partition = (t, ~f) => Belt.List.partition(t, f);

  let fold = (t, ~initial, ~f) => Belt.List.reduce(t, initial, f);

  let count = (t, ~f) =>
    fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0));

  let foldRight = (t, ~initial, ~f) =>
    Belt.List.reduceReverse(t, initial, f);

  let findIndex = (list, ~f) => {
    let rec loop = (i, l) => 
      switch (l) {
      | [] => None
      | [x, ...rest] => {
        if (f(i, x)) { Some((i, x)) } else {
          loop(i + 1, rest)
        }
      };
    };

    loop(0, list);
  };

  let splitAt = (t, ~index) => (
    take(t, ~count=index),
    drop(t, ~count=index),
  );

  let updateAt: (t('a), ~index: int, ~f: 'a => 'a) => t('a) =
    (t, ~index, ~f) =>
      Belt.List.mapWithIndex(t, (i, element) =>
        if (i == index) {
          f(element);
        } else {
          element;
        }
      );

  let length = l => Belt.List.length(l);

  let rec dropWhile = (~f, l) =>
    switch (l) {
    | [] => []
    | [x, ...rest] =>
      if (f(x)) {
        dropWhile(~f, rest);
      } else {
        l;
      }
    };

  let isEmpty = t => t == [];

  let sliding = (~step=1, t, ~size) => {
    let rec loop = t =>
      if (isEmpty(t)) {
        [];
      } else {
        let sample = Belt.List.take(t, size);
        let rest = Belt.List.drop(t, step);
        switch (sample, rest) {
        | (None, _) => []
        | (Some(x), None) => [x]
        | (Some(x), Some(xs)) => [x, ...loop(xs)]
        };
      };

    loop(t);
  };

  let cons = (t, element) => [element, ...t];

  let takeWhile = (t, ~f) => {
    let rec takeWhileHelper = (acc, t) =>
      switch (t) {
      | [] => reverse(acc)
      | [x, ...rest] =>
        if (f(x)) {
          takeWhileHelper([x, ...acc], rest);
        } else {
          reverse(acc);
        }
      };

    takeWhileHelper([], t);
  };

  let all = (t, ~f) => Belt.List.every(t, f);

  let tail = t =>
    switch (t) {
    | [] => None
    | [_, ...rest] => Some(rest)
    };

  let removeAt = (t, ~index) =>
    if (index < 0) {
      t;
    } else {
      let (front, back): (t('a), t('a)) = splitAt(t, ~index);
      switch (tail(back)) {
      | None => t
      | Some(t) => append(front, t)
      };
    };

  let minimum = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(min, element) => {
      switch (min) {
      | None => Some(element)
      | Some(value) => compare(element, value) < 0 ? Some(element) : min
      }
    });

  let maximum = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(max, element) => {
      switch (max) {
      | None => Some(element)
      | Some(value) => compare(element, value) > 0 ? Some(element) : max
      }
    });

  let extent = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(current, element) => {
      switch (current) {
      | None => Some((element, element))
      | Some((min, max)) =>
        Some((
          compare(element, min) < 0 ? element : min,
          compare(element, max) > 0 ? element : max,
        ))
      }
    });

  let sort = (t, ~compare) => Belt.List.sort(t, compare);

  let span = (t, ~f) =>
    switch (t) {
    | [] => ([], [])
    | _ => (takeWhile(t, ~f), dropWhile(t, ~f))
    };

  let rec groupWhile = (t, ~f) =>
    switch (t) {
    | [] => []
    | [x, ...rest] =>
      let (ys, zs) = span(rest, ~f=f(x));
      [[x, ...ys], ...groupWhile(zs, ~f)];
    };

  /* TODO: what about index > length t??? */
  let insertAt = (t, ~index, ~value) => {
    let (front, back) = splitAt(t, ~index);
    append(front, [value, ...back]);
  };

  let splitWhen = (t, ~f) =>
    switch (findIndex(t, ~f=(_, element) => f(element))) {
    | Some((index, _)) =>
      splitAt(t, ~index);
    | None => (t, [])
    };

  let intersperse = (t, ~sep) =>
    switch (t) {
    | [] => []
    | [x] => [x]
    | [x, ...rest] => [
        x,
        ...foldRight(rest, ~initial=[], ~f=(acc, x) => [sep, x, ...acc]),
      ]
    };

  let initialize = (length, ~f) => Belt.List.makeBy(length, f);

  let forEach = (t, ~f): unit => Belt.List.forEach(t, f);

  let forEachI = (t, ~f): unit => Belt.List.forEachWithIndex(t, f);

  let toArray = Array.of_list;

  let join = (strings, ~sep) => Js.Array.joinWith(sep, toArray(strings));
};

module Result = {
  type t('error, 'ok) = Belt.Result.t('ok, 'error);

  let ok = a => Belt.Result.Ok(a);

  let error = e => Belt.Result.Error(e);

  let isError = Belt.Result.isError;

  let isOk = Belt.Result.isOk;

  let both = (a, b) =>
    switch (a, b) {
    | (Ok(a'), Ok(b')) => Ok((a', b'))
    | (Error(a'), _) => Error(a')
    | (_, Error(b')) => Error(b')
    };

  let join = a =>
    switch (a) {
    | Ok(a') => a'
    | Error(error) => Error(error)
    };

  let or_ = (a, b) =>
    switch (a) {
    | Ok(_) => a
    | _ => b
    };

  let and_ = (a, b) =>
    switch (a) {
    | Ok(_) => b
    | _ => a
    };

  let get = (t, ~default) => Belt.Result.getWithDefault(t, default);

  let getOrFailWith = (t, ~exn) =>
    switch (t) {
    | Error(_) => raise(exn)
    | Ok(value) => value
    };

  let getUnsafe = t => Belt.Result.getExn(t);

  let getError = (t, ~default) =>
    switch (t) {
    | Ok(_) => default
    | Error(value) => value
    };

  let map2 = (a, b, ~f) =>
    switch (a, b) {
    | (Ok(a), Ok(b)) => Ok(f(a, b))
    | (Error(a), _) => Error(a)
    | (_, Error(b)) => Error(b)
    };

  let combine = t =>
    List.foldRight(t, ~initial=Ok([]), ~f=map2(~f=(b, a) => [a, ...b]));

  let map = (t, ~f) => Belt.Result.map(t, f);

  let mapError = (t, ~f) =>
    switch (t) {
    | Error(error) => Error(f(error))
    | Ok(value) => Ok(value)
    };

  let fromOption = (ma, ~error) =>
    switch (ma) {
    | None => Belt.Result.Error(error)
    | Some(right) => Belt.Result.Ok(right)
    };

  let toOption = r =>
    switch (r) {
    | Ok(v) => Some(v)
    | Error(_) => None
    };

  let bind = (t, ~f) => Belt.Result.flatMap(t, f);

  let attempt = f =>
    switch (f()) {
    | value => Ok(value)
    | exception error => Error(error)
    };

  let fold = (t, ~initial, ~f) =>
    switch (t) {
    | Ok(a) => f(initial, a)
    | Error(_) => initial
    };

  let transpose = t =>
    switch (t) {
    | Error(error) => Some(Error(error))
    | Ok(None) => None
    | Ok(Some(value)) => Some(Ok(value))
    };

  let forEach = (t, ~f) =>
    switch (t) {
    | Ok(a) => f(a)
    | _ => ()
    };

  let pp = (errf, okf, fmt, t) =>
    switch (t) {
    | Ok(ok) =>
      Format.pp_print_string(fmt, "<ok: ");
      okf(fmt, ok);
      Format.pp_print_string(fmt, ">");
    | Error(err) =>
      Format.pp_print_string(fmt, "<error: ");
      errf(fmt, err);
      Format.pp_print_string(fmt, ">");
    };

  module Infix = {
    let (>>=) = (t, f) => bind(t, ~f);
    let (>>|) = (t, f) => map(t, ~f);
  };
};

module Option = {
  type t('a) = option('a);

  let some = a => Some(a);

  let isSome = Belt.Option.isSome;

  let isNone = Belt.Option.isNone;

  let or_ = (ta, tb) => isSome(ta) ? ta : tb;

  let bind = (t, ~f) =>
    switch (t) {
    | None => None
    | Some(x) => f(x)
    };

  let join =
    fun
    | Some(option) => option
    | None => None;

  let both = (a, b) =>
    switch (a, b) {
    | (Some(a), Some(b)) => Some((a, b))
    | _ => None
    };

  let map = (t, ~f) => Belt.Option.map(t, f);

  let get = (t, ~default) => Belt.Option.getWithDefault(t, default);

  let getOrFailWith = (t, ~exn) =>
    switch (t) {
    | Some(value) => value
    | None => raise(exn)
    };

  let getUnsafe =
    getOrFailWith(~exn=Invalid_argument("Option.getUnsafe called with None"));

  let toArray = t =>
    switch (t) {
    | None => [||]
    | Some(value) => [|value|]
    };

  let toList = t =>
    switch (t) {
    | None => []
    | Some(value) => [value]
    };

  let fold = (t, ~initial, ~f) =>
    switch (t) {
    | None => initial
    | Some(value) => f(initial, value)
    };

  let forEach = (t, ~f) =>
    switch (t) {
    | None => ()
    | Some(x) => f(x)
    };

  let toResult = (t, ~or_) =>
    switch (t) {
    | Some(value) => Ok(value)
    | None => Error(or_)
    };

  module Infix = {
    let (>>=) = (t, f) => bind(t, ~f);
    let (>>|) = (t, f) => map(t, ~f);
  };
};

module Char = {
  type t = char;

  let toCode = (c: char) => Char.code(c);

  let fromCode = (i): option(char) =>
    if (0 <= i && i <= 255) {
      Some(Char.chr(i));
    } else {
      None;
    };

  let toString = c => String.make(1, c);

  let fromString = (str): option(char) =>
    switch (String.length(str)) {
    | 1 => Some(str.[0])
    | _ => None
    };

  let toDigit = char =>
    switch (char) {
    | '0'..'9' => Some(toCode(char) - toCode('0'))
    | _ => None
    };

  let toLowercase = char =>
    switch (char) {
    | 'A'..'Z' => Char.chr(toCode('a') + (toCode(char) - toCode('A')))
    | _ => char
    };

  let toUppercase = char =>
    switch (char) {
    | 'a'..'z' => Char.chr(toCode('A') + (toCode(char) - toCode('a')))
    | _ => char
    };

  let isLowercase =
    fun
    | 'a'..'z' => true
    | _ => false;

  let isUppercase =
    fun
    | 'A'..'Z' => true
    | _ => false;

  let isLetter =
    fun
    | 'a'..'z'
    | 'A'..'Z' => true
    | _ => false;

  let isDigit =
    fun
    | '0'..'9' => true
    | _ => false;

  let isAlphanumeric =
    fun
    | 'a'..'z'
    | 'A'..'Z'
    | '0'..'9' => true
    | _ => false;

  let isPrintable =
    fun
    | ' '..'~' => true
    | _ => false;

  let isWhitespace =
    fun
    | '\t'
    | '\n'
    | '\011' /* vertical tab */
    | '\012' /* form feed */
    | '\r'
    | ' ' => true
    | _ => false;
};

module Float = {
  type t = float;

  let add = (+.);

  let (+) = (+.);

  let subtract = (-.);

  let (-) = (-.);

  let multiply = ( *. );

  let ( * ) = ( *. );

  let divide = (n, ~by) => n /. by;

  let (/) = (/.);

  let power = (~base, ~exponent) => Js.Math.pow_float(~base, ~exp=exponent);

  let ( ** ) = (base, exponent) => power(~base, ~exponent);

  let negate = (~-.);

  let (~-) = (~-.);

  let absolute = Js.Math.abs_float;

  let clamp = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(
        Invalid_argument(
          "~lower:"
          ++ Js.Float.toString(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Js.Float.toString(upper),
        ),
      );
    } else if (Js.Float.isNaN(lower)
               || Js.Float.isNaN(upper)
               || Js.Float.isNaN(n)) {
      nan;
    } else {
      max(lower, min(upper, n));
    };

  let inRange = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(
        Invalid_argument(
          "~lower:"
          ++ Js.Float.toString(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Js.Float.toString(upper),
        ),
      );
    } else {
      n >= lower && n < upper;
    };

  let squareRoot = sqrt;

  let log = (n, ~base) => Js.Math.log(n) / Js.Math.log(base);

  let zero = 0.0;

  let one = 1.0;

  let nan = Js.Float._NaN;

  let infinity = infinity;

  let negativeInfinity = neg_infinity;

  let e = Js.Math._E;

  let pi = Js.Math._PI;

  let epsilon = epsilon_float;

  [@bs.scope "Number"] [@bs.val] external largestValue: t = "MAX_VALUE";

  [@bs.scope "Number"] [@bs.val] external smallestValue: t = "MIN_VALUE";

  [@bs.scope "Number"] [@bs.val]
  external maximumSafeInteger: t = "MAX_SAFE_INTEGER";

  [@bs.scope "Number"] [@bs.val]
  external minimumSafeInteger: t = "MIN_SAFE_INTEGER";

  let isNaN = Js.Float.isNaN;

  let isFinite = Js.Float.isFinite;

  let isInfinite = n => !Js.Float.isFinite(n) && !isNaN(n);

  [@bs.scope "Number"] [@bs.val] external isInteger: t => t = "isInteger";

  [@bs.scope "Number"] [@bs.val]
  external isSafeInteger: t => t = "isSafeInteger";

  let maximum = (x, y) =>
    if (isNaN(x) || isNaN(y)) {
      nan;
    } else if (y > x) {
      y;
    } else {
      x;
    };

  let minimum = (x, y) =>
    if (isNaN(x) || isNaN(y)) {
      nan;
    } else if (y < x) {
      y;
    } else {
      x;
    };

  let hypotenuse = Js.Math.hypot;

  type radians = float;
  // TODO a better? representstion for radians
  // type radians =
  // | Radians(float);

  let degrees = n => n * (pi / 180.0);

  let radians = Fun.identity;

  let turns = n => n * 2. * pi;

  let cos = Js.Math.cos;

  let acos = Js.Math.acos;

  let sin = Js.Math.sin;

  let asin = Js.Math.asin;

  let tan = Js.Math.tan;

  let atan = Js.Math.atan;

  let atan2 = (~y, ~x) => Js.Math.atan2(~y, ~x, ());

  type direction = [
    | `Zero
    | `AwayFromZero
    | `Up
    | `Down
    | `Closest([ | `Zero | `AwayFromZero | `Up | `Down | `ToEven])
  ];

  let round = (~direction=`Closest(`Up), n) =>
    switch (direction) {
    | `Up => Js.Math.ceil_float(n)
    | `Down => Js.Math.floor_float(n)
    | `Zero => Js.Math.trunc(n)
    | `AwayFromZero =>
      if (n > 0.) {
        Js.Math.ceil_float(n);
      } else {
        Js.Math.floor_float(n);
      }
    | `Closest(`Zero) =>
      if (n > 0.) {
        Js.Math.ceil_float(n -. 0.5);
      } else {
        Js.Math.floor_float(n +. 0.5);
      }
    | `Closest(`AwayFromZero) =>
      if (n > 0.) {
        Js.Math.floor_float(n +. 0.5);
      } else {
        Js.Math.ceil_float(n -. 0.5);
      }
    | `Closest(`Down) => Js.Math.ceil_float(n -. 0.5)
    | `Closest(`Up) => Js.Math.round(n)
    | `Closest(`ToEven) =>
      /* Outside of the range (roundNearestLowerBound..roundNearestUpperBound), all representable doubles
            are integers in the mathematical sense, and [round_nearest] should be identity.

            However, for odd numbers with the absolute value between 2**52 and 2**53, the formula
            [round x = floor (x + 0.5)] does not hold:

            {v
              # let naiveRoundNearest x = floor (x +. 0.5);;
              # let x = 2. ** 52. +. 1.;;
              val x : float = 4503599627370497.
              # naive_round_nearest x;;
              - :     float = 4503599627370498.
            v}
         */
      let roundNearestLowerBound = -. (2. ** 52.);
      let roundNearestUpperBound = 2. ** 52.;
      if (n <= roundNearestLowerBound || n >= roundNearestUpperBound) {
        n +. 0.;
      } else {
        let floor = floor(n);
        let ceil_or_succ = floor +. 1.;
        let diff_floor = n -. floor;
        let diff_ceil = ceil_or_succ -. n;
        if (diff_floor < diff_ceil) {
          floor;
        } else if (diff_floor > diff_ceil) {
          ceil_or_succ;
        } else if (mod_float(floor, 2.) == 0.) {
          floor;
        } else {
          ceil_or_succ;
        };
      };
    };

  let floor = Js.Math.floor_float;

  let ceiling = Js.Math.ceil_float;

  let truncate = Js.Math.trunc;

  let fromPolar = ((r, theta)) => (r * cos(theta), r * sin(theta));

  let toPolar = ((x, y)) => (hypotenuse(x, y), atan2(~x, ~y));

  let fromInt = Js.Int.toFloat;

  let fromString = Js.Float.fromString;

  let toInt = f =>
    if (Js.Float.isFinite(f)) {
      Some(Js.Math.unsafe_trunc(f));
    } else {
      None;
    };

  let toString = Js.Float.toString;
};

module Int = {
  type t = int;

  type identity;

  let minimumValue = Js.Int.min;

  let maximumValue = Js.Int.max;

  let zero = 0;

  let one = 1;

  let add = (+);

  let (+) = (+);

  let subtract = (-);

  let (-) = (-);

  let multiply = ( * );

  let ( * ) = multiply;

  let divide = (n, ~by) => n / by;

  let (/) = (/);

  let (/\/) = (n, by) => Js.Int.toFloat(n) /. Js.Int.toFloat(by);

  let power = (~base, ~exponent) => Js.Math.pow_int(~base, ~exp=exponent);

  let ( ** ) = (base, exponent) => Js.Math.pow_int(~base, ~exp=exponent);

  let negate = (~-);

  let (~-) = (~-);

  let modulo = (n, ~by) => n mod by;

  let remainder = (n, ~by) => n mod by;

  let maximum = Js.Math.max_int;

  let minimum = Js.Math.min_int;

  let absolute = n =>
    if (n < 0) {
      n * (-1);
    } else {
      n;
    };

  let isEven = n => n mod 2 == 0;

  let isOdd = n => n mod 2 != 0;

  let clamp = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(Invalid_argument("~lower must be less than or equal to ~upper"));
    } else {
      max(lower, min(upper, n));
    };

  let inRange = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(Invalid_argument("~lower must be less than or equal to ~upper"));
    } else {
      n >= lower && n < upper;
    };

  let toFloat = Js.Int.toFloat;

  let toString = Js.Int.toString;

  let fromString = s =>
    switch (int_of_string(s)) {
    | i => Some(i)
    | exception (Failure(_)) => None
    };
};

module Integer = {
  type t;

  [@bs.val] external fromInt: int => t = "BigInt";

  [@bs.val] external fromInt64: Int64.t => t = "BigInt";

  [@bs.val] external fromFloat: float => t = "BigInt";

  [@bs.val] external fromStringUnsafe: string => (t) = "BigInt";

  let fromString = (string) => {
    // TODO "" should parse as None
    switch (fromStringUnsafe(string)) {
    | value => Some(value)
    | exception _ => None
    };
  };

  let compare = compare;

  let equal = (==);

  let zero = [%raw "BigInt(0)"];

  let one = [%raw "BigInt(1)"];

  let isEven: t => bool = [%raw n => "{ return n % 2 === 0 }"];

  let isOdd: t => bool = [%raw n => "{ return n % 2 !== 0 }"];

  let add = [%raw (a, b) => "{return a + b}"];

  let (+) = add;

  let subtract = [%raw (a, b) => "{return a - b}"];

  let (-) = subtract;

  let multiply: (t, t) => t = [%raw (a, b) => "{return a * b}"];

  let ( * ) = multiply;

  let divide: (t, t) => t = [%raw (a, b) => "{return a / b}"];

  let (/) = divide;

  let divide = (n, ~by) => divide(n, by);

  let negate: t => t = [%raw a => "{return a * BigInt(-1)}"];

  let modulo: (t, t) => t = [%raw (a, b) => "{return a % b}"];

  let modulo = (n: t, ~by: t): t => modulo(n, by);

  let remainder = (n: t, ~by: t): t => modulo(n, ~by);

  let power: (t, t) => t = [%raw (a, ) => "{return a ** b}"];

  let ( ** ) = power;

  let power = (~modulo as modulus=?, ~base: t, ~exponent: t) =>
    if (exponent < zero) {
      zero;
    } else {
      switch (modulus) {
      | None => power(base, exponent)
      | Some(modulus) =>
        let rec loop = (b: t, e: t, result: t) =>
          if (e == zero) {
            result;
          } else {
            loop(
              modulo(b * b, ~by=modulus),
              e,
              // (e / [@raw "2n"]),
              if (isEven(e)) {
                result;
              } else {
                modulo(result * b, ~by=modulus);
              },
            );
          };
        loop(base, exponent, one);
      };
    };

  let maximum = (a, b) =>
    if (a < b) {
      b;
    } else {
      a;
    };

  let minimum = (a, b) =>
    if (a > b) {
      b;
    } else {
      a;
    };

  let absolute = n =>
    if (n < zero) {
      negate(n);
    } else {
      n;
    };

  let clamp = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(Invalid_argument("~lower must be less than or equal to ~upper"));
    } else {
      max(lower, min(upper, n));
    };

  let inRange = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(Invalid_argument("~lower must be less than or equal to ~upper"));
    } else {
      n >= lower && n < upper;
    };

  [@bs.val] [@bs.scope "BigInt"] external asIntN: (int, t) => 'a = "asIntN";

  let toInt = t =>
    if (t > fromInt(Int.maximumValue)) {
      None;
    } else {
      Some(asIntN(53, t));
    };

  let toInt64 = t =>
    if (t > fromInt64(Int64.max_int)) {
      None;
    } else {
      Some(asIntN(63, t));
    };

  // let maxFloat = [@raw "2n ** (64n - 1n) - 1n"];
  // let toFloat = (t) =>
  // if (t > maxFloat) {
  // None;
  // } else {
  // Some(asIntN(53, t));
  // };
  let toFloat = _ => None;

  [@bs.send] external toString: t => string = "toString";
};

module Tuple = {
  type t('a, 'b) = ('a, 'b);

  let make = (a, b) => (a, b);

  let fromArray = array => {
    switch (array) {
    | [||]
    | [|_|] => None
    | [|a, b|] => Some((a, b))
    | _ => None
    };
  };

  let fromList = list => {
    switch (list) {
    | []
    | [_] => None
    | [a, b, ..._rest] => Some((a, b))
    };
  };

  let first = ((a, _)) => a;

  let second = ((_, b)) => b;

  let mapFirst = ((a, b), ~f) => (f(a), b);

  let mapSecond = ((a, b), ~f) => (a, f(b));

  let mapEach = ((a, b), ~f, ~g) => (f(a), g(b));

  let mapAll = ((a1, a2), ~f) => (f(a1), f(a2));

  let swap = ((a, b)) => (b, a);

  let curry = (f: (('a, 'b)) => 'c, a, b): 'c => f((a, b));

  let uncurry = (f: ('a, 'b) => 'c, (a, b): ('a, 'b)): 'c => f(a, b);

  let toArray = ((a, b)) => [|a, b|];

  let toList = ((a, b)) => [a, b];
};

module Tuple3 = {
  type t('a, 'b, 'c) = ('a, 'b, 'c);

  let make = (a, b, c) => (a, b, c);

  let fromArray = array => {
    switch (array) {
    | [||]
    | [|_|]
    | [|_, _|] => None
    | [|a, b, c|] => Some((a, b, c))
    | _ => None
    };
  };

  let fromList = list => {
    switch (list) {
    | []
    | [_]
    | [_, _] => None
    | [a, b, c, ..._rest] => Some((a, b, c))
    };
  };

  let first = ((a, _, _)) => a;

  let second = ((_, b, _)) => b;

  let third = ((_, _, c)) => c;

  let initial = ((a, b, _)) => (a, b);

  let tail = ((_, b, c)) => (b, c);

  let mapFirst = ((a, b, c), ~f) => (f(a), b, c);

  let mapSecond = ((a, b, c), ~f) => (a, f(b), c);

  let mapThird = ((a, b, c), ~f) => (a, b, f(c));

  let mapEach = ((a, b, c), ~f, ~g, ~h) => (f(a), g(b), h(c));

  let mapAll = ((a1, a2, a3), ~f) => (f(a1), f(a2), f(a3));

  let rotateLeft = ((a, b, c)) => (b, c, a);

  let rotateRight = ((a, b, c)) => (c, a, b);

  let curry = (f, a, b, c) => f((a, b, c));

  let uncurry = (f, (a, b, c)) => f(a, b, c);

  let toArray = ((a, b, c)) => [|a, b, c|];

  let toList = ((a, b, c)) => [a, b, c];
};

module String = {
  type t = string;

  type identity;

  let initialize = (length, ~f) =>
    Js.Array.joinWith("", Array.init(length, f));

  let isEmpty = t => t == "";

  let length = String.length;

  let uncons = s =>
    switch (s) {
    | "" => None
    | s => Some((s.[0], String.sub(s, 1, String.length(s) - 1)))
    };

  let dropLeft = (~count, s) => Js.String.substr(~from=count, s);

  let dropRight = (~count, s) =>
    if (count < 1) {
      s;
    } else {
      Js.String.slice(~from=0, ~to_=- count, s);
    };

  let split = (t, ~on) => Js.String.split(on, t);

  let lines = split(~on="\n");

  let words = split(~on=" ");

  let endsWith = (t, ~suffix) => Js.String.endsWith(suffix, t);

  let startsWith = (t, ~prefix) => Js.String.startsWith(prefix, t);

  let toLowercase = s => String.lowercase_ascii(s);

  let toUppercase = s => String.uppercase_ascii(s);

  let uncapitalize = String.uncapitalize_ascii;

  let capitalize = String.capitalize_ascii;

  let isCapitalized = s => s == String.capitalize_ascii(s);

  let includes = (t, ~substring) => Js.String.includes(substring, t);

  let repeat = (s, ~count) => Js.String.repeat(count, s);

  let reverse = s =>
    Js.Array.joinWith("", Js.Array.reverseInPlace(Js.String.split("", s)));

  // TODO why the trip to code?
  let fromArray = characters =>
    Js.Array.joinWith(
      "",
      Array.map(
        character => Char.toCode(character)->Js.String.fromCharCode,
        characters,
      ),
    );

  // TODO why the trip to code
  let fromList = t =>
    Js.Array.joinWith(
      "",
      Array.map(
        character => Char.toCode(character)->Js.String.fromCharCode,
        Array.of_list(t),
      ),
    );

  let toArray = t => Js.String.castToArrayLike(t)->Js.Array.from;

  let toList = (s): list(char) =>
    Js.String.castToArrayLike(s)->Js.Array.from->Belt.List.fromArray;

  let fromInt = i => Printf.sprintf("%d", i);

  let fromChar = c => Char.toCode(c)->Js.String.fromCharCode;

  let slice = (~to_=?, t: string, ~from): string =>
    Js.String.slice(~from, ~to_=Option.get(to_, ~default=length(t)), t);

  let trim = Js.String.trim;

  let insertAt = (t, ~index, ~value) =>
    Js.String.slice(~from=0, ~to_=index, t)
    ++ value
    ++ Js.String.sliceToEnd(~from=index, t);
};

module Set = {
  type t('a, 'cmp) = Belt.Set.t('a, 'cmp);


  let empty =
      (
        type a,
        type id,
        module Cmp: Comparator with type t = a and type identity = id,
      ) =>
    Belt.Set.make(
      ~id=(module
           {
             type t = Cmp.t;
             type identity = Cmp.identity;
             let cmp = Cmp.compare->Obj.magic;
           }): Belt.Map.id(Cmp.t, Cmp.identity),
    );

  let length = Belt.Set.size;

  let isEmpty = Belt.Set.isEmpty;

  let includes = Belt.Set.has;

  let add = Belt.Set.add;

  let remove = Belt.Set.remove;

  let difference = Belt.Set.diff;

  let intersection = Belt.Set.intersect;

  let union = Belt.Set.union;

  let filter = (s, ~f) => Belt.Set.keep(s, f);

  let partition = (s, ~f) => Belt.Set.partition(s, f);

  let find = (s, ~f) => Belt.Set.toArray(s)->Belt.Array.getBy(f);

  let all = (s, ~f) => Belt.Set.every(s, f);

  let any = (s, ~f) => Belt.Set.some(s, f);

  let forEach = (s, ~f) => Belt.Set.forEach(s, f);

  let fold = (s, ~initial, ~f) => Belt.Set.reduce(s, initial, f);

  let toArray = Belt.Set.toArray;

  let toList = Belt.Set.toList;

  module Poly = {
    type identity;

    type nonrec t('a) = t('a, identity);

    let fromArray = (type a, a: array(a)): t(a) =>
      Belt.Set.fromArray(
        a,
        ~id=
          (module
           {
             type t = a;
             type nonrec identity = identity;
             let cmp = Pervasives.compare->Obj.magic;
           }),
      );

    let fromList = l => Array.of_list(l)->fromArray;

    let empty = () => fromArray([||]);

    let singleton = a => fromArray([|a|]);
  };

  module Int = {
    type nonrec t = t(Int.t, Int.identity);

    let fromArray = a => Poly.fromArray(a)->Obj.magic;

    let empty = fromArray([||]);

    let singleton = a => fromArray([|a|]);

    let fromList = l => Array.of_list(l)->fromArray;
  };

  module String = {
    type nonrec t = t(String.t, String.identity);

    let fromArray = a => Poly.fromArray(a)->Obj.magic;

    let empty = fromArray([||]);

    let singleton = a => fromArray([|a|]);

    let fromList = l => Array.of_list(l)->fromArray;
  };
};

module Map = {
  type t('key, 'value, 'cmp) = Belt.Map.t('key, 'value, 'cmp);

  let empty =
      (
        type k,
        type id,
        module Cmp: Comparator with type t = k and type identity = id,
      ) =>
    Belt.Map.make(
      ~id=(module
           {
             type t = Cmp.t;
             type identity = Cmp.identity;
             let cmp = Cmp.compare->Obj.magic;
           }): Belt.Map.id(Cmp.t, Cmp.identity),
    );

  let isEmpty = Belt.Map.isEmpty;

  let includes = Belt.Map.has;

  let length = Belt.Map.size;

  let add = (m, ~key, ~value) => Belt.Map.set(m, key, value);

  let remove = Belt.Map.remove;

  let get = Belt.Map.get;

  let update = (m, ~key, ~f) => Belt.Map.update(m, key, f);

  let merge = (m1, m2, ~f) => Belt.Map.merge(m1, m2, f);

  let map = (m, ~f) => Belt.Map.map(m, value => f(value));

  let mapI = (t, ~f) => Belt.Map.mapWithKey(t, f);

  let filter = (m, ~f) => Belt.Map.keep(m, (_, value) => f(value));

  let partition = (m, ~f) =>
    Belt.Map.partition(m, (key, value) => f(~key, ~value));

  let find = (m, ~f) =>
    Belt.Map.findFirstBy(m, (key, value) => f(~key, ~value));

  let any = (m, ~f) => Belt.Map.some(m, (_, value) => f(value));

  let all = (m, ~f) => Belt.Map.every(m, (_, value) => f(value));

  let forEach = (m, ~f) => Belt.Map.forEach(m, (_, value) => f(value));

  let fold = (m, ~initial, ~f) =>
    Belt.Map.reduce(m, initial, (acc, key, data) =>
      f(acc, ~key, ~value=data)
    );

  let keys = m => Belt.Map.keysToArray(m)->Array.to_list;

  let values = m => Belt.Map.valuesToArray(m)->Array.to_list;

  let maximum = Belt.Map.maxKey;

  let minimum = Belt.Map.minKey;

  let extent = t =>
    Option.both(minimum(t), maximum(t));

  let toArray = Belt.Map.toArray;

  let toList = Belt.Map.toList;

  module Poly = {
    type identity;

    type nonrec t('k, 'v) = t('k, 'v, identity);

    let fromArray = (type k, type v, a: array((k, v))): t(k, v) =>
      Belt.Map.fromArray(
        a,
        ~id=
          (module
           {
             type t = k;
             type nonrec identity = identity;
             let cmp = Pervasives.compare->Obj.magic;
           }),
      );

    let empty = () => fromArray([||]);

    let fromList = l => fromArray(Array.of_list(l));

    let singleton = (~key, ~value) => fromArray([|(key, value)|]);
  };

  module Int = {
    type nonrec t('v) = t(Int.t, 'v, Int.identity);

    let fromArray = a => Poly.fromArray(a)->Obj.magic;

    let empty = fromArray([||]);

    let singleton = (~key, ~value) => fromArray([|(key, value)|]);

    let fromList = l => fromArray(Array.of_list(l));
  };

  module String = {
    type nonrec t('v) = t(String.t, 'v, String.identity);

    let fromArray = a => Poly.fromArray(a)->Obj.magic;

    let empty = fromArray([||]);

    let singleton = (~key, ~value) => fromArray([|(key, value)|]);

    let fromList = l => fromArray(Array.of_list(l));
  };
};

module Array = {
  type t('a) = array('a);

  let empty = [||];

  let singleton = a => [|a|];

  let clone = t => Array.map(Fun.identity, t);

  let length = Belt.Array.length;

  let isEmpty = a => length(a) == 0;

  let initialize = (length, ~f) => Belt.Array.makeBy(length, f);

  let range = (~from=0, to_) => Belt.Array.makeBy(to_ - from, i => i + from);

  let fromList = Belt.List.toArray;

  let toList = Belt.List.fromArray;

  let toIndexedList = array =>
    Belt.Array.reduceReverse(array, (length(array) - 1, []), ((i, acc), x) =>
      (i - 1, [(i, x), ...acc])
    )
    ->snd;

  let get = Belt.Array.getExn;

  let getAt = (t, ~index) => Belt.Array.get(t, index);

  let first = t => getAt(t, ~index=0);

  let last = t => getAt(t, ~index=Array.length(t) - 1);

  let set = (t, index, value) => t[index] = value;

  let setAt = (t, ~index, ~value) => t[index] = value;

  let sum = (type a, t, module M: Container.Sum with type t = a): a =>
    Array.fold_left(M.add, M.zero, t);

  let filter = (t, ~f) => Belt.Array.keep(t, f);

  let swap = (t, i, j) => {
    let temp = t[i];
    t[i] = t[j];
    t[j] = temp;
    ();
  };

  let fold = (t, ~initial, ~f) => Belt.Array.reduce(t, initial, f);

  let foldRight = (t, ~initial, ~f) =>
    Belt.Array.reduceReverse(t, initial, f);

  let maximum = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(max, element) => {
      switch (max) {
      | None => Some(element)
      | Some(current) => compare(element, current) > 0 ? Some(element) : max
      }
    });

  let minimum = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(min, element) => {
      switch (min) {
      | None => Some(element)
      | Some(current) => compare(element, current) < 0 ? Some(element) : min
      }
    });

  let extent = (t, ~compare) =>
    fold(t, ~initial=None, ~f=(range, element) => {
      switch (range) {
      | None => Some((element, element))
      | Some((min, max)) =>
        Some((
          compare(element, min) < 0 ? element : min,
          compare(element, max) > 0 ? element : max,
        ))
      }
    });

  let map = (t, ~f) => Belt.Array.map(t, f);

  let mapI = (t, ~f) => Belt.Array.mapWithIndex(t, f);

  let map2 = (a, b, ~f: ('a, 'b) => 'c): array('c) =>
    Belt.Array.zipBy(a, b, f);

  let map3 = (as_, bs, cs: t('c), ~f) => {
    let minLength =
      Belt.Array.reduce([|length(bs), length(cs)|], length(as_), min);

    Belt.Array.makeBy(minLength, i => f(as_[i], bs[i], cs[i]));
  };

  let zip = map2(~f=(a, b) => (a, b));

  let bind = (t, ~f) => Belt.Array.map(t, f)->Belt.Array.concatMany;

  let sliding = (~step=1, a, ~size) => {
    let n = Array.length(a);
    if (size > n) {
      empty;
    } else {
      initialize(1 + (n - size) / step, ~f=i =>
        initialize(size, ~f=j => a[i * step + j])
      );
    };
  };

  let find = (t, ~f) => {
    let rec find_loop = (t, ~f, ~length, i) =>
      if (i >= length) {
        None;
      } else if (f(t[i])) {
        Some(t[i]);
      } else {
        find_loop(t, ~f, ~length, i + 1);
      };

    find_loop(t, ~f, ~length=length(t), 0);
  };

  let findIndex = (array, ~f) => {
    let rec loop = index =>
      if (index >= length(array)) {
        None;
      } else if (f(index, array[index])) {
        Some((index, array[index]));
      } else {
        loop(index + 1);
      };

    loop(0);
  };

  let any = (t, ~f) => Belt.Array.some(t, f);

  let all = (t, ~f) => Belt.Array.every(t, f);

  let includes = (t, v, ~equal) => any(t, ~f=equal(v));

  let append = (a, a') => Belt.Array.concat(a, a');

  let concatenate = (ars: array(array('a))) => Belt.Array.concatMany(ars);

  let intersperse = (t, ~sep) =>
    Belt.Array.makeBy(max(0, length(t) * 2 - 1), i =>
      if (i mod 2 != 0) {
        sep;
      } else {
        t[i / 2];
      }
    );

  let slice = (~to_=?, array, ~from) => {
    let defaultTo =
      switch (to_) {
      | None => length(array)
      | Some(i) => i
      };
    let sliceFrom =
      if (from >= 0) {
        min(length(array), from);
      } else {
        max(0, min(length(array), length(array) + from));
      };

    let sliceTo =
      if (defaultTo >= 0) {
        min(length(array), defaultTo);
      } else {
        max(0, min(length(array), length(array) + defaultTo));
      };

    if (sliceFrom >= sliceTo) {
      empty;
    } else {
      Belt.Array.makeBy(sliceTo - sliceFrom, i => array[i + sliceFrom]);
    };
  };

  let count = (t, ~f) =>
    fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0));

  let chunksOf = (t, ~size) => sliding(t, ~step=size, ~size);

  let reverse = Belt.Array.reverseInPlace;

  let shuffle = t => Belt.Array.shuffle(t);

  let forEach = (t, ~f): unit => Belt.Array.forEach(t, f);

  let forEachI = (t, ~f): unit =>
    for (i in 0 to length(t) - 1) {
      f(i, t[i]);
    };

  let partition = (t, ~f) =>
    fold(
      t,
      ~initial=([], []),
      ~f=(result, element) => {
        let update =
          if (f(element)) {
            Tuple.mapFirst;
          } else {
            Tuple.mapSecond;
          };
        update(result, ~f=result => List.cons(result, element));
      },
    )
    ->Tuple.mapAll(~f=list => List.reverse(list)->fromList);

  // TODO what about negative indicies?
  let splitAt = (t, ~index) => {
    (slice(t, ~from=0, ~to_=index), slice(t, ~from=index, ~to_=length(t)));
  };

  let splitWhen = (t, ~f) => {
    switch (findIndex(t, ~f=(_, e) => f(e))) {
    | None => (t, [||])
    | Some((index, _)) => splitAt(t, ~index)
    };
  };

  let unzip = t => (
    Array.init(length(t), i => Tuple.first(t[i])),
    Array.init(length(t), i => Tuple.second(t[i])),
  );

  let unzip3 = t => (
    Array.init(length(t), i => Tuple3.first(t[i])),
    Array.init(length(t), i => Tuple3.second(t[i])),
    Array.init(length(t), i => Tuple3.third(t[i])),
  );

  let repeat = (element, ~length) =>
    Array.init(max(length, 0), _ => element);

  let filterMap = (t, ~f) =>
    fold(t, ~initial=[], ~f=(results, element) => {
      switch (f(element)) {
      | None => results
      | Some(value) => [value, ...results]
      }
    })
    ->fromList;

  let sort = (a, ~compare) => Array.sort(compare, a);

  let values = t =>
    fold(t, ~initial=[], ~f=(results, element) =>
      switch (element) {
      | None => results
      | Some(value) => [value, ...results]
      }
    )
    ->fromList;

  let join = (t, ~sep) => Js.Array.joinWith(sep, t);

  let groupBy = (t, comparator, ~f) =>
    fold(
      t,
      ~initial=Map.empty(comparator),
      ~f=(map, element) => {
        let key = f(element);
        Map.update(
          map,
          ~key,
          ~f=
            fun
            | None => Some([element])
            | Some(elements) => Some([element, ...elements]),
        );
      },
    );
};