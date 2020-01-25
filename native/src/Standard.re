module Fun = {
  external identity: 'a => 'a = "%identity";

  external ignore: _ => unit = "%ignore";

  let constant = (a, _) => a;

  let sequence = (a, b) => {
    ignore(a);
    b;
  };

  let flip = (f, a, b) => f(b, a);

  let apply = (f, a) => f(a);

  let (<|) = (f, a) => f(a);

  external pipe: ('a, 'a => 'b) => 'b = "%revapply";

  external (|>): ('a, 'a => 'b) => 'b = "%revapply";

  let compose = (f, g, a) => f(g(a));

  let (<<) = compose;

  let composeRight = (f, g, a) => g(f(a));

  let (>>) = composeRight;

  let tap = (value, ~f) => {
    f(value);
    value;
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

  let ofInt = i =>
    switch (i) {
    | 0 => Some(false)
    | 1 => Some(true)
    | _ => None
    };

  let ofString = string =>
    switch (string) {
    | "false" => Some(false)
    | "true" => Some(true)
    | _ => None
    };

  [@bs.send] external toString: bool => string = "toString";

  let toInt = t => t ? 1 : 0;
};

module Tuple = {
  type t('a, 'b) = ('a, 'b);

  let make = (a, b) => (a, b);

  let ofArray =
    fun
    | [|a, b|] => Some((a, b))
    | _ => None;

  let ofList =
    fun
    | [a, b, ..._] => Some((a, b))
    | _ => None;

  let first = ((a, _)) => a;

  let second = ((_, b)) => b;

  let mapFirst = ((a, b), ~f) => (f(a), b);

  let mapSecond = ((a, b), ~f) => (a, f(b));

  let mapEach = ((a, b), ~f, ~g) => (f(a), g(b));

  let mapAll = ((a, b), ~f) => (f(a), f(b));

  let swap = ((a, b)) => (b, a);

  let curry = (f: (('a, 'b)) => 'c, a: 'a, b: 'b): 'c => f((a, b));

  let uncurry = (f: ('a, 'b) => 'c, (a, b): ('a, 'b)): 'c => f(a, b);

  let toArray = ((a, b)) => [|a, b|];

  let toList = ((a, b)) => [a, b];
};

module Tuple3 = {
  type t('a, 'b, 'c) = ('a, 'b, 'c);

  let make = (a, b, c) => (a, b, c);

  let ofArray =
    fun
    | [|a, b, c|] => Some((a, b, c))
    | _ => None;

  let ofList =
    fun
    | [a, b, c, ..._] => Some((a, b, c))
    | _ => None;

  let first = ((a, _, _): ('a, 'b, 'c)): 'a => a;

  let second = ((_, b, _): ('a, 'b, 'c)): 'b => b;

  let third = ((_, _, c): ('a, 'b, 'c)): 'c => c;

  let initial = ((a, b, _): ('a, 'b, 'c)): ('a, 'b) => (a, b);

  let tail = ((_, b, c): ('a, 'b, 'c)): ('b, 'c) => (b, c);

  let mapFirst = ((a, b, c), ~f) => (f(a), b, c);

  let mapSecond = ((a, b, c), ~f) => (a, f(b), c);

  let mapThird = ((a, b, c), ~f) => (a, b, f(c));

  let mapEach = ((a, b, c), ~f, ~g, ~h) => (f(a), g(b), h(c));

  let mapAll = ((a1, a2, a3), ~f) => (f(a1), f(a2), f(a3));

  let rotateLeft = ((a, b, c): ('a, 'b, 'c)): ('b, 'c, 'a) => (b, c, a);

  let rotateRight = ((a, b, c): ('a, 'b, 'c)): ('c, 'a, 'b) => (c, a, b);

  let curry = (f: (('a, 'b, 'c)) => 'd, a: 'a, b: 'b, c: 'c): 'd =>
    f((a, b, c));

  let uncurry = (f: ('a, 'b, 'c) => 'd, (a, b, c): ('a, 'b, 'c)): 'd =>
    f(a, b, c);

  let toArray = ((a, b, c)) => [|a, b, c|];

  let toList = ((a, b, c): ('a, 'a, 'a)): list('a) => [a, b, c];
};

module List = {
  type t('a) = list('a);

  let empty = [];

  let singleton = Base.List.return;

  let repeat = (element, ~times) => Base.List.init(times, ~f=_ => element);

  let rec range = (~from=0, to_) =>
    if (from >= to_) {
      [];
    } else {
      [from, ...range(~from=from + 1, to_)];
    };

  let initialize = Base.List.init;

  let sum = (type a, a: t(a), module M: Container.Sum with type t = a): a =>
    Base.List.fold(a, ~init=M.zero, ~f=M.add);

  let ofArray = Base.Array.to_list;

  let isEmpty = (l: list('a)): bool => l == [];

  let head = Base.List.hd;

  let tail = Base.List.tl;

  let cons = (list, element) => [element, ...list];

  let take = (t, ~count) => Base.List.take(t, count);

  let takeWhile = (l: list('a), ~f: 'a => bool): list('a) => {
    let rec takeWhileHelper = (acc, l') =>
      switch (l') {
      | [] => Base.List.rev(acc)
      | [x, ...rest] =>
        if (f(x)) {
          takeWhileHelper([x, ...acc], rest);
        } else {
          Base.List.rev(acc);
        }
      };

    takeWhileHelper([], l);
  };

  let drop = (t, ~count) => Base.List.drop(t, count);

  let rec dropWhile = (l: list('a), ~f: 'a => bool): list('a) =>
    switch (l) {
    | [] => []
    | [x, ...rest] =>
      if (f(x)) {
        dropWhile(~f, rest);
      } else {
        l;
      }
    };

  let initial = (l: list('a)): option(list('a)) =>
    switch (Base.List.rev(l)) {
    | [] => None
    | [_, ...rest] => Some(Base.List.rev(rest))
    };

  let rec last = (l: list('a)): option('a) =>
    switch (l) {
    | [] => None
    | [a] => Some(a)
    | [_, ...tail] => last(tail)
    };

  // let slice = (~to_=?, t, ~from) => {
  //   let n = Base.List.length(t);
  //   let defaultTo =
  //     switch (to_) {
  //     | None => n
  //     | Some(i) => i
  //     };
  //   let sliceFrom =
  //     if (from >= 0) {
  //       min(n, from);
  //     } else {
  //       max(0, min(n, n + from));
  //     };

  //   let sliceTo =
  //     if (defaultTo >= 0) {
  //       min(n, defaultTo);
  //     } else {
  //       max(0, min(n, n + defaultTo));
  //     };

  //   if (sliceFrom >= sliceTo) {
  //     empty;
  //   } else {
  //     take(~count=sliceTo, drop(~count=sliceFrom, t));
  //   };
  // };

  let append = (l1: list('a), l2: list('a)): list('a) =>
    Base.List.append(l1, l2);

  let concatenate = Base.List.concat;

  /* TODO this throws */
  let map2 = Base.List.map2_exn;

  /* TODO this throws! */
  let map3 = Base.List.map3_exn;

  let reverse = (l: list('a)): list('a) => Base.List.rev(l);

  let map = Base.List.map;

  let mapI = Base.List.mapi;

  let bind = Base.List.concat_map;

  let includes = Base.List.mem;

  let find = Base.List.find;

  let findIndex = Base.List.findi;

  let any = Base.List.exists;

  let all = Base.List.for_all;

  let getAt = (l: list('a), ~index: int): option('a) =>
    Base.List.nth(l, index);

  let filterMap = Base.List.filter_map;

  let filter = (t, ~f) => Base.List.filter(t, ~f);

  let filterI = (t, ~f) => Base.List.filteri(t, ~f);

  let partition = Base.List.partition_tf;

  let fold = (t, ~initial, ~f) => Base.List.fold(t, ~init=initial, ~f);

  let count = Base.List.count;

  let foldRight = (t, ~initial, ~f) =>
    Base.List.fold_right(t, ~init=initial, ~f=Fun.flip(f));

  let splitAt = (l: list('a), ~index: int): (list('a), list('a)) => (
    take(~count=index, l),
    drop(~count=index, l),
  );

  let splitWhen = (l: list('a), ~f: 'a => bool): (list('a), list('a)) =>
    switch (findIndex(~f=(_, element) => f(element), l)) {
    | Some((index, _)) => splitAt(~index, l)
    | None => (l, [])
    };

  let updateAt = (l: list('a), ~index: int, ~f: 'a => 'a): list('a) =>
    if (index < 0) {
      l;
    } else {
      let (front, back) = splitAt(~index, l);
      switch (back) {
      | [] => l
      | [x, ...rest] => append(front, [f(x), ...rest])
      };
    };

  let length = (l: list('a)): int => List.length(l);

  let removeAt = (l: list('a), ~index: int): list('a) =>
    if (index < 0) {
      l;
    } else {
      let (front, back) = splitAt(~index, l);
      switch (tail(back)) {
      | None => l
      | Some(t) => append(front, t)
      };
    };

  let minimum = Base.List.min_elt;

  let maximum = Base.List.max_elt;

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

  let insertAt = (l: list('a), ~index: int, ~value: 'a): list('a) => {
    let (front, back) = splitAt(~index, l);
    append(front, [value, ...back]);
  };

  let unzip = Base.List.unzip;

  let unzip3 = Base.List.unzip3;

  let sliding = (~step=1, t: t('a), ~size: int): t(t('a)) => {
    let rec takeAllOrEmpty = (t, n, (current, count)) =>
      if (count == n) {
        reverse(current);
      } else {
        switch (t) {
        | [] => []
        | [x, ...xs] => takeAllOrEmpty(xs, n, ([x, ...current], count + 1))
        };
      };

    let rec loop = t =>
      if (isEmpty(t)) {
        [];
      } else {
        let sample = takeAllOrEmpty(t, size, ([], 0));
        if (isEmpty(sample)) {
          [];
        } else {
          [sample, ...loop(Base.List.drop(t, step))];
        };
      };

    loop(t);
  };

  let intersperse = (l: list('a), ~sep): list('a) =>
    switch (l) {
    | [] => []
    | [x] => [x]
    | [x, ...rest] => [
        x,
        ...foldRight(rest, ~initial=[], ~f=(acc, x) => [sep, x, ...acc]),
      ]
    };

  let forEach = (l, ~f) => Base.List.iter(l, ~f);

  let forEachI = Base.List.iteri;

  // let unique = (t, ~compare) => Base.List.dedup_and_sort(t, ~compare);

  let toArray = Base.List.to_array;

  let groupWhile = (t, ~f) => Base.List.group(t, ~break=f);

  let sort = Base.List.sort;

  let join = (t, ~sep) => String.concat(sep, t);
};

module Option = {
  type t('a) = option('a);

  let some = a => Some(a);

  let isSome = Option.is_some;

  let isNone = Option.is_none;

  let and_ = (ta, tb) => isSome(ta) ? tb : None;

  let or_ = (ta, tb) => isSome(ta) ? ta : tb;

  let orElse = (t, ~f) => isSome(t) ? t : f();

  let bind = (t, ~f) =>
    switch (t) {
    | Some(x) => f(x)
    | None => None
    };

  let join = Option.join;

  let both = (a, b) =>
    switch (a, b) {
    | (Some(a), Some(b)) => Some((a, b))
    | _ => None
    };

  let map = (t, ~f) => Option.map(f, t);

  let map2 = (ta: t('a), tb: t('b), ~f: ('a, 'b) => 'c): t('c) => 
    switch(ta, tb) {
    | (Some(a), Some(b)) => Some(f(a, b))
    | _ => None
    };
  
  let get = (t, ~default) =>
    switch (t) {
    | None => default
    | Some(value) => value
    };

  let getOrFailWith = (t, ~exn) =>
    switch (t) {
    | Some(value) => value
    | None => raise(exn)
    };

  

  let getUnsafe = x =>
    switch (x) {
    | None => raise(Invalid_argument("getUnsafe called with None"))
    | Some(x) => x
    };

  let fold = (t, ~initial, ~f) =>
    switch (t) {
    | None => initial
    | Some(value) => f(initial, value)
    };

  let forEach = (t, ~f) => Option.iter(f, t);

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

  let toResult = (t, ~or_) =>
    switch (t) {
    | Some(value) => Ok(value)
    | None => Error(or_)
    };

  module Infix = {
    let (|?) = (t, default) => get(t, ~default);
    let (>>=) = (t, f) => bind(t, ~f);
    let (>>|) = (t, f) => map(t, ~f);
  };
};

module Result = {
  type t('err, 'ok) = Result.t('ok, 'err);

  let ok = Result.ok;

  let error = Result.error;

  let ofOption = (ma, ~error) =>
    switch (ma) {
    | None => Result.Error(error)
    | Some(right) => Result.Ok(right)
    };

  let isOk = Result.is_ok;

  let isError = Result.is_error;

  let both = (a, b) =>
    switch (a, b) {
    | (Ok(a'), Ok(b')) => Ok((a', b'))
    | (Error(a'), _) => Error(a')
    | (_, Error(b')) => Error(b')
    };

  let join = Result.join;

  let or_ = (a, b) =>
    switch (a) {
    | Ok(_) => a
    | _ => b
    };

  let orElse = (t, ~f) => switch(t) {
    | Ok(_) => t
    | Error(error) => f(error)
  };

  let and_ = (a, b) =>
    switch (a) {
    | Ok(_) => b
    | _ => a
    };

  let get = Result.value;

  let getOrFailWith = (t, ~exn) =>
    switch (t) {
    | Error(_) => raise(exn)
    | Ok(value) => value
    };

  let getUnsafe = Result.get_ok;

  let getError = (t, ~default) =>
    switch (t) {
    | Ok(_) => default
    | Error(error) => error
    };



  let map = (t, ~f) => Result.map(f, t);

  let map2 = (a, b, ~f) =>
    switch (a, b) {
    | (Ok(a), Ok(b)) => Ok(f(a, b))
    | (Error(a), _) => Error(a)
    | (_, Error(b)) => Error(b)
    };

  let mapError = (t, ~f) =>
    switch (t) {
    | Error(error) => Error(f(error))
    | Ok(value) => Ok(value)
    };

  let combine = t =>
    List.foldRight(t, ~initial=Ok([]), ~f=map2(~f=(b, a) => [a, ...b]));

  let toOption = r =>
    switch (r) {
    | Ok(v) => Some(v)
    | Error(_) => None
    };

  let bind = (t, ~f) => Result.bind(t, f);

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

  let pp =
      (
        errf: (Format.formatter, 'err) => unit,
        okf: (Format.formatter, 'ok) => unit,
        fmt: Format.formatter,
        r: t('err, 'ok),
      ) =>
    switch (r) {
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
    let (|?) = (t, default) => get(t, ~default);
    let (>>=) = (t, f) => bind(t, ~f);
    let (>>|) = (t, f) => map(t, ~f);
  };
};

module Char = {
  type t = char;

  let toCode = (c: char): int => Base.Char.to_int(c);

  let ofCode = (i: int): option(char) =>
    if (0 <= i && i <= 255) {
      Some(Char.chr(i));
    } else {
      None;
    };

  let toString = Base.Char.to_string;

  let ofString = (str: string): option(char) =>
    switch (String.length(str)) {
    | 1 => Some(str.[0])
    | _ => None
    };

  let toDigit = char =>
    switch (char) {
    | '0'..'9' => Some(toCode(char) - toCode('0'))
    | _ => None
    };

  let toLowercase = Base.Char.lowercase;

  let toUppercase = Base.Char.uppercase;

  let isLowercase = Base.Char.is_lowercase;

  let isUppercase = Base.Char.is_uppercase;

  let isLetter = Base.Char.is_alpha;

  let isDigit = Base.Char.is_digit;

  let isAlphanumeric = Base.Char.is_alphanum;

  let isPrintable = Base.Char.is_print;

  let isWhitespace = Base.Char.is_whitespace;
};

module Float = {
  type t = float;

  type radians = t;

  let ofInt = Base.Float.of_int;

  let ofString = Base.Float.of_string;

  let zero = 0.0;

  let one = 1.0;

  let nan = Base.Float.nan;

  let infinity = Base.Float.infinity;

  let negativeInfinity = Base.Float.neg_infinity;

  let e = Base.Float.euler;

  let pi = Base.Float.pi;

  let epsilon = Base.Float.epsilon_float;

  let maximumSafeInteger = 2. ** 52. -. 1.;

  let minimumSafeInteger = (-2.) ** 52. -. 1.;

  let largestValue = Base.Float.max_finite_value;

  let smallestValue = Base.Float.min_positive_normal_value;

  let add = (+.);

  let (+) = (+.);

  let subtract = (-.);

  let (-) = (-.);

  let multiply = ( *. );

  let ( * ) = ( *. );

  let divide = (n, ~by) => n /. by;

  let (/) = (/.);

  let power = (~base, ~exponent) => base ** exponent;

  let ( ** ) = ( ** );

  let negate = Base.Float.neg;

  let (~-) = negate;

  let absolute = Base.Float.abs;

  let isInteger = t => t == Base.Float.round(t);

  // TODO
  let isSafeInteger = _t => false;

  let clamp = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(
        Invalid_argument(
          "~lower:"
          ++ Base.Float.to_string(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Base.Float.to_string(upper),
        ),
      );
    } else if (Base.Float.is_nan(lower)
               || Base.Float.is_nan(upper)
               || Base.Float.is_nan(n)) {
      Base.Float.nan;
    } else {
      max(lower, min(upper, n));
    };

  let inRange = (n, ~lower, ~upper) =>
    if (Base.Float.(upper < lower)) {
      raise(
        Invalid_argument(
          "~lower:"
          ++ Base.Float.to_string(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Base.Float.to_string(upper),
        ),
      );
    } else {
      n >= lower && n < upper;
    };

  let squareRoot = sqrt;

  let log = (n, ~base) => Base.Float.(log10(n) / log10(base));

  let isNaN = Base.Float.is_nan;

  let isInfinite = Base.Float.is_inf;

  let isFinite = n => !isInfinite(n) && !isNaN(n);

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

  let hypotenuse = (x, y) => squareRoot(x * x + y * y);

  let degrees = n => n * (pi / 180.0);

  let radians = Fun.identity;

  let turns = n => n * 2. * pi;

  let cos = Base.Float.cos;

  let acos = Base.Float.acos;

  let sin = Base.Float.sin;

  let asin = Base.Float.asin;

  let tan = Base.Float.tan;

  let atan = Base.Float.atan;

  let atan2 = (~y, ~x) => Base.Float.atan2(y, x);

  type direction = [
    | `Zero
    | `AwayFromZero
    | `Up
    | `Down
    | `Closest([ | `Zero | `AwayFromZero | `Up | `Down | `ToEven])
  ];

  let round = (~direction=`Closest(`Up), n) =>
    switch (direction) {
    | (`Up | `Down | `Zero) as dir => Base.Float.round(n, ~dir)
    | `AwayFromZero =>
      if (n < 0.) {
        Base.Float.round(n, ~dir=`Down);
      } else {
        Base.Float.round(n, ~dir=`Up);
      }
    | `Closest(`Zero) =>
      if (n > 0.) {
        Base.Float.round(n -. 0.5, ~dir=`Up);
      } else {
        Base.Float.round(n +. 0.5, ~dir=`Down);
      }
    | `Closest(`AwayFromZero) =>
      if (n > 0.) {
        Base.Float.round(n +. 0.5, ~dir=`Down);
      } else {
        Base.Float.round(n -. 0.5, ~dir=`Up);
      }
    | `Closest(`Down) => Base.Float.round(n -. 0.5, ~dir=`Up)
    | `Closest(`Up) => Base.Float.round_nearest(n)
    | `Closest(`ToEven) => Base.Float.round_nearest_half_to_even(n)
    };

  let floor = Base.Float.round_down;

  let ceiling = Base.Float.round_up;

  let truncate = Base.Float.round_towards_zero;

  let ofPolar = ((r, theta)) => (r * cos(theta), r * sin(theta));

  let toPolar = ((x, y)) => (hypotenuse(x, y), atan2(~x, ~y));

  let toInt = Base.Float.iround_towards_zero;

  let toString = Base.Float.to_string;
};

module Int = {
  type t = int;

  type identity = Base.Int.comparator_witness;

  let ofString = int_of_string_opt;

  let minimumValue = Base.Int.min_value;

  let maximumValue = Base.Int.max_value;

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

  let (/\/) = Base.Int.(/\/);

  let power = (~base, ~exponent) => Base.Int.(base ** exponent);

  let ( ** ) = Base.Int.( ** );

  let negate = (~-);

  let (~-) = (~-);

  let modulo = (n, ~by) => n mod by;

  let remainder = (n, ~by) => Base.Int.rem(n, by);

  let maximum = Base.Int.max;

  let minimum = Base.Int.min;

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
      raise(
        Invalid_argument(
          "~lower:"
          ++ Base.Int.to_string(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Base.Int.to_string(upper),
        ),
      );
    } else {
      max(lower, min(upper, n));
    };

  let inRange = (n, ~lower, ~upper) =>
    if (upper < lower) {
      raise(
        Invalid_argument(
          "~lower:"
          ++ Base.Int.to_string(lower)
          ++ " must be less than or equal to ~upper:"
          ++ Base.Int.to_string(upper),
        ),
      );
    } else {
      n >= lower && n < upper;
    };

  let toFloat = Base.Int.to_float;

  let toString = Base.Int.to_string;
};

module Integer = {
  type t = Z.t;

  let ofInt = Z.of_int;

  let ofInt64 = Z.of_int64;

  let ofFloat = Z.of_float;

  let ofString = string => {
    switch (Z.of_string(string)) {
    | value => Some(value)
    | exception _ => None
    };
  };

  let compare = Z.compare;

  let equal = (==);

  let zero = Z.zero;

  let one = Z.one;

  let isEven = t => Z.(t mod ~$2 == zero);

  let isOdd = t => Z.(t mod ~$2 != zero);

  let add = Z.add;

  let (+) = Z.(+);

  let subtract = Z.sub;

  let (-) = subtract;

  let multiply = Z.mul;

  let ( * ) = multiply;

  let divide = Z.div;

  let (/) = divide;

  let divide = (n, ~by) => divide(n, by);

  let negate = Z.neg;

  let modulo = (n: t, ~by: t): t => Z.rem(n, by);

  let remainder = (n: t, ~by: t): t => Z.rem(n, by);

  let ( ** ) = Z.( ** );

  let power = (~base, ~exponent, ~modulo) => Z.powm(base, exponent, modulo);

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

  let toInt = t =>
    if (t > ofInt(Int.maximumValue)) {
      None;
    } else {
      Some(Z.to_int(t));
    };

  let toInt64 = t =>
    if (t > ofInt64(Int64.max_int)) {
      None;
    } else {
      Some(Z.to_int64(t));
    };

  // let maxFloat = [@raw "2n ** (64n - 1n) - 1n"];
  // let toFloat = (t) =>
  // if (t > maxFloat) {
  // None;
  // } else {
  // Some(asIntN(53, t));
  // };
  let toFloat = t =>
    if (t > ofFloat(Base.Float.max_finite_value)
        || t < ofFloat(Base.Float.(- max_finite_value))) {
      None;
    } else {
      Some(Z.to_float(t));
    };

  [@bs.send] external toString: t => string = "toString";
};

module String = {
  type t = string;

  type identity = Base.String.comparator_witness;

  let initialize = (length, ~f) =>
    Base.List.init(length, ~f) |> Base.String.of_char_list;

  let repeat = (t, ~count) =>
    Base.List.init(count, ~f=_ => t) |> Base.String.concat;

  let ofArray = characters =>
    Base.(Array.to_list(characters) |> String.of_char_list);

  let ofList = Base.String.of_char_list;

  let length = String.length;

  let isEmpty = t => length(t) == 0;

  let uncons = (s: string): option((char, string)) =>
    switch (s) {
    | "" => None
    | s => Some((s.[0], String.sub(s, 1, String.length(s) - 1)))
    };

  let dropLeft = (~count: int, s: string): string =>
    Base.String.drop_prefix(s, count);

  let dropRight = (~count: int, s: string): string =>
    Base.String.drop_suffix(s, count);

  let split = (t, ~on: string): list(string) => {
    Str.split(Str.regexp_string(on), t);
  };

  let words = split(~on=" ");

  let lines = split(~on="\n");

  let startsWith = (t, ~prefix) => Base.String.is_prefix(~prefix, t);

  let endsWith = (t, ~suffix) => Base.String.is_suffix(~suffix, t);

  let toLowercase = (s: string): string => String.lowercase_ascii(s);

  let toUppercase = (s: string): string => String.uppercase_ascii(s);

  let uncapitalize = (s: string): string => String.uncapitalize_ascii(s);

  let capitalize = (s: string): string => String.capitalize_ascii(s);

  let isCapitalized = (s: string): bool => s == String.capitalize_ascii(s);

  let includes = (t, ~substring): bool =>
    Base.String.is_substring(t, ~substring);

  let reverse = Base.String.rev;

  let ofChar = Base.String.of_char;

  let slice = (~to_=0, str, ~from) => String.sub(str, from, to_ - from);

  let trim = String.trim;

  // TODO bad implementation
  let insertAt = (t, ~index: int, ~value: string): string => {
    let length = length(t);
    let startCount = index;
    let endCount = length - index;
    let start = dropRight(~count=endCount, t);
    let end_ = dropLeft(~count=startCount, t);
    String.concat("", [start, value, end_]);
  };

  let toArray = string => Base.String.to_list(string) |> Array.of_list;

  let toList = Base.String.to_list;
};

module Set = {
  type t('a, 'cmp) = Base.Set.t('a, 'cmp);

  let length = Base.Set.length;

  let isEmpty = Base.Set.is_empty;

  let includes = Base.Set.mem;

  let add = Base.Set.add;

  let remove = Base.Set.remove;

  let difference = Base.Set.diff;

  let intersection = Base.Set.inter;

  let union = Base.Set.union;

  let filter = Base.Set.filter;

  let partition = Base.Set.partition_tf;

  let find = Base.Set.find;

  let all = Base.Set.for_all;

  let any = Base.Set.exists;

  let forEach = Base.Set.iter;

  let fold = (s, ~initial, ~f) => Base.Set.fold(s, ~init=initial, ~f);

  let toArray = Base.Set.to_array;

  let toList = Base.Set.to_list;

  module Poly = {
    type identity = Base.Comparator.Poly.comparator_witness;

    type nonrec t('a) = t('a, identity);

    let empty = () => Base.Set.Poly.empty;

    let singleton = Base.Set.Poly.singleton;

    let ofArray = Base.Set.Poly.of_array;

    let ofList = Base.Set.Poly.of_list;
  };

  module Int = {
    type nonrec t = t(Base.Int.t, Base.Int.comparator_witness);

    let empty = Base.Set.empty((module Base.Int));

    let singleton = Base.Set.singleton((module Base.Int));

    let ofArray = Base.Set.of_array((module Base.Int));

    let ofList = Base.Set.of_list((module Base.Int));
  };

  module String = {
    type nonrec t = t(String.t, Base.String.comparator_witness);

    let empty = Base.Set.empty((module Base.String));

    let singleton = Base.Set.singleton((module Base.String));

    let ofArray = Base.Set.of_array((module Base.String));

    let ofList = Base.Set.of_list((module Base.String));
  };
};

module Map = {
  type t('key, 'value, 'cmp) = Base.Map.t('key, 'value, 'cmp);

  let convertComparator =
      (
        type k,
        type id,
        module Cmp: Comparator with type t = k and type identity = id,
      )
      : Base.Map.comparator(Cmp.t, Cmp.identity) =>
    (module
     {
       type t = Cmp.t;
       type comparator_witness = Cmp.identity;
       let comparator = Obj.magic(Cmp.compare);
     });

  let empty = comp => Base.Map.empty(convertComparator(comp));

  let isEmpty = Base.Map.is_empty;

  let includes = Base.Map.mem;

  let length = Base.Map.length;

  let minimum = t => Base.Map.min_elt(t) |> Option.map(~f=Tuple.first);

  let maximum = t => Base.Map.max_elt(t) |> Option.map(~f=Tuple.first);

  let extent = t => Option.both(minimum(t), maximum(t));

  let add = (m, ~key, ~value) => Base.Map.set(m, ~key, ~data=value);

  let remove = Base.Map.remove;

  let get = Base.Map.find;

  let update = (m, ~key, ~f) => Base.Map.change(m, key, ~f);

  let merge = (m1, m2, ~f) =>
    Base.Map.merge(m1, m2, ~f=(~key, desc) =>
      switch (desc) {
      | `Left(v1) => f(key, Some(v1), None)
      | `Right(v2) => f(key, None, Some(v2))
      | `Both(v1, v2) => f(key, Some(v1), Some(v2))
      }
    );

  let map = Base.Map.map;

  let mapI = (t, ~f) => Base.Map.mapi(t, ~f=(~key, ~data) => f(key, data));

  let filter = Base.Map.filter;

  let partition = (m, ~f) =>
    Base.Map.partition_mapi(m, ~f=(~key, ~data) =>
      if (f(~key, ~value=data)) {
        `Fst(data);
      } else {
        `Snd(data);
      }
    );

  let find = (m, ~f) =>
    Base.Map.fold(m, ~init=None, ~f=(~key, ~data, matching) =>
      switch (matching) {
      | Some(_) => matching
      | None =>
        if (f(~key, ~value=data)) {
          Some((key, data));
        } else {
          None;
        }
      }
    );

  let any = Base.Map.exists;

  let all = Base.Map.for_all;

  let forEach = Base.Map.iter;

  let fold = (m, ~initial, ~f) =>
    Base.Map.fold(m, ~init=initial, ~f=(~key, ~data, acc) =>
      f(acc, ~key, ~value=data)
    );

  let keys = Base.Map.keys;

  let values = Base.Map.data;

  let toArray = m => Base.Map.to_alist(m) |> Base.List.to_array;

  let toList = m => Base.Map.to_alist(m);

  module Poly = {
    type identity = Base.Comparator.Poly.comparator_witness;

    type nonrec t('k, 'v) = t('k, 'v, identity);

    let empty = () => Base.Map.Poly.empty;

    let singleton = (~key, ~value) => Base.Map.Poly.singleton(key, value);

    let ofList = l =>
      Base.Map.Poly.of_alist_reduce(l, ~f=(_, curr) => curr);

    let ofArray = a => Base.Array.to_list(a) |> ofList;
  };

  module Int = {
    type nonrec t('v) = t(Int.t, 'v, Int.identity);

    let empty = Base.Map.empty((module Base.Int));

    let singleton = (~key, ~value) =>
      Base.Map.singleton((module Base.Int), key, value);

    let ofList = l =>
      Base.Map.of_alist_reduce((module Base.Int), l, ~f=(_, curr) => curr);

    let ofArray = a => Base.Array.to_list(a) |> ofList;
  };

  module String = {
    type nonrec t('v) = t(String.t, 'v, Base.String.comparator_witness);

    let empty = Base.Map.empty((module Base.String));

    let singleton = (~key, ~value) =>
      Base.Map.singleton((module Base.String), key, value);

    let ofList = l =>
      Base.Map.of_alist_reduce((module Base.String), l, ~f=(_, curr) => curr);

    let ofArray = a => Base.Array.to_list(a) |> ofList;
  };
};

module Array = {
  type t('a) = array('a);

  let empty = [||];

  let singleton = (a: 'a): array('a) => [|a|];

  let clone = Base.Array.copy;

  let initialize = (length: int, ~f: int => 'a) =>
    if (length <= 0) {
      empty;
    } else {
      Base.Array.init(length, ~f);
    };

  let repeat = (element, ~length) =>
    initialize(length, ~f=Fun.constant(element));

  let range = (~from=0, to_: int): array(int) =>
    Base.Array.init(max(0, to_ - from), ~f=i => i + from);

  let ofList = Base.List.to_array;

  let length = (a: array('a)): int => Base.Array.length(a);

  let isEmpty = (a: array('a)): bool => length(a) == 0;

  let first = t =>
    if (length(t) < 1) {
      None;
    } else {
      Some(t[0]);
    };

  let last = t =>
    if (length(t) < 1) {
      None;
    } else {
      Some(t[length(t) - 1]);
    };

  let get = Base.Array.get;

  let getAt = (a, ~index) =>
    if (index >= 0 && index < length(a)) {
      Some(Base.Array.get(a, index));
    } else {
      None;
    };

  let set = Base.Array.set;

  let setAt = (t, ~index, ~value) => set(t, index, value);

  let filter = Base.Array.filter;

  let sum = (type a, t, module M: Container.Sum with type t = a) =>
    Base.Array.fold(t, ~init=M.zero, ~f=M.add);

  let filterMap = Base.Array.filter_map;

  let bind = Base.Array.concat_map;

  let fold = (a, ~initial, ~f) => Base.Array.fold(a, ~init=initial, ~f);

  let foldRight = (a, ~initial, ~f) =>
    Base.Array.fold_right(a, ~init=initial, ~f=Fun.flip(f));

  let count = (t, ~f) =>
    fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0));

  let groupBy = (t, comparator, ~f) =>
    fold(
      t,
      ~initial=Map.empty(comparator),
      ~f=(map, element) => {
        let key = f(element);
        Base.Map.update(
          map,
          key,
          ~f=
            fun
            | None => [element]
            | Some(elements) => [element, ...elements],
        );
      },
    );

  let swap = Base.Array.swap;

  let find = Base.Array.find;

  let findIndex = Base.Array.findi;

  let map = Base.Array.map;

  let mapI = Base.Array.mapi;

  let map2 = (a: array('a), b: array('b), ~f: ('a, 'b) => 'c): array('c) => {
    let minLength = min(length(a), length(b));
    Base.Array.init(minLength, ~f=i => f(a[i], b[i]));
  };

  let zip = map2(~f=Tuple.make);

  let map3 =
      (
        arrayA: array('a),
        arrayB: array('b),
        arrayC: array('c),
        ~f: ('a, 'b, 'c) => 'd,
      ) => {
    let minLength =
      Base.min(length(arrayA), Base.min(length(arrayC), length(arrayB)));

    Base.Array.init(minLength, ~f=i => f(arrayA[i], arrayB[i], arrayC[i]));
  };

  let partition = Base.Array.partition_tf;

  let splitAt = (a, ~index) => (
    Base.Array.init(index, ~f=i => a[i]),
    Base.Array.init(length(a) - 1, ~f=i => a[index + i]),
  );

  let splitWhen = (a, ~f) =>
    switch (findIndex(a, ~f=(_index, element) => f(element))) {
    | None => (a, [||])
    | Some((index, _)) => splitAt(a, ~index)
    };

  let unzip = Base.Array.unzip;

  let unzip3 = t => (
    map(t, ~f=Tuple3.first),
    map(t, ~f=Tuple3.second),
    map(t, ~f=Tuple3.third),
  );

  let append = (a: array('a), a': array('a)): array('a) =>
    Base.Array.append(a, a');

  let concatenate = (al: array(array('a))): array('a) =>
    Base.Array.concat(Base.Array.to_list(al));

  let intersperse = (array, ~sep) =>
    Base.Array.init(max(0, Array.length(array) * 2 - 1), ~f=i =>
      if (i mod 2 != 0) {
        sep;
      } else {
        array[i / 2];
      }
    );

  let any = Base.Array.exists;

  let all = Base.Array.for_all;

  let includes = Base.Array.mem;

  let values = t =>
    fold(t, ~initial=[], ~f=(results, element) =>
      switch (element) {
      | None => results
      | Some(value) => [value, ...results]
      }
    )
    |> ofList;

  let join = (t, ~sep) => Caml.String.concat(sep, Array.to_list(t));

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
      Base.Array.init(sliceTo - sliceFrom, ~f=i => array[i + sliceFrom]);
    };
  };

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

  let chunksOf = (t, ~size) => sliding(t, ~step=size, ~size);

  // let group = (t, ~break) =>
  //   Base.Array.to_list(t)
  //   |> Base.List.group(~break)
  //   |> Base.List.map(~f=Base.List.to_array)
  //   |> Base.List.to_array;

  let maximum = Base.Array.max_elt;

  let minimum = Base.Array.min_elt;

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

  let sort = t => Base.Array.sort(t);

  // let unique = (a, ~compare) => Base.Array.sorted_copy(a, ~compare); /* TODO */

  let reverse = Base.Array.rev_inplace;

  let shuffle = t => {
    for (i in length(t) downto 2) {
      swap(t, i - 1, Base.Random.State.int(Base.Random.State.default, i));
    };
  };

  let forEach = (a, ~f) => Base.Array.iter(a, ~f);

  let forEachI = (a, ~f) => Base.Array.iteri(a, ~f);

  let toList = (a: array('a)): list('a) => Base.Array.to_list(a);

  let toIndexedList = a =>
    Base.Array.fold_right(a, ~init=(length(a) - 1, []), ~f=(x, (i, acc)) =>
      (i - 1, [(i, x), ...acc])
    )
    |> Base.snd;
};
