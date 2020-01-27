/** Functions for working with functions. */
module Fun: {
  /** Functions for working with functions. */;

  /** Given a value, returns exactly the same value. This may seem pointless at first glance but it can often be useful when an api offers you more control than you actually need.

    Perhaps you want to create an array of integers

    {[Array.initialize 6 ~f:Fun.identity = [|0;1;2;3;4;5|]]}

    (In this particular case you probably want to use {!Array.range}.)

    Or maybe you need to register a callback, but dont want to do anything:

    {[
      let httpMiddleware = HttpLibrary.createMiddleWare(
        ~onEventYouDoCareAbout=transformAndReturn,
        ~onEventYouDontCareAbout=Fun.identity,
      }
    ]}
  */
  external identity: 'a => 'a = "%identity";

  /** Discards the value it is given and returns [()]

    This is primarily useful when working with imperative side-effecting code or to avoid [unused value](TODO) compiler warnings when you really meant it, and haven't just made a mistake.

    {[
      module PretendMutableQueue : sig
        type 'a t

        val pushReturningIndex : 'a t -> 'a -> int
      end

      let addListToQueue queue list =
        List.forEach list ~f:(fun element ->
          ignore (PretentMutableQueue.pushReturningIndex queue element)
        )
    ]}
  */
  external ignore: _ => unit = "%ignore";

  /** Create a function that {b always} returns the same value.

    Useful with functions like {!List.map}:

    {[List.map ~f:(Fun.constant 0) [1;2;3;4;5] = [0;0;0;0;0]]}

    or {!Array.initialize}

    {[Array.initialize 6 ~f:(Fun.constant 0) = [|0;0;0;0;0;0|]]}
  */
  let constant: ('a, 'b) => 'a;

  /** A function which always returns its second argument. */
  let sequence: ('a, 'b) => 'b;

  /** [flip f] reverses the argument order of the binary function [f].
    For any arguments [x] and [y], [(flip f) x y] is [f y x].

    Perhaps you want to [fold] something, but the arguments of a function you already have access to are in the wrong order.
  */
  let flip: (('a, 'b) => 'c, 'b, 'a) => 'c;

  /** See {!Fun.(<|)} */
  let apply: ('a => 'b, 'a) => 'b;

  /** [f <| x] is exactly the same as [f x].
    It can help you avoid parentheses, which can be nice sometimes.
    Maybe you want to apply a function to a [match] expression? That sort of thing.
  */
  let (<|): ('a => 'b, 'a) => 'b;

  /** See {!Fun.(|>)} */ external pipe: ('a, 'a => 'b) => 'b = "%revapply";

  /** Saying [x |> f] is exactly the same as [f x], just a bit longer.

    It is called the “pipe” operator because it lets you write “pipelined” code.
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
   */
  external (|>): ('a, 'a => 'b) => 'b = "%revapply";

  /** Function composition, passing results along in the suggested direction.
    For example, the following code (in a very roundabout way) checks if a number divided by two is odd:

    {[let isHalfOdd = Fun.(not << Int.isEven << Int.divide ~by:2)]}

    You can think of this operator as equivalent to the following:

    {[(g << f)  ==  (fun x -> g (f x))]}

    So our example expands out to something like this:

    {[let isHalfOdd = fun n -> not (Int.isEven (Int.divide ~by:2 n))]}
  */
  let compose: ('b => 'c, 'a => 'b, 'a) => 'c;

  /** See {!Fun.compose} */
  let (<<): ('b => 'c, 'a => 'b, 'a) => 'c;

  /** Function composition, passing results along in the suggested direction.
    For example, the following code checks if the square root of a number is odd:

    {[Int.squareRoot >> Int.isEven >> not]}
  */
  let composeRight: ('a => 'b, 'b => 'c, 'a) => 'c;

  /** See {!Fun.composeRight} */
  let (>>): ('a => 'b, 'b => 'c, 'a) => 'c;

  /**
    Useful for performing some side affect in {!Fun.pipe}-lined code.

    Most commonly used to log a value in the middle of a pipeline of function calls.

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
  */
  let tap: ('a, ~f: 'a => unit) => 'a;

  /**  Useful in combination with functions like `filter` */
  let negate: ('a => bool, 'a) => bool;

  /** Runs the provided function, forever. */
  let forever: (unit => unit) => unit;

  /** Runs [f] n times.  */
  let times: (int, ~f: unit => unit) => unit;
};

module Container: {
  /** 
    This module contains module signatures which are used in functions which accept first class modules.

    TODO this could do with some links explaining those concepts

    TODO Maybe just remove the 'Contianer' wrapper
  */
  
  module type Sum = {
    /** 
      Modules which conform (TODO correct terminology?) to this signature can be used with functions like 
      {!Array.sum}, {!List.sum} or {!Set.sum}
    */
    type t;
    let zero: t;
    let add: (t, t) => t;
  };
};

module type Comparable = {
  /** TODO */
  // file:///Users/deanmerchant/.opam/4.07.1/var/cache/odig/html/base/Base/Comparable/index.html
  type t;
  let compare: (t, t) => int;
};

module type Comparator = {
  /** TODO */
  type t;
  type identity;
  let compare: (t, t) => int;
};

/** TODO */
type comparator('k, 'id) = (module Comparator with
                               type identity = 'id and type t = 'k);

/** Functions for working with boolean ([true] or [false]) values. */
module Bool: {
  /** Functions for working with boolean values.

    Booleans in Ocaml / Reason are represented by the [true] and [false] literals.
  */

  type t = bool;

  /** {1 Creation} */

  /** Convert an {!Int} into a {!Bool}.

      {e Examples}
      
      {[Bool.ofInt 0 = Some false]}

      {[Bool.ofInt 1 = Some true]}

      {[Bool.ofInt 8 = None]}

      {[Bool.ofInt -3 = None]}
  */
  let ofInt: int => option(t);

  /** Convert a {!String} into a {!Bool}.
    
      {e Examples}
      
      {[Bool.ofString "true" = Some true]}

      {[Bool.ofString "false" = Some false]}

      {[Bool.ofString "True" = None]}

      {[Bool.ofString "False" = None]}

      {[Bool.ofString "0" = None]}

      {[Bool.ofString "1" = None]}

      {[Bool.ofString "Not even close" = None]}
  */
  let ofString: string => option(t);

  /** The lazy logical AND operator.

      Returns [true] if both of its operands evaluate to [true].

      If the 'left' operand evaluates to [false], the 'right' operand is not evaluated.

      Right-associative operator at precedence level 3/11.

      TODO explain precendence
      TODO explain Right-associative

      {e Examples}

      {[Bool.(true && true) = true]}

      {[Bool.(true && false) = false]}

      {[Bool.(false && true) = false]}

      {[Bool.(false && false) = false]}
  */
  external (&&): (bool, bool) => bool = "%sequand";

  /** The lazy logical OR operator.

      Returns [true] if one of its operands evaluates to [true].

      If the 'left' operand evaluates to [true], the 'right' operand is not evaluated.

      Right-associative operator at precedence level 2/11.
      
      {e Examples}
      
      {[Bool.(true || true) = true]}

      {[Bool.(true || false) = true]}

      {[Bool.(false || true) = true]}

      {[Bool.(false || false) = false]}
  */
  external (||): (bool, bool) => bool = "%sequor";

  /** The exclusive or operator.

      Returns [true] if exactly one of its operands is [true].

      {e Examples}

      {[Bool.xor true true  = false]}
      
      {[Bool.xor true false = true]}
      
      {[Bool.xor false true  = true]}
      
      {[Bool.xor false false = false]}
  */
  let xor: (t, t) => t;

  /** Negate a [bool].
    
    {e Examples}

    {[Bool.not false = true]}

    {[Bool.not true = false]}
  */
  let (!): t => t;

  /** Negate a function.

      This can be useful in combination with {!List.filter} / {!Array.filter} or {!List.find} / {!Array.find}

      {e Examples}

      {[
        let isOdd = Bool.negate Int.isEven in
        isOdd 7 = true
      ]}

      {[
        let isLessThanTwelve = Bool.negate (fun n -> n >= 12) in
        isLessThanTwelve 12 = false
      ]}
  */
  let negate: ('a => bool, 'a) => bool;

  /** Test for the equality of two [bool] values.
    
      {e Examples}

      {[Bool.equal true true = true]}

      {[Bool.equal false false = true]}

      {[Bool.equal false true = false]}
  */
  let equal: (t, t) => t;

  /** Compare two boolean values 
   
      {e Examples}

      {[Bool.compare true false = 1]} 

      {[Bool.compare false true = -1]} 

      {[Bool.compare true true = 0]} 

      {[Bool.compare false false = 0]} 
  */
  let compare: (t, t) => int;

  /** {1 Conversion} */

  /** Convert a [bool] to a {!String}
      
      {e Examples}
      
      {[Bool.toString true = "true"]}

      {[Bool.toString false = "false"]}
  */
  let toString: t => string;

  /** Convert a [bool] to an {!Int}.
    
      {e Examples}
      
      {[Bool.toInt true = 1]}

      {[Bool.toInt false = 0]}
  */
  let toInt: t => int;
};

/** Functions for working with computations which may fail. */
module Result: {
  /**
    TODO
    A [Result] is a variant (TODO link to variants) type, which has a constructor for
    successful results ([Ok('ok)]), and one for unsuccessful results ([Error('error)]).

    {[
        type t('error, 'ok) = 
          | Ok('ok)
          | Error('error);
      ]}

    Here is how you would annotate a [Result] variable whose [Ok]
    variant is an integer and whose [Error] variant is a string:

    {[
      let x: Result.t(string, int) = Ok(3);
      let y: Result.t(string, int) = Error("bad")
    ]}

    A {!Result} is used to represent a computation which may fail.

    // TODO Examples
    // TODO When should you use an option
    // TODO When should you use an exception

  */

  type t('error, 'ok) = Result.t('ok, 'error);

  /** {1 Creation} */

  /** A function alternative to the [Ok] constructor which can be used in places where 
      the constructor isn't permitted such as at the of a {!(|>)} or functions like {!List.map}.

      {e Examples}

      {[String.reverse "desserts" |> Result.ok = Ok "stressed"]}

      {[List.map [1; 2; 3] ~f:Result.ok = [Ok 1; Ok 2; Ok 3]]}
  */
  let ok: 'ok => t('error, 'ok);

  /** A function alternative to the [Error] constructor which can be used in places where 
      the constructor isn't permitted such as at the of a {!Fun.pipe} or functions like {!List.map}.
    
      {b Note}

      When using the reson syntax you {b can} use constructors with the fast pipe.

      TODO link

      And you can use the placeholder syntax for use with functions like {!List.map}
      TODO can you?

      {[
        List.map([1,2,3], ~f:Ok(_)) == [Ok(1),Ok(2),Ok(3)]
      ]}

      {e Examples}

      {[Int.negate 3 |> Result.error 3 = Error (-3)]}

      {[List.map [1; 2; 3] ~f:Result.error = [Error 1; Error 2; Error 3]]}
  */
  let error: 'error => t('error, 'ok);

  /** Run the provided function and wrap the returned value in a {!Result}, catching any exceptions raised. 
      
      {e Examples}
      
      // TODO what exception *does* this throw?
      {[Result.attempt(() => 5 / 0) = Error(Division_by_zero)]}

      // TODO what exception *does* this throw?
      {[
        let numbers = [|1,2,3|];
        Result.attempt(() => numbers[3]) = Error(Invalid_argument("Out of bounds"))
      ]}
  */
  let attempt: (unit => 'ok) => t(exn, 'ok);

  /** Convert an {!Option} to a {!Result} where a [Some(value)] becomes [Ok(value)] and a [None] becomes [Error(error)]. 
    
      {e Examples}

      {[Result.ofOption(Some(84), ~error="Greater than 100") == Ok(8)]}

      {[Result.ofOption(None, ~error="Greater than 100") == Error("Greater than 100")]}
  */
  let ofOption: (option('ok), ~error: 'error) => t('error, 'ok);

  /** Check if a {!Result} is an [Ok]. 
      
      Useful when you want to perform some side affect based on the presence of an [Ok].
      TODO define side affect 
      
      {b Note} if you need access to the contained value its often better just to use pattern matching directly.

      {e Examples}

      {[Result.isOk(Ok(3)) == true]}

      {[Result.isOk(Error(3)) == false]}
  */
  let isOk: t(_, _) => bool;

  /** Check if a {!Result} is an [Error]. 
      
      Useful when you want to perform some side affect based on the presence of an [Error].
      TODO define side affect 
      
      {b Note} if you need access to the contained value its often better just to use pattern matching directly.

      {e Examples}

      {[Result.isOk(Ok(3)) == true]}

      {[Result.isOk(Error(3)) == false]}
  */
  let isError: t(_, _) => bool;

  /** Returns [Error(error)] if the first argument is [Error(error)], otherwise return the second argument.

      Unlike the built in [&&] operator, the [and_] function does not short-circuit. 
      When you call [and_], both arguments are evaluated before being passed to the function.

      {e Examples}

      {[Result.and_ (Ok 11) (Ok 22) = Ok 22]}

      {[Result.and_ (Error "TODO think of some good text") (Ok 22) = (Error "TODO think of some good text")]}

      {[Result.and_ (Ok 11) (Error "TODO think of some good text") = (Error "TODO think of some good text")]}

      {[Result.and_ (Error "TODO 1") (Error "TODO 2") = (Error "TODO 1")]}
  */
  let and_: (t('error, 'ok), t('error, 'ok)) => t('error, 'ok);

  /** Return the first argument if it {!isOk}, otherwise return the second.

    Unlike the built in [||] operator, the [or_] function does not short-circuit. 
    When you call [or_], both arguments are evaluated before being passed to the function.

    {e Examples}

    {[Result.or_ (Ok 11) (Ok 22) = Ok 11]}

    {[Result.or_ Error("TODO think of some good text") (Ok 22) = Ok 22]}

    {[Result.or_ (Ok 11) Error("TODO think of some good text") = Ok 11]}

    {[Result.or_ Error("TODO 1") Error("TODO 2") = Error("TODO 2")]}
  */
  let or_: (t('error, 'ok), t('error, 'ok)) => t('error, 'ok);

  let orElse: (t('error, 'ok), ~f:('error) => t('error, 'ok)) => t('error, 'ok);

  
  let both: (t('error, 'a), t('error, 'b)) => t('error, ('a, 'b));

  /** Remove a layer of nesting from a result  
   
      {e Examples}

      {[Result.join (Ok (Ok 2)) = Ok 2]}

      {[Result.join (Ok (Error "TODO 2")) = Error "TODO 2"]}

      {[Result.join (Error "TODO 1") = Error "TODO 1"]}
  */
  let join: t('error, t('error, 'a)) => t('error, 'a);

  /** Unwrap a Result using the [~default] value in case of an [Error]
      
      {e Exmples}
    
      {[Result.get ~default:0 (Ok 12) = 12]}

      {[Result.get ~default:0 (Error "bad") = 0]}
  */
  let get: (t('error, 'ok), ~default: 'ok) => 'ok;

  let getOrFailWith: (t(_, 'a), ~exn: exn) => 'a;

  let getUnsafe: t(_, 'a) => 'a;

  let getError: (t('error, 'ok), ~default: 'error) => 'error;

  /**
    [Result.map2 ~f:fcn resultA result_b] applies
    [fcn], a function taking two non-[Result] parameters and returning a
    non-[Result] result to two [Result] arguments [resultA] and [result_b] as follows:

    If [resultA] and [result_b] are of the form [Ok a] and [OK b],
    the return value is [Ok (f a b)].

    If only one of [resultA] and [result_b] is of the form [Error err],
    that becomes the return result.  If both are [Error] values,
    [Result.map2] returns [resultA].

    {[
      let sum_diff x y = (x + y) * (x - y)
      Result.map2 ~f:sum_diff (Ok 7) (Ok 3) = Ok 40
      Result.map2 ~f:sum_diff (Error "err A") (Ok 3) = Error "err A"
      Result.map2 ~f:sum_diff (Ok 7) (Error "err B") = Error "err B"
      Result.map2 ~f:sum_diff (Error "err A") (Error "err B") = Error ("err A")
    ]}
  */
  let map2:
    (t('error, 'a), t('error, 'b), ~f: ('a, 'b) => 'c) => t('error, 'c);

  /**
    [Result.combine results] takes a list of [Result] values. If all
    the elements in [results] are of the form [Ok x], then [Result.combine]
    makes a list [xs] of all the values extracted from their [Ok]s, and returns
    [Ok xs]

    If any of the elements in [results] are of the form [Error err],
    the first of them is returned as the result of [Result.combine].

    {[Result.combine [Ok 1; Ok 2; Ok 3; Ok 4] = Ok [1; 2; 3; 4]]}

    {[Result.combine [Ok 1; Error "two"; Ok 3; Error "four"] = Error "two"]}
  */
  let combine: list(t('error, 'ok)) => t('error, list('ok));

  /** TODO

    {[Result.map (fun x -> x * x) (Ok 3) = Ok 9]}

    {[Result.map (fun x -> x * x) (Error "bad") = Error "bad"]}
  */
  let map: (t('error, 'a), ~f: 'a => 'b) => t('error, 'b);

  let mapError: (t('a, 'ok), ~f: 'a => 'b) => t('b, 'ok);


  /**  */
  let transpose: t('error, option('ok)) => option(t('error, 'ok));

  /**
    {[
    let recip (x:float) : (string, float) Standard.Result.t = (
      if (x = 0.0) then
        Error "Divide by zero"
      else
        Ok (1.0 /. x)
    )

    Result.bind ~f:recip (Ok 4.0) = Ok 0.25
    Result.bind ~f:recip (Error "bad") = Error "bad"
    Result.bind ~f:recip (Ok 0.0) = Error "Divide by zero"
    ]}

    {[
      let root (x:float) : (string, float) Standard.Result.t = (
        if (x < 0.0) then
          Error "Cannot be negative"
        else
          Ok (Float.squareRoot x)
      )

      root 4.0 |> Result.bind ~f:recip = Ok 0.5
      root (-2.0) |> Result.bind ~f:recip = Error "Cannot be negative"
      root(0.0) |> Result.bind ~f:recip = Error "Divide by zero"
    ]}
  */
  let bind: (t('error, 'a), ~f: 'a => t('error, 'b)) => t('error, 'b);

  let fold: (t(_, 'ok), ~initial: 'b, ~f: ('b, 'ok) => 'b) => 'b;

  let forEach: (t(_, 'ok), ~f: 'ok => unit) => unit;

  /** {1 Conversion} */

  /**
    [Result.to_option r] converts a [Result] value [r] to an [Option] value as follows:
    a value of [Ok x] becomes [Some x]; a value of [Error err] becomes [None].

    {[Result.to_option (Ok 42) = Some 42]}

    {[Result.to_option (Error "bad") = None]}
  */
  let toOption: t(_, 'ok) => option('ok);

  /**
    [Result.pp(errorFormater, okFormatter, formatter, result)] “pretty-prints”
    the [result], using [errFormat] if the [result] is an [Error] value or
    [okFormat] if the [result] is an [Ok] value. [destFormat] is a formatter
    that tells where to send the output.

    The following example will print [<ok: 42><error: bad>].


    {[
      let good: (string, int) Standard.Result.t = Ok 42
      let not_good: (string, int) Standard.Result.t = Error "bad"
      Result.pp Format.pp_print_string Format.pp_print_int Format.std_formatter good
      Result.pp Format.pp_print_string Format.pp_print_int Format.std_formatter not_good
      Format.pp_print_newline Format.std_formatter ();
    ]}
  */
  let pp:
    (
      (Format.formatter, 'error) => unit,
      (Format.formatter, 'ok) => unit,
      Format.formatter,
      t('error, 'ok)
    ) =>
    unit;

  
  /** In functions that make heavy use of {!Result}s 
      {[open Result.Infix;]}

  */
  module Infix: {
    /** Module doc S */

    /** An operator version of {!get}
      
        {3 Examples}
    */
    let (|?): (t('err, 'a), 'a) => 'a;

    /** An operator version of {!bind} */
    let (>>=): (t('err, 'ok), 'ok => t('err, 'ok)) => t('err, 'ok);

    /** An operator version of {!map} */
    let (>>|): (t('err, 'a), 'a => 'b) => t('err, 'b);
  };
};

/** Functions for working with optional values. */
module Option: {
  /** {!Option} represents a value which may not be present.
      
      {!Option} values are very common and they are used in a number of ways:
      - Initial values
      - Optional function arguments
      - Optional record fields
      - Return values for functions that are not defined over their entire input range (partial functions).
      - Return value for otherwise reporting simple errors, where None is returned on error.

      If the function you are writing can fail in a variety of ways, use a {!Result} instead to
      better communicate with the caller.

      If a function only fails in unexpected, unpreventable ways, maybe you want raise exception.

      TODO

      But how to get the value {b out} of the option
      - use {!map} or {!bind} to work with the wrapped value instead.
      - {!get}, or its operator {!Infix.(|?)}
      - {!orElse}
      - {!getOrFailWith}
  */

  /** A variant containing the [Some('a)] and [None] constructors 
    
      {[
        type t('a) = 
          | Some('a)
          | None
      ]}
  */
  type t('a) = option('a);

  /** A function version of the [Some] constructor.
    
      In most situations you just want to use the [Some] constructor directly.
    
      However Ocaml doesn't support piping to variant constructors. 

      Note that when using the Reason syntax you {b can} use fast pipe ( [->] ) with variant constructors, so you don't need this function.

      {[
        String.reverse("desserts")
        |> Option.some
         = Some "desserts"
      ]}
   */
  let some: 'a => option('a);

  /** Returns [None] if the first argument is [None], otherwise return the second argument.

    Unlike the built in [&&] operator, the [and_] function does not short-circuit. 

    When you call [and_], both arguments are evaluated before being passed to the function.

    {e Examples}

    {[Option.and_ (Some 11) (Some 22) = Some 22]}

    {[Option.and_ None (Some 22) = None]}

    {[Option.and_ (Some 11) None = None]}

    {[Option.and_ None None = None]}
  */
  let and_: (t('a), t('a)) => t('a);

  /** Return the first argument if it {!isSome}, otherwise return the second.

    Unlike the built in [||] operator, the [or_] function does not short-circuit. 
    When you call [or_], both arguments are evaluated before being passed to the function.

    {e Examples}

    {[Option.or_ (Some 11) (Some 22) = Some 11]}

    {[Option.or_ None (Some 22) = Some 22]}

    {[Option.or_ (Some 11) None = Some 11]}

    {[Option.or_ None None = None]}
  */
  let or_: (t('a), t('a)) => t('a);

  /** Similar to {!or_}, but evaluates its second argument only if the first is [None]. 
    
      {e Examples}

      {[Option.orElse (Some 11) ~f:(fun () -> (Some 22)) = Some 11]}
      
      {[Option.orElse None ~f:(fun () -> (Some 22)) = Some 22]}
  */
  let orElse: (t('a), ~f: unit => t('a)) => t('a);

  /** Transform two options into an option of a {!Tuple}. 
    
      Returns None if either of the aguments is None.

      {e Examples}

      {[Option.both (Some 3004) (Some "Ant") = Some (3004, "Ant")]}

      {[Option.both (Some 3004) None = None]}

      {[Option.both None (Some "Ant") = None]}

      {[Option.both None None = None]}
  */
  let both: (t('a), t('b)) => t(('a, 'b));

  /** Flatten two optional layers into a single optional layer.
    
      {e Examples}

      {[Option.join (Some (Some 4)) = Some 4]}

      {[Option.join (Some None) = None]}

      {[Option.join (None) = None]}
  */
  let join: t(t('a)) => t('a);

  /** Transform the value inside an option. 
    
      Leaves [None] untouched.

      See {!Infix.(>>|)} for an operator version of this function.

      {e Examples}

      {[Option.map ~f:(fun x -> x * x) (Some 9) = Some 81]}

      {[Option.map ~f:Int.toString (Some 9) = Some "9"]}
      
      {[Option.map ~f:(fun x -> x * x) None = None]}
  */
  let map: (t('a), ~f: 'a => 'b) => t('b);

  /** Apply a function if all the arguments are [Some] value 
      
      {e Examples}

      {[Option.map2 (Some 3) (Some 4) ~f=Int.add = Some 7]}

      {[Option.map2 (Some 3) (Some 4) ~f=Tuple.make = Some (3, 4)]}
  */
  let map2: (t('a), t('b), ~f: ('a, 'b) => 'c) => t('c);

  /** Chain together many computations that may not return a value. 
    
      It is helpful to see its definition:
      {[
        let bind = (t, ~f) =>
          switch (t) {
          | Some(x) => f(x)
          | None => None
          };
      ]}

      This means we only continue with the callback if we have a value. 

      For example, say you need to parse some user input as a month:

      {[
        let toValidMonth = (month: int): option(int) =>
          if (1 <= month && month <= 12) {
            Some(month)
          } else {
            None
          }
        
        let parseMonth = (userInput: string): option(int) => 
          Int.ofString(userInput)          
          |> Option.bind(~f=toValidMonth)
      ]}

      In the [parseMonth] function, if [String.toInt] produces [None] (because
      the [userInput] was not an integer) this entire chain of operations will
      short-circuit and result in [None]. If [toValidMonth] results in [None],
      again the chain of computations will result in [None].

      See {!Infix.(>>=)} for an operator version of this function.

      {e Examples}

      {[Option.bind (Some [1, 2, 3]) ~f=List.head = Some 1]}

      {[Option.bind (Some []) ~f=List.head = None]}

      {5 Aliases}

      - andThen
      - flatMap
  */
  let bind: (t('a), ~f: 'a => t('b)) => t('b);

  /** Unwrap an [option('a)] returning [default] if called with [None].
    
      This comes in handy when paired with functions like {!Map.get} or {!List.head} which return an {!Option}. 
    
      See {!Infix.(|?)} for an operator version of this function.

      {b Note} This can be overused! Many cases are better handled using pattern matching, {!map} or {!bind}.      

      {e Examples}

      {[Option.get ~default:99 (Some 42) = 42]}

      {[Option.get ~default:99 None = 99]}

      {[Option.get ~default:"unknown" (Map.get Map.String.empty "Tom") = "unknown"]}
  */
  let get: (t('a), ~default: 'a) => 'a;

  /** Unwrap an [option('a)] returning the enclosed ['a]. 
    
      Raises the provided [exn] if called with [None].

      {b Note} in most situations it is encouraged to use pattern matching, {!get}, {!map} or {!bind}. 
      Can you structure your code slightly differently to avoid potentially raising an exception?
   
      {2 Exceptions}

      Raises an [Invalid_argument] exception if called with [None]
   
      {e Examples}

      [TODO]
  */
  let getOrFailWith: (t('a), ~exn: exn) => 'a;

  /** Unwrap an [option('a)] returning the enclosed ['a]. 
    
      {b Note} in most situations it is better to use pattern matching, {!get}, {!map} or {!bind}. 
      Can you structure your code slightly differently to avoid potentially raising an exception?
      
      {2 Exceptions}

      Raises an [Invalid_argument] exception if called with [None]
      
      {e Examples}
      
      {[List.head [1;2;3] |> Option.getUnsafe = 1]}

      {[List.head [] |> Option.getUnsafe]}
  */
  let getUnsafe: t('a) => 'a;

  /** Check if an {!Option} is a [Some].

      In most situtations you should just use pattern matching instead.

      {e Examples}

      {[Option.isSome (Some 3004) = true]}

      {[Option.isSome None = false]}
  */
  let isSome: t('a) => bool;

  /** Check if an {!Option} is a [None].

      In most situtations you should just use pattern matching instead.

      {e Examples}

      {[Option.isNone (Some 3004) = false]}
      
      {[Option.isNone None = true]}
  */
  let isNone: t('a) => bool;

  /** Run a function against a value, if it is present.

      {e Examples}

      TODO
   */
  let forEach: (t('a), ~f: 'a => unit) => unit;

  /** Run a function against a value, if it is present.

      {e Examples}

      TODO
   */
  let fold: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;

  let toArray: t('a) => array('a);
  /** Convert an option to a {!Array}. 

      [None] is represented as an empty list and [Some] is represented as a list of one element.

      {e Examples}

      {[Option.toArray (Some 3004) = [|3004|]]}

      {[Option.toArray (None) = [||]]}
  */

  let toList: t('a) => list('a);
  /** Convert an option to a {!List}. 

      [None] is represented as an empty list and [Some] is represented as a list of one element.

      {e Examples}

      {[Option.toList (Some 3004) = [3004]]}
        
      {[Option.toList (None) = []]}
  */

  let toResult: (t('ok), ~or_: 'error) => Result.t('error, 'ok);
  /** Convert an option to a {!Result}. 

      [Some(value)] is transformed into [Ok(value)].

      [None] is transformed into [Error(or_)].

      {e Examples}

      {[Option.toResult (Some 3004) ~or:"Missing number!" = Ok 3004]}
        
      {[Option.toResult (None) ~or:"Missing number!" = Error "Missing number!"]}
  */

  module Infix: {
    /** Operators for code that works extensively with {!Option}s.
      
        This module is intended to be [open]ed at the top of a block of code (or module) that uses 
        its operators extensively.

        {[
          open Option.Infix;
          let nameToAge = Map.String.ofArray([|
            ("Ant", 1),
            ("Bat", 5),
            ("Cat", 19),
          |]);

          let catAge = Map.get nameToAge "Cat" |? 8;
          // 19

          let ageDifference = 
            Map.get nameToAge "Ant" 
            >>= (antAge => {
              Map.get nameToAge "Bat"
              >>| (batAge => {
                Int.absolute(batAge - antAge)
              }) 
            });
          // Some (4)
        ]}
     */

    let (|?): (t('a), 'a) => 'a;
    /** The operator version of {!get} 
       
       {e Examples}

       {[Some 3004 |? 8 = 3004]}

       {[None |? 8 = 8]}
    */

    let (>>|): (t('a), 'a => 'b) => t('b);
    /** The operator version of {!map} 
      
        {e Examples}

        {[Some "desserts" >>| String.reverse = Some "stressed"]}

        {[None >>| String.reverse = None]}
    */

    let (>>=): (t('a), 'a => t('b)) => t('b);
    /** The operator version of {!bind} 
      
        {e Examples}

        {[Some [1, 2, 3] >>= List.head = Some 1]}

        {[Some [] >>= List.head = None]}
    */
  };
};

/** Functions for working with single characters. */
module Char: {
  /** Functions for working with single characters. 
    
      Character literals are enclosed in ['a'] pair of single quotes.

      The functions in this module work on ASCII characters (range 0-255) only, not Unicode.

      Since character 128 through 255 have varying values
      depending on what standard you are using (ISO 8859-1
      or Windows 1252), you are advised to stick to the
      0-127 range.
  */

  type t = char;

  /** Convert an ASCII [code point][cp] to a character.
    
    returns [None] if the codepoint is outside the range of 0 / 255.

    {e Examples}

    {[Char.ofCode 65 = Some 'A']}

    {[Char.ofCode 66 = Some 'B']}

    {[Char.ofCode 3000 = None]}

    {[Char.ofCode (-1) = None]}

    The full range of extended ASCII is from [0] to [255]. For numbers outside that range, you get [None].

    [cp]: https://en.wikipedia.org/wiki/Code_point 
  */
  let ofCode: int => option(char);

  /** Converts a string to character. Returns None when the string isn't of length one.
    
      {e Examples}

      {[Char.ofString "A" = Some 'A']}

      {[Char.ofString " " = Some ' ']}

      {[Char.ofString "" = None]}

      {[Char.ofString "abc" = None]}

      {[Char.ofString " a" = None]} 
  */
  let ofString: string => option(char);

  /** Convert to the corresponding ASCII [code point][cp].

    [cp]: https://en.wikipedia.org/wiki/Code_point

    {e Examples}

    {[Char.toCode 'A' = 65]}

    {[Char.toCode 'B' = 66]}

    {[Char.toCode 'þ' = 254]} */
  let toCode: char => int;

  /** Convert a character into a string.
      
      {e Examples}
      
      {[Char.toString 'A' = "A"]}

      {[Char.toString '{' = "{"]}

      {[Char.toString '7' = "7"]} */
  let toString: char => string;

  /** Converts a digit character to its corresponding {!Int}. 
     
      Returns None when the character isn't a digit.

      {[Char.toDigit "7" = Some 7]}

      {[Char.toDigit "0" = Some 0]}

      {[Char.toDigit "A" = None]}

      {[Char.toDigit "" = None]} */
  let toDigit: char => option(int);

  /** Converts an ASCII character to lower case, preserving non alphabetic ASCII characters.

      {e Examples}  

      {[Char.toLowercase 'A' = 'a']}

      {[Char.toLowercase 'B' = 'b']}

      {[Char.toLowercase '7' = '7']} */
  let toLowercase: char => char;

  /**
    For characters in the range ['A'] to ['Z'], [Char.toUppercase ch] returns the
    corresponding upper case letter; for any characters outside that
    range, returns the character unchanged.

    {[
    Char.toUppercase 'g' = 'G'
    Char.toUppercase 'H' = 'H'
    Char.toUppercase '%' = '%'
    ]}
  */
  /** Convert an ASCII character to upper case, preserving non alphabetic ASCII characters.
      [toUppercase 'a' = 'A']

      [toUppercase 'b' = 'B']

      [toUppercase '7' = '7'] */
  let toUppercase: char => char;

  /**
    [Char.isLowercase ch] returns [true] if [ch]
    is in the range ['a'] to ['z'],
    [false] otherwise.

    {[
    Char.isLowercase 'g' = true
    Char.isLowercase 'H' = false
    Char.isLowercase '%' = false
    ]}
  */
  /** Detect lower case ASCII characters.

    [isLowercase 'a' = true]

    [isLowercase 'b' = true]

    ...

    [isLowercase 'z' = true]

    [isLowercase '0' = false]

    [isLowercase 'A' = false]

    [isLowercase '-' = false]

    [isLowercase 'ã' = false] */
  let isLowercase: char => bool;

  /**
    [Char.isUppercase ch] returns [true] if [ch]
    is in the range ['A'] to ['Z'],
    [false] otherwise.

    {[
    Char.isUppercase 'G' = true
    Char.isUppercase 'h' = false
    Char.isUppercase '%' = false
    ]}
  */;
  /** Detect upper case ASCII characters.

     [isUppercase 'A' = true]

     [isUppercase 'B' = true]

     [...]

     [isUppercase 'Z' = true]

     [isUppercase '0' = false]

     [isUppercase 'Ý' = false]

     [isUppercase '-' = false] */;
  let isUppercase: char => bool;

  /**
    [Char.isLetter ch] returns [true] if [ch] is in the range ['A'] to ['Z']
    or ['a'] to ['z'], [false] otherwise.

    {[
    Char.isLetter 'G' = true
    Char.isLetter 'h' = true
    Char.isLetter '%' = false
    ]}
  */;
  /** Detect upper and lower case ASCII alphabetic characters.

      [isLetter 'a' = true]

      [isLetter 'b' = true]

      [isLetter 'E' = true]

      [isLetter 'Y' = true]

      [isLetter '0' = false]

      [isLetter 'ý' = false]

      [isLetter '-' = false] */;
  let isLetter: char => bool;

  /**
    [Char.is_digit ch] returns [true] if [ch] is in the range ['0'] to ['9'];
    [false] otherwise.

    {[
    Char.is_digit '3' = true
    Char.is_digit 'h' = false
    Char.is_digit '%' = false
    ]}
  */
  /** Detect when a character is a number

    [isDigit '0' = true]

    [isDigit '1' = true]
    ...
    [isDigit '9' = true]

    [isDigit 'a' = false]

    [isDigit 'b' = false]

    [isDigit 'ý' = false] */
  let isDigit: char => bool;

  /**
    [Char.isAlphanumeric ch] returns [true] if [ch] is
    in the range ['0'] to ['9'], ['A'] to ['Z'], or ['a'] to ['z'];
    [false] otherwise.

    {[
    Char.isAlphanumeric '3' = true
    Char.isAlphanumeric 'G' = true
    Char.isAlphanumeric 'h' = true
    Char.isAlphanumeric '%' = false
    ]}
  */;
  /** Detect upper case, lower case and digit ASCII characters.

        [isAlphanumeric 'a' = true]

        [isAlphanumeric 'b' = true]

        [isAlphanumeric 'E' = true]

        [isAlphanumeric 'Y' = true]

        [isAlphanumeric '0' = true]

        [isAlphanumeric '7' = true]

        [isAlphanumeric '-' = false] */;
  let isAlphanumeric: char => bool;

  /**
    [Char.isPrintable ch] returns [true] if [ch] is
    in the range [' '] to ['~'], (ASCII 32 to 127, inclusive)
    [false] otherwise.

    {[
    Char.isPrintable 'G' = true
    Char.isPrintable '%' = true
    Char.isPrintable '\t' = false
    Char.isPrintable '\007' = false
    ]}
  */;
  /** Detect if a character is a [printable] character
       https://en.wikipedia.org/wiki/ASCII#Printable_characters

       [isPrintable ' ' = true] */;
  let isPrintable: char => bool;

  /**
    [Char.isWhitespace ch] returns [true] if [ch] is one of:
    ['\t'] (tab), ['\n'] (newline), ['\011'] (vertical tab),
    ['\012'] (form feed), ['\r'] (carriage return), or
    [' '] (space). Returns [false] otherwise.

    {[
    Char.isWhitespace '\t' = true
    Char.isWhitespace ' ' = true
    Char.isWhitespace '?' = false
    Char.isWhitespace 'G' = false
    ]}
  */
  /** Detect ' ', '\t', '\r' or '\n' characters.

    [isWhitespace ' ' = true]

    [isWhitespace 'b' = false] */
  let isWhitespace: char => bool;
};

/** Functions for working with floating point numbers. */
module Float: {
  /** A module for working with {{: https://en.wikipedia.org/wiki/Floating-point_arithmetic } floating-point numbers}. Valid syntax for [float]s includes:
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

    Without opening this module you can use the [.] suffixed operators e.g

    {[ 1. +. 2. /. 0.25 *. 2. = 17. ]}

    But by opening this module locally you can use the un-suffixed operators

    {[Float.((10.0 - 1.5 / 0.5) ** 3.0) = 2401.0]}

    {b Historical Note: } The particular details of floats (e.g. [NaN]) are
    specified by {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } which is literally hard-coded into almost all
    CPUs in the world.
  */;

  type t = float;

  /** {1 Constants} */;

  /** The literal [0.0] as a named value */
  let zero: t;

  /** The literal [1.0] as a named value */
  let one: t;

  /** [NaN] as a named value. NaN stands for {{: https://en.wikipedia.org/wiki/NaN } not a number}.

      {b Note } comparing values with {!Float.nan} will {b always return } [false] even if the value you are comparing against is also [NaN].

      e.g

      {[
        let isNotANumber x = Float.(x = nan) in

        isNotANumber nan = false
      ]}

      For detecting [Nan] you should use {!Float.isNaN} */
  let nan: t;

  /** Positive {{: https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity } infinity }

    {[Float.log ~base:10.0 0.0 = Float.infinity]} */
  let infinity: t;

  /** Negative infinity, see {!Float.infinity} */
  let negativeInfinity: t;

  /** An approximation of {{: https://en.wikipedia.org/wiki/E_(mathematical_constant) } Euler's number }. */
  let e: t;

  /** An approximation of {{: https://en.wikipedia.org/wiki/Pi } pi }. */
  let pi: t;

  /** The smallest interval between two representable numbers. */
  let epsilon: t;

  /** The largest (furthest from zero) representable positive [float] */
  let largestValue: t;

  /** The smallest representable positive [float]. The closest to zero without actually being zero.
      TODO isn't this epsilon?
   */
  let smallestValue: t;

  /** For floats greater than [maximumSafeInteger], it no longer holds that [Float.(n + 1.) > n]  */
  // TODO verify
  let maximumSafeInteger: t;

  /** For floats less than [minimumSafeInteger], it no longer holds that [Float.(n - 1.) < n]  */;
  // TODO verify
  let minimumSafeInteger: t;

  /** {1 Basic arithmetic and operators} */;

  /** Addition for floating point numbers.

    {[
      Float.add 3.14 3.14 = 6.28
      Float.(3.14 + 3.14 = 6.28)
    ]}

    Although [int]s and [float]s support many of the same basic operations such as
    addition and subtraction you {b cannot} [add] an [int] and a [float] directly which
    means you need to use functions like {!Int.toFloat} or {!Float.roundToInt} to convert both values to the same type.

    So if you needed to add a {!List.length} to a [float] for some reason, you
    could:

    {[Float.add 3.14 (Int.toFloat (List.length [1,2,3])) = 6.14]}

    or

    {[Float.roundToInt 3.14 + List.length [1,2,3] = 6]}

    Languages like Java and JavaScript automatically convert [int] values
    to [float] values when you mix and match. This can make it difficult to be sure
    exactly what type of number you are dealing with and cause unexpected behavior.

    OCaml has opted for a design that makes all conversions explicit.
  */
  let add: (t, t) => t;

  /** See {!Float.add} */
  let (+): (t, t) => t;

  /** Subtract numbers
    {[Float.subtract 4.0 3.0 = 1.0]}

    Alternatively the [-] operator can be used:

    {[Float.(4.0 - 3.0) = 1.0]}
  */
  let subtract: (t, t) => t;

  /** See {!Float.subtract} */
  let (-): (t, t) => t;

  /** Multiply numbers like

    {[Float.multiply 2.0 7.0 = 14.0]}

    Alternatively the [*] operator can be used:

    {[Float.(2.0 * 7.0) = 14.0]}
  */
  let multiply: (t, t) => t;

  /** See {!Float.multiply} */
  let ( * ): (t, t) => t;

  /** Floating-point division:

    {[Float.divide 3.14 ~by:2.0 = 1.57]}

    Alternatively the [/] operator can be used:

    {[Float.(3.14 / 2.0) = 1.57]}
  */
  let divide: (t, ~by: t) => t;

  /** See {!Float.divide} */
  let (/): (t, t) => t;

  /** Exponentiation, takes the base first, then the exponent.

    {[Float.power ~base:7.0 ~exponent:3.0 = 343.0]}

    Alternatively the [**] operator can be used:

    {[Float.(7.0 ** 3.0) = 343.0]}
  */
  let power: (~base: t, ~exponent: t) => t;

  /** See {!Float.power} */
  let ( ** ): (t, t) => t;

  /** Flips the 'sign' of a [float] so that positive floats become negative and negative integers become positive. Zero stays as it is.

    {[
    Float.negate 8 = (-8)
    Float.negate (-7) = 7
    Float.negate 0 = 0
    ]}

    Alternatively an operator is available:

    {[Float.(~- 4.0) = (-4.0)]}
  */
  let negate: t => t;

  /** See {!Float.negate} */
  let (~-): t => t;

  /** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value} of a number.

    {[
    Float.absolute 8. = 8.
    Float.absolute (-7) = 7
    Float.absolute 0 = 0
    ]}
  */
  let absolute: t => t;

  /** Returns the larger of two [float]s, if both arguments are equal, returns the first argument

    {[
    Float.maximum 7. 9. = 9.
    Float.maximum (-4.) (-1.) = (-1.)
    ]}

    If either (or both) of the arguments are [NaN], returns [NaN]

    {[Float.(isNaN (maximum 7. nan)) = true]}
  */
  let maximum: (t, t) => t;

  /** Returns the smaller of two [float]s, if both arguments are equal, returns the first argument

    {[
    Float.minimum 7.0 9.0 = 7.0
    Float.minimum (-4.0) (-1.0) = (-4.0)
    ]}

    If either (or both) of the arguments are [NaN], returns [NaN]

    {[Float.(isNaN (minimum 7. nan)) = true]}
  */
  let minimum: (t, t) => t;

  /** Clamps [n] within the inclusive [lower] and [upper] bounds.

    {[
    Float.clamp ~lower:0. ~upper:8. 5. = 5.
    Float.clamp ~lower:0. ~upper:8. 9. = 8.
    Float.clamp ~lower:(-10.) ~upper:(-5.) 5. = -5.
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */

  let clamp: (t, ~lower: t, ~upper: t) => t;

  /** {1 Fancier math} */;

  /** Take the square root of a number.
    {[
    Float.squareRoot 4.0 = 2.0
    Float.squareRoot 9.0 = 3.0
    ]}

    [squareRoot] returns [NaN] when its argument is negative. See {!Float.nan} for more.
  */

  let squareRoot: t => t;

  /** Calculate the logarithm of a number with a given base.

    {[
    Float.log ~base:10. 100. = 2.
    Float.log ~base:2. 256. = 8.
    ]}
  */

  let log: (t, ~base: t) => t;

  /** {1 Checks} */;

  /** Determine whether a float is an undefined or unrepresentable number.

    {[
    Float.is_nan (0.0 / 0.0) = true
    Float.(is_nan (squareRoot (-1.0))) = true
    Float.is_nan (1.0 / 0.0) = false  (* Float.infinity {b is} a number *)
    Float.is_nan 1. = false
    ]}

    {b Note } this function is more useful than it might seem since [NaN] {b does not } equal [Nan]:

    {[Float.(nan = nan) = false]}
  */
  let isNaN: t => bool;

  /** Determine whether a float is finite number. True for any float except [Infinity], [-Infinity] or [NaN]

    {[
    Float.isFinite (0. / 0.) = false
    Float.(isFinite (squareRoot (-1.))) = false
    Float.isFinite (1. / 0.) = false
    Float.isFinite 1. = true
    Float.(isFinite nan) = false
    ]}

    Notice that [NaN] is not finite!

    For a [float] [n] to be finite implies that [Float.(not (isInfinite n || is_nan n))] evaluates to [true].
  */
  let isFinite: t => bool;

  /** Determine whether a float is positive or negative infinity.

    {[
    Float.isInfinite (0. / 0.) = false
    Float.(isInfinite (squareRoot (-1.))) = false
    Float.isInfinite (1. / 0.) = true
    Float.isInfinite 1. = false
    Float.(isInfinite nan) = false
    ]}
  */
  let isInfinite: t => bool;

  /** Determine whether the passed value is an integer. */
  let isInteger: t => bool;

  /** Determine whether the passed value is a safe integer (number between -(2**53 - 1) and 2**53 - 1). */
  let isSafeInteger: t => bool;

  /** Checks if [n] is between [lower] and up to, but not including, [upper].
    If [lower] is not specified, it's set to to [0.0].

    {[
    Float.inRange ~lower:2. ~upper:4. 3. = true
    Float.inRange ~lower:1. ~upper:2. 2. = false
    Float.inRange ~lower:5.2 ~upper:7.9 9.6 = false
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */
  let inRange: (t, ~lower: t, ~upper: t) => bool;

  /** {1 Angles} */;

  type radians = float;
  // type radians =
  // | Radians(float);

  /** [hypotenuse x y] returns the length of the hypotenuse of a right-angled triangle with sides of length [x] and [y], or, equivalently, the distance of the point [(x, y)] to [(0, 0)].

    {[Float.hypotenuse 3. 4. = 5.]}
  */

  let hypotenuse: (t, t) => t;

  /** Converts an angle in {{: https://en.wikipedia.org/wiki/Degree_(angle) } degrees} to {!Float.radians}.

    {[Float.degrees 180. = v]}
  */
  let degrees: t => radians;

  /** Convert a {!Float.t} to {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {[Float.(radians pi) = 3.141592653589793]}

    {b Note } This function doesn't actually do anything to its argument, but can be useful to indicate intent when inter-mixing angles of different units within the same function.
  */
  let radians: t => radians;

  /** Convert an angle in {{: https://en.wikipedia.org/wiki/Turn_(geometry) } turns } into {!Float.radians}.

    One turn is equal to 360°.

    {[
    Float.(turns (1. / 2.)) = pi
    Float.(turns 1. = degrees 360.)
    ]}
  */
  let turns: t => radians;

  /** {1 Polar coordinates} */;

  /** Convert {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } (r, θ) to {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } (x,y).

    {[Float.(ofPolar (squareRoot 2., degrees 45.)) = (1., 1.)]}
  */
  let ofPolar: ((float, radians)) => (float, float);

  /** Same as {!Float.toPolar}. */
  let toPolar: ((float, float)) => (float, radians);

  /** Convert {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } (x,y) to {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } (r, θ).

    {[
    Float.toPolar (3.0, 4.0) = (5.0, 0.9272952180016122)
    Float.toPolar (5.0, 12.0) = (13.0, 1.1760052070951352)
    ]}
  */

  /** Figure out the cosine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {[
    Float.(cos (degrees 60.)) = 0.5000000000000001
    Float.(cos (radians (pi / 3.))) = 0.5000000000000001
    ]}
  */

  let cos: radians => t;

  /** Figure out the arccosine for [adjacent / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

    {[Float.(acos (radians 1.0 / 2.0)) = Float.radians 1.0471975511965979 (* 60° or pi/3 radians *)]}
  */

  let acos: radians => t;

  /** Figure out the sine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {[
    Float.(sin (degrees 30.)) = 0.49999999999999994
    Float.(sin (radians (pi / 6.))) = 0.49999999999999994
    ]}
  */

  let sin: radians => t;

  /** Figure out the arcsine for [opposite / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

    {[Float.(asin (1.0 / 2.0)) = 0.5235987755982989 (* 30° or pi / 6 radians *)]}
  */

  let asin: radians => t;

  /** Figure out the tangent given an angle in radians.

    {[
    Float.(tan (degrees 45.)) = 0.9999999999999999
    Float.(tan (radians (pi / 4.))) = 0.9999999999999999
    Float.(tan (pi / 4.)) = 0.9999999999999999
    ]}
  */

  let tan: radians => t;

  /** This helps you find the angle (in radians) to an [(x, y)] coordinate, but
    in a way that is rarely useful in programming.

    {b You probably want } {!atan2} instead!

    This version takes [y / x] as its argument, so there is no way to know whether
    the negative signs comes from the [y] or [x] value. So as we go counter-clockwise
    around the origin from point [(1, 1)] to [(1, -1)] to [(-1,-1)] to [(-1,1)] we do
    not get angles that go in the full circle:

    {[
    Float.atan (1. /. 1.) = 0.7853981633974483  (* 45° or pi/4 radians *)
    Float.atan (1. /. -1.) = -0.7853981633974483  (* 315° or 7 * pi / 4 radians *)
    Float.atan (-1. /. -1.) = 0.7853981633974483 (* 45° or pi/4 radians *)
    Float.atan (-1. /.  1.) = -0.7853981633974483 (* 315° or 7 * pi/4 radians *)
    ]}

    Notice that everything is between [pi / 2] and [-pi/2]. That is pretty useless
    for figuring out angles in any sort of visualization, so again, check out
    {!Float.atan2} instead!
  */

  let atan: t => radians;

  /** This helps you find the angle (in radians) to an [(x, y)] coordinate. So rather than saying [Float.(atan (y / x))] you can [Float.atan2 ~y ~x] and you can get a full range of angles:

    {[
    Float.atan2 ~y:1. ~x:1. = 0.7853981633974483  (* 45° or pi/4 radians *)
    Float.atan2 ~y:1. ~x:(-1.) = 2.3561944901923449  (* 135° or 3 * pi/4 radians *)
    Float.atan2 ~y:(-1.) ~x:(-1.) = -(2.3561944901923449) (* 225° or 5 * pi/4 radians *)
    Float.atan2 ~y:(-1.) ~x:1. = -(0.7853981633974483) (* 315° or 7 * pi/4 radians *)
    ]}
  */

  let atan2: (~y: t, ~x: t) => radians;

  /** {1 Conversion} */;

  type direction = [
    | `Zero
    | `AwayFromZero
    | `Up
    | `Down
    | `Closest([ | `Zero | `AwayFromZero | `Up | `Down | `ToEven])
  ];

  /** Round a number, by default to the to the closest [int] with halves rounded [`Up] (towards positive infinity)

    {[
      Float.round 1.2 = 1.0
      Float.round 1.5 = 2.0
      Float.round 1.8 = 2.0
      Float.round -1.2 = -1.0
      Float.round -1.5 = -1.0
      Float.round -1.8 = -2.0
    ]}

    Other rounding strategies are available by using the optional [~direction] label.

    {2 Towards zero}

    {[
      Float.round ~direction:`Zero 1.2 = 1.0
      Float.round ~direction:`Zero 1.5 = 1.0
      Float.round ~direction:`Zero 1.8 = 1.0
      Float.round ~direction:`Zero (-1.2) = -1.0
      Float.round ~direction:`Zero (-1.5) = -1.0
      Float.round ~direction:`Zero (-1.8) = -1.0
    ]}

    {2 Away from zero}

    {[
      Float.round ~direction:`AwayFromZero 1.2 = 1.0
      Float.round ~direction:`AwayFromZero 1.5 = 1.0
      Float.round ~direction:`AwayFromZero 1.8 = 1.0
      Float.round ~direction:`AwayFromZero (-1.2) = -1.0
      Float.round ~direction:`AwayFromZero (-1.5) = -1.0
      Float.round ~direction:`AwayFromZero (-1.8) = -1.0
    ]}

    {2 Towards infinity}

    This is also known as {!Float.ceiling}

    {[
      Float.round ~direction:`Up 1.2 = 1.0
      Float.round ~direction:`Up 1.5 = 1.0
      Float.round ~direction:`Up 1.8 = 1.0
      Float.round ~direction:`Up (-1.2) = -1.0
      Float.round ~direction:`Up (-1.5) = -1.0
      Float.round ~direction:`Up (-1.8) = -1.0
    ]}

    {2 Towards negative infinity}

    This is also known as {!Float.floor}

    {[List.map  ~f:(Float.round ~direction:`Down) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -2.0; 1.0 1.0 1.0]]}

    {2 To the closest integer}

    Rounding a number [x] to the closest integer requires some tie-breaking for when the [fraction] part of [x] is exactly [0.5].

    {3 Halves rounded towards zero}

    {[List.map  ~f:(Float.round ~direction:(`Closest `AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -1.0; -1.0; 1.0 1.0 2.0]]}

    {3 Halves rounded away from zero}

    This method is often known as {b commercial rounding }

    {[List.map  ~f:(Float.round ~direction:(`Closest `AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 2.0 2.0]]}

    {3 Halves rounded down}

    {[List.map  ~f:(Float.round ~direction:(`Closest `Down)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 1.0 2.0]]}

    {3 Halves rounded up}

    This is the default.

    [Float.round 1.5] is the same as [Float.round ~direction:(`Closest `Up) 1.5]

    {3 Halves rounded towards the closest even number}

    This tie-breaking rule is the default rounding mode using in

    {[
      Float.round ~direction:(`Closest `ToEven) -1.5 = -2.0
      Float.round ~direction:(`Closest `ToEven) -2.5 = -2.0
    ]}
  */

  let round: (~direction: direction=?, t) => t;

  /** Floor function, equivalent to [Float.round ~direction:`Down].

    {[
      Float.floor 1.2 = 1.0
      Float.floor 1.5 = 1.0
      Float.floor 1.8 = 1.0
      Float.floor -1.2 = -2.0
      Float.floor -1.5 = -2.0
      Float.floor -1.8 = -2.0
    ]}
  */

  let floor: t => t;

  /** Ceiling function, equivalent to [Float.round ~direction:`Up].

  {[
    Float.ceiling 1.2 = 2.0
    Float.ceiling 1.5 = 2.0
    Float.ceiling 1.8 = 2.0
    Float.ceiling -1.2 = (-1.0)
    Float.ceiling -1.5 = (-1.0)
    Float.ceiling -1.8 = (-1.0)
  ]}
  */

  let ceiling: t => t;

  /** Ceiling function, equivalent to [Float.round ~direction:`Zero].

    {[
      Float.truncate 1.0 = 1
      Float.truncate 1.2 = 1
      Float.truncate 1.5 = 1
      Float.truncate 1.8 = 1
      Float.truncate (-1.2) = -1
      Float.truncate (-1.5) = -1
      Float.truncate (-1.8) = -1
    ]}
  */

  let truncate: t => t;

  /** Convert an {!Int} to a [float]

    {[
      Float.ofInt 5 = 5.0
      Float.ofInt 0 = 0.0
      Float.ofInt -7 = -7.0
    ]}
  */
  let ofInt: int => float;

  /** Converts a [float] to an {!Int} by {b ignoring the decimal portion}. See {!Float.truncate} for examples.

    Returns [None] when trying to round a [float] which can't be represented as an [int] such as {!Float.nan} or {!Float.infinity} or numbers which are too large or small.

    {[
      Float.(toInt nan) = None
      Float.(toInt infinity) = None
    ]}

    You probably want to use some form of {!Float.round} prior to using this function.

    {[Float.(round 1.6 |> toInt) = Some 2]}
  */
  let toInt: t => option(int);

  let toString: t => string;

  let ofString: string => t;
};

/** Fixed precision integers */
module Int: {
  /**
    The platform-dependant {{: https://en.wikipedia.org/wiki/TODO } signed } {{: https://en.wikipedia.org/wiki/Integer } integer} type.

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

    When targeting native OCaml uses 31-bits on 32-bit platforms and 63-bits on 64-bit platforms
    which means that [int] math is well-defined in the range [-2 ** 30] to [2 ** 30 - 1] for 32bit platforms [-2 ** 62] to [2 ** 62 - 1] for 64bit platforms.

    You can read about the reasons for OCamls unusual integer sizes {{: https://v1.realworldocaml.org/v1/en/html/memory-representation-of-values.html} here }.

    TODO verify this, it seems incorrect.
    When targeting JavaScript, that range is [-2 ** 53] to [2 ** 53 - 1].

    Outside of that range, the behavior is determined by the compilation target.

    {e Historical Note: } The name [int] comes from the term {{: https://en.wikipedia.org/wiki/Integer } integer}). It appears
    that the [int] abbreviation was introduced in the programming language ALGOL 68.

    Today, almost all programming languages use this abbreviation.
  */;

  type t = int;

  /** TODO explain what this is for */
  type identity;

  /** {1 Constants } */;

  /** The literal [0] as a named value */
  let zero: t;

  /** The literal [1] as a named value */
  let one: t;

  /** The maximum representable [int] on the current platform */
  let maximumValue: t;

  /** The minimum representable [int] on the current platform */
  let minimumValue: t;

  /** {1 Operators }
    {b Note } You do not need to open the {!Int} module to use the {!( + )}, {!( - )}, {!( * )} or {!( / )} operators, these are available as soon as you [open Standard]
  */;

  /** Add two {!Int} numbers.

    {[Int.add 3002 4004 = 7006]}

    Or using the globally available operator:

    {[3002 + 4004 = 7006]}

    You {e cannot } add an [int] and a [float] directly though.

    See {!Float.add} for why, and how to overcome this limitation.
  */
  let add: (t, t) => t;

  /** See {!Int.add} */
  let (+): (t, t) => t;

  /** Subtract numbers
    {[Int.subtract 4 3 = 1]}

    Alternatively the operator can be used:

    {[4 - 3 = 1]}
  */
  let subtract: (t, t) => t;

  /** See {!Int.subtract} */
  let (-): (t, t) => t;

  /** Multiply [int]s like

    {[Int.multiply 2 7 = 14]}

    Alternatively the operator can be used:

    {[(2 * 7) = 14]}
  */
  let multiply: (t, t) => t;

  /** See {!Int.multiply} */
  let ( * ): (t, t) => t;

  /** Integer division:

    {[Int.divide 3 ~by:2 = 1]}

    {[27 / 5 = 5]}

    Notice that the remainder is discarded.

    Throws [Division_by_zero] when the divisor is [0].
  */
  let divide: (t, ~by: t) => t;

  /** See {!Int.divide} */
  let (/): (t, t) => t;

  /** Floating point division
    {[
      3 // 2 = 1.5
      27 // 5 = 5.25
      8 // 4 = 2.0
    ]}
  */
  let (/\/): (t, t) => float;

  /** Exponentiation, takes the base first, then the exponent.

    {[Int.power ~base:7 ~exponent:3 = 343]}

    Alternatively the [**] operator can be used:

    {[7 ** 3 = 343]}
  */
  let power: (~base: t, ~exponent: t) => t;

  /** See {!Int.power} */
  let ( ** ): (t, t) => t;

  /** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

    {[
    Int.negate 8 = (-8)
    Int.negate (-7) = 7
    Int.negate 0 = 0
    ]}

    Alternatively the [-] operator can be used:

    {[~-(7) = (-7)]}
  */
  let negate: t => t;

  /** See {!Int.negate} */
  let (~-): t => t;

  /** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

    {[
    Int.absolute 8 = 8
    Int.absolute (-7) = 7
    Int.absolute 0 = 0
    ]}
  */
  let absolute: t => t;

  /** Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

    If you intend to use [modulo] to detect even and odd numbers consider using {!Int.isEven} or {!Int.is_odd}.

    {[
    Int.modulo ~by:2 0 = 0
    Int.modulo ~by:2 1 = 1
    Int.modulo ~by:2 2 = 0
    Int.modulo ~by:2 3 = 1
    s]}

    Our [modulo] function works in the typical mathematical way when you run into negative numbers:

    {[
      List.map ~f:(Int.modulo ~by:4) [(-5); (-4); -3; -2; -1;  0;  1;  2;  3;  4;  5 ] =
        [3; 0; 1; 2; 3; 0; 1; 2; 3; 0; 1]
    ]}

    Use {!Int.remainder} for a different treatment of negative numbers.
  */
  let modulo: (t, ~by: t) => t;

  /** Get the remainder after division. Here are bunch of examples of dividing by four:

    {[
      List.map
        ~f:(Int.remainder ~by:4)
        [(-5); (-4); (-3); (-2); (-1); 0; 1; 2; 3; 4; 5] =
          [(-1); 0; (-3); (-2); (-1); 0; 1; 2; 3; 0; 1]
    ]}


    Use {!Int.modulo} for a different treatment of negative numbers.
  */
  let remainder: (t, ~by: t) => t;

  /** Returns the larger of two [int]s

    {[
    Int.maximum 7 9 = 9
    Int.maximum (-4) (-1) = (-1)
    ]}
  */
  let maximum: (t, t) => t;

  /** Returns the smaller of two [int]s

    {[
    Int.minimum 7 9 = 7
    Int.minimum (-4) (-1) = (-4)
    ]}
  */
  let minimum: (t, t) => t;

  /** {1 Checks} */;

  /** Check if an [int] is even

    {[
    Int.isEven 8 = true
    Int.isEven 7 = false
    Int.isEven 0 = true
    ]}
  */
  let isEven: t => bool;

  /** Check if an [int] is odd

    {[
    Int.is_odd 7 = true
    Int.is_odd 8 = false
    Int.is_odd 0 = false
    ]}
  */
  let isOdd: t => bool;

  /** Clamps [n] within the inclusive [lower] and [upper] bounds.

    {[
    Int.clamp ~lower:0 ~upper:8 5 = 5
    Int.clamp ~lower:0 ~upper:8 9 = 8
    Int.clamp ~lower:(-10) ~upper:(-5) 5 = (-5)
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */
  let clamp: (t, ~lower: t, ~upper: t) => t;

  /** Checks if [n] is between [lower] and up to, but not including, [upper].

    {[
    Int.inRange ~lower:2 ~upper:4 3 = true
    Int.inRange ~lower:5 ~upper:8 4 = false
    Int.inRange ~lower:(-6) ~upper:(-2) (-3) = true
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */
  let inRange: (t, ~lower: t, ~upper: t) => bool;

  /** {1 Conversion } */;

  /** Convert an integer into a float. Useful when mixing {!Int} and {!Float} values like this:

    {[
      let halfOf (number : int) : float =
        Float.((Int.toFloat number) / 2)
      in
      halfOf 7 = 3.5
    ]}
    Note that locally opening the {!Float} module here allows us to use the floating point division operator
  */
  let toFloat: t => float;

  let toString: t => string;
  /** Convert an [int] into a [string] representation.

    {[
    Int.to_string 3 = "3"
    Int.to_string (-3) = "-3"
    Int.to_sString 0 = "0"
    ]}

    Guarantees that

    {[Int.(ofString (toString n)) = Some n ]}
   */

  let ofString: string => option(t);
  /** Attempt to parse a [string] into a [int].

    {[
    Int.ofString "0" = Some 0.
    Int.of_string "42" = Some 42.
    Int.of_string "-3" = Some (-3)
    Int.of_string "123_456" = Some 123_456
    Int.of_string "0xFF" = Some 255
    Int.of_string "0x00A" = Some 10
    Int.of_string "Infinity" = None
    Int.of_string "NaN" = None
    ]}
  */;
};

/** Arbitrary precision integers.  */
module Integer: {
  /**
    Arbitrary precision integers

    Backed by https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt when targeting Javascript

    TODO note on cross platform compatability and polyfills
  */

  type t;

  let ofInt: int => t;

  let ofInt64: Int64.t => t;

  let ofFloat: float => t;

  /** Attempt to parse a [string] into a [Integer].
    {[
      Integer.ofString "0" = Some 0.
      Integer.ofString "42" = Some 42.
      Integer.ofString "-3" = Some (-3)
      Integer.ofString "123_456" = Some 123_456
      Integer.ofString "0xFF" = Some 255
      Integer.ofString "0x00A" = Some 10
      Integer.ofString "Infinity" = None
      Integer.ofString "NaN" = None
    ]}
  */
  let ofString: string => option(t);

  let compare: (t, t) => int;

  let equal: (t, t) => bool;

  /** {1 Constants } */

  /** The literal [0] as a named value */
  let zero: t;

  /** The literal [1] as a named value */
  let one: t;

  /** {1 Operators } */

  /** Add two {!Integer}s.

    {[Integer.add(3002, 4004) == 7006]}

    Or using the operator:

    {[
      open Integer;
      ofInt(3002) + ofInt(4004) = 7006
    ]}

    You {e cannot } add an [int] and a [float] directly though.

    See {!Float.add} for why, and how to overcome this limitation.
  */
  let add: (t, t) => t;

  /** See {!Integer.add} */
  let (+): (t, t) => t;

  /** Subtract numbers
    {[Integer.subtract 4 3 = 1]}

    Alternatively the operator can be used:

    {[4 - 3 = 1]}
  */
  let subtract: (t, t) => t;

  /** See {!Integer.subtract} */
  let (-): (t, t) => t;

  /** Multiply two integers

    {[Integer.multiply 2 7 = 14]}

    Alternatively the operator can be used:

    {[(2 * 7) = 14]}
  */
  let multiply: (t, t) => t;

  /** See {!Integer.multiply} */
  let ( * ): (t, t) => t;

  /** Integer division:

    {[Integer.divide 3 ~by:2 = 1]}

    {[27 / 5 = 5]}

    Notice that the remainder is discarded.

    Throws [Division_by_zero] when the divisor is [0].
  */
  let divide: (t, ~by: t) => t;

  /** See {!Integer.divide} */
  let (/): (t, t) => t;

  /** Exponentiation, takes the base first, then the exponent.

    {[Integer.power ~base:7 ~exponent:3 ~modulo:300 = 43]}

    Alternatively the [**] operator can be used:

    {[7 ** 3 = 343]}
  */
  let power: (~base: t, ~exponent: t, ~modulo: t) => t;

  /** See {!Integer.power} */
  let ( ** ): (t, int) => t;

  /** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.
    {[
      Integer.negate 8 = (-8)
      Integer.negate (-7) = 7
      Integer.negate 0 = 0
    ]}
  */
  let negate: t => t;

  /** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

    {[
      Integer.absolute 8 = 8
      Integer.absolute (-7) = 7
      Integer.absolute 0 = 0
    ]}
  */
  let absolute: t => t;

  /** Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

    If you intend to use [modulo] to detect even and odd numbers consider using {!Integer.isEven} or {!Integer.is_odd}.

    {[
    Integer.modulo ~by:2 0 = 0
    Integer.modulo ~by:2 1 = 1
    Integer.modulo ~by:2 2 = 0
    Integer.modulo ~by:2 3 = 1
    s]}

    Our [modulo] function works in the typical mathematical way when you run into negative numbers:

    {[
      List.map ~f:(Integer.modulo ~by:4) [(-5); (-4); -3; -2; -1;  0;  1;  2;  3;  4;  5 ] =
        [3; 0; 1; 2; 3; 0; 1; 2; 3; 0; 1]
    ]}

    Use {!Integer.remainder} for a different treatment of negative numbers.
  */
  let modulo: (t, ~by: t) => t;

  /** Get the remainder after division. Here are bunch of examples of dividing by four:

    {[
      List.map
        ~f:(Integer.remainder ~by:4)
        [(-5); (-4); (-3); (-2); (-1); 0; 1; 2; 3; 4; 5] =
          [(-1); 0; (-3); (-2); (-1); 0; 1; 2; 3; 0; 1]
    ]}

    Use {!Integer.modulo} for a different treatment of negative numbers.
  */
  let remainder: (t, ~by: t) => t;

  /** Returns the larger of two [Integers]s

    {[
    Integer.maximum 7 9 = 9
    Integer.maximum (-4) (-1) = (-1)
    ]}
  */
  let maximum: (t, t) => t;

  /** Returns the smaller of two [Integers]s

    {[
    Integer.minimum 7 9 = 7
    Integer.minimum (-4) (-1) = (-4)
    ]}
  */
  let minimum: (t, t) => t;

  /** {1 Checks} */;

  /** Check if an [int] is even

    {[
    Integer.isEven 8 = true
    Integer.isEven 7 = false
    Integer.isEven 0 = true
    ]}
  */
  let isEven: t => bool;

  /** Check if an [int] is odd

    {[
    Integer.is_odd 7 = true
    Integer.is_odd 8 = false
    Integer.is_odd 0 = false
    ]}
  */
  let isOdd: t => bool;

  /** Clamps [n] within the inclusive [lower] and [upper] bounds.

    {[
      Integer.clamp ~lower:0 ~upper:8 5 = 5
      Integer.clamp ~lower:0 ~upper:8 9 = 8
      Integer.clamp ~lower:(-10) ~upper:(-5) 5 = (-5)
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */
  let clamp: (t, ~lower: t, ~upper: t) => t;

  let inRange: (t, ~lower: t, ~upper: t) => bool;
  /** Checks if [n] is between [lower] and up to, but not including, [upper].

    {[
    Integer.inRange ~lower:2 ~upper:4 3 = true
    Integer.inRange ~lower:5 ~upper:8 4 = false
    Integer.inRange ~lower:(-6) ~upper:(-2) (-3) = true
    ]}

    Throws an [Invalid_argument] exception if [lower > upper]
  */;

  /** {1 Conversion } */;

  let toInt: t => option(int);

  let toInt64: t => option(Int64.t);

  let toFloat: t => option(float);

  let toString: t => string;
};

/** Functions for manipulating pairs of values */
module Tuple: {
  /** Functions for manipulating pairs of values */

  type t('a, 'b) = ('a, 'b);

  /**
    [Tuple.make x y] makes a two-tuple with the given values.

    The values do not have to be of the same type.

    {[Tuple.make(3, 4) = (3, 4)]}

    {[
      let zip = (xs: List.t('a), ys: List.t('b)): List.t(('a, 'b)) =
        List.map2(xs, ys, ~f:Tuple.make);
    ]}
  */

  let make: ('a, 'b) => ('a, 'b);

  let ofArray: array('a) => option(('a, 'a));

  let ofList: list('a) => option(('a, 'a));

  /** Extract the first value from a tuple.

      [Tuple.first (3, 4) = 3]

      [Tuple.first ("john", "doe") = "john"]
  */
  let first: (('a, 'b)) => 'a;

  /** Extract the second value from a tuple.

      [Tuple.second (3, 4) = 4]

      [Tuple.second ("john", "doe") = "doe"]
  */
  let second: (('a, 'b)) => 'b;

  /** Transform the {!first} value in a tuple.

      {[Tuple.mapFirst ~f:String.reverse ("stressed", 16) = ("desserts", 16)]}

      {[Tuple.mapFirst ~f:String.length ("stressed", 16) = (8, 16)]}
  */
  let mapFirst: (('a, 'b), ~f: 'a => 'x) => ('x, 'b);

  /** Transform the second value in a tuple.

      [Tuple.mapSecond ~f:Float.squareRoot ("stressed", 16.) = ("stressed", 4.)]

      [Tuple.mapSecond ~f:(~-) ("stressed", 16) = ("stressed", -16)]
  */;
  let mapSecond: (('a, 'b), ~f: 'b => 'c) => ('a, 'c);

  /** Transform both values of a tuple, using [f] for the first value and [g] for the second.

       {[Tuple.mapEach ~f:String.reverse ~g:Float.squareRoot ("stressed", 16.) = ("desserts", 4.)]}

       {[Tuple.mapEach ~f:String.length ~g:(~-) ("stressed", 16) = (8, -16)]}
  */;
  let mapEach: (('a, 'b), ~f: 'a => 'x, ~g: 'b => 'y) => ('x, 'y);

  /** Transform both of the values of a tuple using the same function.

      [mapAll] can only be used on tuples which have the same type for each value.

      {[Tuple.mapAll ~f:succ (3, 4, 5) = (4, 5, 6)]}

      {[Tuple.mapAll ~f:String.length ("was", "stressed") = (3, 8)]}
  */;
  let mapAll: (('a, 'a), ~f: 'a => 'b) => ('b, 'b);

  /** Switches the first and second values of a tuple.

      [Tuple.swap (3, 4) = (4, 3)]

      [Tuple.swap ("stressed", 16) = (16, "stressed")]
  */
  let swap: (('a, 'b)) => ('b, 'a);

  /** [curry f] takes a function [f] which takes a single argument of a tuple ['a * 'b] and returns a function which takes two arguments that can be partially applied.

      [let squareArea (width, height) = width * height]

      [let curriedArea : float -> float -> float = curry squareArea]

      [let heights = [3, 4, 5]]

      [List.map widths ~f:(curriedArea 4) = [12; 16; 20]]
  */
  let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c;

  /** [uncurry f] takes a function [f] which takes two arguments and returns a function which takes a single argument of a 2-tuple

      [let sum (a : int) (b: int) : int = a + b]

      [let uncurriedSum : (int * int) -> int = uncurry add]

      [uncurriedSum (3, 4) = 7]
  */
  let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;

  /** Turns a tuple into an {!Array} of length two.

      This function can only be used on tuples which have the same type for each value.

      {[Tuple.toArray (3, 4) = [|3; 4|]]}

      {[Tuple.toArray ("was", "stressed") = [|"was"; "stressed"|]]}
  */
  let toArray: (('a, 'a)) => array('a);

  /** Turns a tuple into a list of length two. This function can only be used on tuples which have the same type for each value.

      [Tuple.toList (3, 4) = [3; 4]]

      [Tuple.toList ("was", "stressed") = ["was"; "stressed"]]
  */
  let toList: (('a, 'a)) => list('a);
};

/** Functions for manipulating trios of values */
module Tuple3: {
  /** Functions for manipulating trios of values */

  type t('a, 'b, 'c) = ('a, 'b, 'c);

  /** Create a {!Tuple3}.

    {[Tuple3.create 3 4 5 = (3, 4, 5)]}

    {[
      let zip3 (xs : 'a list) (ys : 'b list) (zs : 'c list) : ('a * 'b * 'c) list =
        List.map3 ~f:Tuple3.create xs ys zs
      in
      zip3 [1;2;3] ['a'; 'b'; 'c'] [4.; 5.; 6.] = [(1, 'a', 4.), (2, 'b', 5.), (3, 'c', 6.)]
    ]}
  */
  let make: ('a, 'b, 'c) => ('a, 'b, 'c);

  let ofArray: array('a) => option(('a, 'a, 'a));

  let ofList: list('a) => option(('a, 'a, 'a));

  /** Extract the first value from a tuple.

    {[Tuple3.first (3, 4, 5) = 3]}

    {[Tuple3.first ("john", "danger", "doe") = "john"]}
  */
  let first: (('a, 'b, 'c)) => 'a;

  /** Extract the second value from a tuple.

    {[Tuple.second (3, 4, 5) = 4]}

    {[Tuple.second ("john", "danger", "doe") = "danger"]}
  */
  let second: (('a, 'b, 'c)) => 'b;

  /** Extract the third value from a tuple.

    {[Tuple.third (3, 4, 5) = 5]}

    {[Tuple.third ("john", "danger", "doe") = "doe"]}
  */
  let third: (('a, 'b, 'c)) => 'c;

  /** Extract the first and second values of a {!Tuple3} as a {!Tuple}.

    {[Tuple3.initial (3, "stressed", false) = (3, "stressed")]}

    {[Tuple3.initial ("john", 16, "doe") = ("john", 16)]}
  */
  let initial: (('a, 'b, 'c)) => ('a, 'b);

  /** Extract the second and third values of a {!Tuple3} as a {!Tuple}.

    {[Tuple3.tail (3, "stressed", false) = ("stressed", false)]}

    {[Tuple3.tail ("john", 16, false) = (16, false)]}
  */
  let tail: (('a, 'b, 'c)) => ('b, 'c);

  /** Transform the first value in a tuple.

    {[Tuple3.mapFirst ~f:String.reverse ("stressed", 16, false) = ("desserts", 16, false)]}

    {[Tuple3.mapFirst ~f:String.length ("stressed", 16, false) = (8, 16, false)]}
  */
  let mapFirst: (('a, 'b, 'c), ~f: 'a => 'x) => ('x, 'b, 'c);

  /** Transform the second value in a tuple.

    {[Tuple3.mapSecond ~f:Float.squareRoot ("stressed", 16., false) = ("stressed", 4., false)]}

    {[Tuple3.mapSecond ~f:(~-) ("stressed", 16, false) = ("stressed", -16, false)]}
  */
  let mapSecond: (('a, 'b, 'c), ~f: 'b => 'y) => ('a, 'y, 'c);

  /** Transform the third value in a tuple.

    {[Tuple3.mapThird ~f:not ("stressed", 16, false) ("stressed", 16, true)]}
  */
  let mapThird: (('a, 'b, 'c), ~f: 'c => 'z) => ('a, 'b, 'z);

  /** Transform each value in a tuple by applying [f] to the {!first} value, [g] to the {!second} value and [h] to the {!third} value.

    {[
      Tuple3.mapEach
        ~f:String.reverse
        ~g:Float.squareRoot
        ~h:Bool.not
        ("stressed", 16., false) = ("desserts", 4., true)
    ]}
  */
  let mapEach:
    (('a, 'b, 'c), ~f: 'a => 'x, ~g: 'b => 'y, ~h: 'c => 'z) => ('x, 'y, 'z);

  /** Transform all the values of a tuple using the same function.

    [mapAll] can only be used on tuples which have the same type for each value.

    {[Tuple.mapAll ~f:Float.squareRoot (9., 16., 25.) = (3., 4., 5.)]}

    {[Tuple.mapAll ~f:String.length ("was", "stressed", "then") = (3, 8, 4)]}
  */
  let mapAll: (('a, 'a, 'a), ~f: 'a => 'b) => ('b, 'b, 'b);

  /** Move each value in the tuple one position to the left, moving the value in the first position into the last position.

    {[Tuple.rotateLeft (3, 4, 5) = (4, 5, 3)]}

    {[Tuple.rotateLeft ("was", "stressed", "then") = ("stressed", "then", "was")]}
  */
  let rotateLeft: (('a, 'b, 'c)) => ('b, 'c, 'a);

  /** Move each value in the tuple one position to the right, moving the value in the last position into the first position.

    {[Tuple.rotateRight (3, 4, 5) = (5, 3, 4)]}

    {[Tuple.rotateRight ("was", "stressed", "then") = ("then", "was", "stressed")]}
  */
  let rotateRight: (('a, 'b, 'c)) => ('c, 'a, 'b);

  let curry: ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd;
  /** [curry f] takes a function [f] which takes a single argument of a tuple ['a * 'b *'c] and returns a function which takes three arguments that can be partially applied.

    {[
      let cubeVolume (width, height, depth) = width * height * depth in
      let curriedVolume : float -> float -> float = curry squareArea in
      let depths = [3; 4; 5] in
      List.map depths ~f:(curriedVolume 3 4) = [36; 48; 60]
    ]}
  */

  let uncurry: (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd;
  /** [uncurry f] takes a function [f] which takes three arguments and returns a function which takes a single argument of a {!Tuple3}  */

  /** Turns a tuple into a {!List} of length three.

    This function can only be used on tuples which have the same type for each value.

    {[Tuple3.toArray (3, 4, 5) = [3; 4; 5]]}

    {[Tuple3.toArray ("was", "stressed", "then") = ["was"; "stressed"; "then"]]}
  */
  let toArray: (('a, 'a, 'a)) => array('a);

  /** Turns a tuple into a {!List} of length three.

    This function can only be used on tuples which have the same type for each value.

    {[Tuple3.toList (3, 4, 5) = [3; 4; 5]]}

    {[Tuple3.toList ("was", "stressed", "then") = ["was"; "stressed"; "then"]]}
  */
  let toList: (('a, 'a, 'a)) => list('a);
};

/** Functions for working with ["strings"] */
module String: {
  /** Functions for working with ["strings"] */

  type t = string;

  /** TODO explain what this is for */
  type identity;

  let ofChar: char => string;
  /** Converts the given character to an equivalent string of length one. */

  let ofArray: array(char) => t;

  let ofList: list(char) => t;
  /**
    Makes a new string from the given list of characters.

    Note that these must be individual characters in single quotes, not strings of length one.

    {[
      String.ofList [] = ""
      String.ofList ['a'; 'b'; 'c'] = "abc"
    ]}
  */

  let repeat: (string, ~count: int) => string;
  /**
    [String.repeat ~count:n s] makes a
    string with [n] repetitions of [s]. If [n] is negative,
    [String.repeat] throws a [RangeError] exception.

    {[
      String.repeat ~count:3 "ok" = "okokok"
      String.repeat ~count:3 "" = ""
      String.repeat ~count:0 "ok" = ""
    ]}
  */

  let initialize: (int, ~f: int => char) => string;

  let isEmpty: string => bool;

  let length: string => int;
  /**
    returns the length of the given string.
  */

  let uncons: string => option((char, string));
  /**
    [String.uncons s] returns [Some (head_ch, tail)],
    where the first element of the
    tuple is the first character of [s] and the second element is a string
    with the remaining characters of [s]. If given an empty string, [String.uncons]
    returns [None].

    {[
      String.uncons "abcde" = Some ('a', "bcde")
      String.uncons "a" = Some ('a', "")
      String.uncons "" = None
    ]}
  */

  let dropLeft: (~count: int, string) => string;
  /**

    {[
      String.dropLeft ~count:3 "abcdefg" = "defg"
      String.dropLeft ~count:0 "abcdefg" = "abcdefg"
      String.dropLeft ~count:7 "abcdefg" = ""
      String.dropLeft ~count:(-2) "abcdefg" = "fg"
      String.dropLeft ~count:8 "abcdefg" = ""
    ]}
  */

  let dropRight: (~count: int, string) => string;
  /**

    {[
      String.dropRight ~count:3 "abcdefg" = "abcd"
      String.dropRight ~count:0 "abcdefg" = "abcdefg"
      String.dropRight ~count:7 "abcdefg" = ""
      String.dropRight ~count:(-2) "abcdefg" = "abcdefg"
      String.dropRight ~count:8 "abcdefg" = ""
    ]}
  */

  let words: t => list(t);

  let lines: t => list(t);

  let split: (t, ~on: t) => list(t);
  /**
    {[
      String.split ~on:"/" "a/b/c" = ["a"; "b"; "c"]
      String.split ~on:"--" "a--b--c" = ["a"; "b"; "c"]
      String.split ~on:"/" "abc" = ["abc"]
      String.split ~on:"/" "" = [""]
      String.split ~on:"" "abc" = ["a"; "b"; "c"]
    ]}

  */

  let startsWith: (t, ~prefix: string) => bool;

  let endsWith: (t, ~suffix: string) => bool;

  let toLowercase: string => string;
  /**
    Converts all upper case letters in [s] to
    lower case. This function works only with ASCII characters,
    not Unicode.

    {[String.toLowercase "AaBbCc123" = "aabbcc123"]}
  */

  let toUppercase: string => string;
  /**
    [String.toUpper s] converts all lower case letters in [s] to
    upper case.  This function works only with ASCII characters,
    not Unicode.

    {[
    String.toUppercase "AaBbCc123" = "AABBCC123"
    ]}
  */

  let uncapitalize: string => string;
  /**
    [String.uncapitalize s] converts the first letter of [s] to lower case if it is upper case.
    This function works only with ASCII characters, not Unicode.
  */

  let capitalize: string => string;
  /**
    [String.capitalize s] converts
    the first letter of [s] to upper case if it is lower case.
    This function works only with ASCII characters,
    not Unicode.
  */

  let isCapitalized: string => bool;
  /**
    [isCapitalized s] returns [true] if  the first letter
    of [s] is upper case, [false] otherwise. This function
    works only with ASCII characters, not Unicode.
  */

  let includes: (t, ~substring: string) => bool;
  /**
    [String.includes ~substring:sub s]
    returns [true] if [sub] is contained anywhere in [s]; [false] otherwise.
    [String.includes ~substring:"" s] returns [true] for all [s].
  */

  let reverse: string => string;
  /**
    [String.reverse s] returns a new string
    with its characters in the reverse order of the characters in
    [s]. This function works with Unicode characters.
  */

  let slice: (~to_: int=?, t, ~from: int) => t;

  let trim: t => t;
  /**
    [String.trim s] returns a new string with
    leading and trailing whitespace (blank, tab, newline,non-breaking
    space and others as described in <https://www.ecma-international.org/ecma-262/5.1/#sec-7.2>)
    removed from [s].

    {[String.trim "  abc  " = "abc"]}
    {[String.trim "  abc def  " = "abc def"]}
    {[String.trim {js|\n\u00a0 \t abc \f\r \t|js} = "abc"]}
  */

  // let trimLeft: t => t;

  // let trimRight: t => t;

  let insertAt: (t, ~index: int, ~value: t) => t;
  /**
    [String.insertAt ~insert:ins, ~index:n, s)] returns a new string with the value [ins]
    inserted at position [n] in [s]. If [n] is less than zero, the position is evaluated as
    [(length s) - (n + 1)]. [n] is pinned to the range [0..length s].

    {[
    String.insertAt ~insert:"**" ~index:2 "abcde" = "ab**cde"
    String.insertAt ~insert:"**" ~index:0 "abcde" = "**abcde"
    String.insertAt ~insert:"**" ~index:5 "abcde" = "abcde**"
    String.insertAt ~insert:"**" ~index:(-2) "abcde" = "abc**de"
    String.insertAt ~insert:"**" ~index:(-9) "abcde" = "**abcde"
    String.insertAt ~insert:"**" ~index:9 "abcde" = "abcde**"
    ]}
  */

  // let forEach: (t, ~f: char => unit) => unit;

  // let fold: (t, ~initial: 'a, ~f: ('a, char) => 'a) => 'a;

  let toArray: string => array(char);

  let toList: string => list(char);
  /**
    [String.toList s] returns a list with the individual characters
    in the given string. Works with Unicode characters, but
    because they don't have a literal representation, there is
    no example here.

    {[
    String.toList "" = []
    String.toList "abc" = ['a'; 'b'; 'c']
    ]}
  */;
};

/** A collection of unique values */
module Set: {
  /**
    A {!Set} represents a unique collection of values.

    [Set] is an immutable data structure which means operations like {!Set.add} and {!Set.remove} do not modify the data structure, but return a new set with the desired changes.

    Since the usage is so common the {!Set.Int} and {!Set.String} modules are available, offering a convenient way to construct new sets.

    For other data types you can use {!Set.Poly} which uses OCaml's polymorphic [compare] function.

    The specialized modules {!Set.Int}, {!Set.String} are in general more efficient.
  */;

  type t('a, 'cmp);

  /** {1 Construction} */;

  /** A [Set] can be constructed using one of the functions available in the {!Set.Int}, {!Set.String} or {!Set.Poly} sub-modules. */;

  /** {1 Basic operations} */;

  /** Insert a value into a set.

    {[Set.add (Set.Int.ofList [1; 2]) 3 |> Set.toList = [1; 2; 3]]}

    {[Set.add (Set.Int.ofList [1; 2]) 2 |> Set.toList = [1; 2]]}
  */
  let add: (t('a, 'cmp), 'a) => t('a, 'cmp);

  /** Remove a value from a set, if the set doesn't contain the value anyway, returns the original set

    {[Set.remove (Set.Int.ofList [1; 2]) 2 |> Set.toList = [1]]}

    {[
      let originalSet = Set.Int.ofList [1; 2] in
      let newSet = Set.remove orignalSet 3 in
      originalSet == newSet
    ]}
  */
  let remove: (t('a, 'cmp), 'a) => t('a, 'cmp);

  /** Determine if a value is in a set

    {[Set.includes (Set.String.ofList ["Ant"; "Bat"; "Cat"]) "Bat" = true]}
  */
  let includes: (t('a, _), 'a) => bool;

  /** Determine the number of elements in a set.

    {[Set.length (Set.Int.ofList [1; 2; 3])) = 3]}
  */
  let length: t(_, _) => int;

  /** Returns, as an {!Option}, the first element for which [f] evaluates to [true]. If [f] doesn't return [true] for any of the elements [find] will return [None].

    {[Set.find ~f:Int.isEven (Set.Int.ofList [1; 3; 4; 8]) = Some 4]}

    {[Set.find ~f:Int.isOdd (Set.Int.ofList [0; 2; 4; 8]) = None]}

    {[Set.find ~f:Int.isEven Set.Int.empty = None]} */
  let find: (t('v, _), ~f: 'v => bool) => option('v);

  /** {1 Query} */;

  /** Check if a set is empty.

    {[Set.isEmpty (Set.Int.empty) = true]}

    {[Set.isEmpty (Set.Int.singleton 4) = false]}
  */
  let isEmpty: t(_, _) => bool;

  /** Determine if [f] returns true for [any] values in a set.

    {[Set.any (Set.Int.ofArray [|2;3|]) ~f:Int.isEven = true]}

    {[Set.any (Set.Int.ofList [1;3]) ~f:Int.isEven = false]}

    {[Set.any (Set.Int.ofList []) ~f:Int.isEven = false]} */
  let any: (t('v, _), ~f: 'v => bool) => bool;

  /** Determine if [f] returns true for [all] values in a set.

    {[Set.all ~f:Int.isEven (Set.Int.ofArray [|2;4|]) = true]}

    {[Set.all ~f:Int.isEven (Set.Int.ofLis [2;3]) = false]}

    {[Set.all ~f:Int.isEven Set.Int.empty = true]} */
  let all: (t('v, _), ~f: 'v => bool) => bool;

  /** {1 Combine} */;

  /** Returns a new set with the values from the first set which are not in the second set.

    {[Set.difference (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList = [1;5]]}

    {[Set.difference (Set.Int.ofList [2;3;4]) (Set.Int.ofList [1;2;5]) |> Set.toList = [3;4]]}
  */
  let difference: (t('a, 'cmp), t('a, 'cmp)) => t('a, 'cmp);

  /** Get the intersection of two sets. Keeps values that appear in both sets.

    {[Set.intersection (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList= [2]]}
  */
  let intersection: (t('a, 'cmp), t('a, 'cmp)) => t('a, 'cmp);

  /** Get the union of two sets. Keep all values.

    {[Set.union (Set.Int.ofList [1;2;5]) (Set.Int.ofList [2;3;4]) |> Set.toList = [1;2;3;4;5]]}
  */
  let union: (t('a, 'cmp), t('a, 'cmp)) => t('a, 'cmp);

  /** {1 Transform} */;

  /** Keep elements that [f] returns [true] for.

    {[Set.filter (Set.Int.ofList [1;2;3]) ~f:Int.isEven |> Set.toList = [2]]}
  */

  let filter: (t('a, 'cmp), ~f: 'a => bool) => t('a, 'cmp);

  /** Divide a set into two according to [f]. The first set will contain the values that [f] returns [true] for, values that [f] returns [false] for will end up in the second. */
  let partition:
    (t('a, 'cmp), ~f: 'a => bool) => (t('a, 'cmp), t('a, 'cmp));

  /** Transform a set into a value which is result of running each element in the set through [f], where each successive invocation is supplied the return value of the previous.

    See {!Array.fold} for a more in-depth explanation.

    {e Examples}

    {[Set.fold ~f:( * ) ~initial:1 (Set.Int.ofList [1;2;3;4]) = 24]}
  */
  let fold: (t('a, _), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;

  /** Runs a function [f] against each element of the set. */
  let forEach: (t('a, _), ~f: 'a => unit) => unit;

  /** {1 Conversion} */;

  /** Converts a set into an {!Array}. */

  let toArray: t('a, _) => array('a);

  /** Converts a set into a {!List}. */

  let toList: t('a, _) => list('a);

  /** Construct sets which can hold any data type using the polymorphic [compare] function */

  module Poly: {
    type identity;

    type nonrec t('a) = t('a, identity);

    /** The empty set */

    let empty: unit => t('a);

    /** Create a set of a single value
      {[Set.Int.singleton (5, "Emu") |> Set.toList = [(5, "Emu")]]}
    */

    let singleton: 'a => t('a);

    /** Create a set from an {!Array}
      {[Set.Poly.ofArray [(1, "Ant");(2, "Bat");(2, "Bat")] |> Set.toList = [(1, "Ant"); (2, "Bat")]]}
    */

    let ofArray: array('a) => t('a);

    /** Create a set from a {!List}
      {[Set.Poly.ofList [(1, "Ant");(2, "Bat");(2, "Bat")] |> Set.toList = [(1, "Ant"); (2, "Bat")]]}
    */

    let ofList: list('a) => t('a);
  };

  /** Construct sets of {!Int}s */

  module Int: {
    type nonrec t = t(Int.t, Int.identity);

    /** A set with nothing in it. */

    let empty: t;

    /** Create a set from a single {!Int}
      {[Set.Int.singleton 5 |> Set.toList = [5]]}
    */

    let singleton: int => t;

    /** Create a set from an {!Array}
      {[Set.Int.ofArray [|1;2;3;3;2;1;7|] |> Set.toArray = [|1;2;3;7|]]}
    */

    let ofArray: array(int) => t;

    /** Create a set from a {!List}
      {[Set.Int.ofList [1;2;3;3;2;1;7] |> Set.toList = [1;2;3;7]]}
    */

    let ofList: list(int) => t;
  };

  /** Construct sets of {!String}s */

  module String: {
    type nonrec t = t(String.t, String.identity);

    /** A set with nothing in it. */

    let empty: t;

    /** Create a set of a single {!String}
      {[Set.String.singleton "Bat" |> Set.toList = ["Bat"]]}
    */

    let singleton: String.t => t;

    /** Create a set from an {!Array}
      {[
      Set.String.ofArray [|"a";"b";"g";"b";"g";"a";"a"|] |> Set.toArray = [|"a";"b";"g"|]
      ]}
    */

    let ofArray: array(String.t) => t;

    /** Create a set from a {!List}
      {[Set.String.ofList [|"a";"b";"g";"b";"g";"a";"a"|] |> Set.toList = ["a";"b";"g"]]}
    */

    let ofList: list(String.t) => t;
  };
};

/** A collection of key-value pairs */
module Map: {
  /** A [Map] represents a unique mapping from keys to values.

  [Map] is an immutable data structure which means operations like {!Map.add} and {!Map.remove} do not modify the data structure, but return a new map with the desired changes.

  Since the usage is so common the {!Map.Int} and {!Map.String} modules are available, offering a convenient way to construct new Maps.

  For other data types you can use {!Map.Poly} which internally uses OCaml's polymorphic [compare] function on the keys.

  The specialized modules {!Map.Int}, {!Map.String} are in general more efficient. */;

  /* TODO explain the type */
  type t('key, 'value, 'cmp);

  /** {1 Construction} */;

  /** A [Map] can be constructed using one of the functions available in {!Map.Int}, {!Map.String} or {!Map.Poly}. */;

  let empty: comparator('k, 'id) => t('k, 'v, 'id);

  /** {1 Basic operations} */;

  let add: (t('k, 'v, 'cmp), ~key: 'k, ~value: 'v) => t('k, 'v, 'cmp);
  /** Adds a new entry to a map. If [key] is allready present, its previous value is replaced with [value].
    {[Map.add (Map.Int.ofList [(1, "Ant"), (2, "Bat")]) ~key:3 ~value:"Cat"  |> Map.toList = [(1, "Ant"), (2, "Bat"), (3, "Cat")]]}

    {[Map.add (Map.Int.ofList [(1, "Ant"), (2, "Bat")]) ~key:2 ~value:"Bug" |> Map.toList = [(1, "Ant"), (2, "Bug")]]}
  */

  let remove: (t('k, 'v, 'cmp), 'k) => t('k, 'v, 'cmp);
  /** Removes a key-value pair from a map based on they provided key. */

  let get: (t('k, 'v, 'cmp), 'k) => option('v);
  /** Get the value associated with a key. If the key is not present in the map, returns [None]. */

  let find:
    (t('k, 'v, _), ~f: (~key: 'k, ~value: 'v) => bool) => option(('k, 'v));
  /** Returns, as an {!Option} the first key-value pair for which [f] evaluates to true. If [f] doesn't return [true] for any of the elements [find] will return [None]. */

  let update:
    (t('k, 'v, 'cmp), ~key: 'k, ~f: option('v) => option('v)) =>
    t('k, 'v, 'cmp);
  /** Update the value for a specific key using [f]. If [key] is not present in the map [f] will be called with [None]. */

  let length: t(_, _, _) => int;
  /** Returns the number of key-value pairs present in the map. */

  let minimum: t('key, _, _) => option('key);
  /** Returns the smallest {b key } in the map. */

  let maximum: t('key, _, _) => option('key);
  /** Returns the largest {b key } in the map. */

  let extent: t('key, _, _) => option(('key, 'key));
  /** Returns a tuple [(minimum, maximum)] {b key } in the map. */;

  /** {1 Checks} */;

  let isEmpty: t(_, _, _) => bool;
  /** Determine if a map is empty. */

  /** Determine if a map includes [key].  */
  let includes: (t('k, _, _), 'k) => bool;

  /** Determine if [f] returns [true] for [any] values in a map. */
  let any: (t(_, 'v, _), ~f: 'v => bool) => bool;

  /** Determine if [f] returns [true] for [all] values in a map. */
  let all: (t(_, 'v, _), ~f: 'v => bool) => bool;

  /** {1 Combine} */;

  let merge:
    (
      t('k, 'v1, 'cmp),
      t('k, 'v2, 'cmp),
      ~f: ('k, option('v1), option('v2)) => option('v3)
    ) =>
    t('k, 'v3, 'cmp);
  /**
    Combine two maps. You provide a function [f] which is provided the key and the optional value from each map and needs to account for the three possibilities:

    1. Only the 'left' map includes a value for the key.
    2. Both maps contain a value for the key.
    3. Only the 'right' map includes a value for the key.

    You then traverse all the keys, building up whatever you want.
  */;

  /** {1 Transformations} */;

  let map: (t('k, 'v, 'cmp), ~f: 'v => 'b) => t('k, 'b, 'cmp);

  let mapI: (t('k, 'va, 'i), ~f: ('k, 'va) => 'vb) => t('k, 'vb, 'i);

  let filter: (t('k, 'v, 'cmp), ~f: 'v => bool) => t('k, 'v, 'cmp);
  /** Keep elements that [f] returns [true] for. */

  let partition:
    (t('k, 'v, 'cmp), ~f: (~key: 'k, ~value: 'v) => bool) =>
    (t('k, 'v, 'cmp), t('k, 'v, 'cmp));
  /** Divide a map into two, the first map will contain the key-value pairs that [f] returns [true] for, pairs that [f] returns [false] for will end up in the second. */

  let fold:
    (t('k, 'v, _), ~initial: 'a, ~f: ('a, ~key: 'k, ~value: 'v) => 'a) => 'a;

  let forEach: (t(_, 'v, _), ~f: 'v => unit) => unit;
  /** Runs a function [f] against each value in the map. */;

  /** {1 Conversion} */;

  let keys: t('k, _, _) => list('k);
  /** Get a {!List} of all of the keys in a map. */

  let values: t(_, 'v, _) => list('v);
  /** Get a {!List} of all of the values in a map. */

  let toArray: t('key, 'value, _) => array(('key, 'value));
  /** Get an {!Array} of all of the key-value pairs in a map. */

  let toList: t('key, 'value, _) => list(('key, 'value));
  /** Get a {!List} of all of the key-value pairs in a map. */

  /** Construct a Map which can be keyed by any data type using the polymorphic [compare] function. */
  module Poly: {
    type identity;

    type nonrec t('key, 'value) = t('key, 'value, identity);

    /** A map with nothing in it. */
    let empty: unit => t('k, 'v);

    /** Create a map from a key and value
      {[Map.Poly.singleton ~key:false ~value:1 |> Map.toList = [(false, 1)]]}
    */
    let singleton: (~key: 'k, ~value: 'v) => t('k, 'v);

    /** Create a map from an {!Array} of key-value tuples */
    let ofArray: array(('key, 'value)) => t('key, 'value);

    /** Create a map from a {!List} of key-value tuples */
    let ofList: list(('key, 'value)) => t('key, 'value);
  };

  /** Construct a Map with {!Int}s for keys. */
  module Int: {
    type nonrec t('value) = t(Int.t, 'value, Int.identity);

    /** A map with nothing in it. */
    let empty: t('value);

    /** Create a map from a key and value
      {[Map.Int.singleton ~key:1 ~value:"Ant" |> Map.toList = [(1, "Ant")]]}
    */
    let singleton: (~key: int, ~value: 'v) => t('v);

    /** Create a map from an {!Array} of key-value tuples */
    let ofArray: array((int, 'value)) => t('value);

    /** Create a map of a {!List} of key-value tuples */
    let ofList: list((int, 'value)) => t('value);
  };

  /** Construct a Map with {!String}s for keys. */
  module String: {
    type nonrec t('value) = t(String.t, 'value, String.identity);

    /** A map with nothing in it. */
    let empty: t('value);

    /** Create a map from a key and value
      {[Map.String.singleton ~key:"Ant" ~value:1 |> Map.toList = [("Ant", 1)]]}
    */
    let singleton: (~key: string, ~value: 'v) => t('v);

    /** Create a map from an {!Array} of key-value tuples */
    let ofArray: array((string, 'value)) => t('value);

    /** Create a map from a {!List} of key-value tuples */
    let ofList: list((string, 'value)) => t('value);
  };
};

/** A fixed lenfth collection of values */
module Array: {
  /** A mutable vector of elements which must have the same type.

      Has constant time (O(1)) {!get}, {!set} and {!length} operations.

      Arrays have a fixed length, if you want to be able to add an arbitrary number of elements maybe you want a {!List}. */
  type t('a) = array('a);

  /** {1 Create}

      You can create an [array] in OCaml with the [[|1; 2; 3|]] syntax. */

  let empty: t('a);
  /** An empty array.

      {[Array.empty = [||]]}

      {[Array.length Array.empty = 0]} */

  /** Create an array with only one element.
      {[Array.singleton 1234 = [|1234|]]}

      {[Array.singleton "hi" = [|"hi"|]]} */
  let singleton: 'a => t('a);

  /** Initialize an array. [Array.initialize n ~f] creates an array of length [n] with
      the element at index [i] initialized to the result of [(f i)].

      {[Array.initialize 4 ~f:identity = [|0; 1; 2; 3|]]}

      {[Array.initialize 4 ~f:(fun n -> n * n) = [|0; 1; 4; 9|]]} */
  let initialize: (int, ~f: int => 'a) => t('a);

  /** Creates an array of length [length] with the value [x] populated at each index.

      {[Array.repeat ~length:5 'a' = [|'a'; 'a'; 'a'; 'a'; 'a'|]]}

      {[Array.repeat ~length:0 7 = [||]]}

      {[Array.repeat ~length:(-1) "Why?" = [||]]} */
  let repeat: ('a, ~length: int) => t('a);

  /** Creates an array containing all of the integers from [from] if it is provided or [0] if not, up to but not including [to]

      {[Array.range 5 = [|0; 1; 2; 3; 4|] ]}

      {[Array.range ~from:2 5 = [|2; 3; 4|] ]}

      {[Array.range ~from:(-2) 3 = [|-2; -1; 0; 1; 2|] ]} */
  // TODO support inclusive / exclusive bounds?
  let range: (~from: int=?, int) => t(int);

  /** Create an array from a {!List}.

      {[Array.fromList [1;2;3] = [|1;2;3|]]} */
  let ofList: list('a) => t('a);

  let clone: t('a) => t('a);

  /** {!1 Query} */

  /** Return the length of an array.

      {[Array.length [|1; 2, 3|] = 3]}

      {[Array.length [||] = 0]} */
  let length: t('a) => int;

  let isEmpty: t('a) => bool;

  /** {1 Basic operations} */

  /** [Array.get a n] returns, the element at index number [n] of array [a].

    The first element has index number 0.

    The last element has index number [Array.length a - 1].

    You can also write [a.(n)] instead of [Array.get n a]

    {3 Exceptions}

    Raises [Invalid_argument "index out of bounds"] for indexes outside of the bounds of the array.

    {3 Examples}

    {[let 2 = [|1,2,3,2,1|][3];]}
  */
  let get: (t('a), int) => 'a;

  /** [Array.get a n] returns, as an {!Option}, the element at index number [n] of array [a].

      Returns [None] if [n] is outside the range [0] to [(Array.length a - 1)].

      {[
        let animals = [|"cat"; "dog"; "eel"|] in
        Array.get animals 2 = Some "eel";
        animals.(2) = Some "eel"
      ]}

      {[Array.getAt([|0; 1; 2|], ~index=5) == None]}

      {[Array.getAt([||], ~index=0) == None]}
  */
  let getAt: (t('a), ~index: int) => option('a);

  let first: t('a) => option('a);

  /**
    Get the last element of a list.

    Returns [None] if the list is empty.

    {[List.last [1;2;3] = Some 3]}

    {[List.last [1] = Some 1]}

    {[List.last [] = None]}
  */
  let last: t('a) => option('a);

  /** {!1 Manipulate} */

  /** [Array.set a index value] modifies array [a] in place, replacing the element at [index] with [value].

      You can also write [a.(index) <- value] instead of [Array.set a index value].

      Raises [Invalid_argument "index out of bounds"] if [n] is outside the range [0] to [Array.length a - 1].

      {[
        let numbers = [|1;2;3|] in

        numbers.(2) <- 0;
        Array.set numbers index 1;

        numbers = [|1;0;0|]
      ]}
  */
  let set: (t('a), int, 'a) => unit;

  let setAt: (t('a), ~index: int, ~value: 'a) => unit;

  let sum: (t('a), (module Container.Sum with type t = 'a)) => 'a;

  let count: (t('a), ~f: 'a => bool) => int;

  /** Find the smallest element using the provided [compare] function.

    Returns [None] if called on an empty array.

    {[Array.minimum [|7;5;8;6|] ~compare:compare = Some 5]}
  */
  let minimum: (t('a), ~compare: ('a, 'a) => int) => option('a);

  /** Find the largest element using the provided [compare] function.

    Returns [None] if called on an empty array.

    {[Array.maximum [|7;5;8;6|] ~compare:compare = Some 8]}
  */
  let maximum: (t('a), ~compare: ('a, 'a) => int) => option('a);

  let extent: (t('a), ~compare: ('a, 'a) => int) => option(('a, 'a));

  /** {!1 Transform} */

  let zip: (t('a), t('b)) => t(('a, 'b));

  /** Keep elements that [f] returns [true] for.

    {[Array.filter ~f:Int.isEven [|1; 2; 3; 4; 5; 6|] = [|2; 4; 6|]]}
  */
  let filter: (t('a), ~f: 'a => bool) => t('a);

  /** Allows you to combine {!map} and {!filter} into a single pass.

    The output array only contains elements for which [f] returns [Some].

    {[Array.filterMap [|3; 4; 5; 6|] ~f:(fun number ->
      if Int.isEven number then
        Some (number * number)
      else
        None
    ) = [16; 36]}

    Why [filterMap] and not just {!filter} then {!map}?

    {!filterMap} removes the {!Option} layer automatically.
    If your mapping is already returning an {!Option} and you want to skip over [None]s, then [filterMap] is much nicer to use.

    TODO a good example of this.
  */
  let filterMap: (t('a), ~f: 'a => option('b)) => t('b);

  /** Returns an array of arrays (i.e., groups) whose concatenation is equal to the original array.

    Each group is broken where [break] returns true on a pair of successive elements.

    {[
      Array.group ~break:(<>) [|'M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'|] =
        [|[|'M'|];[|'i'|];[|'s';'s'|];[|'i'|];[|'s';'s'|];[|'i'|];[|'p';'p'|];[|'i'|]|]
    ]}
  */
  // let group: (t('a), ~break: ('a, 'a) => bool) => t(t('a));

  let groupBy:
    (t('a), comparator('b, 'id), ~f: 'a => 'b) => Map.t('b, list('a), 'id);

  /** [Array.swap array i j] swaps the value at index [i] with the value at index [j].

      {[Array.swap([|1;2;3|], 1, 2) == [|1;3;2|]]} */
  let swap: (t('a), int, int) => unit;

  /** {1 Transform} */

  /** Create a new array which is the result of applying a function [f] to every element.

    {[Array.map ~f:Float.squareRoot [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]} */
  let map: (t('a), ~f: 'a => 'b) => t('b);

  /** Apply a function [f] to every element with its index as the first argument.

    {[Array.mapWithIndex ~f:( * ) [|5; 5; 5|] = [|0; 5; 10|]]} */
  let mapI: (t('a), ~f: (int, 'a) => 'b) => t('b);

  /** Combine two arrays, using [f] to combine each pair of elements.
    If one array is longer, the extra elements are dropped.

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
  */
  let map2: (t('a), t('b), ~f: ('a, 'b) => 'c) => t('c);

  /** Combine three arrays, using [f] to combine each {!Tuple3} of elements.
    If one array is longer, the extra elements are dropped.

    {[
      Array.map3
        ~f:Tuple3.create
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8;|]
        [|true; false; true; false|] =
          [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
    ]}
  */
  let map3: (t('a), t('b), t('c), ~f: ('a, 'b, 'c) => 'd) => t('d);

  /**
    {3 Examples}

    {[Array.bind ~f xs = Array.map ~f xs |> Array.concatenate]}

    {[Array.bind ~f:(fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]} */
  let bind: (t('a), ~f: 'a => t('b)) => t('b);

  let chunksOf: (t('a), ~size: int) => t(t('a));

  /** Provides a sliding 'window' of sub-arrays over an array.

    The first sub-array starts at index [0] of the array and takes the first [size] elements.

    The sub-array then advances the index [step] (which defaults to 1) positions before taking the next [size] elements.

    The sub-arrays are guaranteed to always be of length [size] and iteration stops once a sub-array would extend beyond the end of the array.

    {[Array.sliding [|1;2;3;4;5|] ~size:1 = [|[|1|]; [|2|]; [|3|]; [|4|]; [|5|]|] ]}

    {[Array.sliding [|1;2;3;4;5|] ~size:2 = [|[|1;2|]; [|2;3|]; [|3;4|]; [|4;5|]|] ]}

    {[Array.sliding [|1;2;3;4;5|] ~size:3 = [|[|1;2;3|]; [|2;3;4|]; [|3;4;5|]|] ]}

    {[Array.sliding [|1;2;3;4;5|] ~size:2 ~step:2 = [|[|1;2|]; [|3;4|]|] ]}

    {[Array.sliding [|1;2;3;4;5|] ~size:1 ~step:3 = [|[|1|]; [|4|]|] ]}
  */
  let sliding: (~step: int=?, t('a), ~size: int) => t(t('a));

  /** Returns, as an {!Option}, the first element for which [f] evaluates to [true].

    If [f] doesn't return [true] for any of the elements [find] will return [None]

    {[Array.find ~f:Int.isEven [|1; 3; 4; 8;|] = Some 4]}

    {[Array.find ~f:Int.isOdd [|0; 2; 4; 8;|] = None]}

    {[Array.find ~f:Int.isEven [||] = None]} */
  let find: (t('a), ~f: 'a => bool) => option('a);

  /** Similar to {!Array.find} but [f] is also called with the current index, and the return value will be a tuple of the index the passing value was found at and the passing value.

    {[Array.findIndex ~f:(fun index number -> index > 2 && Int.isEven number) [|1; 3; 4; 8;|] = Some (3, 8)]}
  */
  let findIndex: (t('a), ~f: (int, 'a) => bool) => option((int, 'a));

  let any: (t('a), ~f: 'a => bool) => bool;

  /** Determine if [f] returns true for [all] values in an array.

      {[Array.all ~f:Int.isEven [|2;4|] = true]}

      {[Array.all ~f:Int.isEven [|2;3|] = false]}

      {[Array.all ~f:Int.isEven [||] = true]}
  */
  let all: (t('a), ~f: 'a => bool) => bool;

  /** Test if an array contains the specified element using the provided [equal] to test for equality.

      {[Array.contains([1; 2; 3], 2, ~equal:(=)) == true]}
  */
  let includes: (t('a), 'a, ~equal: ('a, 'a) => bool) => bool;

  /** {1 Combine} */

  /** Creates a new array which is the result of appending the second array onto the end of the first.

    {[
      let fortyTwos = Array.repeat ~length:2 42 in
      let eightyOnes = Array.repeat ~length:3 81 in
      Array.append fourtyTwos eightyOnes = [|42; 42; 81; 81; 81|];
    ]}
  */
  let append: (t('a), t('a)) => t('a);

  /** Concatenate an array of arrays into a single array:

    {[Array.concatenate [|[|1; 2|]; [|3|]; [|4; 5|]|] = [|1; 2; 3; 4; 5|]]}
  */
  let concatenate: t(t('a)) => t('a);

  /** {1 Deconstruct} */

  /** Split an array into a {!Tuple} of arrays. Values which [f] returns true for will end up in {!Tuple.first}.

    {[Array.partition [1;2;3;4;5;6] ~f:Int.isOdd = ([1;3;5], [2;4;6])]}
  */
  let partition: (t('a), ~f: 'a => bool) => (t('a), t('a));

  /** Divides an array into a {!Tuple} of arrays.

    Elements which have index upto (but not including) [index] will be in the first component of the tuple.

    Elements with an index greater than or equal to [index] will be in the second.

    {[Array.splitAt [1;2;3;4;5] ~index:2 = ([1;2], [3;4;5])]}
  */
  /* TODO what about negative indicies*/
  let splitAt: (t('a), ~index: int) => (t('a), t('a));

  /** Divides an array at the first element [f] returns [true] for. */
  let splitWhen: (t('a), ~f: 'a => bool) => (t('a), t('a));

  /** Decompose an array of {!Tuple}s into a {!Tuple} of arrays.

    {[Array.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  */
  let unzip: t(('a, 'b)) => (t('a), t('b));

  /** Decompose an array of {!Tuple3}s into a {!Tuple3} of arrays.

    {[Array.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  */
  let unzip3: t(('a, 'b, 'c)) => (t('a), t('b), t('c));

  /** Places [sep] between all the elements of the given array.

    {[Array.intersperse ~sep:"on" [|"turtles"; "turtles"; "turtles"|] = [|"turtles"; "on"; "turtles"; "on"; "turtles"|]]}

    {[Array.intersperse ~sep:0 [||] = [||]]}
  */
  let intersperse: (t('a), ~sep: 'a) => t('a);

  /** Get a sub-section of a list. [from] is a zero-based index where we will start our slice.
    The [to_] is a zero-based index that indicates the end of the slice.

    The slice extracts up to but not including [to_].

    {[Array.slice ~from:0 ~to_:3 [0; 1; 2; 3; 4] = [0; 1; 2]]}

    {[Array.slice ~from:1 ~to_:4 [0; 1; 2; 3; 4] = [1; 2; 3]]}

    {[Array.slice ~from:5 ~to_:3 [0; 1; 2; 3; 4] = []]}

    Both the [from] and [to_] indexes can be negative, indicating an offset from the end of the list.

    {[Array.slice ~from:1 ~to_:(-1) [0; 1; 2; 3; 4] = [1; 2; 3]]}

    {[Array.slice ~from:(-2) ~to_:5 [0; 1; 2; 3; 4] = [3; 4]]}

    {[Array.slice ~from:(-2) ~to_:(-1) [0; 1; 2; 3; 4] = [3]]}
  */
  let slice: (~to_: int=?, t('a), ~from: int) => t('a);

  /**
    [fold] takes two arguments, an [initial] value and a function [f].

    For each element of the array [f] will be called with two arguments; an 'accumulator' and an element.

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

    {b Examples continued}

    {[Array.fold [|1; 2; 3|] ~initial:[] ~f:(List.cons) = [3; 2; 1]]}

    {[
      let unique integers =
        Array.fold integers ~initial:Set.Int.empty ~f:Set.add |> Set.toArray
      in
      unique [|1; 1; 2; 2; 3|] = [|1; 2; 3|]
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
  */
  let fold: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;

  /** This method is like {!fold} except that it iterates over the elements of the array from last to first.

    {[Array.foldRight ~f:(+) ~initial:0 (Array.repeat ~length:3 5) = 15]}

    {[Array.foldRight ~f:List.cons ~initial:[] [|1; 2; 3|] = [1; 2; 3]]}
  */
  let foldRight: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;

  let shuffle: t('a) => unit;

  /** Reverses an array {b in place}, mutating the existing array.

    {[
      let numbers = [|1; 2; 3|] in
      Array.reverse numbers
      numbers = [|3; 2; 1|];
    ]}
  */
  let reverse: t('a) => unit;

  /** Sort in place, modifying the existing array, using the provided [compare] function to determine order.

    On native it uses {{: https://en.wikipedia.org/wiki/Merge_sort } merge sort} which means the sort is stable,
    runs in constant heap space, logarithmic stack space and n * log (n) time.

    When targeting javascript the time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

    {[Array.sortInPlace [|5;6;8;3;6|] ~compare:compare = [|3;5;6;6;8|]]}
  */
  let sort: (t('a), ~compare: ('a, 'a) => int) => unit;

  /** {1 Iterate} */

  /** Iterates over the elements of invokes [f] for each element.

    {[Array.forEach [|1; 2; 3|] ~f:(fun int -> print (Int.toString int))]} */

  let forEach: (t('a), ~f: 'a => unit) => unit;

  /** Iterates over the elements of invokes [f] for each element.

    {[
      Array.forEachI [|1; 2; 3|] ~f:(fun index int -> printf "%d: %d" index int)
      (*
        0: 1
        1: 2
        2: 3
      *)
    ]}
  */
  let forEachI: (t('a), ~f: (int, 'a) => unit) => unit;

  // TODOs
  let values: t(option('a)) => t('a);

  /** {1 Convert} */

  let join: (t(string), ~sep: string) => string;

  /** Create a {!List} of elements from an array.

    {[Array.toList [|1;2;3|] = [1;2;3]]}

    {[Array.toList (Array.fromList [3; 5; 8]) = [3; 5; 8]]} */
  let toList: t('a) => list('a);

  /**  Create an indexed {!List} from an array. Each element of the array will be paired with its index as a {!Tuple}.

    {[Array.toIndexedList [|"cat"; "dog"|] = [(0, "cat"); (1, "dog")]]} */
  let toIndexedList: t('a) => list((int, 'a));
};

/** Arbitrary length, singly linked lists */
module List: {
  /**
    Immutable singly-linked list of elements which must have the same type.

    Has constant time (O(1)) {!head}, {!tail} and {!cons} operations.

    Slow (i.e., O(n)) access to the back of the list.

    Lists have inefficent {!getAt} and {!length} operations.

    If those are important to your use-case, perhaps you need an {!Array}.
  */
  /**
   * Lists in ReasonML are [immutable](TODO) [linked lists]() which means they have
      Fast (O(1)) operations:
      Accessing the first element
      Adding an element at the front (TODO what does front mean here?)

      Lists are slow when:
      - You need to access an element that isn't at the front of the list
      - You need to add an element to the middle or
      - Counting how many elements are in the list

      If you need to support random access you might want an {!Array}.

      They also support exhaustive pattern matching
      {[
      //   switch(aList) {
      //   | [] => "Empty"
      //   | [a] => "Exactly one element"
      //   | [a, b] => "Exactly two elements"
      //   | [a, b, ...cs] => "More than two elements"
      //   }
      ]}
   */

  type t('a) = list('a);

  /** {1 Create}

    You can create a [list] with the [[1;2;3]] syntax.
  */

  /** An empty list.

    {[List.empty = []]}

    {[List.length List.empty = 0]} */
  let empty: t('a);

  /** Create a list with only one element.

    {[List.singleton 1234 = [1234]]}

    {[List.singleton "hi" = ["hi"]]}
  */
  let singleton: 'a => t('a);

  /** Creates a list of length [times] with the value [x] populated at each index.

    {[List.repeat ~times:5 'a' = ['a'; 'a'; 'a'; 'a'; 'a']]}

    {[List.repeat ~times:0 7 = []]}

    {[List.repeat ~times:(-1) "Why?" = []]}
  */
  let repeat: ('a, ~times: int) => t('a);

  /** Creates a list containing all of the integers from [from] if it is provided or [0] if not, up to but not including [to]

    {[List.range 5 = [0; 1; 2; 3; 4] ]}

    {[List.range ~from:2 5 = [2; 3; 4] ]}

    {[List.range ~from:(-2) 3 = [-2; -1; 0; 1; 2] ]}
  */
  let range: (~from: int=?, int) => t(int);

  /** Initialize a list.

    [List.initialize n ~f] creates a list of length [n] with the [i]th element initialized to the result of [(f i)].

    {[List.initialize 4 ~f:identity = [0; 1; 2; 3]]}

    {[List.initialize 4 ~f:(fun n -> n * n) = [0; 1; 4; 9]]}
  */
  let initialize: (int, ~f: int => 'a) => t('a);

  /** Create a list from an {!Array}.

    {[List.ofArray [|1;2;3|] = [1;2;3]]}
  */
  let ofArray: array('a) => t('a);

  /** {1 Basic operations} */

  /** Returns, as an {!Option}, the first element of a list.

    If the list is empty, returns [None]

    {[List.head [1;2;3] = Some 1]}

    {[List.head [] = None]}
  */
  let head: t('a) => option('a);

  /** Returns, as an {!Option}, a list without its first element.

    If the list is empty, returns [None]

    {[List.tail [1;2;3] = Some [2;3]]}

    {[List.tail [1] = Some []]}

    {[List.tail [] = None]}
  */
  let tail: t('a) => option(t('a));

  /** {1 Combine} */

  /** Creates a new list which is the result of appending the second list onto the end of the first.
    {[
      let fortyTwos = List.repeat ~length:2 42 in
      let eightyOnes = List.repeat ~length:3 81 in
      List.append fourtyTwos eightyOnes = [42; 42; 81; 81; 81];
    ]}
  */
  let append: (t('a), t('a)) => t('a);

  /** Concatenate a list of lists into a single list:

    {[List.concatenate [[1; 2]; [3]; [4; 5]] = [1; 2; 3; 4; 5]]} */
  let concatenate: t(t('a)) => t('a);

  let sum: (t('a), (module Container.Sum with type t = 'a)) => 'a;

  /** {1 Transform} */;

  /** Create a new list which is the result of applying a function [f] to every element.

    {[List.map ~f:sqrt [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]}
  */
  let map: (t('a), ~f: 'a => 'b) => t('b);

  /** Apply a function [f] onto a list and concatenate the resulting list of lists.

    {[List.bind ~f xs = List.map ~f xs |> List.concatenate]}

    {[List.bind ~f:(fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]} */
  let bind: (t('a), ~f: 'a => t('b)) => t('b);

  let mapI: (t('a), ~f: (int, 'a) => 'b) => t('b);
  /**
    Apply a function [f] to every element and its index.

    {[
      List.mapI(
        ["zero", "one", "two"],
        ~f=(idex,  element) => (Int.toString(idx)) ++ ": " ++ element,
      ) =
        ["0: zero"; "1: one"; "2: two"]
    ]}
  */

  /** Combine two lists, using [f] to combine each pair of elements.
    If one list is longer, the extra elements are dropped.

    {[
      let totals (xs : int list) (ys : int list) : int list =
        List.map2 ~f:(+) xs ys in

      totals [|1;2;3|] [|4;5;6|] = [|5;7;9|]
    ]}

    {[
      List.map2
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8|]
        ~f:Tuple.create
          = [|("alice",2); ("bob",5); ("chuck",7)|]
    ]}
  */
  let map2: (t('a), t('b), ~f: ('a, 'b) => 'c) => t('c);

  /** Combine three lists, using [f] to combine each {!Tuple3} of elements.
    If one list is longer, the extra elements are dropped.

    {[
      List.map3
        ~f:Tuple3.create
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8;|]
        [|true; false; true; false|] =
          [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
    ]}
  */
  let map3: (t('a), t('b), t('c), ~f: ('a, 'b, 'c) => 'd) => t('d);

  /** Provides a sliding 'window' of sub-lists over a list.

    The first sub-list starts at the head of the list and takes the first [size] elements.

    The sub-list then advances [step] (which defaults to 1) positions before taking the next [size] elements.

    The sub-lists are guaranteed to always be of length [size] and iteration stops once a sub-list would extend beyond the end of the list.

    {[List.sliding [1;2;3;4;5] ~size:1 = [[1]; [2]; [3]; [4]; [5]] ]}

    {[List.sliding [1;2;3;4;5] ~size:2 = [[1;2]; [2;3]; [3;4]; [4;5]] ]}

    {[List.sliding [1;2;3;4;5] ~size:3 = [[1;2;3]; [2;3;4]; [3;4;5]] ]}

    {[List.sliding [1;2;3;4;5] ~size:2 ~step:2 = [[1;2]; [3;4]] ]}

    {[List.sliding [1;2;3;4;5] ~size:1 ~step:3 = [[1]; [4]] ]}

    {[List.sliding [1;2;3;4;5] ~size:2 ~step:3 = [[1; 2]; [4; 5]]]}

    {[List.sliding [1;2;3;4;5] ~size:7 = []]}
  */
  let sliding: (~step: int=?, t('a), ~size: int) => t(t('a));

  /**
    Get the last element of a list.

    Returns [None] if the list is empty.

    {[List.last [1;2;3] = Some 3]}

    {[List.last [1] = Some 1]}

    {[List.last [] = None]}
  */
  let last: t('a) => option('a);

  /**
    [List.includes ~value: v xs] returns [true]
    if the given value [v] is found in thelist [xs], [false] otherwise.

    {[
    List.includes ~value:3 [1;3;5;7] = true
    List.includes ~value:4 [1;3;5;7] = false
    List.includes ~value:5 [] = false
    ]}
  */
  /** Test if a list contains the specified element using the provided [equal] to test for equality.

    {[List.contains [1; 2; 3] 2 ~equal:(=) = true]}
  */
  let includes: (t('a), 'a, ~equal: ('a, 'a) => bool) => bool;

  let count: (t('a), ~f: 'a => bool) => int;

  /** As an {!Option} get of all of the elements of a list except the last one.

    Returns [None] if the list is empty.

    {3 Examples}

    {[List.initial [1;2;3] = Some [1;2]]}

    {[List.initial [1] = Some []]}

    {[List.initial [] = None]}
  */
  let initial: t('a) => option(t('a));

  /** Allows you to combine {!map} and {!filter} into a single pass.

    The output list only contains elements for which [f] returns [Some].

    {[List.filterMap [3; 4; 5; 6] ~f:(fun number ->
      if Int.isEven number then
        Some (number * number)
      else
        None
    ) = [16; 36]}

    Why [filterMap] and not just {!filter} then {!map}?

    {!filterMap} removes the {!Option} layer automatically.
    If your mapping is already returning an {!Option} and you want to skip over Nones, then [filterMap] is much nicer to use.
  */
  let filterMap: (t('a), ~f: 'a => option('b)) => t('b);

  /** Returns the element at position [index] in the list.

    Returns [None] if the list is empty, or [index] is outside of the bounds of the list.

    {[List.getAt [1;2;3] ~index:1 = Some 2]}

    {[List.getAt [] ~index:2 = None]}

    {[List.getAt [1;2;3] ~index:100 = None]}
  */
  let getAt: (t('a), ~index: int) => option('a);

  /** Keep elements that [f] returns [true] for.

    {[List.filter ~f:Int.isEven [1; 2; 3; 4; 5; 6] = [2; 4; 6]]}
  */
  let filter: (t('a), ~f: 'a => bool) => t('a);

  let filterI: (t('a), ~f: (int, 'a) => bool) => t('a);

  /** {1 Deconstruct} */;

  /** Split a list into a {!Tuple} of lists. Values which [f] returns true for will end up in {!Tuple.first}.

    {[List.partition [1;2;3;4;5;6] ~f:Int.isOdd = ([1;3;5], [2;4;6])]}
  */
  let partition: (t('a), ~f: 'a => bool) => (t('a), t('a));

  /** Decompose a list of {!Tuple} into a {!Tuple} of lists.

    {[List.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  */
  let unzip: t(('a, 'b)) => (t('a), t('b));

  /** Decompose a list of {!Tuple3} into a {!Tuple3} of lists.

    {[List.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
  */
  let unzip3: t(('a, 'b, 'c)) => (t('a), t('b), t('c));

  let fold: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;
  /**
    After applying [f] to every element of the list, [fold] returns the accumulator.

    [fold] iterates over the elements of the list from first to last.

    Folding is useful whenever you have a collection of something, and want to produce a single value from it.

    For examples if we have:

    {[
      let numbers = [1, 2, 3] in
      let sum =
        List.fold numbers ~initial:0 ~f:(fun accumulator element -> accumulator +element)
      in
      sum = 6
    ]}

    Walking though each iteration step by step:

    + [accumulator: 0, element: 1, result: 1]
    + [accumulator: 1, element: 2, result: 3]
    + [accumulator: 3, element: 3, result: 6]

    And so the final result is [6]. (Note that in this case you probably want to use {!Array.sum})

    {b Examples continued}

    {[Array.fold [|1; 2; 3|] ~initial:[] ~f:(List.cons) = [3; 2; 1]]}

    {[
      let unique integers =
        Array.fold integers ~initial:Set.Int.empty ~f:Set.add |> Set.toArray
      in
      unique [|1; 1; 2; 2; 3|] = [|1; 2; 3|]
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
  */;

  /** This method is like {!foldLeft} except that it iterates over the elements of the list from last to first. */;
  let foldRight: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;

  /** Returns, as an option, the first element for which [f] evaluates to true.

    If [f] doesn't return [true] for any of the elements [find] will return [None]

    {[List.find ~f:Int.isEven [|1; 3; 4; 8;|] = Some 4]}

    {[List.find ~f:Int.isOdd [|0; 2; 4; 8;|] = None]}

    {[List.find ~f:Int.isEven [||] = None]}
  */
  let find: (t('a), ~f: 'a => bool) => option('a);

  /** Returns, as an option, a tuple of the first element and its index for which [f] evaluates to true.

    If no element satisfies the [predicate], [findIndex] returns [None].

    {[List.findIndex ~f:(fun index number -> index > 2 && Int.isEven number) [|1; 3; 4; 8;|] = Some (3, 8)]}
  */
  let findIndex: (t('a), ~f: (int, 'a) => bool) => option((int, 'a));

  /**
   * TODO count > length?
  */
  let take: (t('a), ~count: int) => t('a);

  /** Take elements from a list until [f] returns [false]
    {[
      List.takeWhile ~f:Int.isEven [2; 4; 6; 7; 8; 9] = [2; 4; 6]
      List.takeWhile ~f:Int.isEven [2; 4; 6] = [2; 4; 6]
      List.takeWhile ~f:Int.isEven [1; 2; 3] = []
    ]}
  */
  let takeWhile: (t('a), ~f: 'a => bool) => t('a);

  /** Drop the first [count] elements from the front of a list.
    {[List.drop [1;2;3;4] ~count:2 = [3;4]}

    {[List.drop [1;2;3;4] ~count:6 = []]}
  */
  let drop: (t('a), ~count: int) => t('a);

  /**
    Drop elements from a list until [f] returns [false]
    {[
      List.dropWhile ~f:even [2; 4; 6; 7; 8; 9] = [7; 8; 9]
      List.dropWhile ~f:even [2; 4; 6; 8] = []
      List.dropWhile ~f:even [1; 2; 3] = [1; 2; 3]
    ]}
  */
  let dropWhile: (t('a), ~f: 'a => bool) => t('a);

  /** Divides a list into a {!Tuple} of lists.

    Elements which have index upto (but not including) [index] will be in the first component of the tuple.

    Elements with an index greater than or equal to [index] will be in the second.

    {[List.splitAt [1;2;3;4;5] ~index:2 = ([1;2], [3;4;5])]}
  */
  let splitAt: (t('a), ~index: int) => (t('a), t('a));

  /**
    Divides a list at the first element [f] returns [true] for.
  */
  let splitWhen: (t('a), ~f: 'a => bool) => (t('a), t('a));

  /** Returns a new list with the value at [index] updated to be the result of applying [f].

    If [index] is outside of the bounds of the list, returns the list as-is.

    {[List.updateAt [1; 2; 3] ~index:1 ~f:(Int.add 3) = [1; 5; 3]]}

    {[
      let animals = ["Ant"; "Bat"; "Cat"] in
      animals == List.updateAt animals ~index:4 ~f:String.reverse
    ]}
  */
  let updateAt: (t('a), ~index: int, ~f: 'a => 'a) => t('a);

  /**
    [List.removeAt n xs] returns a new list with the item at the given index removed.
    If [n] is less than zero or greater than the length of [xs], the new list is
    the same as the original.

    {[
    List.removeAt ~index:2, ["a"; "b"; "c"; "d"] = ["a"; "b"; "d"]
    List.removeAt ~index:(-2) ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"]
    List.removeAt ~index:7 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"]
    ]}
  */
  /** Creates a new list without the element at [index].
    If [index] is outside of the bounds of the list, returns the list as-is.

    {[List.removeAt [1; 2; 3] ~index:2 = [1; 2]]}

    {[
      let animals = ["Ant"; "Bat"; "Cat"] in
      animals == List.removeAt animals ~index:4
    ]}
  */
  let removeAt: (t('a), ~index: int) => t('a);

  /** Return the number of elements in a list.

      {b Warning} [List.length] needs to access the {b entire} list in order to calculate its result.

      If you need fast access to the length, perhaps you need an {!Array}.

      A common mistake is to have something like the following:

      {[
        if (List.length someList) = 0 then (
          (* ... *)
        ) else (
          (* ... *)
        )
      ]}

      instead you should do

      {[
        if (List.isEmpty someList) then (
          (* ... *)
        ) else (
          (* ... *)
        )
      ]}

      Or

      {[
        match someList with
        | [] -> (* ... *)
        | _ -> (* ... *)
      ]}


      {3 Examples}

      {[List.length([]) == 0]}

      {[List.length([7, 8, 9]) == 3]}
    */;
  let length: t('a) => int;

  /**
    [List.reverse xs] returns a list whose items are in the
    reverse order of those in [xs].
  */
  let reverse: t('a) => t('a);

  /** {1 Query} */

  /** Determine if a list is empty.

    {[List.isEmpty List.empty = true]}

    {[List.isEmpty [||] = true]}

    {[List.isEmpty [|1; 2; 3|] = false]}
  */
  let isEmpty: t(_) => bool;

  /** Prepend a value to the front of a list.

    {[List.cons [2;3;4] 1 = [1;2;3;4]]}

    The [::] operator can also be used

    {[1 :: [2;3;4] = [1;2;3;4]]}

    And in Reason you can use the [spread]() syntax.
    {[
      [1, ...[1,2,3]]
    ]}
  */
  let cons: (t('a), 'a) => t('a);

  /**  Determine if [f] returns true for [any] values in a list.

    {[List.any ~f:isEven [|2;3|] = true]}

    {[List.any ~f:isEven [|1;3|] = false]}

    {[List.any ~f:isEven [||] = false]}
  */
  let any: (t('a), ~f: 'a => bool) => bool;

  /** Determine if [f] returns true for [all] values in a list.

    {[List.all ~f:Int.isEven [|2;4|] = true]}

    {[List.all ~f:Int.isEven [|2;3|] = false]}

    {[List.all ~f:Int.isEven [||] = true]}
  */
  let all: (t('a), ~f: 'a => bool) => bool;

  /** Find the smallest element using the provided [compare] function.

      Returns [None] if called on an empty array.

      {[List.minimum [|7;5;8;6|] ~compare:Int.compare = Some 5]}
  */
  let minimum: (t('a), ~compare: ('a, 'a) => int) => option('a);

  /** Find the largest element using the provided [compare] function.

    Returns [None] if called on an empty array.

    {[List.maximum [|7;5;8;6|] ~compare:compare = Some 8]}
  */
  let maximum: (t('a), ~compare: ('a, 'a) => int) => option('a);

  /** Find a {!Tuple} of the [(minimum, maximum)] elements using the provided [compare] function.

    Returns [None] if called on an empty array.

    {[List.extent [|7;5;8;6|] ~compare:compare = Some (5, 8)]}
  */
  let extent: (t('a), ~compare: ('a, 'a) => int) => option(('a, 'a));

  /** Returns a list of lists (i.e., groups) whose concatenation is equal to the original list.

    Each group is broken where [break] returns true on a pair of successive elements.

    {[
      List.group ~break:(<>) ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] =
        [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
    ]}

    {[
      TODO example showing the groups will have an order

      List.group ~break:(Int.isEven) [1,2,6,3,1,7,8] =
        [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
    ]}
  */
  // let group: (t('a), ~break: ('a, 'a) => bool) => t(t('a));

  let groupWhile: (t('a), ~f: ('a, 'a) => bool) => t(t('a));
  /**
    {[
      List.groupWhile ~f:(fun x y -> x mod 2 == y mod 2)
        [2; 4; 6; 5; 3; 1; 8; 7; 9] = [[2; 4; 6]; [5; 3; 1]; [8]; [7; 9]]
    ]}
  */

  /**
      [List.insertAt ~index=n ~value=v xs] returns a new list with the value [v] inserted
      before position [n] in [xs]. If [n] is less than zero or greater than the length of [xs],
      returns a list consisting only of the value [v].

      {[
      List.insertAt ~index:2 ~value:999 [100; 101; 102; 103] = [100; 101; 999; 102; 103]
      List.insertAt ~index:0 ~value:999 [100; 101; 102; 103] = [999; 100; 101; 102; 103]
      List.insertAt ~index:4 ~value:999 [100; 101; 102; 103] = [100; 101; 102; 103; 999]
      List.insertAt ~index:(-1) ~value:999 [100; 101; 102; 103] = [999]
      List.insertAt ~index:5 ~value:999 [100; 101; 102; 103] = [999]
      ]}
    */
  /** Returns a new list with [value] at position [index].

    The element previously occuping [index] will now be at [index + 1]

    {[List.insertAt [1; 2; 3] ~index:1 ~value:4 = [1; 4; 2; 3]]}

    If [index] is greater than then length of the list, it will be appended:

    {[List.insertAt [1; 2; 3] ~index:100 ~value:4 = [1; 2; 3; 4]]}
  */
  let insertAt: (t('a), ~index: int, ~value: 'a) => t('a);

  /** Places [sep] between all the elements of the given list.

    {[List.intersperse ~sep:"on" [|"turtles"; "turtles"; "turtles"|] = [|"turtles"; "on"; "turtles"; "on"; "turtles"|]]}

    {[List.intersperse ~sep:0 [||] = [||]]}
  */
  let intersperse: (t('a), ~sep: 'a) => t('a);

  /** Sort using the provided [compare] function.

    On native it uses {{: https://en.wikipedia.org/wiki/Merge_sort } merge sort} which means the sort is stable,
    runs in linear heap space, logarithmic stack space and n * log (n) time.

    When targeting javascript the time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

    {[List.sort [5;6;8;3;6] ~compare:compare = [3;5;6;6;8]]}
  */
  let sort: (t('a), ~compare: ('a, 'a) => int) => t('a);

  /**
    [List.sortWith compareFcn xs] returns a new list with the elements in [xs] sorted according
    to [compareFcn]. The [compareFcn] function takes two list items and returns a value
    less than zero if the first item compares less than the second, zero if the items compare equal,
    and one if the first item compares greater than the second.

    This is a stable sort; items with equivalent values according to the [compareFcn]
    appear in the sorted list in the same order as they appeared in the original list.

    let cmp_mod12 a b = (
      (a mod 12) - (b mod 12)
    )

    List.sortWith cmp_mod12 [15; 3; 22; 10; 16] == [3; 15; 10; 22; 10]
  */

  /** {1 Iterate} */

  /** Like {!forEach} but [f] is also called with the elements index.

    {[
      List.forEachIndexed [1; 2; 3] ~f:(fun index int -> printf "%d: %d" index int)
      (*
        0: 1
        1: 2
        2: 3
      *)
    ]}
  */
  let forEachI: (t('a), ~f: (int, 'a) => unit) => unit;

  /**
    [List.forEach ~f: fcn xs] applies the given function
    to each element in [xs]. The function you provide must return [unit], and the
    [forEach] call itself also returns [unit]. You use [List.forEach] when you want to process
    a list only for side effects.

    The following code will print the items in the list to the console.

    {[
    let _ = List.forEach ~f:Js.log ["a"; "b"; "c"]
    ]}
  */
  /** Iterates over the elements of invokes [f] for each element.

    {[List.forEach [|1; 2; 3|] ~f:(fun int -> print (Int.toString int))]} */

  let forEach: (t('a), ~f: 'a => unit) => unit;

  /** {!1 Convert } */

  let join: (t(string), ~sep: string) => string;

  /** Converts a list to an {!Array}. */
  let toArray: t('a) => array('a);
};
