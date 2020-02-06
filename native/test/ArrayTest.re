open Standard;
open AlcoJest;
module AT = Alcotest;

let suite = suite("array", () => {
  describe("get", () => {
    let outOfBounds = Invalid_argument("index out of bounds");

    test("raises for an out of bounds index", () => {
      expect(() => ([|0, 1, 2|][5])|>ignore) |> toRaise(outOfBounds);
    });

    test("raises for empty array", () => {
      expect(() => ignore([||][0])) |> toRaise(outOfBounds);
    });
  });

  AT.check(AT.int, "empty - has length zero", Array.(empty |> length), 0);
  AT.check(
    AT.array(AT.int),
    "empty - equals the empty array literal",
    Array.empty,
    [||],
  );

  AT.check(
    AT.array(AT.int),
    "singleton - equals an array literal of the same value",
    Array.singleton(1234),
    [|1234|],
  );
  AT.check(
    AT.int,
    "singleton - has length one",
    Array.(singleton(1) |> length),
    1,
  );

  AT.check(
    AT.int,
    "length - equals an array literal of the same value",
    Array.length([||]),
    0,
  );
  AT.check(AT.int, "length - has length one", Array.length([|'a'|]), 1);
  AT.check(AT.int, "length - has length two", Array.length([|"a", "b"|]), 2);

  AT.check(
    AT.bool,
    "isEmpty - returns true for empty array literals",
    Array.isEmpty([||]),
    true,
  );
  AT.check(
    AT.bool,
    "isEmpty - returns false for literals with a non-zero number of elements",
    Array.isEmpty([|1234|]),
    false,
  );

  AT.check(
    AT.list(AT.int),
    "map2 empty lists",
    List.map2(~f=(+), [], []),
    [],
  );
  AT.check(
    AT.list(AT.int),
    "map2 one element",
    List.map2(~f=(+), [1], [1]),
    [2],
  );
  AT.check(
    AT.list(AT.int),
    "map2 two elements",
    List.map2(~f=(+), [1, 2], [1, 2]),
    [2, 4],
  );
  AT.check(
    AT.array(AT.int),
    "initialize - create empty array",
    Array.initialize(0, ~f=Fun.identity),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "initialize - negative length gives an empty array",
    Array.initialize(-1, ~f=Fun.identity),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "initialize - create array with initialize",
    Array.initialize(3, ~f=Fun.identity),
    [|0, 1, 2|],
  );

  AT.check(
    AT.list(AT.int),
    "mapI empty list",
    List.mapI(~f=(i, _) => i, []),
    [],
  );
  AT.check(
    AT.list(AT.int),
    "mapI one element",
    List.mapI(~f=(i, _) => i, ['a']),
    [0],
  );
  AT.check(
    AT.list(AT.int),
    "mapI two elements",
    List.mapI(~f=(i, _) => i, ['a', 'b']),
    [0, 1],
  );
  AT.check(
    AT.array(AT.int),
    "repeat - length zero creates an empty array",
    Array.repeat(0, ~length=0),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "repeat - negative length gives an empty array",
    Array.repeat(~length=-1, 0),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "repeat - create array of ints",
    Array.repeat(0, ~length=3),
    [|0, 0, 0|],
  );
  AT.check(
    AT.array(AT.string),
    "repeat - create array strings",
    Array.repeat("cat", ~length=3),
    [|"cat", "cat", "cat"|],
  );

  AT.check(
    AT.array(AT.int),
    "range - returns an array of the integers from zero and upto but not including [to]",
    Array.range(5),
    [|0, 1, 2, 3, 4|],
  );
  AT.check(
    AT.array(AT.int),
    "range - returns an empty array when [to] is zero",
    Array.range(0),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "range - takes an optional [from] argument to start create empty array",
    Array.range(~from=2, 5),
    [|2, 3, 4|],
  );
  AT.check(
    AT.array(AT.int),
    "range - returns an array of the integers from zero and upto but not including [to_]",
    Array.range(5),
    [|0, 1, 2, 3, 4|],
  );
  AT.check(
    AT.array(AT.int),
    "range - returns an array of the integers from zero and upto but not including [to_]",
    Array.range(0),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "range - takes an optional [from] argument to start create empty array",
    Array.range(~from=2, 5),
    [|2, 3, 4|],
  );
  AT.check(
    AT.array(AT.int),
    "range - can start from negative values",
    Array.range(~from=-2, 3),
    [|(-2), (-1), 0, 1, 2|],
  );
  AT.check(
    AT.array(AT.int),
    "range - returns an empty array when [from] > [to_]",
    Array.range(~from=5, 0),
    [||],
  );
  AT.check(
    AT.array(AT.int),
    "range - can start from negative values",
    Array.range(~from=-2, 3),
    [|(-2), (-1), 0, 1, 2|],
  );
  AT.check(
    AT.array(AT.int),
    "range - returns an empty array when [from] > [to_]",
    Array.range(~from=5, 0),
    [||],
  );

  AT.check(
    AT.list(AT.int),
    "mapI empty list",
    List.mapI(~f=(_, n) => n + 1, []),
    [],
  );
  AT.check(
    AT.list(AT.int),
    "mapI one element",
    List.mapI(~f=(_, n) => n + 1, [(-1)]),
    [0],
  );
  AT.check(
    AT.list(AT.int),
    "mapI two elements",
    List.mapI(~f=(_, n) => n + 1, [(-1), 0]),
    [0, 1],
  );
  AT.check(
    AT.array(AT.int),
    "ofList - transforms a list into an array of the same elements",
    Array.(ofList([1, 2, 3])),
    [|1, 2, 3|],
  );

  AT.check(
    AT.list(AT.int),
    "toList - transform an array into a list of the same elements",
    Array.toList([|1, 2, 3|]),
    [1, 2, 3],
  );

  AT.check(
    AT.pair(AT.list(AT.int), AT.list(AT.int)),
    "partition empty list",
    List.partition(~f=Int.isEven, []),
    ([], []),
  );
  AT.check(
    AT.pair(AT.list(AT.int), AT.list(AT.int)),
    "partition one element",
    List.partition(~f=Int.isEven, [1]),
    ([], [1]),
  );
  AT.check(
    AT.pair(AT.list(AT.int), AT.list(AT.int)),
    "partition four elements",
    List.partition(~f=Int.isEven, [1, 2, 3, 4]),
    ([2, 4], [1, 3]),
  );
  AT.check(
    AT.list(AT.pair(AT.int, AT.string)),
    "toIndexedList - returns an empty list for an empty array",
    Array.toIndexedList([||]),
    [],
  );
  AT.check(
    AT.list(AT.pair(AT.int, AT.string)),
    "toIndexedList - transforms an array into a list of tuples",
    Array.toIndexedList([|"cat", "dog"|]),
    [(0, "cat"), (1, "dog")],
  );

  AT.check(
    (AT.string),
    "get - returns Some for an in-bounds index",
    [|"cat", "dog", "eel"|][2],
    ("eel"),
  );
  
  AT.check(
    AT.option(AT.string),
    "getAt - returns Some for an in-bounds index",
    Array.getAt(~index=2, [|"cat", "dog", "eel"|]),
    Some("eel"),
  );
  AT.check(
    AT.option(AT.int),
    "getAt - returns None for an out of bounds index",
    Array.getAt(~index=5, [|0, 1, 2|]),
    None,
  );
  AT.check(
    AT.option(AT.int),
    "getAt - returns None for an empty array",
    Array.getAt(~index=0, [||]),
    None,
  );

  AT.check(
    AT.array(AT.int),
    "set - can set a value at an index",
    {
      let numbers = [|1, 2, 3|];
      numbers[0] = 0;
      numbers;
    },
    [|0, 2, 3|],
  );

  AT.check(
    AT.array(AT.int),
    "setAt - can be partially applied to set an element",
    {
      let setZero = Array.setAt(~value=0);
      let numbers = [|1, 2, 3|];
      setZero(numbers, ~index=2);
      setZero(numbers, ~index=1);
      numbers;
    },
    [|1, 0, 0|],
  );

  AT.check(
    AT.array(AT.string),
    "setAt - can be partially applied to set an index",
    {
      let setZerothElement = Array.setAt(~index=0);
      let animals = [|"ant", "bat", "cat"|];
      setZerothElement(animals, ~value="antelope");
      animals;
    },
    [|"antelope", "bat", "cat"|],
  );

  AT.check(
    AT.int,
    "sum - equals zero for an empty array",
    Array.sum([||], (module Int)),
    0,
  );

  AT.check(
    AT.int,
    "sum - adds up the elements on an integer array",
    Array.sum([|1, 2, 3|], (module Int)),
    6,
  );

  AT.check(
    AT.float(0.),
    "sum - equals zero for an empty array",
    Array.sum([||], (module Float)),
    0.0,
  );

  AT.check(
    AT.float(0.),
    "sum - adds up the elements of a float array",
    Array.sum([|1.2, 2.3, 3.4|], (module Float)),
    6.9,
  );

  AT.check(
    AT.array(AT.int),
    "filter - keep elements that [f] returns [true] for",
    Array.filter(~f=Int.isEven, [|1, 2, 3, 4, 5, 6|]),
    [|2, 4, 6|],
  );

  let numbers = [|1, 2, 3|];
  Array.swap(numbers, 1, 2);
  AT.check(
    AT.array(AT.int),
    "swap - switches values at the given indicies",
    numbers,
    [|1, 3, 2|],
  );

  AT.check(
    AT.array(AT.float(0.)),
    "map - Apply a function [f] to every element in an array",
    Array.map(~f=sqrt, [|1.0, 4.0, 9.0|]),
    [|1.0, 2.0, 3.0|],
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 1",
    [|[|1|], [|2|], [|3|], [|4|], [|5|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=1),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 2",
    [|[|1, 2|], [|2, 3|], [|3, 4|], [|4, 5|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=2),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 3 ",
    [|[|1, 2, 3|], [|2, 3, 4|], [|3, 4, 5|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=3),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 2, size 2",
    [|[|1, 2|], [|3, 4|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=2),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 1, size 3",
    [|[|1|], [|4|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=1, ~step=3),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 7",
    [|[|1, 2|], [|4, 5|]|],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=3),
  );

  AT.check(
    AT.array(AT.array(AT.int)),
    "sliding - step 7",
    [||],
    Array.sliding([|1, 2, 3, 4, 5|], ~size=7),
  );

  AT.check(
    AT.array(AT.int),
    "mapI - equals an array literal of the same value",
    Array.mapI(~f=( * ), [|5, 5, 5|]),
    [|0, 5, 10|],
  );

  AT.check(
    AT.array(AT.int),
    "map2 - works when the order of arguments to `f` is not important",
    Array.map2(~f=(+), [|1, 2, 3|], [|4, 5, 6|]),
    [|5, 7, 9|],
  );

  AT.check(
    AT.array(AT.pair(AT.string, AT.int)),
    "map2 - works when the order of `f` is important",
    Array.map2(
      ~f=Tuple.make,
      [|"alice", "bob", "chuck"|],
      [|2, 5, 7, 8|],
    ),
    [|("alice", 2), ("bob", 5), ("chuck", 7)|],
  );

  AT.check(
    AT.array(Eq.trio(AT.string, AT.int, AT.bool)),
    "map3",
    Array.map3(
      ~f=Tuple3.make,
      [|"alice", "bob", "chuck"|],
      [|2, 5, 7, 8|],
      [|true, false, true, false|],
    ),
    [|("alice", 2, true), ("bob", 5, false), ("chuck", 7, true)|],
  );

  AT.check(
    AT.array(AT.int),
    "bind",
    Array.bind(~f=n => [|n, n|], [|1, 2, 3|]),
    [|1, 1, 2, 2, 3, 3|],
  );

  AT.check(
    AT.option(AT.int),
    "find - returns the first element which `f` returns true for",
    Array.find(~f=Int.isEven, [|1, 3, 4, 8|]),
    Some(4),
  );
  AT.check(
    AT.option(AT.int),
    "find - returns `None` if `f` returns false for all elements",
    Array.find(~f=Int.isOdd, [|0, 2, 4, 8|]),
    None,
  );
  AT.check(
    AT.option(AT.int),
    "find - returns `None` for an empty array",
    Array.find(~f=Int.isEven, [||]),
    None,
  );

  AT.check(
    AT.option(AT.pair(AT.int, AT.int)),
    "findIndex - returns the first (index,element) tuple which `f` returns true for",
    Array.findIndex(
      ~f=(index, number) => index > 2 && Int.isEven(number),
      [|1, 3, 4, 8|],
    ),
    Some((3, 8)),
  );

  AT.check(
    AT.option(AT.pair(AT.int, AT.int)),
    "findIndex - returns `None` if `f` returns false for all elements ",
    Array.findIndex(~f=(_, _) => false, [|0, 2, 4, 8|]),
    None,
  );

  AT.check(
    AT.option(AT.pair(AT.int, AT.int)),
    "findIndex - returns `None` for an empty array",
    Array.findIndex(
      ~f=(index, number) => index > 2 && Int.isEven(number),
      [||],
    ),
    None,
  );

  AT.check(
    AT.bool,
    "any - returns false for empty arrays",
    Array.any([||], ~f=Int.isEven),
    false,
  );
  AT.check(
    AT.bool,
    "any - returns true if at least one of the elements of an array return true for [f]",
    Array.any([|1, 3, 4, 5, 7|], ~f=Int.isEven),
    true,
  );
  AT.check(
    AT.bool,
    "any - returns false if all of the elements of an array return false for [f]",
    Array.any([|1, 3, 5, 7|], ~f=Int.isEven),
    false,
  );

  AT.check(
    AT.bool,
    "all - returns true for empty arrays",
    Array.all(~f=Int.isEven, [||]),
    true,
  );
  AT.check(
    AT.bool,
    "all - returns true if [f] returns true for all elements",
    Array.all(~f=Int.isEven, [|2, 4|]),
    true,
  );
  AT.check(
    AT.bool,
    "all - returns false if a single element fails returns false for [f]",
    Array.all(~f=Int.isEven, [|2, 3|]),
    false,
  );

  AT.check(
    AT.array(AT.int),
    "append",
    Array.append(Array.repeat(~length=2, 42), Array.repeat(~length=3, 81)),
    [|42, 42, 81, 81, 81|],
  );

  AT.check(
    AT.array(AT.int),
    "concatenate",
    Array.concatenate([|[|1, 2|], [|3|], [|4, 5|]|]),
    [|1, 2, 3, 4, 5|],
  );

  AT.check(
    AT.array(AT.string),
    "intersperse - equals an array literal of the same value",
    [|"turtles", "on", "turtles", "on", "turtles"|],
    Array.intersperse(~sep="on", [|"turtles", "turtles", "turtles"|]),
  );

  AT.check(
    AT.array(AT.int),
    "intersperse - equals an array literal of the same value",
    Array.intersperse(~sep=0, [||]),
    [||],
  );

  {
    let array = [|0, 1, 2, 3, 4|];
    let positiveArrayLengths = [
      Array.length(array),
      Array.length(array) + 1,
      1000,
    ];

    let negativeArrayLengths = List.map(~f=Int.negate, positiveArrayLengths);
    AT.check(
      AT.array(AT.int),
      "slice - should work with a positive `from`",
      Array.slice(~from=1, array),
      [|1, 2, 3, 4|],
    );

    AT.check(
      AT.array(AT.int),
      "slice - should work with a negative `from`",
      Array.slice(~from=-1, array),
      [|4|],
    );

    Base.List.iter(positiveArrayLengths, ~f=from =>
      AT.check(
        AT.array(AT.int),
        "slice - should work when `from` >= `length`",
        Array.slice(~from, array),
        [||],
      )
    );

    Base.List.iter(negativeArrayLengths, ~f=from =>
      AT.check(
        AT.array(AT.int),
        "slice - should work when `from` <= negative `length`",
        Array.slice(~from, array),
        array,
      )
    );

    AT.check(
      AT.array(AT.int),
      "slice - should work with a positive `to_`",
      Array.slice(~from=0, ~to_=3, array),
      [|0, 1, 2|],
    );

    AT.check(
      AT.array(AT.int),
      "slice - should work with a negative `to_`",
      Array.slice(~from=1, ~to_=-1, array),
      [|1, 2, 3|],
    );

    Base.List.iter(positiveArrayLengths, ~f=to_ =>
      AT.check(
        AT.array(AT.int),
        "slice - should work when `to_` >= length",
        Array.slice(~from=0, ~to_, array),
        array,
      )
    );

    Base.List.iter(negativeArrayLengths, ~f=to_ =>
      AT.check(
        AT.array(AT.int),
        "slice - should work when `to_` <= negative `length`",
        Array.slice(~from=0, ~to_, array),
        [||],
      )
    );

    AT.check(
      AT.array(AT.int),
      "slice - should work when both `from` and `to_` are negative and `from` < `to_`",
      Array.slice(~from=-2, ~to_=-1, array),
      [|3|],
    );

    AT.check(
      AT.array(AT.int),
      "slice - works when `from` >= `to_`",
      Array.slice(~from=4, ~to_=3, array),
      [||],
    );
  };

  AT.check(
    AT.string,
    "fold - works for an empty array",
    Array.fold([||], ~f=(++), ~initial=""),
    "",
  );
  AT.check(
    AT.int,
    "fold - works for an ascociative operator",
    Array.fold(~f=( * ), ~initial=1, Array.repeat(~length=4, 7)),
    2401,
  );
  AT.check(
    AT.string,
    "fold - works when the order of arguments to `f` is important",
    Array.fold([|"a", "b", "c"|], ~initial="", ~f=(++)),
    "cba",
  );
  AT.check(
    AT.list(AT.int),
    "fold - works when the order of arguments to `f` is important",
    [3, 2, 1],
    Array.fold(
      [|1, 2, 3|],
      ~initial=[],
      ~f=(list, element) => [element, ...list],
    ),
  );

  AT.check(
    AT.string,
    "foldRight - works for empty arrays",
    Array.foldRight([||], ~f=(++), ~initial=""),
    "",
  );
  AT.check(
    AT.int,
    "foldRight - works for an ascociative operator",
    Array.foldRight(~f=(+), ~initial=0, Array.repeat(~length=3, 5)),
    15,
  );
  AT.check(
    AT.string,
    "foldRight - works when the order of arguments to `f` is important",
    Array.foldRight([|"a", "b", "c"|], ~f=(++), ~initial=""),
    "abc",
  );
  AT.check(
    AT.list(AT.int),
    "foldRight - works when the order of arguments to `f` is important",
    Array.foldRight(
      [|1, 2, 3|],
      ~initial=[],
      ~f=(list, element) => [element, ...list],
    ),
    [1, 2, 3],
  );

  AT.check(
    AT.array(AT.int),
    "reverse - empty array",
    [||],
    { 
      let array = [||];
      Array.reverse(array);
      array
    },
  );
  AT.check(
    AT.array(AT.int),
    "reverse - two elements",
    [|1, 0|],
    {
      let array = [|0, 1|];
      Array.reverse(array);
      array
    }
  );
  AT.check(
    AT.array(AT.int),
    "reverse - leaves the original array untouched",
    {
      let array = [|0, 1, 2, 3|];
      let _reversedArray = Array.reverse(array);
      array;
    },
    [|0, 1, 2, 3|],
  );

  AT.check(
    AT.array(AT.int),
    "reverse - alters an array in-place",
    {
      let array = [|1, 2, 3|];
      Array.reverse(array);
      array;
    },
    [|3, 2, 1|],
  );

  AT.check(
    AT.array(AT.int),
    "forEach",
    {
      let index = ref(0);
      let calledValues = [|0, 0, 0|];
      Array.forEach(
        [|1, 2, 3|],
        ~f=value => {
          Array.setAt(calledValues, ~index=index^, ~value);
          index := index^ + 1;
        },
      );

      calledValues;
    },
    [|1, 2, 3|],
  );
});