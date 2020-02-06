open Standard;
open AlcoJest;

let suite =
  suite("Array", () => {
  describe("empty", () => {
    test("has length zero", () => {
      expect(Array.(empty |> length)) |> toEqual(Eq.int, 0)
    });

    test("equals the empty array literal", () => {
      expect(Array.empty) |> toEqual(Eq.(array(int)), [||])
    });
  });

  describe("singleton", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.singleton(1234)) |> toEqual(Eq.(array(int)), [|1234|])
    });

    test("has length one", () => {
      expect(Array.(singleton(1) |> length)) |> toEqual(Eq.((int)), 1)
    });
  });

  describe("length", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.length([||])) |> toEqual(Eq.int, 0)
    });
    test("has length one", () => {
      expect(Array.length([|'a'|])) |> toEqual(Eq.int, 1)
    });
    test("has length two", () => {
      expect(Array.length([|"a", "b"|])) |> toEqual(Eq.int, 2)
    });
  });

  describe("isEmpty", () => {
    test("returns true for empty array literals", () => {
      expect(Array.isEmpty([||])) |> toEqual(Eq.bool, true)
    });

    test("returns false for literals a non-zero number of elements", () => {
      expect(Array.isEmpty([|1234|])) |> toEqual(Eq.bool, false)
    });
  });

  describe("initialize", () => {
    test("create empty array", () => {
      expect(Array.initialize(0, ~f=Fun.identity)) |> toEqual(Eq.(array(int)), [||])
    });

    test("negative length gives an empty array", () => {
      expect(Array.initialize(-1, ~f=Fun.identity)) |> toEqual(Eq.(array(int)),[||])
    });

    test("create array initialize", () => {
      expect(Array.initialize(3, ~f=Fun.identity)) |> toEqual(Eq.(array(int)),[|0, 1, 2|])
    });
  });

  describe("repeat", () => {
    test("length zero creates an empty array", () => {
      expect(Array.repeat(0, ~length=0)) |> toEqual(Eq.(array(int)),[||])
    });

    test("negative length gives an empty array", () => {
      expect(Array.repeat(~length=-1, 0)) |> toEqual(Eq.(array(int)),[||])
    });

    test("create array of ints", () => {
      expect(Array.repeat(0, ~length=3)) |> toEqual(Eq.(array(int)),[|0, 0, 0|])
    });

    test("create array strings", () => {
      expect(Array.repeat("cat", ~length=3))
      |> toEqual(Eq.(array(string)),[|"cat", "cat", "cat"|])
    });
  });

  describe("range", () => {
    test(
      "returns an array of the integers from zero and upto but not including [to]",
      () => {
      expect(Array.range(5)) |> toEqual(Eq.(array(int)),[|0, 1, 2, 3, 4|])
    });

    test("returns an empty array [to] is zero", () => {
      expect(Array.range(0)) |> toEqual(Eq.(array(int)),[||])
    });

    test("takes an optional [from] argument to start create empty array", () => {
      expect(Array.range(~from=2, 5)) |> toEqual(Eq.(array(int)),[|2, 3, 4|])
    });

    test("can start from negative values", () => {
      expect(Array.range(~from=-2, 3)) |> toEqual(Eq.(array(int)), [|(-2), (-1), 0, 1, 2|])
    });

    test("returns an empty array [from] > [to_]", () => {
      expect(Array.range(~from=5, 0)) |> toEqual(Eq.(array(int)), [||])
    });
  });

  describe("fromList", () => {
    test("transforms a list into an array of the same elements", () => {
      expect(Array.(fromList([1, 2, 3]))) |> toEqual(Eq.(array(int)), [|1, 2, 3|])
    })
  });

  describe("toList", () => {
    test("transform an array into a list of the same elements", () => {
      expect(Array.toList([|1, 2, 3|])) |> toEqual(Eq.(list(int)), [1, 2, 3])
    })
  });

  describe("toIndexedList", () => {
    test("returns an empty list for an empty array", () => {
      expect(Array.toIndexedList([||])) |> toEqual(Eq.(list(pair(int, int))), [])
    });

    test("transforms an array into a list of tuples", () => {
      expect(Array.toIndexedList([|"cat", "dog"|]))
      |> toEqual(Eq.(list(pair(int, string))),[(0, "cat"), (1, "dog")])
    });
  });

  describe("get", () => {
    test("returns Some for an in-bounds index", () => {
      expect([|"cat", "dog", "eel"|][2]) |> toEqual(Eq.string, "eel")
    });

    testAll("throws for an out of bounds index", [(-1), 3, 5], index => {
      expect(() => ([|0, 1, 2|][index])) |> toThrow
    });

    test("throws for an empty array", () => {
      expect(() => ([||][0])) |> toThrow
    });
  });

  describe("getAt", () => {
    test("returns Some for an in-bounds index", () => {
      expect(Array.getAt(~index=2, [|"cat", "dog", "eel"|]))
      |> toEqual(Eq.(option(string)), Some("eel"))
    });

    test("returns None for an out of bounds index", () => {
      expect(Array.getAt(~index=5, [|0, 1, 2|])) |> toEqual(Eq.(option(int)), None)
    });

    test("returns None for an empty array", () => {
      expect(Array.getAt(~index=0, [||])) |> toEqual(Eq.(option(string)), None)
    });
  });

  describe("set", () => {
    test("can set a value at an index", () => {
      let numbers = [|1, 2, 3|];
      numbers[0] = 0;
      expect(numbers) |> toEqual(Eq.(array(int)), [|0, 2, 3|]);
    })
  });

  describe("setAt", () => {
    test("can be partially applied to set an element", () => {
      let setZero = Array.setAt(~value=0);
      let numbers = [|1, 2, 3|];
      setZero(numbers, ~index=2);
      setZero(numbers, ~index=1);
      expect(numbers) |> toEqual(Eq.(array(int)), [|1, 0, 0|]);
    });

    test("can be partially applied to set an index", () => {
      let setZerothElement = Array.setAt(~index=0);
      let animals = [|"ant", "bat", "cat"|];
      setZerothElement(animals, ~value="antelope");
      expect(animals) |> toEqual(Eq.(array(string)), [|"antelope", "bat", "cat"|]);
    });
  });

  describe("sum", () => {
    test("equals zero for an empty array", () => {
      expect(Array.sum([||], (module Int))) |> toEqual(Eq.int, 0)
    });

    test("adds up the elements on an integer array", () => {
      expect(Array.sum([|1, 2, 3|], (module Int))) |> toEqual(Eq.int, 6)
    });
  });

  describe("filter", () => {
    test("keep elements that [f] returns [true] for", () => {
      expect(Array.filter(~f=Int.isEven, [|1, 2, 3, 4, 5, 6|]))
      |> toEqual(Eq.(array(int)), [|2, 4, 6|])
    })
  });

  describe("swap", () => {
    test("switches values at the given indicies", () => {
      let numbers = [|1, 2, 3|];
      Array.swap(numbers, 1, 2);
      expect(numbers) |> toEqual(Eq.(array(int)), [|1, 3, 2|]);
    })
  });

  describe("map", () => {
    test("Apply a function [f] to every element in an array", () => {
      expect(Array.map(~f=sqrt, [|1.0, 4.0, 9.0|]))
      |> toEqual(Eq.(array(float)), [|1.0, 2.0, 3.0|])
    })
  });

  describe("mapI", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.mapI(~f=( * ), [|5, 5, 5|])) |> toEqual(Eq.(array(int)), [|0, 5, 10|])
    })
  });

  describe("map2", () => {
    test("works the order of arguments to `f` is not important", () => {
      expect(Array.map2(~f=(+), [|1, 2, 3|], [|4, 5, 6|]))
      |> toEqual(Eq.(array(int)), [|5, 7, 9|])
    });

    test("works the order of `f` is important", () => {
      expect(
        Array.map2(
          ~f=Tuple.make,
          [|"alice", "bob", "chuck"|],
          [|2, 5, 7, 8|],
        ),
      )
      |> toEqual(Eq.(array(pair(string, int))), [|("alice", 2), ("bob", 5), ("chuck", 7)|])
    });
  });

  test("map3", () => {
    expect(
      Array.map3(
        ~f=Tuple3.make,
        [|"alice", "bob", "chuck"|],
        [|2, 5, 7, 8|],
        [|true, false, true, false|],
      ),
    )
    |> toEqual(Eq.(array(trio(string, int, bool))), [|
         ("alice", 2, true),
         ("bob", 5, false),
         ("chuck", 7, true),
       |])
  });

  test("bind", () => {
    let duplicate = n => [|n, n|];
    expect(Array.bind(~f=duplicate, [|1, 2, 3|]))
    |> toEqual(Eq.(array(int)), [|1, 1, 2, 2, 3, 3|]);
  });

  describe("sliding", () => {
    test("size 1", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=1))
      |> toEqual(Eq.(array(array(int))), [|[|1|], [|2|], [|3|], [|4|], [|5|]|])
    });

    test("size 2", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2))
      |> toEqual(Eq.(array(array(int))), [|[|1, 2|], [|2, 3|], [|3, 4|], [|4, 5|]|])
    });

    test("step 3 ", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=3))
      |> toEqual(Eq.(array(array(int))), [|[|1, 2, 3|], [|2, 3, 4|], [|3, 4, 5|]|])
    });

    test("size 2, step 2", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=2))
      |> toEqual(Eq.(array(array(int))), [|[|1, 2|], [|3, 4|]|])
    });

    test("size 1, step 3", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=1, ~step=3))
      |> toEqual(Eq.(array(array(int))), [|[|1|], [|4|]|])
    });

    test("size 2, step 3", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=3))
      |> toEqual(Eq.(array(array(int))), [|[|1, 2|], [|4, 5|]|])
    });

    test("step 7", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=7)) |> toEqual(Eq.(array(int)), [||])
    });
  });

  describe("find", () => {
    test("returns the first element which `f` returns true for", () => {
      expect(Array.find(~f=Int.isEven, [|1, 3, 4, 8|]))
      |> toEqual(Eq.(option(int)), Some(4))
    });

    test("returns `None` if `f` returns false for all elements ", () => {
      expect(Array.find(~f=Int.isOdd, [|0, 2, 4, 8|])) |> toEqual(Eq.(option(int)), None)
    });

    test("returns `None` for an empty array", () => {
      expect(Array.find(~f=Int.isEven, [||])) |> toEqual(Eq.(option(int)), None)
    });
  });

  describe("findIndex", () => {
    test(
      "returns the first (index,element) tuple which `f` returns true for", () => {
      expect(
        Array.findIndex(
          ~f=(index, number) => index > 2 && Int.isEven(number),
          [|1, 3, 4, 8|],
        ),
      )
      |> toEqual(Eq.(option(pair(int, int))), Some((3, 8)))
    });

    test("returns `None` if `f` returns false for all elements ", () => {
      expect(Array.findIndex(~f=(_, _) => false, [|0, 2, 4, 8|]))
      |> toEqual(Eq.(option(int)), None)
    });

    test("returns `None` for an empty array", () => {
      expect(
        Array.findIndex(
          ~f=(index, number) => index > 2 && Int.isEven(number),
          [||],
        ),
      )
      |> toEqual(Eq.(option(int)), None)
    });
  });

  describe("any", () => {
    test("returns false for empty arrays", () => {
      expect(Array.any([||], ~f=Int.isEven)) |> toEqual(Eq.bool, false)
    });

    test(
      "returns true if at least one of the elements of an array return true for [f]",
      () => {
      expect(Array.any([|1, 3, 4, 5, 7|], ~f=Int.isEven)) |> toEqual(Eq.bool, true)
    });

    test(
      "returns false if all of the elements of an array return false for [f]",
      () => {
      expect(Array.any([|1, 3, 5, 7|], ~f=Int.isEven)) |> toEqual(Eq.bool, false)
    });
  });

  describe("all", () => {
    test("returns true for empty arrays", () => {
      expect(Array.all(~f=Int.isEven, [||])) |> toEqual(Eq.bool, true)
    });

    test("returns true if [f] returns true for all elements", () => {
      expect(Array.all(~f=Int.isEven, [|2, 4|])) |> toEqual(Eq.bool, true)
    });

    test("returns false if a single element fails returns false for [f]", () => {
      expect(Array.all(~f=Int.isEven, [|2, 3|])) |> toEqual(Eq.bool, false)
    });
  });

  test("append", () => {
    expect(
      Array.append(
        Array.repeat(~length=2, 42),
        Array.repeat(~length=3, 81),
      ),
    )
    |> toEqual(Eq.(array(int)), [|42, 42, 81, 81, 81|])
  });

  test("concatenate", () => {
    expect(Array.concatenate([|[|1, 2|], [|3|], [|4, 5|]|]))
    |> toEqual(Eq.(array(int)), [|1, 2, 3, 4, 5|])
  });

  describe("intersperse", () => {
    test("equals an array literal of the same value", () => {
      expect(
        Array.intersperse(~sep="on", [|"turtles", "turtles", "turtles"|]),
      )
      |> toEqual(Eq.(array(string)), [|"turtles", "on", "turtles", "on", "turtles"|])
    });

    test("equals an array literal of the same value", () => {
      expect(Array.intersperse(~sep=0, [||])) |> toEqual(Eq.(array(int)), [||])
    });
  });

  describe("slice", () => {
    let array = [|0, 1, 2, 3, 4|];
    let positiveArrayLengths = [
      Array.length(array),
      Array.length(array) + 1,
      1000,
    ];

    let negativeArrayLengths = List.map(~f=Int.negate, positiveArrayLengths);

    test("a positive `from`", () => {
      expect(Array.slice(~from=1, array)) |> toEqual(Eq.(array(int)), [|1, 2, 3, 4|])
    });

    test("a negative `from`", () => {
      expect(Array.slice(~from=-1, array)) |> toEqual(Eq.(array(int)), [|4|])
    });

    testAll("`from` >= `length`", positiveArrayLengths, from =>
      expect(Array.slice(~from, array)) |> toEqual(Eq.(array(int)), [||])
    );

    testAll("`from` <= negative `length`", negativeArrayLengths, from =>
      expect(Array.slice(~from, array)) |> toEqual(array)
    );

    test("a positive `to_`", () => {
      expect(Array.slice(~from=0, ~to_=3, array)) |> toEqual(Eq.(array(int)), [|0, 1, 2|])
    });

    test("a negative `to_`", () => {
      expect(Array.slice(~from=1, ~to_=-1, array)) |> toEqual(Eq.(array(int)), [|1, 2, 3|])
    });

    testAll("`to_` >= length", positiveArrayLengths, to_ =>
      expect(Array.slice(~from=0, ~to_, array)) |> toEqual(array)
    );

    testAll("`to_` <= negative `length`", negativeArrayLengths, to_ =>
      expect(Array.slice(~from=0, ~to_, array)) |> toEqual(Eq.(array(int)), [||])
    );

    test("both `from` and `to_` are negative and `from` < `to_`", () =>
      expect(Array.slice(~from=-2, ~to_=-1, array)) |> toEqual(Eq.(array(int)), [|3|])
    );

    test("works `from` >= `to_`", () => {
      expect(Array.slice(~from=4, ~to_=3, array)) |> toEqual(Eq.(array(int)), [||])
    });
  });

  // TODO the tests and test names suck
  describe("fold", () => {
    test("works for an empty array", () => {
      expect(Array.fold([||], ~f=(++), ~initial="")) |> toEqual("")
    });

    test("works for an ascociative operator", () => {
      expect(Array.fold(~f=( * ), ~initial=1, Array.repeat(~length=4, 7)))
      |> toEqual(2401)
    });

    test("works the order of arguments to `f` is important", () => {
      expect(Array.fold([|"a", "b", "c"|], ~f=(++), ~initial=""))
      |> toEqual("abc")
    });

    test("works the order of arguments to `f` is important", () => {
      expect(Array.fold(~f=List.cons, ~initial=[], [|1, 2, 3|]))
      |> toEqual([3, 2, 1])
    });
  });

  describe("foldRight", () => {
    test("works for empty arrays", () => {
      expect(Array.foldRight([||], ~f=(++), ~initial="")) |> toEqual("")
    });

    test("foldRight", () => {
      expect(
        Array.foldRight(~f=(+), ~initial=0, Array.repeat(~length=3, 5)),
      )
      |> toEqual(15)
    });

    test("works the order of arguments to `f` is important", () => {
      expect(Array.foldRight([|"a", "b", "c"|], ~f=(++), ~initial=""))
      |> toEqual("cba")
    });

    test("works the order of arguments to `f` is important", () => {
      expect(Array.foldRight(~f=List.cons, ~initial=[], [|1, 2, 3|]))
      |> toEqual([1, 2, 3])
    });
  });

  describe("reverse", () => {
    test("alters an array in-place", () => {
      let array = [|1, 2, 3|];
      Array.reverse(array);
      expect(array) |> toEqual(Eq.(array(int)), [|3, 2, 1|]);
    })
  });
  // TODO
  // test("forEach", () => {
  //   let calledValues = [|0, 0, 0|];
  //   let index = ref(0);
  //   Array.forEach(
  //     [|1, 2, 3|],
  //     ~f=value => {
  //       Array.setAt(calledValues, ~index=index^, ~value);
  //       index := index^ + 1;
  //     },
  //   });
  //   expect(calledValues) |> toEqual(Eq.(array(int)), [|1, 2, 3|]);
  // );
});