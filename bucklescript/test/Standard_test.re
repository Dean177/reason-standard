open Standard;
open Jest;
open Expect;

describe("Result", () => {
  describe("fromOption", () => {
    test("maps None into Error", () => {
      expect(Result.(fromOption(~error="error message", None)))
      |> toEqual(Belt.Result.Error("error message"))
    });

    test("maps Some into Ok", () => {
      expect(Result.(fromOption(~error="error message", Some(10))))
      |> toEqual(Belt.Result.Ok(10))
    });
  })
});

describe("Fun", () => {
  test("identity", () => {
    expect(Fun.identity(1)) |> toEqual(1)
  });

  test("ignore", () => {
    expect(Fun.ignore(1)) |> toEqual()
  });

  test("constant", () => {
    expect(Fun.constant(1, 2)) |> toEqual(1)
  });

  test("sequence", () => {
    expect(Fun.sequence(1, 2)) |> toEqual(2)
  });

  test("flip", () => {
    expect(Fun.flip(Int.(/), 2, 4)) |> toEqual(2)
  });

  test("apply", () => {
    expect(Fun.apply(a => a + 1, 1)) |> toEqual(2)
  });

  test("compose", () => {
    let increment = x => x + 1;
    let double = x => x * 2;
    expect(Fun.compose(increment, double, 1)) |> toEqual(3);
  });

  test("composeRight", () => {
    let increment = x => x + 1;
    let double = x => x * 2;
    expect(Fun.composeRight(increment, double, 1)) |> toEqual(4);
  });

  test("tap", () => {
    expect(
      Array.filter([|1, 3, 2, 5, 4|], ~f=Int.isEven)
      |> Fun.tap(~f=numbers => ignore(Belt.Array.set(numbers, 1, 0)))
      |> Fun.tap(~f=Belt.Array.reverseInPlace),
    )
    |> toEqual([|0, 2|])
  });
});

describe("Array", () => {
  describe("empty", () => {
    test("has length zero", () => {
      expect(Array.(empty |> length)) |> toEqual(0)
    });

    test("equals the empty array literal", () => {
      expect(Array.empty) |> toEqual([||])
    });
  });

  describe("singleton", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.singleton(1234)) |> toEqual([|1234|])
    });
    test("has length one", () => {
      expect(Array.(singleton(1) |> length)) |> toEqual(1)
    });
  });

  describe("length", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.length([||])) |> toEqual(0)
    });
    test("has length one", () => {
      expect(Array.length([|'a'|])) |> toEqual(1)
    });
    test("has length two", () => {
      expect(Array.length([|"a", "b"|])) |> toEqual(2)
    });
  });

  describe("isEmpty", () => {
    test("returns true for empty array literals", () => {
      expect(Array.isEmpty([||])) |> toEqual(true)
    });

    test("returns false for literals a non-zero number of elements", () => {
      expect(Array.isEmpty([|1234|])) |> toEqual(false)
    });
  });

  describe("initialize", () => {
    test("create empty array", () => {
      expect(Array.initialize(0, ~f=Fun.identity)) |> toEqual([||])
    });

    test("negative length gives an empty array", () => {
      expect(Array.initialize(-1, ~f=Fun.identity)) |> toEqual([||])
    });

    test("create array initialize", () => {
      expect(Array.initialize(3, ~f=Fun.identity)) |> toEqual([|0, 1, 2|])
    });
  });

  describe("repeat", () => {
    test("length zero creates an empty array", () => {
      expect(Array.repeat(0, ~length=0)) |> toEqual([||])
    });

    test("negative length gives an empty array", () => {
      expect(Array.repeat(~length=-1, 0)) |> toEqual([||])
    });

    test("create array of ints", () => {
      expect(Array.repeat(0, ~length=3)) |> toEqual([|0, 0, 0|])
    });

    test("create array strings", () => {
      expect(Array.repeat("cat", ~length=3))
      |> toEqual([|"cat", "cat", "cat"|])
    });
  });

  describe("range", () => {
    test(
      "returns an array of the integers from zero and upto but not including [to]",
      () => {
      expect(Array.range(5)) |> toEqual([|0, 1, 2, 3, 4|])
    });

    test("returns an empty array [to] is zero", () => {
      expect(Array.range(0)) |> toEqual([||])
    });

    test("takes an optional [from] argument to start create empty array", () => {
      expect(Array.range(~from=2, 5)) |> toEqual([|2, 3, 4|])
    });

    test("can start from negative values", () => {
      expect(Array.range(~from=-2, 3)) |> toEqual([|(-2), (-1), 0, 1, 2|])
    });

    test("returns an empty array [from] > [to_]", () => {
      expect(Array.range(~from=5, 0)) |> toEqual([||])
    });
  });

  describe("fromList", () => {
    test("transforms a list into an array of the same elements", () => {
      expect(Array.(fromList([1, 2, 3]))) |> toEqual([|1, 2, 3|])
    })
  });

  describe("toList", () => {
    test("transform an array into a list of the same elements", () => {
      expect(Array.toList([|1, 2, 3|])) |> toEqual([1, 2, 3])
    })
  });

  describe("toIndexedList", () => {
    test("returns an empty list for an empty array", () => {
      expect(Array.toIndexedList([||])) |> toEqual([])
    });

    test("transforms an array into a list of tuples", () => {
      expect(Array.toIndexedList([|"cat", "dog"|]))
      |> toEqual([(0, "cat"), (1, "dog")])
    });
  });

  describe("get", () => {
    test("returns Some for an in-bounds index", () => {
      expect([|"cat", "dog", "eel"|][2]) |> toEqual("eel")
    });

    testAll("throws for an out of bounds index", [(-1), 3, 5], index => {
      expect(() =>
        [|0, 1, 2|][index]
      ) |> toThrow
    });

    test("throws for an empty array", () => {
      expect(() =>
        [||][0]
      ) |> toThrow
    });
  });

  describe("getAt", () => {
    test("returns Some for an in-bounds index", () => {
      expect(Array.getAt(~index=2, [|"cat", "dog", "eel"|]))
      |> toEqual(Some("eel"))
    });

    test("returns None for an out of bounds index", () => {
      expect(Array.getAt(~index=5, [|0, 1, 2|])) |> toEqual(None)
    });

    test("returns None for an empty array", () => {
      expect(Array.getAt(~index=0, [||])) |> toEqual(None)
    });
  });

  describe("set", () => {
    test("can set a value at an index", () => {
      let numbers = [|1, 2, 3|];
      numbers[0] = 0;
      expect(numbers) |> toEqual([|0, 2, 3|]);
    })
  });

  describe("setAt", () => {
    test("can be partially applied to set an element", () => {
      let setZero = Array.setAt(~value=0);
      let numbers = [|1, 2, 3|];
      setZero(numbers, ~index=2);
      setZero(numbers, ~index=1);
      expect(numbers) |> toEqual([|1, 0, 0|]);
    });

    test("can be partially applied to set an index", () => {
      let setZerothElement = Array.setAt(~index=0);
      let animals = [|"ant", "bat", "cat"|];
      setZerothElement(animals, ~value="antelope");
      expect(animals) |> toEqual([|"antelope", "bat", "cat"|]);
    });
  });

  describe("sum", () => {
    test("equals zero for an empty array", () => {
      expect(Array.sum([||], (module Int))) |> toEqual(0)
    });

    test("adds up the elements on an integer array", () => {
      expect(Array.sum([|1, 2, 3|], (module Int))) |> toEqual(6)
    });
  });

  describe("filter", () => {
    test("keep elements that [f] returns [true] for", () => {
      expect(Array.filter(~f=Int.isEven, [|1, 2, 3, 4, 5, 6|]))
      |> toEqual([|2, 4, 6|])
    })
  });

  describe("swap", () => {
    test("switches values at the given indicies", () => {
      let numbers = [|1, 2, 3|];
      Array.swap(numbers, 1, 2);
      expect(numbers) |> toEqual([|1, 3, 2|]);
    })
  });

  describe("map", () => {
    test("Apply a function [f] to every element in an array", () => {
      expect(Array.map(~f=sqrt, [|1.0, 4.0, 9.0|]))
      |> toEqual([|1.0, 2.0, 3.0|])
    })
  });

  describe("mapI", () => {
    test("equals an array literal of the same value", () => {
      expect(Array.mapI(~f=( * ), [|5, 5, 5|])) |> toEqual([|0, 5, 10|])
    })
  });

  describe("map2", () => {
    test("works the order of arguments to `f` is not important", () => {
      expect(Array.map2(~f=(+), [|1, 2, 3|], [|4, 5, 6|]))
      |> toEqual([|5, 7, 9|])
    });

    test("works the order of `f` is important", () => {
      expect(
        Array.map2(
          ~f=Tuple.make,
          [|"alice", "bob", "chuck"|],
          [|2, 5, 7, 8|],
        ),
      )
      |> toEqual([|("alice", 2), ("bob", 5), ("chuck", 7)|])
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
    |> toEqual([|
         ("alice", 2, true),
         ("bob", 5, false),
         ("chuck", 7, true),
       |])
  });

  test("bind", () => {
    let duplicate = n => [|n, n|];
    expect(Array.bind(~f=duplicate, [|1, 2, 3|]))
    |> toEqual([|1, 1, 2, 2, 3, 3|]);
  });

  describe("sliding", () => {
    test("size 1", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=1))
      |> toEqual([|[|1|], [|2|], [|3|], [|4|], [|5|]|])
    });

    test("size 2", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2))
      |> toEqual([|[|1, 2|], [|2, 3|], [|3, 4|], [|4, 5|]|])
    });

    test("step 3 ", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=3))
      |> toEqual([|[|1, 2, 3|], [|2, 3, 4|], [|3, 4, 5|]|])
    });

    test("size 2, step 2", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=2))
      |> toEqual([|[|1, 2|], [|3, 4|]|])
    });

    test("size 1, step 3", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=1, ~step=3))
      |> toEqual([|[|1|], [|4|]|])
    });

    test("size 2, step 3", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=2, ~step=3))
      |> toEqual([|[|1, 2|], [|4, 5|]|])
    });

    test("step 7", () => {
      expect(Array.sliding([|1, 2, 3, 4, 5|], ~size=7)) |> toEqual([||])
    });
  });

  describe("find", () => {
    test("returns the first element which `f` returns true for", () => {
      expect(Array.find(~f=Int.isEven, [|1, 3, 4, 8|]))
      |> toEqual(Some(4))
    });

    test("returns `None` if `f` returns false for all elements ", () => {
      expect(Array.find(~f=Int.isOdd, [|0, 2, 4, 8|])) |> toEqual(None)
    });

    test("returns `None` for an empty array", () => {
      expect(Array.find(~f=Int.isEven, [||])) |> toEqual(None)
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
      |> toEqual(Some((3, 8)))
    });

    test("returns `None` if `f` returns false for all elements ", () => {
      expect(Array.findIndex(~f=(_, _) => false, [|0, 2, 4, 8|]))
      |> toEqual(None)
    });

    test("returns `None` for an empty array", () => {
      expect(
        Array.findIndex(
          ~f=(index, number) => index > 2 && Int.isEven(number),
          [||],
        ),
      )
      |> toEqual(None)
    });
  });

  describe("any", () => {
    test("returns false for empty arrays", () => {
      expect(Array.any([||], ~f=Int.isEven)) |> toEqual(false)
    });

    test(
      "returns true if at least one of the elements of an array return true for [f]",
      () => {
      expect(Array.any([|1, 3, 4, 5, 7|], ~f=Int.isEven)) |> toEqual(true)
    });

    test(
      "returns false if all of the elements of an array return false for [f]",
      () => {
      expect(Array.any([|1, 3, 5, 7|], ~f=Int.isEven)) |> toEqual(false)
    });
  });

  describe("all", () => {
    test("returns true for empty arrays", () => {
      expect(Array.all(~f=Int.isEven, [||])) |> toEqual(true)
    });

    test("returns true if [f] returns true for all elements", () => {
      expect(Array.all(~f=Int.isEven, [|2, 4|])) |> toEqual(true)
    });

    test("returns false if a single element fails returns false for [f]", () => {
      expect(Array.all(~f=Int.isEven, [|2, 3|])) |> toEqual(false)
    });
  });

  test("append", () => {
    expect(
      Array.append(
        Array.repeat(~length=2, 42),
        Array.repeat(~length=3, 81),
      ),
    )
    |> toEqual([|42, 42, 81, 81, 81|])
  });

  test("concatenate", () => {
    expect(Array.concatenate([|[|1, 2|], [|3|], [|4, 5|]|]))
    |> toEqual([|1, 2, 3, 4, 5|])
  });

  describe("intersperse", () => {
    test("equals an array literal of the same value", () => {
      expect(
        Array.intersperse(~sep="on", [|"turtles", "turtles", "turtles"|]),
      )
      |> toEqual([|"turtles", "on", "turtles", "on", "turtles"|])
    });

    test("equals an array literal of the same value", () => {
      expect(Array.intersperse(~sep=0, [||])) |> toEqual([||])
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
      expect(Array.slice(~from=1, array)) |> toEqual([|1, 2, 3, 4|])
    });

    test("a negative `from`", () => {
      expect(Array.slice(~from=-1, array)) |> toEqual([|4|])
    });

    testAll("`from` >= `length`", positiveArrayLengths, from =>
      expect(Array.slice(~from, array)) |> toEqual([||])
    );

    testAll("`from` <= negative `length`", negativeArrayLengths, from =>
      expect(Array.slice(~from, array)) |> toEqual(array)
    );

    test("a positive `to_`", () => {
      expect(Array.slice(~from=0, ~to_=3, array)) |> toEqual([|0, 1, 2|])
    });

    test("a negative `to_`", () => {
      expect(Array.slice(~from=1, ~to_=-1, array)) |> toEqual([|1, 2, 3|])
    });

    testAll("`to_` >= length", positiveArrayLengths, to_ =>
      expect(Array.slice(~from=0, ~to_, array)) |> toEqual(array)
    );

    testAll("`to_` <= negative `length`", negativeArrayLengths, to_ =>
      expect(Array.slice(~from=0, ~to_, array)) |> toEqual([||])
    );

    test("both `from` and `to_` are negative and `from` < `to_`", () =>
      expect(Array.slice(~from=-2, ~to_=-1, array)) |> toEqual([|3|])
    );

    test("works `from` >= `to_`", () => {
      expect(Array.slice(~from=4, ~to_=3, array)) |> toEqual([||])
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
      expect(array) |> toEqual([|3, 2, 1|]);
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
  //   expect(calledValues) |> toEqual([|1, 2, 3|]);
  // );
});

describe("Char", () => {
  test("toCode", () => {
    expect(Char.toCode('a')) |> toEqual(97)
  });

  describe("fromCode", () => {
    test("valid ASCII codes return the corresponding character", () => {
      expect(Char.fromCode(97)) |> toEqual(Some('a'))
    });
    test("negative integers return none", () => {
      expect(Char.fromCode(-1)) |> toEqual(None)
    });
    test("integers greater than 255 return none", () => {
      expect(Char.fromCode(256)) |> toEqual(None)
    });
  });

  test("toString", () => {
    expect(Char.toString('a')) |> toEqual("a")
  });

  describe("fromString", () => {
    test("one-length string return Some", () => {
      expect(Char.fromString("a")) |> toEqual(Some('a'))
    });
    test("multi character strings return none", () => {
      expect(Char.fromString("abc")) |> toEqual(None)
    });
    test("zero length strings return none", () => {
      expect(Char.fromString("")) |> toEqual(None)
    });
  });

  describe("toLowercase", () => {
    test("converts uppercase ASCII characters to lowercase", () => {
      expect(Char.toLowercase('A')) |> toEqual('a')
    });
    test("perserves lowercase characters", () => {
      expect(Char.toLowercase('a')) |> toEqual('a')
    });
    test("perserves non-alphabet characters", () => {
      expect(Char.toLowercase('7')) |> toEqual('7')
    });
    test("perserves non-ASCII characters", () => {
      expect(Char.toUppercase('\233')) |> toEqual('\233')
    });
  });

  describe("toUppercase", () => {
    test("converts lowercase ASCII characters to uppercase", () => {
      expect(Char.toUppercase('a')) |> toEqual('A')
    });
    test("perserves uppercase characters", () => {
      expect(Char.toUppercase('A')) |> toEqual('A')
    });
    test("perserves non-alphabet characters", () => {
      expect(Char.toUppercase('7')) |> toEqual('7')
    });
    test("perserves non-ASCII characters", () => {
      expect(Char.toUppercase('\233')) |> toEqual('\233')
    });
  });

  describe("toDigit", () => {
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('0')) |> toEqual(Some(0))
    );
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('8')) |> toEqual(Some(8))
    );
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('a')) |> toEqual(None)
    );
  });

  describe("isLowercase", () => {
    test("returns true for any lowercase character", () => {
      expect(Char.isLowercase('a')) |> toEqual(true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isLowercase('7')) |> toEqual(false)
    });
    test("returns false for non-ASCII characters", () => {
      expect(Char.isLowercase('\236')) |> toEqual(false)
    });
  });

  describe("isUppercase", () => {
    test("returns true for any uppercase character", () => {
      expect(Char.isUppercase('A')) |> toEqual(true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isUppercase('7')) |> toEqual(false)
    });
    test("returns false for non-ASCII characters", () => {
      expect(Char.isLowercase('\237')) |> toEqual(false)
    });
  });

  describe("isLetter", () => {
    test("returns true for any ASCII alphabet character", () => {
      expect(Char.isLetter('A')) |> toEqual(true)
    });

    testAll(
      "returns false for all other characters",
      ['7', ' ', '\n', '\011', '\236'],
      char =>
      expect(Char.isLetter(char)) |> toEqual(false)
    );
  });

  describe("isDigit", () => {
    testAll(
      "returns true for digits 0-9",
      ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
      digit =>
      expect(Char.isDigit(digit)) |> toEqual(true)
    );
    test("returns false for all other characters", () => {
      expect(Char.isDigit('a')) |> toEqual(false)
    });
  });

  describe("isAlphanumeric", () => {
    test("returns true for any alphabet or digit character", () => {
      expect(Char.isAlphanumeric('A')) |> toEqual(true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isAlphanumeric('?')) |> toEqual(false)
    });
  });

  describe("isPrintable", () => {
    test("returns true for a printable character", () => {
      expect(Char.isPrintable('~')) |> toEqual(true)
    });

    test("returns false for non-printable character", () => {
      expect(Char.fromCode(31) |> Option.map(~f=Char.isPrintable))
      |> toEqual(Some(false))
    });
  });

  describe("isWhitespace", () => {
    test("returns true for any whitespace character", () => {
      expect(Char.isWhitespace(' ')) |> toEqual(true)
    });
    test("returns false for a non-whitespace character", () => {
      expect(Char.isWhitespace('a')) |> toEqual(false)
    });
  });
});

describe(
  "Float",
  Float.(
    () => {
      test("zero", () => {
        expect(zero) |> toEqual(0.)
      });

      test("one", () => {
        expect(one) |> toEqual(1.)
      });

      test("nan", () => {
        expect(nan == nan) |> toEqual(false)
      });

      test("infinity", () => {
        expect(infinity > 0.) |> toEqual(true)
      });

      test("negativeInfinity", () => {
        expect(negativeInfinity < 0.) |> toEqual(true)
      });

      describe("equals", () => {
        test("zero", () => {
          expect(0. == (-0.)) |> toBe(true)
        })
      });

      describe("add", () => {
        test("add", () => {
          expect(add(3.14, 3.14)) |> toEqual(6.28)
        });
        test("+", () => {
          expect(3.14 + 3.14) |> toEqual(6.28)
        });
      });

      describe("subtract", () => {
        test("subtract", () => {
          expect(subtract(4., 3.)) |> toEqual(1.)
        });
        test("-", () => {
          expect(4. - 3.) |> toEqual(1.)
        });
      });

      describe("multiply", () => {
        test("multiply", () => {
          expect(multiply(2., 7.)) |> toEqual(14.)
        });
        test("*", () => {
          expect(2. * 7.) |> toEqual(14.)
        });
      });

      describe("divide", () => {
        test("divide", () => {
          expect(divide(3.14, ~by=2.)) |> toEqual(1.57)
        });
        test("divide by zero", () => {
          expect(divide(3.14, ~by=0.)) |> toEqual(infinity)
        });
        test("divide by negative zero", () => {
          expect(divide(3.14, ~by=-0.)) |> toEqual(negativeInfinity)
        });

        test("/", () => {
          expect(3.14 / 2.) |> toEqual(1.57)
        });
      });

      describe("power", () => {
        test("power", () => {
          expect(power(~base=7., ~exponent=3.)) |> toEqual(343.)
        });
        test("0 base", () => {
          expect(power(~base=0., ~exponent=3.)) |> toEqual(0.)
        });
        test("0 exponent", () => {
          expect(power(~base=7., ~exponent=0.)) |> toEqual(1.)
        });
        test("**", () => {
          expect(7. ** 3.) |> toEqual(343.)
        });
      });

      describe("negate", () => {
        test("positive number", () => {
          expect(negate(8.)) |> toEqual(-8.)
        });
        test("negative number", () => {
          expect(negate(-7.)) |> toEqual(7.)
        });
        test("zero", () => {
          expect(negate(0.)) |> toEqual(-0.)
        });
        test("~-", () => {
          expect(-7.) |> toEqual(-7.)
        });
      });

      describe("absolute", () => {
        test("positive number", () => {
          expect(absolute(8.)) |> toEqual(8.)
        });
        test("negative number", () => {
          expect(absolute(-7.)) |> toEqual(7.)
        });
        test("zero", () => {
          expect(absolute(0.)) |> toEqual(0.)
        });
      });

      describe("maximum", () => {
        test("positive numbers", () => {
          expect(maximum(7., 9.)) |> toEqual(9.)
        });
        test("negative numbers", () => {
          expect(maximum(-4., -1.)) |> toEqual(-1.)
        });
        test("nan", () => {
          expect(maximum(7., nan)) |> toEqual(nan)
        });
        test("infinity", () => {
          expect(maximum(7., infinity)) |> toEqual(infinity)
        });
        test("negativeInfinity", () => {
          expect(maximum(7., negativeInfinity)) |> toEqual(7.)
        });
      });

      describe("minimum", () => {
        test("positive numbers", () => {
          expect(minimum(7., 9.)) |> toEqual(7.)
        });
        test("negative numbers", () => {
          expect(minimum(-4., -1.)) |> toEqual(-4.)
        });
        test("nan", () => {
          expect(minimum(7., nan)) |> toEqual(nan)
        });
        test("infinity", () => {
          expect(minimum(7., infinity)) |> toEqual(7.)
        });
        test("negativeInfinity", () => {
          expect(minimum(7., negativeInfinity)) |> toEqual(negativeInfinity)
        });
      });

      describe("clamp", () => {
        test("in range", () => {
          expect(clamp(~lower=0., ~upper=8., 5.)) |> toEqual(5.)
        });
        test("above range", () => {
          expect(clamp(~lower=0., ~upper=8., 9.)) |> toEqual(8.)
        });
        test("below range", () => {
          expect(clamp(~lower=2., ~upper=8., 1.)) |> toEqual(2.)
        });
        test("above negative range", () => {
          expect(clamp(~lower=-10., ~upper=-5., 5.)) |> toEqual(-5.)
        });
        test("below negative range", () => {
          expect(clamp(~lower=-10., ~upper=-5., -15.)) |> toEqual(-10.)
        });
        test("nan upper bound", () => {
          expect(clamp(~lower=-7.9, ~upper=nan, -6.6)) |> toEqual(nan)
        });
        test("nan lower bound", () => {
          expect(clamp(~lower=nan, ~upper=0., -6.6)) |> toEqual(nan)
        });
        test("nan value", () => {
          expect(clamp(~lower=2., ~upper=8., nan)) |> toEqual(nan)
        });
        test("invalid arguments", () => {
          expect(() => {
            clamp(~lower=7., ~upper=1., 3.)
          }) |> toThrow
        });
      });

      describe("squareRoot", () => {
        test("whole numbers", () => {
          expect(squareRoot(4.)) |> toEqual(2.)
        });
        test("decimal numbers", () => {
          expect(squareRoot(20.25)) |> toEqual(4.5)
        });
        test("negative number", () => {
          expect(squareRoot(-1.)) |> toEqual(nan)
        });
      });

      describe("log", () => {
        test("base 10", () => {
          expect(log(~base=10., 100.)) |> toEqual(2.)
        });
        test("base 2", () => {
          expect(log(~base=2., 256.)) |> toEqual(8.)
        });
        test("of zero", () => {
          expect(log(~base=10., 0.)) |> toEqual(negativeInfinity)
        });
      });

      describe("isNaN", () => {
        test("nan", () => {
          expect(isNaN(nan)) |> toEqual(true)
        });
        test("non-nan", () => {
          expect(isNaN(91.4)) |> toEqual(false)
        });
      });

      describe("isFinite", () => {
        test("infinity", () => {
          expect(isFinite(infinity)) |> toEqual(false)
        });
        test("negative infinity", () => {
          expect(isFinite(negativeInfinity)) |> toEqual(false)
        });
        test("NaN", () => {
          expect(isFinite(nan)) |> toEqual(false)
        });
        testAll("regular numbers", [(-5.), (-0.314), 0., 3.14], n =>
          expect(isFinite(n)) |> toEqual(true)
        );
      });

      describe("isInfinite", () => {
        test("infinity", () => {
          expect(isInfinite(infinity)) |> toEqual(true)
        });
        test("negative infinity", () => {
          expect(isInfinite(negativeInfinity)) |> toEqual(true)
        });
        test("NaN", () => {
          expect(isInfinite(nan)) |> toEqual(false)
        });
        testAll("regular numbers", [(-5.), (-0.314), 0., 3.14], n =>
          expect(isInfinite(n)) |> toEqual(false)
        );
      });

      describe("inRange", () => {
        test("in range", () => {
          expect(inRange(~lower=2., ~upper=4., 3.)) |> toEqual(true)
        });
        test("above range", () => {
          expect(inRange(~lower=2., ~upper=4., 8.)) |> toEqual(false)
        });
        test("below range", () => {
          expect(inRange(~lower=2., ~upper=4., 1.)) |> toEqual(false)
        });
        test("equal to ~upper", () => {
          expect(inRange(~lower=1., ~upper=2., 2.)) |> toEqual(false)
        });
        test("negative range", () => {
          expect(inRange(~lower=-7.9, ~upper=-5.2, -6.6)) |> toEqual(true)
        });
        test("nan upper bound", () => {
          expect(inRange(~lower=-7.9, ~upper=nan, -6.6)) |> toEqual(false)
        });
        test("nan lower bound", () => {
          expect(inRange(~lower=nan, ~upper=0., -6.6)) |> toEqual(false)
        });
        test("nan value", () => {
          expect(inRange(~lower=2., ~upper=8., nan)) |> toEqual(false)
        });
        test("invalid arguments", () => {
          expect(() => {
            inRange(~lower=7., ~upper=1., 3.)
          }) |> toThrow
        });
      });

      test("hypotenuse", () => {
        expect(hypotenuse(3., 4.)) |> toEqual(5.)
      });

      test("degrees", () => {
        expect(degrees(180.)) |> toEqual(pi)
      });

      test("radians", () => {
        expect(radians(pi)) |> toEqual(pi)
      });

      test("turns", () => {
        expect(turns(1.)) |> toEqual(2. * pi)
      });

      describe("fromPolar", () => {
        let (x, y) = fromPolar((squareRoot(2.), degrees(45.)));
        test("x", () => {
          expect(x) |> toBeCloseTo(1.)
        });
        test("y", () => {
          expect(y) |> toBeCloseTo(1.)
        });
      });

      describe("toPolar", () => {
        test("toPolar", () => {
          expect(toPolar((3.0, 4.0))) |> toEqual((5.0, 0.9272952180016122))
        });

        test("toPolar", () => {
          expect(toPolar((5.0, 12.0)))
          |> toEqual((13.0, 1.1760052070951352))
        });
      });

      describe("cos", () => {
        test("cos", () => {
          expect(cos(degrees(60.))) |> toEqual(0.5000000000000001)
        });

        test("cos", () => {
          expect(cos(radians(pi / 3.))) |> toEqual(0.5000000000000001)
        });
      });

      describe("acos", () => {
        test("1 / 2", () =>
          expect(acos(1. / 2.)) |> toEqual(1.0471975511965979)
        )
      });

      describe("sin", () => {
        test("30 degrees", () => {
          expect(sin(degrees(30.))) |> toEqual(0.49999999999999994)
        });
        test("pi / 6", () => {
          expect(sin(radians(pi / 6.))) |> toEqual(0.49999999999999994)
        });
      });

      describe("asin", () => {
        test("asin", () =>
          expect(asin(1. / 2.)) |> toEqual(0.5235987755982989)
        )
      });

      describe("tan", () => {
        test("45 degrees", () => {
          expect(tan(degrees(45.))) |> toEqual(0.9999999999999999)
        });
        test("pi / 4", () => {
          expect(tan(radians(pi / 4.))) |> toEqual(0.9999999999999999)
        });
        test("0", () => {
          expect(tan(0.)) |> toEqual(0.)
        });
      });

      describe("atan", () => {
        test("0", () => {
          expect(atan(0.)) |> toEqual(0.)
        });
        test("1 / 1", () => {
          expect(atan(1. / 1.)) |> toEqual(0.7853981633974483)
        });
        test("1 / -1", () => {
          expect(atan(1. / (-1.))) |> toEqual(-0.7853981633974483)
        });
        test("-1 / -1", () => {
          expect(atan((-1.) / (-1.))) |> toEqual(0.7853981633974483)
        });
        test("-1 / -1", () => {
          expect(atan((-1.) / 1.)) |> toEqual(-0.7853981633974483)
        });
      });

      describe("atan2", () => {
        test("0", () => {
          expect(atan2(~y=0., ~x=0.)) |> toEqual(0.)
        });
        test("(1, 1)", () => {
          expect(atan2(~y=1., ~x=1.)) |> toEqual(0.7853981633974483)
        });
        test("(-1, 1)", () => {
          expect(atan2(~y=1., ~x=-1.)) |> toEqual(2.3561944901923449)
        });
        test("(-1 -1)", () => {
          expect(atan2(~y=-1., ~x=-1.)) |> toEqual(-2.3561944901923449)
        });
        test("(1, -1)", () => {
          expect(atan2(~y=-1., ~x=1.)) |> toEqual(-0.7853981633974483)
        });
      });

      describe("round", () => {
        test("`Zero", () => {
          expect(round(~direction=`Zero, 1.2)) |> toEqual(1.)
        });
        test("`Zero", () => {
          expect(round(~direction=`Zero, 1.5)) |> toEqual(1.)
        });
        test("`Zero", () => {
          expect(round(~direction=`Zero, 1.8)) |> toEqual(1.)
        });
        test("`Zero", () => {
          expect(round(~direction=`Zero, -1.2)) |> toEqual(-1.)
        });
        test("`Zero", () => {
          expect(round(~direction=`Zero, -1.5)) |> toEqual(-1.)
        });
        test("`Zero", () => {
          expect(round(~direction=`Zero, -1.8)) |> toEqual(-1.)
        });

        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, 1.2)) |> toEqual(2.)
        });
        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, 1.5)) |> toEqual(2.)
        });
        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, 1.8)) |> toEqual(2.)
        });
        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, -1.2)) |> toEqual(-2.)
        });
        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, -1.5)) |> toEqual(-2.)
        });
        test("`AwayFromZero", () => {
          expect(round(~direction=`AwayFromZero, -1.8)) |> toEqual(-2.)
        });

        test("`Up", () => {
          expect(round(~direction=`Up, 1.2)) |> toEqual(2.)
        });
        test("`Up", () => {
          expect(round(~direction=`Up, 1.5)) |> toEqual(2.)
        });
        test("`Up", () => {
          expect(round(~direction=`Up, 1.8)) |> toEqual(2.)
        });
        test("`Up", () => {
          expect(round(~direction=`Up, -1.2)) |> toEqual(-1.)
        });
        test("`Up", () => {
          expect(round(~direction=`Up, -1.5)) |> toEqual(-1.)
        });
        test("`Up", () => {
          expect(round(~direction=`Up, -1.8)) |> toEqual(-1.)
        });

        test("`Down", () => {
          expect(round(~direction=`Down, 1.2)) |> toEqual(1.)
        });
        test("`Down", () => {
          expect(round(~direction=`Down, 1.5)) |> toEqual(1.)
        });
        test("`Down", () => {
          expect(round(~direction=`Down, 1.8)) |> toEqual(1.)
        });
        test("`Down", () => {
          expect(round(~direction=`Down, -1.2)) |> toEqual(-2.)
        });
        test("`Down", () => {
          expect(round(~direction=`Down, -1.5)) |> toEqual(-2.)
        });
        test("`Down", () => {
          expect(round(~direction=`Down, -1.8)) |> toEqual(-2.)
        });

        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), 1.2)) |> toEqual(1.)
        });
        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), 1.5)) |> toEqual(1.)
        });
        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), 1.8)) |> toEqual(2.)
        });
        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), -1.2)) |> toEqual(-1.)
        });
        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), -1.5)) |> toEqual(-1.)
        });
        test("`Closest `Zero", () => {
          expect(round(~direction=`Closest(`Zero), -1.8)) |> toEqual(-2.)
        });

        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), 1.2))
          |> toEqual(1.)
        });
        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), 1.5))
          |> toEqual(2.)
        });
        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), 1.8))
          |> toEqual(2.)
        });
        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), -1.2))
          |> toEqual(-1.)
        });
        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), -1.5))
          |> toEqual(-2.)
        });
        test("`Closest `AwayFromZero", () => {
          expect(round(~direction=`Closest(`AwayFromZero), -1.8))
          |> toEqual(-2.)
        });

        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), 1.2)) |> toEqual(1.)
        });
        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), 1.5)) |> toEqual(2.)
        });
        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), 1.8)) |> toEqual(2.)
        });
        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), -1.2)) |> toEqual(-1.)
        });
        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), -1.5)) |> toEqual(-1.)
        });
        test("`Closest `Up", () => {
          expect(round(~direction=`Closest(`Up), -1.8)) |> toEqual(-2.)
        });

        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), 1.2)) |> toEqual(1.)
        });
        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), 1.5)) |> toEqual(1.)
        });
        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), 1.8)) |> toEqual(2.)
        });
        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), -1.2)) |> toEqual(-1.)
        });
        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), -1.5)) |> toEqual(-2.)
        });
        test("`Closest `Down", () => {
          expect(round(~direction=`Closest(`Down), -1.8)) |> toEqual(-2.)
        });

        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 1.2)) |> toEqual(1.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 1.5)) |> toEqual(2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 1.8)) |> toEqual(2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 2.2)) |> toEqual(2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 2.5)) |> toEqual(2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), 2.8)) |> toEqual(3.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -1.2)) |> toEqual(-1.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -1.5)) |> toEqual(-2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -1.8)) |> toEqual(-2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -2.2)) |> toEqual(-2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -2.5)) |> toEqual(-2.)
        });
        test("`Closest `ToEven", () => {
          expect(round(~direction=`Closest(`ToEven), -2.8)) |> toEqual(-3.)
        });
      });

      describe("floor", () => {
        test("floor", () => {
          expect(floor(1.2)) |> toEqual(1.)
        });
        test("floor", () => {
          expect(floor(1.5)) |> toEqual(1.)
        });
        test("floor", () => {
          expect(floor(1.8)) |> toEqual(1.)
        });
        test("floor", () => {
          expect(floor(-1.2)) |> toEqual(-2.)
        });
        test("floor", () => {
          expect(floor(-1.5)) |> toEqual(-2.)
        });
        test("floor", () => {
          expect(floor(-1.8)) |> toEqual(-2.)
        });
      });

      describe("ceiling", () => {
        test("ceiling", () => {
          expect(ceiling(1.2)) |> toEqual(2.)
        });
        test("ceiling", () => {
          expect(ceiling(1.5)) |> toEqual(2.)
        });
        test("ceiling", () => {
          expect(ceiling(1.8)) |> toEqual(2.)
        });
        test("ceiling", () => {
          expect(ceiling(-1.2)) |> toEqual(-1.)
        });
        test("ceiling", () => {
          expect(ceiling(-1.5)) |> toEqual(-1.)
        });
        test("ceiling", () => {
          expect(ceiling(-1.8)) |> toEqual(-1.)
        });
      });

      describe("truncate", () => {
        test("truncate", () => {
          expect(truncate(1.2)) |> toEqual(1.)
        });
        test("truncate", () => {
          expect(truncate(1.5)) |> toEqual(1.)
        });
        test("truncate", () => {
          expect(truncate(1.8)) |> toEqual(1.)
        });
        test("truncate", () => {
          expect(truncate(-1.2)) |> toEqual(-1.)
        });
        test("truncate", () => {
          expect(truncate(-1.5)) |> toEqual(-1.)
        });
        test("truncate", () => {
          expect(truncate(-1.8)) |> toEqual(-1.)
        });
      });

      describe("fromInt", () => {
        test("5", () => {
          expect(fromInt(5)) |> toEqual(5.0)
        });
        test("0", () => {
          expect(zero) |> toEqual(0.0)
        });
        test("-7", () => {
          expect(fromInt(-7)) |> toEqual(-7.0)
        });
      });

      describe("toInt", () => {
        test("5.", () => {
          expect(toInt(5.)) |> toEqual(Some(5))
        });
        test("5.3", () => {
          expect(toInt(5.3)) |> toEqual(Some(5))
        });
        test("0.", () => {
          expect(toInt(0.)) |> toEqual(Some(0))
        });
        test("-7.", () => {
          expect(toInt(-7.)) |> toEqual(Some(-7))
        });
        test("nan", () => {
          expect(toInt(nan)) |> toEqual(None)
        });
        test("infinity", () => {
          expect(toInt(infinity)) |> toEqual(None)
        });
        test("negativeInfinity", () => {
          expect(toInt(negativeInfinity)) |> toEqual(None)
        });
      });
    }
  ),
);

describe("Int", () => {
  open Int;
  test("zero", () => {
    expect(zero) |> toEqual(0)
  });

  test("one", () => {
    expect(one) |> toEqual(1)
  });

  test("minimumValue", () => {
    expect(minimumValue - 1) |> toEqual(maximumValue)
  });

  test("maximumValue", () => {
    expect(maximumValue + 1) |> toEqual(minimumValue)
  });

  describe("add", () => {
    test("add", () => {
      expect(add(3002, 4004)) |> toEqual(7006)
    });
    test("+", () => {
      expect(3002 + 4004) |> toEqual(7006)
    });
  });

  describe("subtract", () => {
    test("subtract", () => {
      expect(subtract(4, 3)) |> toEqual(1)
    });
    test("-", () => {
      expect(4 - 3) |> toEqual(1)
    });
  });

  describe("multiply", () => {
    test("multiply", () => {
      expect(multiply(2, 7)) |> toEqual(14)
    });
    test("*", () => {
      expect(2 * 7) |> toEqual(14)
    });
  });

  describe("divide", () => {
    test("divide", () => {
      expect(divide(3, ~by=2)) |> toEqual(1)
    });
    test("division by zero", () => {
      expect(() => {
        divide(3, ~by=0)
      }) |> toThrow
    });

    test("/", () => {
      expect(27 / 5) |> toEqual(5)
    });

    test("//", () => {
      expect(3 /\/ 2) |> toEqual(1.5)
    });
    test("//", () => {
      expect(27 /\/ 5) |> toEqual(5.4)
    });
    test("//", () => {
      expect(8 /\/ 4) |> toEqual(2.0)
    });

    test("x // 0", () => {
      expect(8 /\/ 0) |> toEqual(Float.infinity)
    });
    test("-x // 0", () => {
      expect((-8) /\/ 0) |> toEqual(Float.negativeInfinity)
    });
  });

  describe("power", () => {
    test("power", () => {
      expect(power(~base=7, ~exponent=3)) |> toEqual(343)
    });
    test("0 base", () => {
      expect(power(~base=0, ~exponent=3)) |> toEqual(0)
    });
    test("0 exponent", () => {
      expect(power(~base=7, ~exponent=0)) |> toEqual(1)
    });
    test("**", () => {
      expect(7 ** 3) |> toEqual(343)
    });
  });

  describe("negate", () => {
    test("positive number", () => {
      expect(negate(8)) |> toEqual(-8)
    });
    test("negative number", () => {
      expect(negate(-7)) |> toEqual(7)
    });
    test("zero", () => {
      expect(negate(0)) |> toEqual(-0)
    });
    test("~-", () => {
      expect(-7) |> toEqual(-7)
    });
  });

  describe("absolute", () => {
    test("positive number", () => {
      expect(absolute(8)) |> toEqual(8)
    });
    test("negative number", () => {
      expect(absolute(-7)) |> toEqual(7)
    });
    test("zero", () => {
      expect(absolute(0)) |> toEqual(0)
    });
  });

  describe("clamp", () => {
    test("in range", () => {
      expect(clamp(~lower=0, ~upper=8, 5)) |> toEqual(5)
    });
    test("above range", () => {
      expect(clamp(~lower=0, ~upper=8, 9)) |> toEqual(8)
    });
    test("below range", () => {
      expect(clamp(~lower=2, ~upper=8, 1)) |> toEqual(2)
    });
    test("above negative range", () => {
      expect(clamp(~lower=-10, ~upper=-5, 5)) |> toEqual(-5)
    });
    test("below negative range", () => {
      expect(clamp(~lower=-10, ~upper=-5, -15)) |> toEqual(-10)
    });
    test("invalid arguments", () => {
      expect(() => {
        clamp(~lower=7, ~upper=1, 3)
      }) |> toThrow
    });
  });

  describe("inRange", () => {
    test("in range", () => {
      expect(inRange(~lower=2, ~upper=4, 3)) |> toEqual(true)
    });
    test("above range", () => {
      expect(inRange(~lower=2, ~upper=4, 8)) |> toEqual(false)
    });
    test("below range", () => {
      expect(inRange(~lower=2, ~upper=4, 1)) |> toEqual(false)
    });
    test("equal to ~upper", () => {
      expect(inRange(~lower=1, ~upper=2, 2)) |> toEqual(false)
    });
    test("negative range", () => {
      expect(inRange(~lower=-7, ~upper=-5, -6)) |> toEqual(true)
    });
    test("invalid arguments", () => {
      expect(() => {
        inRange(~lower=7, ~upper=1, 3)
      }) |> toThrow
    });
  });

  describe("toFloat", () => {
    test("5", () => {
      expect(toFloat(5)) |> toEqual(5.)
    });
    test("0", () => {
      expect(toFloat(0)) |> toEqual(0.)
    });
    test("-7", () => {
      expect(toFloat(-7)) |> toEqual(-7.)
    });
  });

  describe("fromString", () => {
    test("0", () => {
      expect(fromString("0")) |> toEqual(Some(0))
    });
    test("-0", () => {
      expect(fromString("-0")) |> toEqual(Some(-0))
    });
    test("42", () => {
      expect(fromString("42")) |> toEqual(Some(42))
    });
    test("123_456", () => {
      expect(fromString("123_456")) |> toEqual(Some(123_456))
    });
    test("-42", () => {
      expect(fromString("-42")) |> toEqual(Some(-42))
    });
    test("0XFF", () => {
      expect(fromString("0XFF")) |> toEqual(Some(255))
    });
    test("0X000A", () => {
      expect(fromString("0X000A")) |> toEqual(Some(10))
    });
    test("Infinity", () => {
      expect(fromString("Infinity")) |> toEqual(None)
    });
    test("-Infinity", () => {
      expect(fromString("-Infinity")) |> toEqual(None)
    });
    test("NaN", () => {
      expect(fromString("NaN")) |> toEqual(None)
    });
    test("abc", () => {
      expect(fromString("abc")) |> toEqual(None)
    });
    test("--4", () => {
      expect(fromString("--4")) |> toEqual(None)
    });
    test("empty string", () => {
      expect(fromString(" ")) |> toEqual(None)
    });
  });

  describe("toString", () => {
    test("positive number", () => {
      expect(toString(1)) |> toEqual("1")
    });
    test("negative number", () => {
      expect(toString(-1)) |> toEqual("-1")
    });
  });
});

// TODO
// Jest matchers in v24 don't support BigInt
// Once jest 25 is released we can upgrade and enable this suite of tests.
Skip.describe("Integer", () => {
  open Integer;

  describe("add", () => {
    testAll("add", [add, (+)], op => {
      expect(op(fromInt(3002), fromInt(4004))) |> toEqual(fromInt(7006))
    });
  });

  describe("subtract", () => {
    testAll("subtract", [subtract, (-)], op => {
      expect(op(fromInt(4), fromInt(3))) |> toEqual(one)
    });
  });

  describe("multiply", () => {
    testAll("multiply", [multiply, (*)], op => {
      expect(op(fromInt(2), fromInt(7))) |> toEqual(fromInt(14))
    });
  });

  describe("divide", () => {
    test("divide", () => {
      expect(divide(fromInt(3), ~by=fromInt(2))) |> toEqual(one)
    });

    test("division by zero", () => {
      expect(() => {
        divide(fromInt(3), ~by=zero)
      }) |> toThrow
    });

    test("/", () => {
      expect(fromInt(27) / fromInt(5)) |> toEqual(fromInt(5))
    });
  });

  describe("power", () => {
    test("**", () => {
      expect(fromInt(7) ** fromInt(3)) |> toEqual(fromInt(343))
    });
  });

  describe("negate", () => {
    test("positive number", () => {
      expect(negate(fromInt(8))) |> toEqual(fromInt(-8))
    });
    test("negative number", () => {
      expect(negate(fromInt(-7))) |> toEqual(fromInt(7))
    });
    test("zero", () => {
      expect(negate(zero)) |> toEqual(zero)
    });
  });

  describe("absolute", () => {
    test("positive number", () => {
      expect(absolute(fromInt(8))) |> toEqual(fromInt(8))
    });
    test("negative number", () => {
      expect(absolute(fromInt(-7))) |> toEqual(fromInt(7))
    });
    test("zero", () => {
      expect(absolute(zero)) |> toEqual(zero)
    });
  });

  describe("clamp", () => {
    test("in range", () => {
      expect(clamp(~lower=zero, ~upper=fromInt(8), fromInt(5))) |> toEqual(fromInt(5))
    });
    test("above range", () => {
      expect(clamp(~lower=zero, ~upper=fromInt(8), fromInt(9))) |> toEqual(fromInt(8))
    });
    test("below range", () => {
      expect(clamp(~lower=fromInt(2), ~upper=fromInt(8), one)) |> toEqual(fromInt(2))
    });
    test("above negative range", () => {
      expect(clamp(~lower=fromInt(-10), ~upper=fromInt(-5), fromInt(5))) |> toEqual(fromInt(-5))
    });
    test("below negative range", () => {
      expect(clamp(~lower=fromInt(-10), ~upper=fromInt(-5), fromInt(-15))) |> toEqual(fromInt(-10))
    });
    test("invalid arguments", () => {
      expect(() => {
        clamp(~lower=fromInt(7), ~upper=one, fromInt(3))
      }) |> toThrow
    });
  });

  describe("inRange", () => {
    test("in range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(3))) |> toEqual(true)
    });
    test("above range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(8))) |> toEqual(false)
    });
    test("below range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(1))) |> toEqual(false)
    });
    test("equal to ~upper", () => {
      expect(inRange(~lower=fromInt(1), ~upper=fromInt(2), fromInt(2))) |> toEqual(false)
    });
    test("negative range", () => {
      expect(inRange(~lower=fromInt(-7), ~upper=fromInt(-5), fromInt(-6))) |> toEqual(true)
    });
    test("invalid arguments", () => {
      expect(() => {
        inRange(~lower=fromInt(7), ~upper=one, fromInt(3))
      }) |> toThrow
    });
  });

  describe("toFloat", () => {
    test("5", () => {
      expect(toFloat(fromInt(5))) |> toEqual(Some(5.))
    });
    test("0", () => {
      expect(toFloat(zero)) |> toEqual(Some(0.))
    });
    test("-7", () => {
      expect(toFloat(fromInt(-7))) |> toEqual(Some(-7.))
    });
  });

  describe("fromString", () => {
    test("0", () => {
      expect(fromString("0")) |> toEqual(Some(zero))
    });
    test("-0", () => {
      expect(fromString("-0")) |> toEqual(Some(zero))
    });
    test("42", () => {
      expect(fromString("42")) |> toEqual(Some(fromInt(42)))
    });
    test("123_456", () => {
      expect(fromString("123_456")) |> toEqual(Some(fromInt(123_456)))
    });
    test("-42", () => {
      expect(fromString("-42")) |> toEqual(Some(fromInt(-42)))
    });
    test("0XFF", () => {
      expect(fromString("0XFF")) |> toEqual(Some(fromInt(255)))
    });
    test("0X000A", () => {
      expect(fromString("0X000A")) |> toEqual(Some(fromInt(10)))
    });
    test("Infinity", () => {
      expect(fromString("Infinity")) |> toEqual(None)
    });
    test("-Infinity", () => {
      expect(fromString("-Infinity")) |> toEqual(None)
    });
    test("NaN", () => {
      expect(fromString("NaN")) |> toEqual(None)
    });
    test("abc", () => {
      expect(fromString("abc")) |> toEqual(None)
    });
    test("--4", () => {
      expect(fromString("--4")) |> toEqual(None)
    });
    test("empty string", () => {
      expect(fromString(" ")) |> toEqual(None)
    });
  });

  describe("toString", () => {
    test("positive number", () => {
      expect(toString(fromInt(1))) |> toEqual("1")
    });
    test("negative number", () => {
      expect(toString(fromInt(-1))) |> toEqual("-1")
    });
  });
});

describe("List", () => {
  open List;

  describe("drop", () => {
    test("from an empty list", () =>
      expect(drop([], ~count=1)) |> toEqual([])
    );

    test("zero elements", () =>
      expect(drop([1, 2, 3], ~count=0)) |> toEqual([1, 2, 3])
    );

    test("the first element", () =>
      expect(drop([1, 2, 3], ~count=1)) |> toEqual([2, 3])
    );

    test("all elements", () =>
      expect(drop([1, 2, 3], ~count=3)) |> toEqual([])
    );

    test("greater than the number of elements", () =>
      expect(drop([1, 2, 3], ~count=4)) |> toEqual([])
    );
  });

  describe("findIndex", () => {
    test(
      "returns the first (index, element) tuple which f returns true for", () => {
      expect(
        findIndex(
          ~f=(index, number) => index > 2 && Int.isEven(number),
          [1, 3, 4, 8],
        ),
      )
      |> toEqual(Some((3, 8)))
    });

    test("returns `None` if `f` returns false for all elements ", () => {
      expect(findIndex(~f=(_, _) => false, [0, 2, 4, 8])) |> toEqual(None)
    });

    test("returns `None` for an empty array", () => {
      expect(
        findIndex(
          ~f=(index, number) => index > 2 && Int.isEven(number),
          [],
        ),
      )
      |> toEqual(None)
    });
  });

  describe("reverse", () => {
    test("reverse empty list", () => {
      expect(reverse([])) |> toEqual([])
    });
    test("reverse one element", () => {
      expect(reverse([0])) |> toEqual([0])
    });
    test("reverse two elements", () => {
      expect(reverse([0, 1])) |> toEqual([1, 0])
    });
  });

  describe("map2", () => {
    test("map2 empty lists", () => {
      expect(map2(~f=(+), [], [])) |> toEqual([])
    });
    test("map2 one element", () => {
      expect(map2(~f=(+), [1], [1])) |> toEqual([2])
    });
    test("map2 two elements", () => {
      expect(map2(~f=(+), [1, 2], [1, 2])) |> toEqual([2, 4])
    });
  });

  describe("mapI", () => {
    test("mapI empty list", () => {
      expect(mapI(~f=(i, _) => i, [])) |> toEqual([])
    });
    test("mapI one element", () => {
      expect(mapI(~f=(i, _) => i, ['a'])) |> toEqual([0])
    });
    test("mapI two elements", () => {
      expect(mapI(~f=(i, _) => i, ['a', 'b'])) |> toEqual([0, 1])
    });
  });

  describe("sliding", () => {
    test("size 1", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=1))
      |> toEqual([[1], [2], [3], [4], [5]])
    });

    test("size 2", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=2))
      |> toEqual([[1, 2], [2, 3], [3, 4], [4, 5]])
    });

    test("step 3 ", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=3))
      |> toEqual([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
    });

    test("size 2, step 2", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=2, ~step=2))
      |> toEqual([[1, 2], [3, 4]])
    });

    test("size 1, step 3", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=1, ~step=3))
      |> toEqual([[1], [4]])
    });

    test("size 2, step 3", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=2, ~step=3))
      |> toEqual([[1, 2], [4, 5]])
    });

    test("step 7", () => {
      expect(sliding([1, 2, 3, 4, 5], ~size=7)) |> toEqual([])
    });
  });

  describe("partition", () => {
    test("empty list", () => {
      expect(partition(~f=Int.isEven, [])) |> toEqual(([], []))
    });
    test("one element", () => {
      expect(partition(~f=Int.isEven, [1])) |> toEqual(([], [1]))
    });
    test("four elements", () => {
      expect(partition(~f=Int.isEven, [1, 2, 3, 4]))
      |> toEqual(([2, 4], [1, 3]))
    });
  });

  describe("minimum", () => {
    test("minimum non-empty list", () => {
      expect(minimum([7, 9, 15, 10, 3], ~compare)) |> toEqual(Some(3))
    });
    test("minimum empty list", () => {
      expect(minimum([], ~compare)) |> toEqual(None)
    });
  });

  describe("maximum", () => {
    test("maximum non-empty list", () => {
      expect(maximum([7, 9, 15, 10, 3], ~compare)) |> toEqual(Some(15))
    });
    test("maximum empty list", () => {
      expect(maximum([], ~compare)) |> toEqual(None)
    });
  });

  describe("splitAt", () => {
    test("empty list", () => {
      expect(splitAt([], ~index=1)) |> toEqual(([], []))
    });
    test("at evens", () => {
      expect(splitAt(~index=0, [2, 4, 6])) |> toEqual(([], [2, 4, 6]))
    });
    test("four elements", () => {
      expect(splitAt(~index=2, [1, 3, 2, 4]))
      |> toEqual(([1, 3], [2, 4]))
    });
    test("at end", () => {
      expect(splitAt(~index=3, [1, 3, 5])) |> toEqual(([1, 3, 5], []))
    });
  });

  describe("splitWhen", () => {
    test("empty list", () => {
      expect(splitWhen(~f=Int.isEven, [])) |> toEqual(([], []))
    });
    test("the first element satisfies f", () => {
      expect(splitWhen(~f=Int.isEven, [2, 4, 6]))
      |> toEqual(([], [2, 4, 6]))
    });
    test("the last element satisfies f", () => {
      expect(splitWhen(~f=Int.isEven, [1, 3, 2, 4]))
      |> toEqual(([1, 3], [2, 4]))
    });
    test("no element satisfies f", () => {
      expect(splitWhen(~f=Int.isEven, [1, 3, 5]))
      |> toEqual(([1, 3, 5], []))
    });
  });

  describe("intersperse", () => {
    test("intersperse empty list", () => {
      expect(intersperse([], ~sep="on")) |> toEqual([])
    });
    test("intersperse one turtle", () => {
      expect(intersperse(~sep="on", ["turtles"])) |> toEqual(["turtles"])
    });
    test("intersperse three turtles", () => {
      expect(intersperse(~sep="on", ["turtles", "turtles", "turtles"]))
      |> toEqual(["turtles", "on", "turtles", "on", "turtles"])
    });
  });

  describe("initial", () => {
    test("empty list", () => {
      expect(initial([])) |> toEqual(None)
    });
    test("one element", () => {
      expect(initial(['a'])) |> toEqual(Some([]))
    });
    test("two elements", () => {
      expect(initial(['a', 'b'])) |> toEqual(Some(['a']))
    });
  });

  describe("append", () => {
    test("append empty lists", () => {
      expect(append([], [])) |> toEqual([])
    });
    test("append empty list", () => {
      expect(append([], ["turtles"])) |> toEqual(["turtles"])
    });
    test("append empty list", () => {
      expect(append(["turtles"], [])) |> toEqual(["turtles"])
    });
    test("append two lists", () => {
      expect(append(["on"], ["turtles"])) |> toEqual(["on", "turtles"])
    });
  });

  describe("folds", () => {
    test("empty list", () => {
      expect(fold(~f=cons, ~initial=[], [])) |> toEqual([])
    });
    test("one element", () => {
      expect(fold(~f=cons, ~initial=[], [1])) |> toEqual([1])
    });
    test("three elements", () => {
      expect(fold(~f=cons, ~initial=[], [1, 2, 3])) |> toEqual([3, 2, 1])
    });
    test("foldr empty list", () => {
      expect(foldRight(~f=cons, ~initial=[], [])) |> toEqual([])
    });
    test("foldr one element", () => {
      expect(foldRight(~f=cons, ~initial=[], [1])) |> toEqual([1])
    });
    test("foldr three elements", () => {
      expect(foldRight(~f=cons, ~initial=[], [1, 2, 3]))
      |> toEqual([1, 2, 3])
    });
    test("-", () => {
      expect(fold(~f=(-), ~initial=0, [1, 2, 3])) |> toEqual(-6)
    });
    test("- foldRight", () => {
      expect(foldRight(~f=(-), ~initial=0, [1, 2, 3])) |> toEqual(-6)
    });
  });

  describe("insertAt", () => {
    test("insertAt empty list", () => {
      expect(insertAt(~index=0, ~value=1, [])) |> toEqual([1])
    });
    test("insertAt in the middle", () => {
      expect(insertAt(~index=1, ~value=2, [1, 3])) |> toEqual([1, 2, 3])
    });
    test("insertAt in the front", () => {
      expect(insertAt(~index=0, ~value=2, [1, 3])) |> toEqual([2, 1, 3])
    });

    /*      the test below fails on native, both should show the same behaviour  */
    test("insertAt after end of list", () => {
      expect(insertAt(~index=4, ~value=2, [1, 3])) |> toEqual([2])
    });
  });

  describe("updateAt", () => {
    test("updateAt index smaller 0", () => {
      expect(updateAt(~index=-1, ~f=x => x + 1, [1, 3])) |> toEqual([1, 3])
    });
    test("updateAt empty list", () => {
      expect(updateAt(~index=0, ~f=x => x + 1, [])) |> toEqual([])
    });
    test("updateAt empty list", () => {
      expect(updateAt(~index=2, ~f=x => x + 1, [])) |> toEqual([])
    });
    test("updateAt inside the list", () => {
      expect(updateAt(~index=1, ~f=x => x + 1, [1, 3])) |> toEqual([1, 4])
    });
    test("updateAt in the front", () => {
      expect(updateAt(~index=0, ~f=x => x + 1, [1, 3])) |> toEqual([2, 3])
    });
    test("updateAt after end of list", () => {
      expect(updateAt(~index=4, ~f=x => x + 1, [1, 3])) |> toEqual([1, 3])
    });
  });

  describe("concatenate", () => {
    test("two empty lists", () => {
      expect(concatenate([[], []])) |> toEqual([])
    });
    test("one empty list", () => {
      expect(concatenate([[1], []])) |> toEqual([1])
    });
    test("one empty list", () => {
      expect(concatenate([[], [1]])) |> toEqual([1])
    });
    test("several lists", () => {
      expect(concatenate([[1], [2], [3]])) |> toEqual([1, 2, 3])
    });
    test("several lists", () => {
      expect(concatenate([[1], [], [2], [], [3]])) |> toEqual([1, 2, 3])
    });
  });

  describe("initialize", () => {
    test("initialize length 0", () => {
      expect(initialize(0, ~f=i => i)) |> toEqual([])
    });
    test("initialize length 1", () => {
      expect(initialize(1, ~f=i => i)) |> toEqual([0])
    });
    test("initialize length 2", () => {
      expect(initialize(2, ~f=i => i)) |> toEqual([0, 1])
    });
  });

  describe("removeAt", () => {
    test("removeAt index smaller 0", () => {
      expect(removeAt(~index=-1, [1, 3])) |> toEqual([1, 3])
    });
    test("removeAt empty list", () => {
      expect(removeAt(~index=0, [])) |> toEqual([])
    });
    test("removeAt empty list", () => {
      expect(removeAt(~index=2, [])) |> toEqual([])
    });
    test("removeAt index 1", () => {
      expect(removeAt(~index=1, [1, 3])) |> toEqual([1])
    });
    test("removeAt index 0", () => {
      expect(removeAt(~index=0, [1, 3])) |> toEqual([3])
    });
    test("removeAt after end of list", () => {
      expect(removeAt(~index=4, [1, 3])) |> toEqual([1, 3])
    });
  });
});

describe("Map", () => {
  describe("Poly", () => {
    test("Can be used other Set functions", () => {
      let map = Map.Poly.fromList([(`Ant, "Ant"), (`Bat, "Bat")]);
      expect(Map.get(map, `Ant)) |> toBe(Some("Ant"));
    })
  });

  describe("Int", () => {
    test("Can be used other Map functions", () => {
      let map = Map.Int.fromList([(1, "Ant"), (2, "Bat")]);
      expect(Map.get(map, 1)) |> toBe(Some("Ant"));
    })
  });

  describe("String", () => {
    test("Can be used other Map functions", () => {
      let map = Map.String.fromList([("Ant", 1), ("Bat", 1)]);
      expect(Map.get(map, "Ant")) |> toBe(Some(1));
    })
  });
});

describe("Set", () => {
  describe("Poly", () => {
    test("Can be used other Set functions", () => {
      let set = Set.Poly.fromList([`Ant, `Bat]);
      expect(Set.includes(set, `Ant)) |> toBe(true);
    })
  });

  describe("Int", () => {
    test("Can be used other Set functions", () => {
      let set = Set.Int.fromList([1, 2]);
      expect(Set.includes(set, 1)) |> toBe(true);
    })
  });

  describe("String", () => {
    test("Can be used other Set functions", () => {
      let set = Set.String.fromList(["Ant", "Bat"]);
      expect(Set.includes(set, "Ant")) |> toBe(true);
    })
  });
});

describe("String", () => {
  test("length empty string", () => {
    expect(String.length("")) |> toEqual(0)
  });
  test("length", () => {
    expect(String.length("123")) |> toEqual(3)
  });
  test("reverse empty string", () => {
    expect(String.reverse("")) |> toEqual("")
  });
  test("reverse", () => {
    expect(String.reverse("stressed")) |> toEqual("desserts")
  });
});

describe("Tuple", () => {
  test("make", () => {
    expect(Tuple.make(3, 4)) |> toEqual((3, 4))
  });

  test("first", () => {
    expect(Tuple.first((3, 4))) |> toEqual(3)
  });

  test("second", () => {
    expect(Tuple.second((3, 4))) |> toEqual(4)
  });

  test("mapFirst", () => {
    expect(Tuple.mapFirst(~f=String.reverse, ("stressed", 16)))
    |> toEqual(("desserts", 16))
  });

  test("mapSecond", () => {
    expect(Tuple.mapSecond(~f=sqrt, ("stressed", 16.)))
    |> toEqual(("stressed", 4.))
  });

  test("mapEach", () => {
    expect(Tuple.mapEach(~f=String.reverse, ~g=sqrt, ("stressed", 16.)))
    |> toEqual(("desserts", 4.))
  });

  test("mapAll", () => {
    expect(Tuple.mapAll(~f=String.reverse, ("was", "stressed")))
    |> toEqual(("saw", "desserts"))
  });

  test("swap", () => {
    expect(Tuple.swap((3, 4))) |> toEqual((4, 3))
  });

  test("curry", () => {
    let tupleAdder = ((a, b)) => a + b;
    expect(Tuple.curry(tupleAdder, 3, 4)) |> toEqual(7);
  });

  test("uncurry", () => {
    let curriedAdder = (a, b) => a + b;
    expect(Tuple.uncurry(curriedAdder, (3, 4))) |> toEqual(7);
  });

  test("toList", () => {
    expect(Tuple.toList((3, 4))) |> toEqual([3, 4])
  });
});

describe("Tuple3", () => {
  test("make", () => {
    expect(Tuple3.make(3, 4, 5)) |> toEqual((3, 4, 5))
  });

  test("first", () => {
    expect(Tuple3.first((3, 4, 5))) |> toEqual(3)
  });

  test("second", () => {
    expect(Tuple3.second((3, 4, 5))) |> toEqual(4)
  });

  test("third", () => {
    expect(Tuple3.third((3, 4, 5))) |> toEqual(5)
  });

  test("initial", () => {
    expect(Tuple3.initial((3, 4, 5))) |> toEqual((3, 4))
  });

  test("tail", () => {
    expect(Tuple3.tail((3, 4, 5))) |> toEqual((4, 5))
  });

  test("mapFirst", () => {
    expect(Tuple3.mapFirst(~f=String.reverse, ("stressed", 16, false)))
    |> toEqual(("desserts", 16, false))
  });

  test("mapSecond", () => {
    expect(Tuple3.mapSecond(~f=sqrt, ("stressed", 16., false)))
    |> toEqual(("stressed", 4., false))
  });

  test("mapThird", () => {
    expect(Tuple3.mapThird(~f=(!), ("stressed", 16, false)))
    |> toEqual(("stressed", 16, true))
  });

  test("mapEach", () => {
    expect(
      Tuple3.mapEach(
        ~f=String.reverse,
        ~g=sqrt,
        ~h=(!),
        ("stressed", 16., false),
      ),
    )
    |> toEqual(("desserts", 4., true))
  });

  test("mapAll", () => {
    expect(Tuple3.mapAll(~f=String.reverse, ("was", "stressed", "now")))
    |> toEqual(("saw", "desserts", "won"))
  });

  test("rotateLeft", () => {
    expect(Tuple3.rotateLeft((3, 4, 5))) |> toEqual((4, 5, 3))
  });

  test("rotateRight", () => {
    expect(Tuple3.rotateRight((3, 4, 5))) |> toEqual((5, 3, 4))
  });

  test("curry", () => {
    let tupleAdder = ((a, b, c)) => a + b + c;
    expect(Tuple3.curry(tupleAdder, 3, 4, 5)) |> toEqual(12);
  });

  test("uncurry", () => {
    let curriedAdder = (a, b, c) => a + b + c;
    expect(Tuple3.uncurry(curriedAdder, (3, 4, 5))) |> toEqual(12);
  });

  test("toList", () => {
    expect(Tuple3.toList((3, 4, 5))) |> toEqual([3, 4, 5])
  });
});

describe("Option", () => {
  test("getUnsafe Some(1)", () => {
    expect(Option.getUnsafe(Some(1))) |> toEqual(1)
  });

  test("getUnsafe None", () => {
    expect(() =>
      Option.getUnsafe(None)
    ) |> toThrow
  });
});