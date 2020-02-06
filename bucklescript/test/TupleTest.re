open Standard;
open AlcoJest;


suite("Tuple", () => {
  test("make", () => {
    expect(Tuple.make(3, 4)) |> toEqual(Eq.(option(pair(int, int))), (3, 4))
  });

  test("first", () => {
    expect(Tuple.first((3, 4))) |> toEqual(Eq.(option((int))), 3)
  });

  test("second", () => {
    expect(Tuple.second((3, 4))) |> toEqual(Eq.(option((int))),4)
  });

  test("mapFirst", () => {
    expect(Tuple.mapFirst(~f=String.reverse, ("stressed", 16)))
    |> toEqual(("desserts", 16))
  });

  test("mapSecond", () => {
    expect(Tuple.mapSecond(~f=sqrt, ("stressed", 16.)))
    |> toEqual(Eq.(option(pair(string, int))), ("stressed", 4.))
  });

  test("mapEach", () => {
    expect(Tuple.mapEach(~f=String.reverse, ~g=sqrt, ("stressed", 16.)))
    |> toEqual(Eq.(option(pair(string, int))), ("desserts", 4.))
  });

  test("mapAll", () => {
    expect(Tuple.mapAll(~f=String.reverse, ("was", "stressed")))
    |> toEqual(Eq.(option(pair(string, string))), ("saw", "desserts"))
  });

  test("swap", () => {
    expect(Tuple.swap((3, 4))) |> toEqual(Eq.(option(pair(int, int))), (4, 3))
  });

  test("curry", () => {
    let tupleAdder = ((a, b)) => a + b;
    expect(Tuple.curry(tupleAdder, 3, 4)) |> toEqual(Eq.(option((int))), 7);
  });

  test("uncurry", () => {
    let curriedAdder = (a, b) => a + b;
    expect(Tuple.uncurry(curriedAdder, (3, 4))) |> toEqual(Eq.(option(int)), 7);
  });

  test("toList", () => {
    expect(Tuple.toList((3, 4))) |> toEqual(Eq.(option(list(int))), [3, 4])
  });
});