open Standard;
open AlcoJest;

suite("Tuple3", () => {
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