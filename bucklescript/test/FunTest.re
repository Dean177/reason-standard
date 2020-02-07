open Standard;
open AlcoJest;

let suite = suite("Fun", () => {
  test("identity", () => {
    expect(Fun.identity(1)) |> toEqual(Eq.int, 1)
  });

  test("ignore", () => {
    expect(Fun.ignore(1)) |> toEqual(Eq.unit, ())
  });

  test("constant", () => {
    expect(Fun.constant(1, 2)) |> toEqual(Eq.int, 1)
  });

  test("sequence", () => {
    expect(Fun.sequence(1, 2)) |> toEqual(Eq.int, 2)
  });

  test("flip", () => {
    expect(Fun.flip(Int.(/), 2, 4)) |> toEqual(Eq.int, 2)
  });

  test("apply", () => {
    expect(Fun.apply(a => a + 1, 1)) |> toEqual(Eq.int, 2)
  });

  let increment = x => x + 1;
  let double = x => x * 2;
  test("compose", () => {
    expect(Fun.compose(increment, double, 1)) |> toEqual(Eq.int, 3);
  });

  test("<<", () => {
    expect(Fun.(increment << double)(1)) |> toEqual(Eq.int, 3);
  });

  test("composeRight", () => {
    expect(Fun.composeRight(increment, double, 1)) |> toEqual(Eq.int, 4);
  });

  test(">>", () => {
    expect(Fun.(increment >> double)(1)) |> toEqual(Eq.int, 4)
  });

  test("tap", () => {
    expect(
      Array.filter([|1, 3, 2, 5, 4|], ~f=Int.isEven)
      |> Fun.tap(~f=numbers => ignore(numbers[1] = 0))
      |> Fun.tap(~f=Array.reverse),
    )
    |> toEqual(Eq.(array(int)), [|0, 2|])
  });
});