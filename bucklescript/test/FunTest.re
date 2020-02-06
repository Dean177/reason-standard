open Standard;
open AlcoJest;

suite("Fun", () => {
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