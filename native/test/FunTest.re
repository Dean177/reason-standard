open AlcoJest;

let suite = suite("Fun", () => {

  open Fun;
  AT.check(AT.int, "identity", 1, Fun.identity(1));

  AT.check(AT.unit, "ignore", (), Fun.ignore(1));

  AT.check(AT.int, "constant", 1, Fun.constant(1, 2));

  AT.check(AT.int, "sequence", 2, Fun.sequence(1, 2));

  AT.check(AT.int, "flip", 2, Fun.flip(Int.(/), 2, 4));

  AT.check(AT.int, "apply", 2, Fun.apply(a => a + 1, 1));

  AT.check(
    AT.int,
    "compose",
    3,
    {
      let increment = x => x + 1;
      let double = x => x * 2;
      Fun.compose(increment, double, 1);
    },
  );

  AT.check(
    AT.int,
    "composeRight",
    4,
    {
      let increment = x => x + 1;
      let double = x => x * 2;
      Fun.composeRight(increment, double, 1);
    },
  );

  AT.check(
    AT.array(AT.int),
    "tap",
    [|0, 2|],
    Array.filter([|1, 3, 2, 5, 4|], ~f=Int.isEven)
    |> Fun.tap(~f=numbers => Base.Array.set(numbers, 1, 0))
    |> Fun.tap(~f=Base.Array.rev_inplace),
  );

});
