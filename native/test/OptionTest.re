open Standard;
open AlcoJest;

let suite = suite("Option", () => {
  open Option;

  describe("getUnsafe", () => {
    test("returns the wrapped value for a Some", () => {
      expect(getUnsafe(Some(1))) |> toEqual(AT.int, 1);
    });

    test("raises for a None", () => {
      expect(() => ignore(getUnsafe(None))) |> toRaise(Invalid_argument("Option.getUnsafe called with None"));
    });
  });
});