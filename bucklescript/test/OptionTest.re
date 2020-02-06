open Standard;
open AlcoJest;


suite("Option", () => {
  test("getUnsafe Some(1)", () => {
    expect(Option.getUnsafe(Some(1))) |> toEqual(1)
  });

  test("getUnsafe None", () => {
    expect(() =>
      Option.getUnsafe(None)
    ) |> toThrow
  });
});