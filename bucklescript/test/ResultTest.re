open Standard;
open AlcoJest;

suite("Result", () => {
  open Result;

  describe("fromOption", () => {
    test("maps None into Error", () => {
      expect((fromOption(~error="error message", None)))
      |> toEqual(Eq.(result(string, int)), Error("error message"))
    });

    test("maps Some into Ok", () => {
      expect((fromOption(~error="error message", Some(10))))
      |> toEqual(Eq.(result(string, int)), Ok(10))
    });
  })
});
