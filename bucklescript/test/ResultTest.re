open Standard;
open AlcoJest;

let suite = suite("Result", () => {
  open Result;

  describe("ofOption", () => {
    test("maps None into Error", () => {
      expect((ofOption(~error="error message", None)))
      |> toEqual(Eq.(result(int, string)), Error("error message"))
    });

    test("maps Some into Ok", () => {
      expect((ofOption(~error="error message", Some(10))))
      |> toEqual(Eq.(result(int, string)), Ok(10))
    });
  })
});
