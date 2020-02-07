open Standard;
open AlcoJest;

let suite =
  suite("String", () => {
    test("length empty string", () => {
      expect(String.length("")) |> toEqual(Eq.int, 0)
    });
    test("length", () => {
      expect(String.length("123")) |> toEqual(Eq.int, 3)
    });
    test("reverse empty string", () => {
      expect(String.reverse("")) |> toEqual(Eq.string, "")
    });
    test("reverse", () => {
      expect(String.reverse("stressed")) |> toEqual(Eq.string, "desserts")
    });
  });
