open Standard;
open AlcoJest;

suite("Map", () => {
  describe("Poly.ofList", () => {
    test("creates a map from a list", () => {
      let map = Map.Poly.ofList([(`Ant, "Ant"), (`Bat, "Bat")]);
      expect(Map.get(map, `Ant))
      |> toEqual(Eq.(option(string)), Some("Ant"));
    })
  });

  describe("Int.ofList", () => {
    test("creates a map from a list", () => {
      let map = Map.Int.ofList([(1, "Ant"), (2, "Bat")]);
      expect(Map.get(map, 1)) |> toEqual(Eq.(option(string)), Some("Ant"));
    })
  });

  describe("String.ofList", () => {
    test("creates a map from a list", () => {
      let map = Map.String.ofList([("Ant", 1), ("Bat", 1)]);
      expect(Map.get(map, "Ant")) |> toEqual(Eq.(option(int)), Some(1));
    })
  });
});
