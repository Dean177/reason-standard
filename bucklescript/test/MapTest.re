open Standard;
open AlcoJest;

suite("Map", () => {  
  describe("Poly", () => {
    test("Can be used other Set functions", () => {
      let map = Map.Poly.fromList([(`Ant, "Ant"), (`Bat, "Bat")]);
      expect(Map.get(map, `Ant)) |> toEqual(Eq.(option(string)), Some("Ant"));
    })
  });

  describe("Int", () => {
    test("Can be used other Map functions", () => {
      let map = Map.Int.fromList([(1, "Ant"), (2, "Bat")]);
      expect(Map.get(map, 1)) |> toEqual(Eq.(option(string)), Some("Ant"));
    })
  });

  describe("String", () => {
    test("Can be used other Map functions", () => {
      let map = Map.String.fromList([("Ant", 1), ("Bat", 1)]);
      expect(Map.get(map, "Ant")) |> toEqual(Eq.(option(int)), Some(1));
    })
  });
});