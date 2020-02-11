open Standard;
open AlcoJest;

let suite = suite("Set", () => {
  describe("Poly.ofList", () => {
    test("creates a set from a list", () => {
      let set = Set.Poly.ofList([`Ant, `Bat]);
      expect(Set.includes(set, `Ant)) |> toEqual(Eq.bool, true);
    })
  });

  describe("Map.Int", () => {
    test("creates a set from a list", () => {
      let set = Set.Int.ofList([1, 2]);
      expect(Set.includes(set, 1)) |> toEqual(Eq.bool, true);
    })
  });

  describe("Map.String", () => {
    test("creates a set from a list", () => {
      let set = Set.String.ofList(["Ant", "Bat"]);
      expect(Set.includes(set, "Ant")) |> toEqual(Eq.bool, true);
    })
  });
});
