open Standard;
open AlcoJest;

suite("Set", () => {
  describe("Poly", () => {
    test("Can be used other Set functions", () => {
      let set = Set.Poly.fromList([`Ant, `Bat]);
      expect(Set.includes(set, `Ant)) |> toBeTrue();
    })
  });

  describe("Int", () => {
    test("Can be used other Set functions", () => {
      let set = Set.Int.fromList([1, 2]);
      expect(Set.includes(set, 1)) |> toBeTrue();
    })
  });

  describe("String", () => {
    test("Can be used other Set functions", () => {
      let set = Set.String.fromList(["Ant", "Bat"]);
      expect(Set.includes(set, "Ant")) |> toBeTrue();
    })
  });
});