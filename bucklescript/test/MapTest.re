open Standard;
open AlcoJest;

module Coordinate = {
  include (
            val Comparator.make(
                  ~compare=Tuple.compare(Int.compare, Int.compare),
                )
          );
};

let suite = suite("Map", () => {  
  describe("empty", () =>
    test("has length zero", () =>
      expect(Map.empty((module Coordinate)) |> Map.length)
      |> toEqual(Eq.int, 0)
    )
  );

  describe("ofArray", () =>
    test("has length zero", () =>
      expect(Map.ofArray((module Coordinate), [||]) |> Map.length)
      |> toEqual(Eq.int, 0)
    )
  );

  describe("ofList", () =>
    test("has length zero", () =>
      expect(Map.ofList((module Coordinate), []) |> Map.length)
      |> toEqual(Eq.int, 0)
    )
  );

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
