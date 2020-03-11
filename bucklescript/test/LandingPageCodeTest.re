open Standard;
open AlcoJest;

let suite =
  suite("LandingPageCode", () => {
    test("primary example", () => {
      let result =
        String.toList("Standard")
        |> List.filterMap(~f=character =>
             Char.toCode(character) |> Int.add(1) |> Char.ofCode
           )
        |> String.ofList;

      expect(result) |> toEqual(Eq.string, "Tuboebse");
    })
  });