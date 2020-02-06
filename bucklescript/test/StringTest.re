open Standard;
open AlcoJest;

describe("String", () => {
  test("length empty string", () => {
    expect(String.length("")) |> toEqual(0)
  });
  test("length", () => {
    expect(String.length("123")) |> toEqual(3)
  });
  test("reverse empty string", () => {
    expect(String.reverse("")) |> toEqual("")
  });
  test("reverse", () => {
    expect(String.reverse("stressed")) |> toEqual("desserts")
  });
});