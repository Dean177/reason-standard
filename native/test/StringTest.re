open AlcoJest;

let suite = suite("String", () => {
  AT.check(
    AT.bool,
    "imported correctly",
    true,
    Standard.String.length === String.length,
  );

  AT.check(AT.int, "length", String.length(""), 0);
  AT.check(AT.int, "length", String.length("123"), 3);

  AT.check(AT.string, "reverse", String.reverse(""), "");
  AT.check(AT.string, "reverse", String.reverse("stressed"), "desserts");
});
