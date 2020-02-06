open AlcoJest;

let suite = suite("Tuple", () => {
  AT.check(AT.pair(AT.int, AT.int), "create", Tuple.make(3, 4), (3, 4));

  AT.check(AT.int, "first", Tuple.first((3, 4)), 3);

  AT.check(AT.int, "second", Tuple.second((3, 4)), 4);

  AT.check(
    AT.pair(AT.string, AT.int),
    "mapFirst",
    Tuple.mapFirst(~f=String.reverse, ("stressed", 16)),
    ("desserts", 16),
  );

  AT.check(
    AT.pair(AT.string, AT.float(0.)),
    "mapSecond",
    Tuple.mapSecond(~f=sqrt, ("stressed", 16.)),
    ("stressed", 4.),
  );

  AT.check(
    AT.pair(AT.string, AT.float(0.)),
    "mapEach",
    Tuple.mapEach(~f=String.reverse, ~g=sqrt, ("stressed", 16.)),
    ("desserts", 4.),
  );

  AT.check(
    AT.pair(AT.string, AT.string),
    "mapAll",
    Tuple.mapAll(~f=String.reverse, ("was", "stressed")),
    ("saw", "desserts"),
  );

  AT.check(AT.pair(AT.int, AT.int), "swap", Tuple.swap((3, 4)), (4, 3));

  AT.check(AT.int, "curry", Tuple.curry(((a, b)) => a + b, 3, 4), 7);

  AT.check(AT.int, "uncurry", Tuple.uncurry((a, b) => a + b, (3, 4)), 7);

  AT.check(AT.list(AT.int), "toList", Tuple.toList((3, 4)), [3, 4]);

  ();
});