open AlcoJest;

let suite = suite("Tuple3", () => {
  AT.check(
    Eq.trio(AT.int, AT.int, AT.int),
    "create",
    Tuple3.make(3, 4, 5),
    (3, 4, 5),
  );

  AT.check(AT.int, "first", Tuple3.first((3, 4, 5)), 3);

  AT.check(AT.int, "second", Tuple3.second((3, 4, 5)), 4);

  AT.check(AT.int, "third", Tuple3.third((3, 4, 5)), 5);

  AT.check(
    AT.pair(AT.int, AT.int),
    "init",
    Tuple3.initial((3, 4, 5)),
    (3, 4),
  );

  AT.check(
    AT.pair(AT.int, AT.int),
    "tail",
    Tuple3.tail((3, 4, 5)),
    (4, 5),
  );

  AT.check(
    Eq.trio(AT.string, AT.int, AT.bool),
    "mapFirst",
    Tuple3.mapFirst(~f=String.reverse, ("stressed", 16, false)),
    ("desserts", 16, false),
  );

  AT.check(
    Eq.trio(AT.string, AT.float(0.), AT.bool),
    "mapSecond",
    Tuple3.mapSecond(~f=sqrt, ("stressed", 16., false)),
    ("stressed", 4., false),
  );

  AT.check(
    Eq.trio(AT.string, AT.int, AT.bool),
    "mapThird",
    Tuple3.mapThird(~f=(!), ("stressed", 16, false)),
    ("stressed", 16, true),
  );

  AT.check(
    Eq.trio(AT.string, AT.float(0.), AT.bool),
    "mapEach",
    Tuple3.mapEach(
      ~f=String.reverse,
      ~g=sqrt,
      ~h=(!),
      ("stressed", 16., false),
    ),
    ("desserts", 4., true),
  );

  AT.check(
    Eq.trio(AT.string, AT.string, AT.string),
    "mapAll",
    Tuple3.mapAll(~f=String.reverse, ("was", "stressed", "now")),
    ("saw", "desserts", "won"),
  );

  AT.check(
    Eq.trio(AT.int, AT.int, AT.int),
    "rotateLeft",
    Tuple3.rotateLeft((3, 4, 5)),
    (4, 5, 3),
  );

  AT.check(
    Eq.trio(AT.int, AT.int, AT.int),
    "rotateRight",
    Tuple3.rotateRight((3, 4, 5)),
    (5, 3, 4),
  );

  AT.check(
    AT.int,
    "curry",
    Tuple3.curry(((a, b, c)) => a + b + c, 3, 4, 5),
    12,
  );

  AT.check(
    AT.int,
    "uncurry",
    Tuple3.uncurry((a, b, c) => a + b + c, (3, 4, 5)),
    12,
  );

  AT.check(AT.list(AT.int), "toList", Tuple3.toList((3, 4, 5)), [3, 4, 5]);

  ();
});