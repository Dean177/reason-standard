open AlcoJest;

let suite = suite("Int", () => {
  open Int;
  AT.check(AT.int, "zero", zero, 0);

  AT.check(AT.int, "one", one, 1);

  AT.check(AT.int, "minimumValue", minimumValue - 1, maximumValue);

  AT.check(AT.int, "maximumValue", maximumValue + 1, minimumValue);

  AT.check(AT.int, "add", add(3002, 4004), 7006);
  AT.check(AT.int, "+", 3002 + 4004, 7006);

  AT.check(AT.int, "subtract", subtract(4, 3), 1);
  AT.check(AT.int, "-", 4 - 3, 1);

  AT.check(AT.int, "multiply", multiply(2, 7), 14);
  AT.check(AT.int, "*", 2 * 7, 14);

  AT.check(AT.int, "divide", divide(3, ~by=2), 1);
  AT.check_raises("division by zero", Division_by_zero, () =>
    ignore(divide(3, ~by=0))
  );

  AT.check(AT.int, "/", 27 / 5, 5);

  AT.check(AT.float(0.), "//", 3 /\/ 2, 1.5);
  AT.check(AT.float(0.), "//", 27 /\/ 5, 5.4);
  AT.check(AT.float(0.), "//", 8 /\/ 4, 2.0);
  AT.check(AT.bool, "x // 0", 8 /\/ 0 == Float.infinity, true);
  AT.check(AT.bool, "-x // 0", (-8) /\/ 0 == Float.negativeInfinity, true);

  AT.check(AT.int, "power - power", power(~base=7, ~exponent=3), 343);
  AT.check(AT.int, "power - 0 base", power(~base=0, ~exponent=3), 0);
  AT.check(AT.int, "power - 0 exponent", power(~base=7, ~exponent=0), 1);
  AT.check(AT.int, "power - **", 7 ** 3, 343);

  AT.check(AT.int, "negate - positive number", negate(8), -8);
  AT.check(AT.int, "negate - negative number", negate(-7), 7);
  AT.check(AT.int, "negate - zero", negate(0), -0);
  AT.check(AT.int, "negate - ~-", - 7, -7);

  AT.check(AT.int, "absolute - positive number", absolute(8), 8);
  AT.check(AT.int, "absolute - negative number", absolute(-7), 7);
  AT.check(AT.int, "absolute - zero", absolute(0), 0);

  AT.check(AT.int, "clamp - in range", clamp(~lower=0, ~upper=8, 5), 5);
  AT.check(AT.int, "clamp - above range", clamp(~lower=0, ~upper=8, 9), 8);
  AT.check(AT.int, "clamp - below range", clamp(~lower=2, ~upper=8, 1), 2);
  AT.check(
    AT.int,
    "clamp - above negative range",
    clamp(~lower=-10, ~upper=-5, 5),
    -5,
  );
  AT.check(
    AT.int,
    "clamp - below negative range",
    clamp(~lower=-10, ~upper=-5, -15),
    -10,
  );
  AT.check_raises(
    "clamp - invalid arguments",
    Invalid_argument("~lower:7 must be less than or equal to ~upper:1"),
    () =>
    ignore(clamp(~lower=7, ~upper=1, 3))
  );

  AT.check(
    AT.bool,
    "inRange - in range",
    inRange(~lower=2, ~upper=4, 3),
    true,
  );
  AT.check(
    AT.bool,
    "inRange - above range",
    inRange(~lower=2, ~upper=4, 8),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - below range",
    inRange(~lower=2, ~upper=4, 1),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - equal to ~upper",
    inRange(~lower=1, ~upper=2, 2),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - negative range",
    inRange(~lower=-7, ~upper=-5, -6),
    true,
  );
  AT.check_raises(
    "inRange - invalid arguments",
    Invalid_argument("~lower:7 must be less than or equal to ~upper:1"),
    () =>
    ignore(inRange(~lower=7, ~upper=1, 3))
  );

  AT.check(AT.float(0.), "toFloat - 5", toFloat(5), 5.);
  AT.check(AT.float(0.), "toFloat - 0", toFloat(0), 0.);
  AT.check(AT.float(0.), "toFloat - -7", toFloat(-7), -7.);

  AT.check(AT.(option(int)), "ofString - 0", ofString("0"), Some(0));
  AT.check(AT.(option(int)), "ofString - -0", ofString("-0"), Some(-0));
  AT.check(AT.(option(int)), "ofString - 42", ofString("42"), Some(42));
  AT.check(
    AT.(option(int)),
    "ofString - 123_456",
    ofString("123_456"),
    Some(123_456),
  );
  AT.check(AT.(option(int)), "ofString - -42", ofString("-42"), Some(-42));
  AT.check(
    AT.(option(int)),
    "ofString - 0XFF",
    ofString("0XFF"),
    Some(255),
  );
  AT.check(
    AT.(option(int)),
    "ofString - 0X000A",
    ofString("0X000A"),
    Some(10),
  );
  AT.check(
    AT.(option(int)),
    "ofString - Infinity",
    ofString("Infinity"),
    None,
  );
  AT.check(
    AT.(option(int)),
    "ofString - -Infinity",
    ofString("-Infinity"),
    None,
  );
  AT.check(AT.(option(int)), "ofString - NaN", ofString("NaN"), None);
  AT.check(AT.(option(int)), "ofString - abc", ofString("abc"), None);
  AT.check(AT.(option(int)), "ofString - --4", ofString("--4"), None);
  AT.check(
    AT.(option(int)),
    "ofString - empty string",
    ofString(" "),
    None,
  );

  AT.check(AT.string, " toString - positive number", toString(1), "1");
  AT.check(AT.string, " toString - negative number", toString(-1), "-1");

});