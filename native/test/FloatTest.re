open AlcoJest;

let suite = suite("Float", () => {
  open Standard.Float;
  
  AT.check(AT.float(0.), "zero", zero, 0.);

  AT.check(AT.float(0.), "one", one, 1.);

  AT.check(AT.bool, "nan", nan == nan, false);

  AT.check(AT.bool, "infinity", infinity > 0., true);

  AT.check(AT.bool, "negativeInfinity", negativeInfinity < 0., true);

  AT.check(AT.bool, "equals zero", 0. == (-0.), true);

  AT.check(AT.float(0.), "add", add(3.14, 3.14), 6.28);
  AT.check(AT.float(0.), "+", 3.14 + 3.14, 6.28);

  AT.check(AT.float(0.), "subtract", subtract(4., 3.), 1.);
  AT.check(AT.float(0.), "-", 4. - 3., 1.);

  AT.check(AT.float(0.), "multiply", multiply(2., 7.), 14.);
  AT.check(AT.float(0.), "*", 2. * 7., 14.);

  AT.check(AT.float(0.), "divide", divide(3.14, ~by=2.), 1.57);
  AT.check(
    AT.bool,
    "divide by zero",
    divide(3.14, ~by=0.) == infinity,
    true,
  );
  AT.check(
    AT.bool,
    "divide by negative zero",
    divide(3.14, ~by=-0.) == negativeInfinity,
    true,
  );
  AT.check(AT.float(0.), "/", 3.14 / 2., 1.57);

  AT.check(AT.float(0.), "power", power(~base=7., ~exponent=3.), 343.);
  AT.check(
    AT.float(0.),
    "power - 0 base",
    power(~base=0., ~exponent=3.),
    0.,
  );
  AT.check(
    AT.float(0.),
    "power - 0 exponent",
    power(~base=7., ~exponent=0.),
    1.,
  );
  AT.check(AT.float(0.), "**", 7. ** 3., 343.);

  AT.check(AT.float(0.), "negate - positive number", negate(8.), -8.);
  AT.check(AT.float(0.), "negate - negative number", negate(-7.), 7.);
  AT.check(AT.float(0.), "negate - zero", negate(0.), -0.);
  AT.check(AT.float(0.), "negate - ~-", - 7., -7.);

  AT.check(AT.float(0.), "absolute - positive number", absolute(8.), 8.);
  AT.check(AT.float(0.), "absolute - negative number", absolute(-7.), 7.);
  AT.check(AT.float(0.), "absolute - zero", absolute(0.), 0.);

  AT.check(AT.float(0.), "maximum - positive numbers", maximum(7., 9.), 9.);
  AT.check(
    AT.float(0.),
    "maximum - negative numbers",
    maximum(-4., -1.),
    -1.,
  );
  AT.check(AT.bool, "maximum - nan", maximum(7., nan) |> isNaN, true);
  AT.check(
    AT.bool,
    "maximum - infinity",
    maximum(7., infinity) == infinity,
    true,
  );
  AT.check(
    AT.float(0.),
    "maximum - negativeInfinity",
    maximum(7., negativeInfinity),
    7.,
  );

  AT.check(AT.float(0.), "minimum - positive numbers", minimum(7., 9.), 7.);
  AT.check(
    AT.float(0.),
    "minimum - negative numbers",
    minimum(-4., -1.),
    -4.,
  );
  AT.check(AT.bool, "minimum - nan", minimum(7., nan) |> isNaN, true);
  AT.check(AT.float(0.), "minimum - infinity", minimum(7., infinity), 7.);
  AT.check(
    AT.bool,
    "minimum - negativeInfinity",
    minimum(7., negativeInfinity) == negativeInfinity,
    true,
  );

  AT.check(
    AT.float(0.),
    "clamp - in range",
    clamp(~lower=0., ~upper=8., 5.),
    5.,
  );
  AT.check(
    AT.float(0.),
    "clamp - above range",
    clamp(~lower=0., ~upper=8., 9.),
    8.,
  );
  AT.check(
    AT.float(0.),
    "clamp - below range",
    clamp(~lower=2., ~upper=8., 1.),
    2.,
  );
  AT.check(
    AT.float(0.),
    "clamp - above negative range",
    clamp(~lower=-10., ~upper=-5., 5.),
    -5.,
  );
  AT.check(
    AT.float(0.),
    "clamp - below negative range",
    clamp(~lower=-10., ~upper=-5., -15.),
    -10.,
  );
  AT.check(
    AT.bool,
    "clamp - nan upper bound",
    clamp(~lower=-7.9, ~upper=nan, -6.6) |> isNaN,
    true,
  );
  AT.check(
    AT.bool,
    "clamp - nan lower bound",
    clamp(~lower=nan, ~upper=0., -6.6) |> isNaN,
    true,
  );
  AT.check(
    AT.bool,
    "clamp - nan value",
    clamp(~lower=2., ~upper=8., nan) |> isNaN,
    true,
  );
  AT.check_raises(
    "clamp - invalid arguments",
    Invalid_argument("~lower:7. must be less than or equal to ~upper:1."),
    () => ignore(clamp(~lower=7., ~upper=1., 3.))
  );

  AT.check(AT.float(0.), "squareRoot - whole numbers", squareRoot(4.), 2.);
  AT.check(
    AT.float(0.),
    "squareRoot - decimal numbers",
    squareRoot(20.25),
    4.5,
  );
  AT.check(
    AT.bool,
    "squareRoot - negative number",
    squareRoot(-1.) |> isNaN,
    true,
  );

  AT.check(AT.float(0.), "log - base 10", log(~base=10., 100.), 2.);
  AT.check(AT.float(0.), "log - base 2", log(~base=2., 256.), 8.);
  AT.check(
    AT.bool,
    "log - of zero",
    log(~base=10., 0.) == negativeInfinity,
    true,
  );

  AT.check(AT.bool, "isNaN - nan", isNaN(nan), true);
  AT.check(AT.bool, "isNaN - non-nan", isNaN(91.4), false);

  AT.check(AT.bool, "isFinite - infinity", isFinite(infinity), false);
  AT.check(
    AT.bool,
    "isFinite - negative infinity",
    isFinite(negativeInfinity),
    false,
  );
  AT.check(AT.bool, "isFinite - NaN", isFinite(nan), false);
  List.forEach([(-5.), (-0.314), 0., 3.14], ~f=n =>
    AT.check(
      AT.bool,
      "isFinite - regular numbers - " ++ Base.Float.to_string(n),
      isFinite(n),
      true,
    )
  );

  AT.check(AT.bool, "isInfinite - infinity", isInfinite(infinity), true);
  AT.check(
    AT.bool,
    "isInfinite - negative infinity",
    isInfinite(negativeInfinity),
    true,
  );
  AT.check(AT.bool, "isInfinite - NaN", isInfinite(nan), false);
  List.forEach([(-5.), (-0.314), 0., 3.14], ~f=n =>
    AT.check(
      AT.bool,
      "isInfinite - regular numbers - " ++ Base.Float.to_string(n),
      isInfinite(n),
      false,
    )
  );

  AT.check(
    AT.bool,
    "inRange - in range",
    inRange(~lower=2., ~upper=4., 3.),
    true,
  );
  AT.check(
    AT.bool,
    "inRange - above range",
    inRange(~lower=2., ~upper=4., 8.),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - below range",
    inRange(~lower=2., ~upper=4., 1.),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - equal to ~upper",
    inRange(~lower=1., ~upper=2., 2.),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - negative range",
    inRange(~lower=-7.9, ~upper=-5.2, -6.6),
    true,
  );
  AT.check(
    AT.bool,
    "inRange - nan upper bound",
    inRange(~lower=-7.9, ~upper=nan, -6.6),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - nan lower bound",
    inRange(~lower=nan, ~upper=0., -6.6),
    false,
  );
  AT.check(
    AT.bool,
    "inRange - nan value",
    inRange(~lower=2., ~upper=8., nan),
    false,
  );
  AT.check_raises(
    "inRange - invalid arguments",
    Invalid_argument("~lower:7. must be less than or equal to ~upper:1."),
    () =>
    ignore(inRange(~lower=7., ~upper=1., 3.))
  );

  AT.check(AT.float(0.), "hypotenuse", hypotenuse(3., 4.), 5.);

  AT.check(AT.float(0.), "degrees", degrees(180.), pi);

  AT.check(AT.float(0.), "radians", radians(pi), pi);

  AT.check(AT.float(0.), "turns", turns(1.), 2. * pi);

  AT.check(
    AT.pair(AT.float(0.001), AT.float(0.001)),
    "ofPolar",
    ofPolar((squareRoot(2.), degrees(45.))),
    (1., 1.),
  );

  AT.check(
    AT.pair(AT.float(0.), AT.float(0.)),
    "toPolar",
    toPolar((3.0, 4.0)),
    (5.0, 0.9272952180016122),
  );
  AT.check(
    AT.pair(AT.float(0.), AT.float(1e-15)),
    "toPolar",
    toPolar((5.0, 12.0)),
    (13.0, 1.1760052070951352),
  );

  AT.check(AT.float(0.), "cos", cos(degrees(60.)), 0.5000000000000001);
  AT.check(AT.float(0.), "cos", cos(radians(pi / 3.)), 0.5000000000000001);

  AT.check(AT.float(1e-15), "acos", acos(1. / 2.), 1.0471975511965979) /* pi / 3. */;

  AT.check(
    AT.float(0.),
    "sin - 30 degrees",
    sin(degrees(30.)),
    0.49999999999999994,
  );
  AT.check(
    AT.float(0.),
    "sin - pi / 6",
    sin(radians(pi / 6.)),
    0.49999999999999994,
  );

  AT.check(AT.float(1e-15), "asin", asin(1. / 2.), 0.5235987755982989) /* ~ pi / 6. */;

  AT.check(
    AT.float(0.),
    "tan - 45 degrees",
    tan(degrees(45.)),
    0.9999999999999999,
  );
  AT.check(
    AT.float(0.),
    "tan - pi / 4",
    tan(radians(pi / 4.)),
    0.9999999999999999,
  );
  AT.check(AT.float(0.), "tan - 0", tan(0.), 0.);

  AT.check(AT.float(0.), "atan - 0", atan(0.), 0.);
  AT.check(AT.float(0.), "atan - 1 / 1", atan(1. / 1.), 0.7853981633974483);
  AT.check(
    AT.float(0.),
    "atan - 1 / -1",
    atan(1. / (-1.)),
    -0.7853981633974483,
  );
  AT.check(
    AT.float(0.),
    "atan - -1 / -1",
    atan((-1.) / (-1.)),
    0.7853981633974483,
  );
  AT.check(
    AT.float(0.),
    "atan - -1 / -1",
    atan((-1.) / 1.),
    -0.7853981633974483,
  );

  AT.check(AT.float(0.), "atan2 0", 0., atan2(~y=0., ~x=0.));
  AT.check(
    AT.float(0.),
    "atan2 (1, 1)",
    0.7853981633974483,
    atan2(~y=1., ~x=1.),
  );
  AT.check(
    AT.float(0.),
    "atan2 (-1, 1)",
    2.3561944901923449,
    atan2(~y=1., ~x=-1.),
  );
  AT.check(
    AT.float(0.),
    "atan2 (-1, -1)",
    -2.3561944901923449,
    atan2(~y=-1., ~x=-1.),
  );
  AT.check(
    AT.float(0.),
    "atan2 (1, -1)",
    -0.7853981633974483,
    atan2(~y=-1., ~x=1.),
  );

  AT.check(AT.float(0.), "round `Zero", 1., round(~direction=`Zero, 1.2));
  AT.check(AT.float(0.), "round `Zero", 1., round(~direction=`Zero, 1.5));
  AT.check(AT.float(0.), "round `Zero", 1., round(~direction=`Zero, 1.8));
  AT.check(AT.float(0.), "round `Zero", -1., round(~direction=`Zero, -1.2));
  AT.check(AT.float(0.), "round `Zero", -1., round(~direction=`Zero, -1.5));
  AT.check(AT.float(0.), "round `Zero", -1., round(~direction=`Zero, -1.8));

  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    2.,
    round(~direction=`AwayFromZero, 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    2.,
    round(~direction=`AwayFromZero, 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    2.,
    round(~direction=`AwayFromZero, 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    -2.,
    round(~direction=`AwayFromZero, -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    -2.,
    round(~direction=`AwayFromZero, -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `AwayFromZero",
    -2.,
    round(~direction=`AwayFromZero, -1.8),
  );

  AT.check(AT.float(0.), "round `Up", 2., round(~direction=`Up, 1.2));
  AT.check(AT.float(0.), "round `Up", 2., round(~direction=`Up, 1.5));
  AT.check(AT.float(0.), "round `Up", 2., round(~direction=`Up, 1.8));
  AT.check(AT.float(0.), "round `Up", -1., round(~direction=`Up, -1.2));
  AT.check(AT.float(0.), "round `Up", -1., round(~direction=`Up, -1.5));
  AT.check(AT.float(0.), "round `Up", -1., round(~direction=`Up, -1.8));

  AT.check(AT.float(0.), "round `Down", 1., round(~direction=`Down, 1.2));
  AT.check(AT.float(0.), "round `Down", 1., round(~direction=`Down, 1.5));
  AT.check(AT.float(0.), "round `Down", 1., round(~direction=`Down, 1.8));
  AT.check(AT.float(0.), "round `Down", -2., round(~direction=`Down, -1.2));
  AT.check(AT.float(0.), "round `Down", -2., round(~direction=`Down, -1.5));
  AT.check(AT.float(0.), "round `Down", -2., round(~direction=`Down, -1.8));

  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    1.,
    round(~direction=`Closest(`Zero), 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    1.,
    round(~direction=`Closest(`Zero), 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    2.,
    round(~direction=`Closest(`Zero), 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    -1.,
    round(~direction=`Closest(`Zero), -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    -1.,
    round(~direction=`Closest(`Zero), -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Zero",
    -2.,
    round(~direction=`Closest(`Zero), -1.8),
  );

  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    1.,
    round(~direction=`Closest(`AwayFromZero), 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    2.,
    round(~direction=`Closest(`AwayFromZero), 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    2.,
    round(~direction=`Closest(`AwayFromZero), 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    -1.,
    round(~direction=`Closest(`AwayFromZero), -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    -2.,
    round(~direction=`Closest(`AwayFromZero), -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `AwayFromZero",
    -2.,
    round(~direction=`Closest(`AwayFromZero), -1.8),
  );

  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    1.,
    round(~direction=`Closest(`Up), 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    2.,
    round(~direction=`Closest(`Up), 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    2.,
    round(~direction=`Closest(`Up), 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    -1.,
    round(~direction=`Closest(`Up), -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    -1.,
    round(~direction=`Closest(`Up), -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Up",
    -2.,
    round(~direction=`Closest(`Up), -1.8),
  );

  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    1.,
    round(~direction=`Closest(`Down), 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    1.,
    round(~direction=`Closest(`Down), 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    2.,
    round(~direction=`Closest(`Down), 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    -1.,
    round(~direction=`Closest(`Down), -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    -2.,
    round(~direction=`Closest(`Down), -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `Down",
    -2.,
    round(~direction=`Closest(`Down), -1.8),
  );

  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    1.,
    round(~direction=`Closest(`ToEven), 1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    2.,
    round(~direction=`Closest(`ToEven), 1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    2.,
    round(~direction=`Closest(`ToEven), 1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    2.,
    round(~direction=`Closest(`ToEven), 2.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    2.,
    round(~direction=`Closest(`ToEven), 2.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    3.,
    round(~direction=`Closest(`ToEven), 2.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -1.,
    round(~direction=`Closest(`ToEven), -1.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -2.,
    round(~direction=`Closest(`ToEven), -1.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -2.,
    round(~direction=`Closest(`ToEven), -1.8),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -2.,
    round(~direction=`Closest(`ToEven), -2.2),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -2.,
    round(~direction=`Closest(`ToEven), -2.5),
  );
  AT.check(
    AT.float(0.),
    "round `Closest `ToEven",
    -3.,
    round(~direction=`Closest(`ToEven), -2.8),
  );

  AT.check(AT.float(0.), "floor", floor(1.2), 1.);
  AT.check(AT.float(0.), "floor", floor(1.5), 1.);
  AT.check(AT.float(0.), "floor", floor(1.8), 1.);
  AT.check(AT.float(0.), "floor", floor(-1.2), -2.);
  AT.check(AT.float(0.), "floor", floor(-1.5), -2.);
  AT.check(AT.float(0.), "floor", floor(-1.8), -2.);

  AT.check(AT.float(0.), "ceiling", ceiling(1.2), 2.);
  AT.check(AT.float(0.), "ceiling", ceiling(1.5), 2.);
  AT.check(AT.float(0.), "ceiling", ceiling(1.8), 2.);
  AT.check(AT.float(0.), "ceiling", ceiling(-1.2), -1.);
  AT.check(AT.float(0.), "ceiling", ceiling(-1.5), -1.);
  AT.check(AT.float(0.), "ceiling", ceiling(-1.8), -1.);

  AT.check(AT.float(0.), "truncate", truncate(1.2), 1.);
  AT.check(AT.float(0.), "truncate", truncate(1.5), 1.);
  AT.check(AT.float(0.), "truncate", truncate(1.8), 1.);
  AT.check(AT.float(0.), "truncate", truncate(-1.2), -1.);
  AT.check(AT.float(0.), "truncate", truncate(-1.5), -1.);
  AT.check(AT.float(0.), "truncate", truncate(-1.8), -1.);

  AT.check(AT.float(0.), "ofInt - 5", ofInt(5), 5.0);
  AT.check(AT.float(0.), "ofInt - 0", ofInt(0), 0.0);
  AT.check(AT.float(0.), "ofInt - -7", ofInt(-7), -7.0);

  AT.check(AT.option(AT.int), "toInt - 5.", toInt(5.), Some(5));
  AT.check(AT.option(AT.int), "toInt - 5.3", toInt(5.3), Some(5));
  AT.check(AT.option(AT.int), "toInt - 0.", toInt(0.), Some(0));
  AT.check(AT.option(AT.int), "toInt - -7.", toInt(-7.), Some(-7));
  AT.check(AT.option(AT.int), "toInt - nan", toInt(nan), None);
  AT.check(AT.option(AT.int), "toInt - infinity", toInt(infinity), None);
  AT.check(
    AT.option(AT.int),
    "toInt - negativeInfinity",
    toInt(negativeInfinity),
    None,
  );

  ();
})