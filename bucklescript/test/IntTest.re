open Standard;
open AlcoJest;

let suite = suite("Int", () => {
  open Int;
  test("zero", () => {
    expect(zero) |> toEqual(Eq.int, 0)
  });

  test("one", () => {
    expect(one) |> toEqual(Eq.int, 1)
  });

  test("minimumValue", () => {
    expect(minimumValue - 1) |> toEqual(Eq.int, maximumValue)
  });

  test("maximumValue", () => {
    expect(maximumValue + 1) |> toEqual(Eq.int, minimumValue)
  });

  describe("add", () => {
    test("add", () => {
      expect(add(3002, 4004)) |> toEqual(Eq.int, 7006)
    });
    test("+", () => {
      expect(3002 + 4004) |> toEqual(Eq.int, 7006)
    });
  });

  describe("subtract", () => {
    test("subtract", () => {
      expect(subtract(4, 3)) |> toEqual(Eq.int, 1)
    });
    test("-", () => {
      expect(4 - 3) |> toEqual(Eq.int, 1)
    });
  });

  describe("multiply", () => {
    test("multiply", () => {
      expect(multiply(2, 7)) |> toEqual(Eq.int, 14)
    });
    test("*", () => {
      expect(2 * 7) |> toEqual(Eq.int, 14)
    });
  });

  describe("divide", () => {
    test("divide", () => {
      expect(divide(3, ~by=2)) |> toEqual(Eq.int, 1)
    });
    test("division by zero", () => {
      expect(() => {
        divide(3, ~by=0)
      }) |> toThrow
    });

    test("/", () => {
      expect(27 / 5) |> toEqual(Eq.int, 5)
    });

    test("//", () => {
      expect(3 % 2) |> toEqual(Eq.float, 1.5)
    });
    test("//", () => {
      expect(27 % 5) |> toEqual(Eq.float, 5.4)
    });
    test("//", () => {
      expect(8 % 4) |> toEqual(Eq.float, 2.0)
    });

    test("x // 0", () => {
      expect(8 % 0 == Float.infinity) |> toEqual(Eq.bool, true)
    });
    test("-x // 0", () => {
      expect((-8) % 0 == Float.negativeInfinity) |> toEqual(Eq.bool, true)
    });
  });

  describe("power", () => {
    test("power", () => {
      expect(power(~base=7, ~exponent=3)) |> toEqual(Eq.int, 343)
    });
    test("0 base", () => {
      expect(power(~base=0, ~exponent=3)) |> toEqual(Eq.int, 0)
    });
    test("0 exponent", () => {
      expect(power(~base=7, ~exponent=0)) |> toEqual(Eq.int, 1)
    });
    test("**", () => {
      expect(7 ** 3) |> toEqual(Eq.int,343)
    });
  });

  describe("negate", () => {
    test("positive number", () => {
      expect(negate(8)) |> toEqual(Eq.int,-8)
    });
    test("negative number", () => {
      expect(negate(-7)) |> toEqual(Eq.int,7)
    });
    test("zero", () => {
      expect(negate(0)) |> toEqual(Eq.int,-0)
    });
    test("~-", () => {
      expect(-7) |> toEqual(Eq.int,-7)
    });
  });

  describe("absolute", () => {
    test("positive number", () => {
      expect(absolute(8)) |> toEqual(Eq.int, 8)
    });
    test("negative number", () => {
      expect(absolute(-7)) |> toEqual(Eq.int, 7)
    });
    test("zero", () => {
      expect(absolute(0)) |> toEqual(Eq.int, 0)
    });
  });

  describe("clamp", () => {
    test("in range", () => {
      expect(clamp(~lower=0, ~upper=8, 5)) |> toEqual(Eq.int, 5)
    });
    test("above range", () => {
      expect(clamp(~lower=0, ~upper=8, 9)) |> toEqual(Eq.int, 8)
    });
    test("below range", () => {
      expect(clamp(~lower=2, ~upper=8, 1)) |> toEqual(Eq.int, 2)
    });
    test("above negative range", () => {
      expect(clamp(~lower=-10, ~upper=-5, 5)) |> toEqual(Eq.int, -5)
    });
    test("below negative range", () => {
      expect(clamp(~lower=-10, ~upper=-5, -15)) |> toEqual(Eq.int, -10)
    });
    test("invalid arguments", () => {
      expect(() => {
        clamp(~lower=7, ~upper=1, 3)
      }) |> toThrow
    });
  });

  describe("inRange", () => {
    test("in range", () => {
      expect(inRange(~lower=2, ~upper=4, 3)) |> toEqual(Eq.bool, true)
    });
    test("above range", () => {
      expect(inRange(~lower=2, ~upper=4, 8)) |> toEqual(Eq.bool, false)
    });
    test("below range", () => {
      expect(inRange(~lower=2, ~upper=4, 1)) |> toEqual(Eq.bool, false)
    });
    test("equal to ~upper", () => {
      expect(inRange(~lower=1, ~upper=2, 2)) |> toEqual(Eq.bool, false)
    });
    test("negative range", () => {
      expect(inRange(~lower=-7, ~upper=-5, -6)) |> toEqual(Eq.bool, true)
    });
    test("invalid arguments", () => {
      expect(() => {
        inRange(~lower=7, ~upper=1, 3)
      }) |> toThrow
    });
  });

  describe("toFloat", () => {
    test("5", () => {
      expect(toFloat(5)) |> toEqual(Eq.float, 5.)
    });
    test("0", () => {
      expect(toFloat(0)) |> toEqual(Eq.float, 0.)
    });
    test("-7", () => {
      expect(toFloat(-7)) |> toEqual(Eq.float, -7.)
    });
  });

  describe("ofString", () => {
    test("0", () => {
      expect(ofString("0")) |> toEqual(Eq.(option(int)), Some(0))
    });
    test("-0", () => {
      expect(ofString("-0")) |> toEqual(Eq.(option(int)), Some(-0))
    });
    test("42", () => {
      expect(ofString("42")) |> toEqual(Eq.(option(int)), Some(42))
    });
    test("123_456", () => {
      expect(ofString("123_456")) |> toEqual(Eq.(option(int)), Some(123_456))
    });
    test("-42", () => {
      expect(ofString("-42")) |> toEqual(Eq.(option(int)), Some(-42))
    });
    test("0XFF", () => {
      expect(ofString("0XFF")) |> toEqual(Eq.(option(int)), Some(255))
    });
    test("0X000A", () => {
      expect(ofString("0X000A")) |> toEqual(Eq.(option(int)), Some(10))
    });
    test("Infinity", () => {
      expect(ofString("Infinity")) |> toEqual(Eq.(option(int)), None)
    });
    test("-Infinity", () => {
      expect(ofString("-Infinity")) |> toEqual(Eq.(option(int)), None)
    });
    test("NaN", () => {
      expect(ofString("NaN")) |> toEqual(Eq.(option(int)), None)
    });
    test("abc", () => {
      expect(ofString("abc")) |> toEqual(Eq.(option(int)), None)
    });
    test("--4", () => {
      expect(ofString("--4")) |> toEqual(Eq.(option(int)), None)
    });
    test("empty string", () => {
      expect(ofString(" ")) |> toEqual(Eq.(option(int)), None)
    });
  });

  describe("toString", () => {
    test("positive number", () => {
      expect(toString(1)) |> toEqual(Eq.string, "1")
    });
    test("negative number", () => {
      expect(toString(-1)) |> toEqual(Eq.string, "-1")
    });
  });
});