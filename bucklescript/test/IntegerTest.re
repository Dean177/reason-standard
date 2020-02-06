open Standard;
open AlcoJest;

suite("Integer", () => {
  open Integer;

  describe("add", () => {
    testAll("add", [add, (+)], op => {
      expect(op(fromInt(3002), fromInt(4004))) |> toEqual(fromInt(7006))
    })
  });

  describe("subtract", () => {
    testAll("subtract", [subtract, (-)], op => {
      expect(op(fromInt(4), fromInt(3))) |> toEqual(one)
    })
  });

  describe("multiply", () => {
    testAll("multiply", [multiply, ( * )], op => {
      expect(op(fromInt(2), fromInt(7))) |> toEqual(fromInt(14))
    })
  });

  describe("divide", () => {
    test("divide", () => {
      expect(divide(fromInt(3), ~by=fromInt(2))) |> toEqual(one)
    });

    test("division by zero", () => {
      expect(() => {
        divide(fromInt(3), ~by=zero)
      }) |> toThrow
    });

    test("/", () => {
      expect(fromInt(27) / fromInt(5)) |> toEqual(fromInt(5))
    });
  });

  describe("power", () => {
    test("**", () => {
      expect(fromInt(7) ** fromInt(3)) |> toEqual(fromInt(343))
    })
  });

  describe("negate", () => {
    test("positive number", () => {
      expect(negate(fromInt(8))) |> toEqual(fromInt(-8))
    });
    test("negative number", () => {
      expect(negate(fromInt(-7))) |> toEqual(fromInt(7))
    });
    test("zero", () => {
      expect(negate(zero)) |> toEqual(zero)
    });
  });

  describe("absolute", () => {
    test("positive number", () => {
      expect(absolute(fromInt(8))) |> toEqual(fromInt(8))
    });
    test("negative number", () => {
      expect(absolute(fromInt(-7))) |> toEqual(fromInt(7))
    });
    test("zero", () => {
      expect(absolute(zero)) |> toEqual(zero)
    });
  });

  describe("clamp", () => {
    test("in range", () => {
      expect(clamp(~lower=zero, ~upper=fromInt(8), fromInt(5)))
      |> toEqual(fromInt(5))
    });
    test("above range", () => {
      expect(clamp(~lower=zero, ~upper=fromInt(8), fromInt(9)))
      |> toEqual(fromInt(8))
    });
    test("below range", () => {
      expect(clamp(~lower=fromInt(2), ~upper=fromInt(8), one))
      |> toEqual(fromInt(2))
    });
    test("above negative range", () => {
      expect(clamp(~lower=fromInt(-10), ~upper=fromInt(-5), fromInt(5)))
      |> toEqual(fromInt(-5))
    });
    test("below negative range", () => {
      expect(clamp(~lower=fromInt(-10), ~upper=fromInt(-5), fromInt(-15)))
      |> toEqual(fromInt(-10))
    });
    test("invalid arguments", () => {
      expect(() => {
        clamp(~lower=fromInt(7), ~upper=one, fromInt(3))
      })
      |> toThrow
    });
  });

  describe("inRange", () => {
    test("in range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(3)))
      |> toEqual(Eq.bool, true)
    });
    test("above range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(8)))
      |> toEqual(Eq.bool, false)
    });
    test("below range", () => {
      expect(inRange(~lower=fromInt(2), ~upper=fromInt(4), fromInt(1)))
      |> toEqual(Eq.bool, false)
    });
    test("equal to ~upper", () => {
      expect(inRange(~lower=fromInt(1), ~upper=fromInt(2), fromInt(2)))
      |> toEqual(Eq.bool, false)
    });
    test("negative range", () => {
      expect(inRange(~lower=fromInt(-7), ~upper=fromInt(-5), fromInt(-6)))
      |> toEqual(Eq.bool, true)
    });
    test("invalid arguments", () => {
      expect(() => {
        inRange(~lower=fromInt(7), ~upper=one, fromInt(3))
      })
      |> toThrow
    });
  });

  describe("toFloat", () => {
    test("5", () => {
      expect(toFloat(fromInt(5))) |> toEqual(Eq.(option(float)), Some(5.))
    });
    test("0", () => {
      expect(toFloat(zero)) |> toEqual(Eq.(option(float)), Some(0.))
    });
    test("-7", () => {
      expect(toFloat(fromInt(-7))) |> toEqual(Eq.(option(float)), Some(-7.))
    });
  });

  describe("fromString", () => {
    test("0", () => {
      expect(fromString("0")) |> toEqual(Eq.(option(integer)), Some(zero))
    });
    test("-0", () => {
      expect(fromString("-0")) |> toEqual(Eq.(option(integer)), Some(zero))
    });
    test("42", () => {
      expect(fromString("42")) |> toEqual(Eq.(option(integer)), Some(fromInt(42)))
    });
    test("123_456", () => {
      expect(fromString("123_456")) |> toEqual(Eq.(option(integer)), Some(fromInt(123_456)))
    });
    test("-42", () => {
      expect(fromString("-42")) |> toEqual(Eq.(option(integer)), Some(fromInt(-42)))
    });
    test("0XFF", () => {
      expect(fromString("0XFF")) |> toEqual(Eq.(option(integer)), Some(fromInt(255)))
    });
    test("0X000A", () => {
      expect(fromString("0X000A")) |> toEqual(Eq.(option(integer)), Some(fromInt(10)))
    });
    test("Infinity", () => {
      expect(fromString("Infinity")) |> toEqual(Eq.(option(integer)), None)
    });
    test("-Infinity", () => {
      expect(fromString("-Infinity")) |> toEqual(Eq.(option(integer)), None)
    });
    test("NaN", () => {
      expect(fromString("NaN")) |> toEqual(Eq.(option(integer)), None)
    });
    test("abc", () => {
      expect(fromString("abc")) |> toEqual(Eq.(option(integer)), None)
    });
    test("--4", () => {
      expect(fromString("--4")) |> toEqual(Eq.(option(integer)), None)
    });
    test("empty string", () => {
      expect(fromString(" ")) |> toEqual(Eq.(option(integer)), None)
    });
  });

  describe("toString", () => {
    test("positive number", () => {
      expect(toString(fromInt(1))) |> toEqual("1")
    });
    test("negative number", () => {
      expect(toString(fromInt(-1))) |> toEqual("-1")
    });
  });
});
