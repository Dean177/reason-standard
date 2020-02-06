open AlcoJest;

suite("Char", () => {
  open Standard.Char;

  test("toCode", () => {
    expect(Char.toCode('a')) |> toEqual(97)
  });

  describe("fromCode", () => {
    test("valid ASCII codes return the corresponding character", () => {
      expect(Char.fromCode(97)) |> toEqual(Eq.(option(char)), Some('a'))
    });
    test("negative integers return none", () => {
      expect(Char.fromCode(-1)) |> toEqual(Eq.(option(char)), None)
    });
    test("integers greater than 255 return none", () => {
      expect(Char.fromCode(256)) |> toEqual(Eq.(option(char)), None)
    });
  });

  test("toString", () => {
    expect(Char.toString('a')) |> toEqual("a")
  });

  describe("fromString", () => {
    test("one-length string return Some", () => {
      expect(Char.fromString("a")) |> toEqual(Eq.(option(char)), Some('a'))
    });
    test("multi character strings return none", () => {
      expect(Char.fromString("abc")) |> toEqual(Eq.(option(char)), None)
    });
    test("zero length strings return none", () => {
      expect(Char.fromString("")) |> toEqual(Eq.(option(int)), None)
    });
  });

  describe("toLowercase", () => {
    test("converts uppercase ASCII characters to lowercase", () => {
      expect(Char.toLowercase('A')) |> toEqual('a')
    });
    test("perserves lowercase characters", () => {
      expect(Char.toLowercase('a')) |> toEqual('a')
    });
    test("perserves non-alphabet characters", () => {
      expect(Char.toLowercase('7')) |> toEqual('7')
    });
    test("perserves non-ASCII characters", () => {
      expect(Char.toUppercase('\233')) |> toEqual('\233')
    });
  });

  describe("toUppercase", () => {
    test("converts lowercase ASCII characters to uppercase", () => {
      expect(Char.toUppercase('a')) |> toEqual('A')
    });
    test("perserves uppercase characters", () => {
      expect(Char.toUppercase('A')) |> toEqual('A')
    });
    test("perserves non-alphabet characters", () => {
      expect(Char.toUppercase('7')) |> toEqual('7')
    });
    test("perserves non-ASCII characters", () => {
      expect(Char.toUppercase('\233')) |> toEqual('\233')
    });
  });

  describe("toDigit", () => {
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('0')) |> toEqual(Eq.(option(int)), Some(0))
    );
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('8')) |> toEqual(Eq.(option(int)), Some(8))
    );
    test(
      "toDigit - converts ASCII characters representing digits into integers",
      () =>
      expect(Char.toDigit('a')) |> toEqual(Eq.(option(int)), None)
    );
  });

  describe("isLowercase", () => {
    test("returns true for any lowercase character", () => {
      expect(Char.isLowercase('a')) |> toEqual(Eq.bool, true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isLowercase('7')) |> toEqual(Eq.bool, false)
    });
    test("returns false for non-ASCII characters", () => {
      expect(Char.isLowercase('\236')) |> toEqual(Eq.bool, false)
    });
  });

  describe("isUppercase", () => {
    test("returns true for any uppercase character", () => {
      expect(Char.isUppercase('A')) |> toEqual(Eq.bool, true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isUppercase('7')) |> toEqual(Eq.bool, false)
    });
    test("returns false for non-ASCII characters", () => {
      expect(Char.isLowercase('\237')) |> toEqual(Eq.bool, false)
    });
  });

  describe("isLetter", () => {
    test("returns true for any ASCII alphabet character", () => {
      expect(Char.isLetter('A')) |> toEqual(Eq.bool, true)
    });

    testAll(
      "returns false for all other characters",
      ['7', ' ', '\n', '\011', '\236'],
      char =>
      expect(Char.isLetter(char)) |> toEqual(Eq.bool, false)
    );
  });

  describe("isDigit", () => {
    testAll(
      "returns true for digits 0-9",
      ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
      digit =>
      expect(Char.isDigit(digit)) |> toEqual(Eq.bool, true)
    );
    test("returns false for all other characters", () => {
      expect(Char.isDigit('a')) |> toEqual(Eq.bool, false)
    });
  });

  describe("isAlphanumeric", () => {
    test("returns true for any alphabet or digit character", () => {
      expect(Char.isAlphanumeric('A')) |> toEqual(Eq.bool, true)
    });
    test("returns false for all other characters", () => {
      expect(Char.isAlphanumeric('?')) |> toEqual(Eq.bool, false)
    });
  });

  describe("isPrintable", () => {
    test("returns true for a printable character", () => {
      expect(Char.isPrintable('~')) |> toEqual(Eq.bool, true)
    });

    test("returns false for non-printable character", () => {
      expect(Char.fromCode(31) |> Option.map(~f=Char.isPrintable))
      |> toEqual(Eq.(option(bool)), Some(false))
    });
  });

  describe("isWhitespace", () => {
    test("returns true for any whitespace character", () => {
      expect(Char.isWhitespace(' ')) |> toEqual(Eq.bool, true)
    });
    test("returns false for a non-whitespace character", () => {
      expect(Char.isWhitespace('a')) |> toEqual(Eq.bool, false)
    });
  });
});