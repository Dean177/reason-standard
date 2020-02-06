open AlcoJest;

let suite = suite("Char", () => {

  AT.check(AT.int, "toCode", Char.toCode('a'), 97);

  AT.check(
    AT.option(AT.char),
    "ofCode - valid ASCII codes return the corresponding character",
    Char.ofCode(97),
    Some('a'),
  );
  AT.check(
    AT.option(AT.char),
    "ofCode - negative integers return none",
    Char.ofCode(-1),
    None,
  );
  AT.check(
    AT.option(AT.char),
    "ofCode - integers greater than 255 return none",
    Char.ofCode(256),
    None,
  );

  AT.check(AT.string, "toString", Char.toString('a'), "a");

  AT.check(
    AT.option(AT.char),
    "ofString - one-length string return Some",
    Char.ofString("a"),
    Some('a'),
  );
  AT.check(
    AT.option(AT.char),
    "ofString - multi character strings return none",
    Char.ofString("abc"),
    None,
  );
  AT.check(
    AT.option(AT.char),
    "ofString - zero length strings return none",
    Char.ofString(""),
    None,
  );

  AT.check(
    AT.char,
    "toLowercase - converts uppercase ASCII characters to lowercase",
    Char.toLowercase('A'),
    'a',
  );
  AT.check(
    AT.char,
    "toLowercase - perserves lowercase characters",
    Char.toLowercase('a'),
    'a',
  );
  AT.check(
    AT.char,
    "toLowercase - perserves non-alphabet characters",
    Char.toLowercase('7'),
    '7',
  );
  AT.check(
    AT.char,
    "toUppercase - perserves non-ASCII characters",
    Char.toUppercase('\237'),
    '\237',
  );

  AT.check(
    AT.char,
    "toUppercase - converts lowercase ASCII characters to uppercase",
    Char.toUppercase('a'),
    'A',
  );
  AT.check(
    AT.char,
    "toUppercase - perserves uppercase characters",
    Char.toUppercase('A'),
    'A',
  );
  AT.check(
    AT.char,
    "toUppercase - perserves non-alphabet characters",
    Char.toUppercase('7'),
    '7',
  );
  AT.check(
    AT.char,
    "toUppercase - perserves non-ASCII characters",
    Char.toUppercase('\236'),
    '\236',
  );

  AT.check(
    AT.option(AT.int),
    "toDigit - converts ASCII characters representing digits into integers",
    Char.toDigit('0'),
    Some(0),
  );
  AT.check(
    AT.option(AT.int),
    "toDigit - converts ASCII characters representing digits into integers",
    Char.toDigit('8'),
    Some(8),
  );
  AT.check(
    AT.option(AT.int),
    "toDigit - converts ASCII characters representing digits into integers",
    Char.toDigit('a'),
    None,
  );

  AT.check(
    AT.bool,
    "isLowercase - returns true for any lowercase character",
    Char.isLowercase('a'),
    true,
  );
  AT.check(
    AT.bool,
    "isLowercase - returns false for all other characters",
    Char.isLowercase('7'),
    false,
  );
  AT.check(
    AT.bool,
    "isLowercase - returns false for non-ASCII characters",
    Char.isLowercase('\236'),
    false,
  );

  AT.check(
    AT.bool,
    "isUppercase - returns true for any uppercase character",
    Char.isUppercase('A'),
    true,
  );
  AT.check(
    AT.bool,
    "isUppercase - returns false for all other characters",
    Char.isUppercase('7'),
    false,
  );
  AT.check(
    AT.bool,
    "isUppercase - returns false for non-ASCII characters",
    Char.isLowercase('\237'),
    false,
  );

  AT.check(
    AT.bool,
    "isLetter - returns true for any ASCII alphabet character",
    Char.isLetter('A'),
    true,
  );
  AT.check(
    AT.bool,
    "isLetter - returns false for all other characters",
    Char.isLetter('\n'),
    false,
  );
  AT.check(
    AT.bool,
    "isLetter - returns false for non-ASCII characters",
    Char.isLetter('\236'),
    false,
  );

  AT.check(
    AT.bool,
    "isDigit - returns true for digits 0-9",
    Char.isDigit('5'),
    true,
  );
  AT.check(
    AT.bool,
    "isDigit - returns false for all other characters",
    Char.isDigit('a'),
    false,
  );

  AT.check(
    AT.bool,
    "isAlphanumeric - returns true for any alphabet or digit character",
    Char.isAlphanumeric('A'),
    true,
  );
  AT.check(
    AT.bool,
    "isAlphanumeric - returns false for all other characters",
    Char.isAlphanumeric('?'),
    false,
  );

  AT.check(
    AT.bool,
    "isPrintable - returns true for a printable character",
    Char.isPrintable('~'),
    true,
  );
  AT.check(
    AT.option(AT.bool),
    "isPrintable - returns false for non-printable character",
    Char.ofCode(31) |> Option.map(~f=Char.isPrintable),
    Some(false),
  );

  AT.check(
    AT.bool,
    "isWhitespace - returns true for any whitespace character",
    Char.isWhitespace(' '),
    true,
  );
  AT.check(
    AT.bool,
    "isWhitespace - returns false for a non-whitespace character",
    Char.isWhitespace('a'),
    false,
  );
});
