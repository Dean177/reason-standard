module AT = Alcotest;

type expectation('a) = Expect(string, 'a);

let expect = (description, actual) => Expect(description, actual);

let toEqual = (matcher, expected, Expect(description, actual)) => 
  AT.check(matcher, description, expected, actual);

let toRaise = (matcher, expected, Expect(description, run)) => 
  AT.check_raises(description, expected, run);

let trio = (a, b, c) => {
  let eq = ((a1, b1, c1), (a2, b2, c2)) =>
    AT.equal(a, a1, a2) && AT.equal(b, b1, b2) && AT.equal(c, c1, c2);

  let pp = (ppf, (x, y, z)) =>
    Fmt.pf(
      ppf,
      "@[<1>(@[%a@],@ @[%a@],@ @[%a@])@]",
      AT.pp(a),
      x,
      AT.pp(b),
      y,
      AT.pp(c),
      z,
    );

  AT.testable(pp, eq);
};

let describe = (moduleName, callback) => {
  let test = (testName, callback) => {
    callback(expect(moduleName ++ " - " ++ testName));
  }

  () => callback(test);
};
