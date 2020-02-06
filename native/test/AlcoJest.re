module AT = Alcotest;

type expectation('a) = Expectation(string, 'a);

let currentFunction = ref("");
let currentDescription = ref("");

let suite = (moduleName, callback) => {
  (moduleName, `Quick, callback)  ;
};

let describe = (functionName, callback)  => {
  currentFunction := functionName;
  callback();
  currentFunction := "";
};
  
let test = (description, callback) => {  
  currentDescription := description;
  callback();
  currentDescription := "";
};

let expect = (actual: 'a): expectation('a) => {
  Expectation(currentFunction^ ++ " - " ++ currentDescription^, actual)
};

let toEqual = (matcher, expected, Expectation(description, actual)) => 
  AT.check(matcher, description, expected, actual);

let toRaise = (expected: exn, Expectation(description, run): expectation(unit => 'a)): unit => {
  AT.check_raises(description, expected, run);
};

module Eq = {
  include AT;
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
}
