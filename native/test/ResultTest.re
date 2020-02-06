open AlcoJest;

let suite = suite("Result", () => {
  AT.check(
    AT.result(AT.int, AT.string),
    "fromOption - maps None into Error",
    Result.(ofOption(~error="error message", None: option(int))),
    Error("error message"),
  );
  AT.check(
    AT.result(AT.int, AT.string),
    "ofOption - maps Some into Ok",
    Result.(ofOption(~error="error message", Some(10))),
    Ok(10),
  );
});
