open Standard;
open AlcoJest;

let () = {
  let (suite, exit) =
    Junit_alcotest.run_and_report("Standard", [("tests", [
      ArrayTest.suite   ,   
      OptionTest.suite,
    ])]);

  let report = Junit.make([suite]);
  Junit.to_file(report, "test-native.xml");
};