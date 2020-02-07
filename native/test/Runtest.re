let () = {
  let (suite, exit) =
    Junit_alcotest.run_and_report("Standard", [("tests", [
      ArrayTest.suite,   
      BoolTest.suite,
      CharTest.suite,
      FloatTest.suite,
      FunTest.suite,
      IntegerTest.suite,
      IntTest.suite,
      ListTest.suite,
      OptionTest.suite,
      ResultTest.suite,
      StringTest.suite,
      Tuple3Test.suite,
      TupleTest.suite,
    ])]);

  let report = Junit.make([suite]);
  Junit.to_file(report, "test-native.xml");
};