let () = {
  Alcotest.run("Standard", [("tests", [
    ArrayTest.suite,   
    BoolTest.suite,
    CharTest.suite,
    FloatTest.suite,
    FunTest.suite,
    IntegerTest.suite,
    IntegerPlatformSpecificTest.suite,
    IntTest.suite,
    ListTest.suite,
    OptionTest.suite,
    ResultTest.suite,
    StringTest.suite,
    Tuple3Test.suite,
    TupleTest.suite,
  ])]);
};