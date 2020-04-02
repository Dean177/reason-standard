open AlcoJest;
open Standard;

let suite = suite("Vector", () => {
  let ones = Vector.initialize(3, ~f=Fun.constant(1));

  test("empty", () => {
    expect(Vector.empty |> Vector.toList) |> toEqual(Eq.(list(int)), [])
  })

  describe("initialize", () => {
    test("creates a vector ", () => {
      expect(Vector.initialize(5, ~f=Fun.identity) |> Vector.toList)
      |> toEqual(Eq.list(Eq.int), [0,1,2,3,4])
    });
  });

  describe("get", () => {
    test("get an element from an empty vector", () => {
      expect(Vector.get(Vector.empty, 0))
      |> toEqual(Eq.(option(int)), None)
    });    

    test("get an element from a non-empty vector", () => {
      expect(Vector.get(ones, 0))
      |> toEqual(Eq.(option(int)), Some(1))
    });    
  });

  describe("set", () => {
    test("set an element into an empty vector", () => {      
      expect(
        Vector.set(Vector.empty, 0, "Orange")
        |> Vector.toList
      )
      |> toEqual(Eq.(list(string)), [])
    });    
  });

  describe("push", () => {
    test("push an element into an empty vector", () => {
      Vector.push(Vector.empty, 1)
      |> Vector.toList
      |> expect
      |> toEqual(Eq.(list(int)), [1])
    });    
  });

  describe("foldRight", () => {
    test("processes elements from highest to lowest index", () => {
      expect(
        Vector.foldRight(
          Vector.initialize(3, ~f=Fun.identity), 
          ~initial=[], 
          ~f=List.cons,
        )
      )
      |> toEqual(Eq.(list(int)), [0,1,2])
    });    
  });

  test("toList", () => {
    expect(Vector.initialize(4, ~f=Fun.constant(0)) |> Vector.toList) 
    |> toEqual(Eq.(list(int)), [0,0,0,0])
  })
});