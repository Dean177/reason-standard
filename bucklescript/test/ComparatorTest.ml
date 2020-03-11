open Standard
open AlcoJest

module Book = struct
  module T = struct
    type t = {
      isbn: string;
      title: string;
    }

    let compare bookA bookB = String.compare bookA.isbn bookB.isbn
  end
  include T

  module ByIsbn = struct 
    include T
    include Comparator.Make(T)
  end

  module ByTitle = (
    val Comparator.make ~compare:(fun (bookA: t) bookB -> 
      String.compare bookA.title bookB.title
    )
  )
end  

let book = (
  let eq (a: Book.t) (b: Book.t) : bool = (a = b) in
  let pp formatter ({title; isbn} : Book.t) : unit =
    Format.pp_print_string
      formatter
      ({|{ "title":"|} ^ title ^ {|", "isbn": "|} ^ isbn ^ {|" }|})
  in
  Eq.make eq pp
)

let mobyDick: Book.t = { 
  isbn="9788460767923"; 
  title="Moby Dick or The Whale" 
}
let mobyDickReissue: Book.t = {
  isbn="9788460767924"; 
  title="Moby Dick or The Whale";
}
let frankenstein: Book.t = {
  isbn="9781478198406";
  title="Frankenstein";
} 
let frankensteinAltTitle: Book.t = {
  isbn="9781478198406";
  title="The Modern Prometheus";
} 

let suite = suite "Comparator" (fun () -> 
  describe "Make" (fun () -> 
    test "module documentation example" (fun () -> 
      
      let result: Book.t list = 
        Set.ofList (module Book.ByIsbn) [frankenstein; frankensteinAltTitle]
        |> Set.toList
      in
      expect result |> toEqual (Eq.list book) [frankenstein]
    )
  );

  describe "make" (fun () -> 
    test "module documentation example" (fun () -> 
      
      let result: Book.t list = 
        Set.ofList (module Book.ByTitle) [mobyDick; mobyDickReissue]
        |> Set.toList
      in
      expect result |> toEqual (Eq.list book) [mobyDick]
    )
  )
)