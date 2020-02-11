 type 'a t = 'a list

 type 'a undocumented = 'a list


let bind list f =  List.map f list |> List.concat

let constant = 9.344

let another_constant = 5.6


module Sub = struct
   type nonrec t = int t

   let ofList _ = [2]
end

