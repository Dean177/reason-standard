type 'a t

val empty : 'a t

(** Initialize a new Vector. 
    
    Creates an Vector of length `n` with the element at index `i` initialized to the result of `(f i)`.

    {2 Examples}

    {[initialize 4 Fun.identity |> toList = [0; 1; 2; 3]]}
    {[initialize 4 (fun n -> n * n) |> toList = fromList [0; 1; 4; 9]]}
    {[initialize 4 (Fun.constant 0) = [0; 0; 0; 0]]}
*)
val initialize : int -> f:(int -> 'a) -> 'a t

(** Set the element at a particular index. 
    
    Returns an updated Vector.
    
    If the index is out of range, the array is unaltered.

    O(log_32(length))
*)
val get : 'a t -> int -> 'a option

(** Set the element at a particular index. 
    
    Returns an updated Vector.
    
    If the index is out of range, the array is unaltered.

    O(?)
*)
val set : 'a t -> int -> 'a -> 'a t

(** Set the element at a particular index. 
    
    Returns an updated Vector.
    
    If the index is out of range, the array is unaltered.

    O(1)
*)
val push : 'a t -> 'a -> 'a t

(* val append : 'a t -> 'a t -> 'a t *)

(** Set the element at a particular index. 
    
    Returns an updated Vector.
    
    If the index is out of range, the array is unaltered.

    O(?)
*)
val foldRight : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b

(** Create a vector from a {!List}
    
    O(Vector.length)
*)
val toList : 'a t -> 'a list


