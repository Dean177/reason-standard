open Core
let branchFactor = 32
let shiftStep = 5
let bitMask = 0x01f

module IArray : sig
 (** Immutable, fixed length arrays.  Operations like set and push copy the entire array *)
  type 'a t
  val empty : 'a t
  val initialize : int -> f:(int -> 'a) -> 'a t
  val ofList : int -> 'a list -> ('a t * 'a list)
  val singleton : 'a -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val push : 'a t -> 'a -> 'a t
  val foldRight : 'a t -> initial:'b -> f:('b -> 'a -> 'b) -> 'b
end = struct
  type 'a t = 'a array

  let ofList (take : int) (list : 'a list) =
    (let count = ref 0 in
      let (elements, tail) =
        List.splitWhen list
          ~f:(fun _ -> Ref.increment count; (!count) >= take) in
      ((List.toArray elements), tail) : ('a array * 'a list))

  let initialize = Array.initialize

  (* Bucklescript types the empty array weakly, since we share an implementaion, do some magic to prevent this causing a compile error  *)
  let empty = Obj.magic [||]

  let singleton value = ([|value|] : 'a t)

  let length = Array.length

  let get (t : 'a t) (index : int) = (t.(index) : 'a)

  let set (t : 'a t) (index : int) (value : 'a) : 'a t =
    (Array.initialize (Array.length t)
        ~f:(fun i -> if i = index then value else t.(i)) : 'a t)

  let push (t : 'a t) (value : 'a) =
    Array.initialize 
      ((Array.length t) + 1)
      ~f:(fun index ->
            if index = (Array.length t) then value else t.(index)) 

  let foldRight = Array.foldRight
end 
      
module Tree = struct
  type 'a t = 'a node IArray.t
  and 'a node =
    | Leaf of 'a IArray.t 
    | SubTree of 'a t 

  let empty = IArray.empty

  let rec get (t : 'a t) (shift : int) (index : int) =
    (let pos = (index lsr shift) land bitMask in
      match IArray.get t pos with
      | ((SubTree (t'))) ->
          get t' (shift - shiftStep) index
      | ((Leaf (values))) ->
          IArray.get values (index land bitMask) : 'a)

  let rec set (t : 'a t) (shift : int) (index : int) (value : 'a) =
    (let pos = (index lsr shift) land bitMask in
      match IArray.get t pos with
      | ((SubTree (subTree))) ->
          let newSub = set subTree (shift - shiftStep) index value in
          IArray.set t pos ((SubTree (newSub)))
      | ((Leaf (values))) ->
          let newLeaf = IArray.set values (index land bitMask) value in
          IArray.set t pos ((Leaf (newLeaf))) : 'a t)

  let rec insertTail tree tail shift index =
    (let pos = (index lsr shift) land bitMask in
      if pos >= (IArray.length tree)
      then
        (if shift = shiftStep
        then IArray.push tree ((Leaf (tail)))
        else
          (let newSub =
              insertTail IArray.empty tail (shift - shiftStep) index in
            IArray.push tree ((SubTree (newSub)))))
      else
        (let value = IArray.get tree pos in
        let newSub =
          match value with
          | ((SubTree (subTree))) ->
              insertTail subTree tail (shift - shiftStep) index
          | Leaf _ -> insertTail tree tail (shift - shiftStep) index in
        IArray.set tree pos ((SubTree (newSub)))) : 
    'a t)

  let foldRight tree ~initial  ~f  =
    let rec loop acc node =
      match node with
      | ((SubTree (subTree))) ->
          IArray.foldRight subTree ~initial:acc ~f:loop
      | ((Leaf (values))) ->
          IArray.foldRight values ~initial:acc ~f in
    IArray.foldRight tree ~initial ~f:loop
end

type 'a t = {
  length: int;
  startShift: int;
  tree: 'a Tree.t;
  tail: 'a IArray.t;
}

let tailIndex length = (length lsr 5) lsl 5

let empty = {
  length = 0;
  startShift = shiftStep;
  tree = Tree.empty;
  tail = IArray.empty;
}

type 'a builder = {
  tail: 'a IArray.t;
  nodeList: 'a Tree.node list;
  nodeListLength: int;
}

let rec compressNodes (nodes : 'a Tree.node list) acc =
  let (node, remainingNodes) = IArray.ofList branchFactor nodes in
  let newAcc = ((Tree.SubTree (node))) :: acc in
  match remainingNodes with
  | [] -> List.reverse newAcc
  | _ -> compressNodes remainingNodes newAcc

let rec treeFromBuilder (nodeList : 'a Tree.node list) (nodeListLength : int) =
  (let newNodeSize =
     (let open Float in
        (((ofInt nodeListLength) / (ofInt branchFactor)) |> ceiling) |> toInt)
       |> Option.getUnsafe in
   if newNodeSize = 1
   then (IArray.ofList branchFactor nodeList) |> Tuple.first
   else treeFromBuilder (compressNodes nodeList []) newNodeSize : 'a Tree.t)

let builderToArray (reverseNodeList : bool) (builder : 'a builder) =
  (if builder.nodeListLength = 0
   then
     {
       length = (IArray.length builder.tail);
       startShift = shiftStep;
       tree = IArray.empty;
       tail = (builder.tail)
     }
   else
     (let treeLen = builder.nodeListLength * branchFactor in
      let depth =
        ((((Float.ofInt (treeLen - 1)) |>
             (Float.log ~base:(Float.ofInt branchFactor)))
            |> Float.floor)
           |> Float.toInt)
          |> Option.getUnsafe in
      let correctNodeList =
        if reverseNodeList
        then List.reverse builder.nodeList
        else builder.nodeList in
      let tree = treeFromBuilder correctNodeList builder.nodeListLength in
      {
        length = ((IArray.length builder.tail) + treeLen);
        startShift = (Int.maximum 5 (depth * shiftStep));
        tree;
        tail = (builder.tail)
      }) : 'a t)

let rec initializeHelp fn fromIndex len nodeList tail =
  if fromIndex < 0
  then
    builderToArray false
      { tail; nodeList; nodeListLength = (len / branchFactor) }
  else
    (let leaf =
       ((Tree.Leaf
           ((IArray.initialize branchFactor
               ~f:(fun index -> fn (index + fromIndex)))))
       ) in
     initializeHelp fn (fromIndex - branchFactor) len (leaf :: nodeList) tail)

let initialize (length : int) ~f:(f : int -> 'a)  =
  (if length <= 0
   then empty
   else
     (let tailLen = Int.remainder length ~by:branchFactor in
      let tail =
        IArray.initialize tailLen
          ~f:(fun index -> f (index + (length - tailLen))) in
      let initialFromIndex = (length - tailLen) - branchFactor in
      initializeHelp f initialFromIndex length [] tail) : 'a t)

let get (t : 'a t) (index : int) =
  (if (index < 0) || (index >= t.length)
   then None
   else
     if index >= (tailIndex t.length)
     then ((Some ((IArray.get t.tail index))))
     else ((Some ((Tree.get t.tree t.startShift index)))) : 
  'a option)

let set t index value =
  (if (index < 0) || (index >= t.length)
   then t
   else
     if index >= (tailIndex t.length)
     then { t with tail = (IArray.set t.tail (index land bitMask) value) }
     else { t with tree = (Tree.set t.tree t.startShift index value) } : 
  'a t)

let unsafeReplaceTail (t : 'a t) (newTail : 'a IArray.t) =
  (let originalTailLen = IArray.length t.tail in
   let newTailLen = IArray.length newTail in
   let newArrayLen = t.length + (newTailLen - originalTailLen) in
   if newTailLen = branchFactor
   then
     let hasOverflown = (newArrayLen lsr shiftStep) > (t.startShift lsl 1) in
     (if hasOverflown
      then
        let newShift = t.startShift + shiftStep in
        let newTree =
          Tree.insertTail
            (IArray.singleton ((Tree.SubTree ((t.tree)))))
            newTail newShift t.length in
        {
          length = newArrayLen;
          startShift = newShift;
          tree = newTree;
          tail = IArray.empty
        }
      else
        {
          length = newArrayLen;
          startShift = (t.startShift);
          tree = (Tree.insertTail t.tree newTail t.startShift t.length);
          tail = IArray.empty
        })
   else
     {
       length = newArrayLen;
       startShift = (t.startShift);
       tree = (t.tree);
       tail = newTail
     } : 'a t)

let push t value = (unsafeReplaceTail t (IArray.push t.tail value) : 'a t)

let foldRight (t : 'a t) ~initial  ~f  =
  let tailAcc = IArray.foldRight t.tail ~initial ~f in
  Tree.foldRight t.tree ~initial:tailAcc ~f

let toList = foldRight ~initial:[] ~f:List.cons
