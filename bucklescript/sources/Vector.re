open Core;

let branchFactor = 32;
let shiftStep = 5; /* log_2 branchFactor */
let bitMask = 0xFFFFFFFF lsr (branchFactor - shiftStep)

/**
  Immutable, fixed length arrays. 
  
  Operations like set and push copy the entire array  
*/ 
module IArray: {
  type t('a);
  let empty : t('a);
  let initialize: (int, ~f:(int => 'a)) => t('a);
  let ofList: (int, list('a)) => (t('a), list('a));
  let singleton: ('a) => (t('a));
  let length: t('a) => int;
  let get: (t('a), int) => ('a);
  let set: (t('a), int, 'a) => t('a);
  let push: (t('a), 'a) => t('a);
  let foldRight: (t('a), ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b;
} = {    
  type t('a) = array('a);

  let ofList = (take: int, list: list('a)): (array('a), list('a)) => {
    let count = ref(0)
    
    let (elements, tail) = List.splitWhen(list, ~f=(_ => {
      Ref.increment(count);
      count^ >= take
    }));

    (List.toArray(elements), tail)
  };

  let initialize = Array.initialize;

  let empty = [||];

  let singleton = (value): t('a) => [|(value)|];

  let length = Array.length;

  let get = (t: t('a), index: int): 'a => 
    t[index];

  let set = (t: t('a), index: int, value: 'a): t('a) => 
    Array.initialize(Array.length(t), ~f=i => 
      if(i == index) {
        (value)
      } else {
        t[i]
      } 
    );

  let push = (t: t('a), value: 'a): t('a) => 
    Array.initialize(Array.length(t) + 1, ~f=(index) => 
      if(index == Array.length(t)) {
        (value)
      } else {
        t[index]
      } 
    )

  let foldRight = Array.foldRight; 
};

/*
  Immutable trie 
*/ 
module Tree = {
  type t('a) = IArray.t(node('a))
  and node('a) = 
    | Leaf(IArray.t('a))
    | SubTree((t('a)));

  let empty = IArray.empty
    
  let rec get = (t: t('a), shift: int, index: int): 'a => {
    let pos = ((index lsr shift)) land bitMask;
      switch (IArray.get(t, pos)) {
      | SubTree(t') => get(t', shift - shiftStep, index)
      | Leaf(values) => IArray.get(values, index land bitMask)
      }
  }

  let rec set = (t: t('a), shift: int, index: int, value: 'a): t('a) => {
    let pos = ((index lsr shift)) land bitMask;
    switch (IArray.get(t, pos)) {
    | SubTree(subTree) => {
      let newSub = set(subTree, (shift - shiftStep), index, value)
      IArray.set(t, pos, SubTree(newSub))
    }
    | Leaf(values) => {
      let newLeaf = IArray.set(values, index land bitMask, value)
      IArray.set(t, pos, Leaf(newLeaf))
    }
    }    
  }

  let rec insertTail = (tree, tail, shift, index): t('a) => {
    let pos = (index lsr shift) land bitMask;
    if (pos >= IArray.length(tree)) {
      if (shift == shiftStep) {
        IArray.push(tree, Leaf(tail))
      } else {
        let newSub = insertTail(IArray.empty, tail, (shift - shiftStep), index);
        IArray.push(tree, SubTree(newSub))
      }
    } else {
      let value = IArray.get(tree, pos);    
      let newSub = 
        switch(value) {
        | SubTree(subTree) =>
            insertTail(subTree, tail, (shift - shiftStep), index);
        | Leaf(_) => 
            insertTail(tree, tail, (shift - shiftStep), index) 
        };
      IArray.set(tree, pos, SubTree(newSub))
    }
  };

  let foldRight = (tree, ~initial, ~f) => {
    let rec loop = (acc, node) => {
      switch (node) {
      | SubTree(subTree) =>
        IArray.foldRight(subTree, ~initial=acc, ~f=loop)
      | Leaf(values) =>
        IArray.foldRight(values, ~initial=acc, ~f)
      }
    };

    IArray.foldRight(tree, ~initial, ~f=loop)
  };
};

type t('a) = {
  length: int,
  startShift: int,
  tree: Tree.t('a),
  tail: IArray.t('a),
};

/* 
  Given an array length, return the index of the first element in the tail.
  Commonly used to check if a given index references something in the tail.
*/
let tailIndex = (length) =>
  (length lsr shiftStep) lsl shiftStep

let empty = {
  length: 0,
  startShift: shiftStep,
  tree:  Tree.empty,
  tail: IArray.empty,
};

type builder('a) = {
  tail: IArray.t('a),
  nodeList: list(Tree.node('a)),
  nodeListLength: int,
}

/* Takes a list of nodes and return a list of `SubTree`s containing those nodes.*/
let rec compressNodes = (nodes: list(Tree.node('a)), acc) => {
  let (node, remainingNodes) = IArray.ofList(branchFactor, nodes);
  let newAcc = [Tree.SubTree(node), ...acc];
  switch(remainingNodes) {
  | [] => List.reverse(newAcc)
  | _ => compressNodes(remainingNodes, newAcc)
  }
};          

/* Takes a list of leaves and an `Int` specifying how many leaves there are, and builds a tree structure to be used in an `Array`. */
let rec treeFromBuilder = (nodeList:list(Tree.node('a)), nodeListLength: int): Tree.t('a) => {
  let newNodeSize = Float.((ofInt(nodeListLength)) / ofInt(branchFactor) |> ceiling |> toInt) |> Option.getUnsafe;
  if (newNodeSize == 1) {
    IArray.ofList(branchFactor, nodeList) |> Tuple.first
  } else {
    treeFromBuilder(compressNodes(nodeList, []), newNodeSize)
  }
}

/* 
  Construct an array with the information in a given builder.

  Due to the nature of `List` the list of nodes in a builder will often
  be in reverse order (that is, the first leaf of the array is the last
  node in the node list). This function therefore allows the caller to
  specify if the node list should be reversed before building the array.
*/
let builderToArray = (reverseNodeList: bool, builder: builder('a)): t('a) => {
  if (builder.nodeListLength == 0) {        
    { 
      length: IArray.length(builder.tail),
      startShift: shiftStep,
      tree: IArray.empty,
      tail: builder.tail
    }
  } else {
    let treeLen = builder.nodeListLength * branchFactor;
    let depth = 
      Float.ofInt(treeLen - 1)        
      |> Float.log(~base=Float.ofInt(branchFactor))
      |> Float.floor
      |> Float.toInt
      |> Option.getUnsafe;

    let correctNodeList =
      if (reverseNodeList) {
        List.reverse(builder.nodeList)}
      else {
        builder.nodeList
      };

    let tree = 
      treeFromBuilder(correctNodeList, builder.nodeListLength);            
    
    { 
      length: IArray.length(builder.tail) + treeLen,
      startShift: (Int.maximum(5, depth * shiftStep)),
      tree,
      tail: builder.tail,
    }
  }
}

let rec initializeHelp = (fn, fromIndex, len, nodeList, tail) => {
  if (fromIndex < 0) {
    builderToArray(
      false, 
      { tail, nodeList, nodeListLength: len / branchFactor }
    )
  } else {
    let leaf = Tree.Leaf(
      IArray.initialize(branchFactor, ~f=(index) => fn(index +fromIndex))
    );
    initializeHelp(
      fn,
      (fromIndex - branchFactor),
      len,
      [leaf,  ...nodeList],
      tail
    )
  }
};

let initialize = (length: int,  ~f:int => 'a): t('a) => {
  if (length <= 0) {
    empty
  } else {
    let tailLen = Int.remainder(length, ~by=branchFactor);
    let tail = IArray.initialize(tailLen, ~f=(index) => f(index + (length - tailLen)));
    let initialFromIndex = length - tailLen - branchFactor;
    initializeHelp(f, initialFromIndex, length, [], tail)
  }
};

let get = (t:t('a), index:int): option('a) => {
  if (index < 0 || index >= t.length) {
    None
  } else if (index >= tailIndex(t.length)) {
    Some(IArray.get(t.tail, index))
  } else {
    Some(Tree.get(t.tree, t.startShift, index))
  }
}

let set = (t, index, value): t('a) => {
  if (index < 0 || index >= t.length) {
    t
  } else if (index >= tailIndex(t.length)) {
    {...t, tail: IArray.set(t.tail, index land bitMask, value) }
  } else {        
    { ...t, tree: Tree.set(t.tree, t.startShift, index, value)}
  }
}

/* 
  Replaces the tail of an array. If the length of the tail equals the
  `branchFactor`, it is inserted into the tree, and the tail cleared.

  WARNING: For performance reasons, this function does not check if the new tail
  has a length equal to or beneath the `branchFactor`. Make sure this is the case
  before using this function.
*/
let unsafeReplaceTail = (t: t('a), newTail: IArray.t('a)): t('a) => {
  let originalTailLen = IArray.length(t.tail);
  let newTailLen = IArray.length(newTail);
  let newArrayLen = t.length + (newTailLen - originalTailLen);
  if(newTailLen == branchFactor) {
    let hasOverflown = 
      newArrayLen lsr shiftStep > t.startShift lsl 1;
    if (hasOverflown) {
      let newShift = t.startShift + shiftStep;
      let newTree =
        Tree.insertTail(
          IArray.singleton(Tree.SubTree(t.tree)), 
          newTail, 
          newShift, 
          t.length
        );
      { 
        length: newArrayLen,
        startShift: newShift,
        tree: newTree,
        tail: IArray.empty,
      }
    } else {
      { 
        length: newArrayLen,
        startShift: t.startShift,
        tree: Tree.insertTail(t.tree, newTail, t.startShift, t.length),
        tail: IArray.empty,
      }
    }
  } else {
    { 
      length: newArrayLen,
      startShift: t.startShift,
      tree: t.tree,
      tail: newTail,
    }
  }
};

let push = (t, value): t('a) => {
  unsafeReplaceTail(t, IArray.push(t.tail, value))
}

/* O(n) */
let foldRight = (t: t('a), ~initial, ~f) => {
  let tailAcc = IArray.foldRight(t.tail, ~initial, ~f)  
  Tree.foldRight(t.tree, ~initial=tailAcc, ~f)  
}


/*
 let filter = (vector, ~f) =>
   ofList(foldRight vector, ~initial=[], ~f=((acc, element) => 
     if (f(x)) {[element,...acc]} else {acc}
   ))
*/


/* O(n)*/
let toList = foldRight(~initial=[], ~f=List.cons)

/* O(n * log_32(n))? Can we do better? */
/* let toArray = t => Array.initialize(t.length, ~f=getUnsafe) */


