module Binary_tree_as_tuple = struct
    type 'a tree =
      | Leaf
      | Node of 'a * 'a tree * 'a tree
    let t = Node(4,
                 Node(2,
                      Node(1, Leaf, Leaf),
                      Node(3, Leaf, Leaf)
                   ),
                 Node(5,
                      Node(6, Leaf, Leaf),
                      Node(7, Leaf, Leaf)
                   )
              )
    (*Function to find size of a Tree*)
    let rec size (tr: 'a tree): int =
      match tr with
      | Leaf -> 0
      | Node (_, l, r) -> 1 + size l + size r
    (*Function to Test if a Value is a Member of a Tree*)
    let rec member (x: 'a) (tr: 'a tree): bool =
      match tr with
      | Leaf -> false
      | Node (value, left, right) -> value = x || member x left || member x right

    let () =
      print_endline "Testing Size of Tree";
      assert(size t = 7);
      print_endline "Testing Member of Tree";
      assert(member 7 t = true);
end

module Binary_tree_as_record = struct
  type 'a tree =
    | Leaf
    | Node of 'a node
  and 'a node = {
      value: 'a;
      left: 'a tree;
      right: 'a tree
    }

  let t =
    Node {
        value = 2;
        left = Node {value=1; left=Leaf; right=Leaf};
        right = Node {value=3; left=Leaf; right=Leaf}
      }

  (*Evaluate the Size of a Tree*)
  let rec size (tr: 'a tree): int =
    match tr with
    | Leaf -> 0
    | Node{value; left; right} -> 1 + size left + size right

  (*Recursive Search of Tree*)
  let rec member (x: 'a) (tr: 'a tree): bool =
    match tr with
    | Leaf -> false
    | Node{value; left; right} -> value = x || member x left || member x right

  let () =
    print_endline "Testing Member";
    assert(member 3 t = true);
    print_endline "Testing Size";
    assert(size t = 3)
end

