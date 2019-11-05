open List

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let isBalanced t =
  (* aux: btree -> isBalanced * subtreeNodeTally *)
  let rec aux t =
    match t with
      Leaf -> (true, 0)
    | Node(l,_,r) -> let (isLeftBalanced, leftTally) = aux l and (isRightBalanced, rightTally) = aux r in
      (isLeftBalanced && isRightBanalced && abs(leftTally-rightTally)<=1, leftTally+rightTally+1)
  in
  let (b, _) = aux t in b;;

let rec makeBalancedTree xs =
  let rec split l r cnt n =
    if cnt = n/2
    then (rev l),r
    else match r with
        [] -> l,r
      | hd::tl -> split (hd::l) tl (cnt+1) n
  in
  match xs with
    [] -> Leaf
  | hd::tl -> let (l, r) = (split [] tl 0 (length tl)) in
    Node(
      makeBalancedTree l,
      hd,
      makeBalancedTree r
    );;

let t1 = Node
    (Node
       (
         Node(Leaf, 'c', Leaf),
         'b',
         Node(Leaf, 'd', Leaf)
       ),
     'a',
     Node(Leaf, 'e', Leaf)
    );;

isBalanced t1;;

let t2 = Node
    (Node
       (
         Node(Leaf, 'c', Leaf),
         'b',
         Leaf),
     'a',
     Node(Leaf, 'd', Leaf)
    );;

isBalanced t2;;


makeBalancedTree [1;2;3];;
makeBalancedTree [1;2;3;4;5];;

isBalanced (makeBalancedTree [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21]);;
