open List

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let isBalanced t =
  let rec countNodes t =
    match t with
      Leaf -> 0
    | Node(l, _, r) -> countNodes(l) + countNodes(r) + 1
  in
  match t with
    Leaf -> true
  | Node(l, _, r) -> abs(countNodes(l)-countNodes(r)) <= 1;;

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

let t2 = Node
    (Node
       (
         Node(Leaf, 'c', Leaf),
         'b',
         Leaf),
     'a',
     Node(Leaf, 'd', Leaf)
    );;

isBalanced t1;;
isBalanced t2;;

let rec makeBalancedTree xs =
  let rec split l r cnt n =
    match r with
      [] -> failwith "error"
    | hd::tl ->
      if cnt = n/2
      then (rev l)*hd*tl
      else split (hd::l) tl (cnt+1) n
  in
  match xs with
    [] -> Leaf
  | hd::tl -> let (l, x, r) = (split [] xs 0 (length xs)) in
    Node(
      makeBalancedTree l,
      x,
      makeBalancedTree r
    );;
