type btree = Leaf of int | Node of btree * btree;;

let rec prod_cps t k =
  match t with
    Leaf(v) ->
    if v = 0
    then 0
    else k v
  | Node(l, r) -> prod_cps l (fun u-> k (u*prod_cps r (fun u -> u)));;

let prod t =
  prod_cps t (fun u -> u);;

let t1 =
  Node(
    Node(
      Leaf 2,
      Leaf 3
    ),
    Node(
      Leaf 2,
      Node(
        Leaf 1,
        Leaf 3
      )
    )
  );;

let t2 =
  Node(
    Node(
      Leaf 2,
      Leaf 3
    ),
    Node(
      Leaf 2,
      Node(
        Leaf 0,
        Leaf 3
      )
    )
  );;
