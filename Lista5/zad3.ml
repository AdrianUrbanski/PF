type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let rec prod t =
  match t with
    Leaf -> 1
  | Node(l, v, r) -> (prod l) * v * (prod r);;

let rec prod_cps t k =
  match t with
    Leaf -> k 1
  | Node(l, 0, r) -> 0
  | Node(l, v, r) ->
    let vl = (prod_cps r (fun u -> u*v)) in
    if vl = 0
    then 0
    else (prod_cps l (fun u -> k u*vl));;

let prod2 t = prod_cps t (fun v -> v);;

let t1 =
  Node(
    Node(
      Leaf,
      3,
      Leaf
    ),
    2,
    Node(
      Node(
        Leaf,
        3,
        Leaf),
      2,
      Leaf)
  )
