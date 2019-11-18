type 'a place =
  Place of 'a list * 'a list;;

let findNth xs n =
  let rec aux acc xs n =
    if n = 0
    then Place(acc, xs)
    else match xs with
        [] -> failwith "List too short"
      | hd::tl -> aux (hd::acc) tl (n-1)
  in
  aux [] xs n;;

let collapse p =
  match p with
  Place(l, r) -> List.rev_append l r;;

let add elem p =
  match p with
    Place(l, r) -> Place(l, elem::r);;

let del p =
  match p with
    Place(l, []) -> failwith "No element to delete"
  | Place(l, hd::tl) -> Place(l, tl);;

let next p =
  match p with
    Place(l, []) -> failwith "There is no next element"
  | Place(l, hd::tl) -> Place(hd::l, tl);;

let prev p =
  match p with
    Place([], r) -> failwith "There is no previous element"
  | Place(hd::tl, r) -> Place(tl, hd::r);;

collapse (add 3 (findNth [1;2;4] 2));;
collapse (del (findNth [1;2;4] 2));;

findNth[1;2;3;4;5] 2;;
next(findNth [1;2;3;4;5] 2);;
prev(findNth [1;2;3;4;5] 2);;
