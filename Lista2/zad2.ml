let rev xs =
  let rec aux xs acc =
    match xs with
      [] -> acc
    | hd::tl -> aux tl (hd::acc)
  in
  aux xs [];;

let rec append xs ys =
  match xs with
    [] -> ys
  | hd::tl -> (hd :: append tl ys);;

let cycle list n =
  let rec aux xs n acc =
    if n = 0
    then append acc (rev xs)
    else
      match xs with
        [] -> (rev acc)
      | hd::tl -> aux tl (n-1) (hd::acc)
  in
  aux (rev list) n [];;

cycle [1;2;3;4] 3;;
cycle [1;2;3;4;5;6] 4;;
