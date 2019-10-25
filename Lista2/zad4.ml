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

let len xs =
  let rec aux xs n =
    match xs with
      [] -> n
    | hd::tl -> aux tl (n+1)
  in
  aux xs 0;;

let partition p xs =
  let rec aux xs good bad =
    match xs with
      [] -> (rev good), (rev bad)
    | hd::tl ->
      if p hd
      then aux tl (hd::good) bad
      else aux tl good (hd::bad)
  in aux xs [] [];;

let rec quicksort cmp xs =
  match xs with
    [] -> []
  | hd::tl -> match (partition (cmp hd) tl) with
    lesser, greater -> (append (quicksort cmp lesser) (hd::(quicksort cmp greater)));;

quicksort (>) [1;3;5;2;6;4];;
