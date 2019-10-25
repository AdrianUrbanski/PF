let rev xs =
  let rec aux xs acc =
    match xs with
      [] -> acc
    | hd::tl -> aux tl (hd::acc)
  in
  aux xs [];;

let map f xs =
  let rec aux xs acc =
    match xs with
      [] -> acc
    | hd::tl -> aux tl ((f hd)::acc)
  in
  aux xs [];;

let rec append xs ys =
  match xs with
    [] -> ys
  | hd::tl -> (hd :: append tl ys);;

let suffixes list =
  let rec aux xs acc =
    match xs with
      [] -> acc
    | hd::tl -> aux tl (xs::acc)
  in
  rev(aux list []);;

let prefixes list =
  map rev (suffixes (rev list));;

suffixes [1;2;3;4];;

prefixes [1;2;3;4];;
