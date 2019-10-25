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

let sublists list =
  let rec aux oldSubs newSubs list =
    match list with
      [] -> oldSubs
    | hdList::tlList ->
      match oldSubs with
        [] -> aux (rev newSubs) newSubs tlList
      | hdOldSubs::tlOldSubs -> aux tlOldSubs ((hdList::hdOldSubs)::newSubs) list
  in rev(map rev (aux [[]] [[]] list));;

sublists [1;2;3;4];;
