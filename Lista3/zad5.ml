open List

let next_perm perm =
  let rec swap_mg sw acc prev =
    match acc with
      [] -> [sw]@prev
    | hd::tl -> if hd > sw
      then prev@(swap_mg sw tl [hd])
      else (sw::acc)@prev
  in
  let rec aux acc perm =
    match perm with
      [] -> acc
    | hdp::tlp -> if hdp > (hd acc)
      then aux (hdp::acc) tlp
      else (swap_mg hdp acc [])@tlp
  in
  match perm with
    [] -> []
  | hd::tl -> aux [hd] tl;;

let factorial n =
  let rec aux acc n =
    if n = 1
    then acc
    else aux (acc*n) (n-1)
  in
  aux 1 n;;

let find_perms list =
  let rec aux acc perm n =
    if n = 0
    then (rev acc)
    else aux (perm::acc) (next_perm perm) (n-1)
  in
  aux [] list (factorial (length list));;

next_perm [3;4;2;1];;
next_perm ['b';'c';'a'];;
next_perm [1;2;3];;
next_perm [2;3;4;1];;

find_perms ['c';'b';'a'];;
find_perms [4;3;2;1];;
