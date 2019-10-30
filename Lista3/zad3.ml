let group_duplicates xs =
  let rec aux acc duplicates xs =
    match xs with
      [] -> (duplicates::acc)
    | hd::tl -> if (List.hd duplicates) = hd
        then aux acc (hd::duplicates) tl
        else aux (duplicates::acc) [hd] tl
  in
  match xs with
    [] -> []
  | hd::tl -> List.rev(aux [] [hd] tl);;

group_duplicates [1; 2; 2; 5; 6; 6; 6; 2; 2];;
