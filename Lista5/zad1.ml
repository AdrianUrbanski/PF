let isPalindrome xs =
  let rec aux acc xs =
    match acc, xs with
      xs, [] -> false
    | acc, hd::tl ->
      if acc=tl || acc=(hd::tl)
      then true
      else aux (hd::acc) tl
  in
  aux [] xs;;


isPalindrome [1;2;3;4;5];;
isPalindrome [1;2;3;2;1];;
isPalindrome [1;2;3;4];;
isPalindrome [1;2;2;1];;
