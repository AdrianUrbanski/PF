let rec f n =
  if n = 0 then 0
  else 2 * f (n-1) + 1;;

let rec fo cnt n acc =
  if cnt = n then acc
  else fo (cnt + 1) n (2*acc + 1);;
