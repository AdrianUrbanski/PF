let rec flatten xxs xs =
  if xxs = []
  then xs
  else flatten (List.tl xxs) (xs@(List.hd xxs));;

let rec count x xs counter =
  if xs = []
  then counter
  else count x (List.tl xs) (if (List.hd xs) = x then counter + 1 else counter);;
