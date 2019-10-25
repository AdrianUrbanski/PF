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

(* Ogonowa *)
let merge cmp xs ys =
  let rec aux acc xs ys =
    match xs, ys with
      xs, [] -> append (rev acc) xs
    | [], ys -> append (rev acc) ys
    | hdxs::tlxs, hdys::tlys ->
      if (cmp hdxs hdys)
      then aux (hdxs::acc) tlxs ys
      else aux (hdys::acc) xs tlys
  in
  aux [] xs ys;;

(* Nie ogonowa *)
let rec merge' cmp xs ys =
  match xs, ys with
    xs, [] -> xs
  | [], ys -> ys
  | hdxs::tlxs, hdys::tlys ->
    if (cmp hdxs hdys)
    then hdxs::(merge cmp tlxs ys)
    else hdys::(merge cmp xs tlys);;

(* Bez odwracania *)
let merge cmp xs ys =
  let rec aux acc xs ys =
    match xs, ys with
      xs, [] -> append (rev xs) acc
    | [], ys -> append (rev ys) acc
    | hdxs::tlxs, hdys::tlys ->
      if (cmp hdxs hdys)
      then aux (hdxs::acc) tlxs ys
      else aux (hdys::acc) xs tlys
  in
  aux [] xs ys;;

let split xs =
  let rec aux acc xs n =
    if n=0
    then (rev acc), xs
    else match xs with
        [] -> [], []
      | hd::tl -> aux (hd::acc) tl (n-1)
  in
  aux [] xs ((len xs)/2);;

let rec mergesort xs =
  if (len xs) = 1
  then xs
  else match (split xs) with
    first, second -> (merge (<=) (mergesort first) (mergesort second));;

let revcmp cmp a b =
  if(cp a b)
  then b
  else a;;

let rec mergesort' cmp xs =
  if (len xs) = 1
  then xs
  else match (split xs) with
    first, second -> (merge'' cmp (mergesort (revcmp cmp) first) (mergesort (revcmp cmp) second));;

merge (<=) [1;2;5] [3;4;5];;

mergesort [1;5;2;4;2;3;6;8;7];;
mergesort' (<=) [1;5;2;4;3;6];;

(*
let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

time (merge (<=) [1;2;5;7;8;10;15]) [3;4;5;6;8;9;10;12];;

time (merge' (<=) [1;2;5;7;8;10;15]) [3;4;5;6;8;9;10;12];;
 *)
