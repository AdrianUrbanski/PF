open List

let isMatrix matrix =
  let len = length matrix in
  for_all (fun xs -> length xs = len) matrix;;


let col matrix n =
 map (fun xs -> (nth xs n)) matrix;;

let transposition matrix =
  let rec aux acc n =
    if n = 0
    then acc
    else aux ((col matrix (n-1))::acc) (n-1)
  in
  aux [] (length matrix);;

transposition [[1;2;3];[4;5;6];[7;8;9]];;
(*
 * let zip xs ys =
 *   map2 (fun x y -> x,y) xs ys;;
 *
 * zipf = map2?
*)

let zip xs ys =
  let rec aux xs ys acc =
    match xs, ys with
      [], [] -> acc
    | ((_::_, []) | ([], _::_)) -> raise (Failure "Lists are not the same length")
    | hdx::tlx, hdy::tly -> aux tlx tly ((hdx,hdy)::acc)
  in
  rev(aux xs ys []);;

zip [1.; 2.; 3.] ["a"; "b"; "c"];;

let zipf f xs ys =
  map (fun pair -> f (fst pair) (snd pair)) (zip xs ys);;

zipf (+.) [1.;2.;3.] [4.;5.;6.];;

let mult_vec vector matrix =
  map (fold_left ( +. ) 0.) (map (zipf ( *. ) vector) (transposition matrix));;

mult_vec [1.; 2.] [[2.; 0.]; [4.; 5.]];;

let mult_mat matrix1 matrix2 =
  map (fun vector -> mult_vec vector matrix2) matrix1;;

mult_mat [[1.; 2.];[2.; 1.]] [[2.; 0.]; [4.; 5.]];;
