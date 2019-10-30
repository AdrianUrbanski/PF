let eval_rec polynominal value =
  let rec aux acc xs =
    match xs with
      [] -> acc
    | hd::tl -> aux (acc*.value +. hd) tl
  in
  aux 0. polynominal;;

let eval_fold polynominal value =
  List.fold_left (fun acc a -> acc*.value +. a) 0. polynominal;;
