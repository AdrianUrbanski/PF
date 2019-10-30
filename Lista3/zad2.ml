let rec eval_rec polynominal value =
  match polynominal with
    [] -> 0.
  | hd::tl -> (eval_rec tl value)*.value +. hd;;

let eval_fold polynominal value =
  List.fold_right (fun a acc -> acc*.value +. a)  polynominal 0.;;

eval_rec [1.; 0.; -1.; 2.] 2.;;
eval_fold [1.; 0.; -1.; 2.] 2.;;

eval_rec [1.; 0.; -1.; 2.] 0.5;;
eval_fold [1.; 0.; -1.; 2.] 0.5;;
