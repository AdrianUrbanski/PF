type ('arg,'value) calls =
    Cnil
  | Ccons of ('arg*'value) * ('arg, 'value) calls;;

type 'a arg =
    Nil
  | Arg of 'a;;

let calls_empty =
  ref Cnil;;

let calls_find arg calls =
  let rec aux calls =
    match calls with
      Cnil -> Nil
    | Ccons ((a,v), ctl) ->
      (
        if a = arg
        then Arg(v)
        else aux ctl
      )
  in
  aux !calls;;

let calls_add arg v calls =
  calls := Ccons((arg,v), !calls);;

let fib_calls = calls_empty;;
calls_add 0 0 fib_calls;;
calls_add 1 1 fib_calls;;

let fib_calls2 = calls_empty;;

let rec fib n =
  match n with
    0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2);;

let memo f calls n =
  match calls_find n calls with
    Arg(v) -> v
  | Nil -> let v = f n in
	   calls_add n v;
	   v;;

let rec fib_memo n =
  match calls_find n fib_calls with
    Arg(v) -> v
  | Nil -> let fibn = fib_memo (n-1) + fib_memo(n-2) in
    calls_add n fibn fib_calls;
    fibn;;

(* https://stackoverflow.com/questions/9061421/running-time-in-ocaml *)
let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

time fib 30;;
time fib_memo 30;;
