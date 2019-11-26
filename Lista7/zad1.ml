let rec fix f x = f (fix f) x;;

let fact = fix (fun f -> fun n -> if n = 0 then 1 else n*f(n-1));;

let fact_ref n =
  let c = ref n in
  let r = ref n in
  begin
    while !c>1 do
      c := !c-1;
      r := !r * !c
    done;
  end;
  !r
;;

let fix_ref f x =
  let g = ref (fun a -> a) in
  begin
    while not ((f !g) x = !g x) do
      g := f !g
    done;
  end;
  (!g x)
;;

let fact2 = fix_ref (fun f -> fun n -> if n = 0 then 1 else n*f(n-1));;

fact 5;;
fact2 5;;
