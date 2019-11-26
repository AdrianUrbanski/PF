let (fresh, reset) =
  let x = ref 0 in
  let fresh name =
    let y = x in
    y:= !y+1;
    name^(string_of_int !y)
  in
  let reset n =
    let y = x in
    y:= n
  in
  (fresh, reset);;

fresh "x";;
fresh "x";;
reset 5;;
fresh "x";;
