let x = ref 0;;

let fresh name =
  let y = x in
  y:= !y+1;
  name^(string_of_int !y);;

let reset n =
  let y = x in
  y:= n;;
