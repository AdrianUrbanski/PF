let inc x = x + 1;;
let identity f = f;;

let compose f g x = (f (g x));;

let rec iter f t comp x =
  if t = 0
  then comp x
  else iter f (t - 1) (compose comp f) x;;

let mult a b =
  iter ((+) a) (b-1) identity a;;

let pow a b =
  iter (mult a) (b-1) identity a;;

let (^) a b = pow a b;;
