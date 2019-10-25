let inc n = n+1;;

let compose f g x =
  f (g x);;

let identity n = n;;

let rec iter f t =
  if t = 0
  then identity
  else compose f (iter f (t-1));;

let zero f a =
  a;;

let succ num f a =
   num f (f a);;

let add num1 num2 f a =
  num1 f (num2 f a);;

let mul num1 num2 f a =
  num1 (num2 f) a;;

(* funkcja isZero v1:
 * let isZero num f a =
 * num f a = a;;
*)

(* funkcja isZero - reprezentacja bool z zad5
 * let isZero2 num a b =
 * if num inc 0 = 0
 * then a
 * else b;;
*)

let isZero num =
  num inc 0 = 0;;

let cnum_of_int n f a =
  iter f n a;;

let int_of_cnum num =
  num inc 0;;

let one = succ zero;;
let two = succ one;;
let three = succ two;;
let four = succ three;;

print_string "Liczby w reprezentacji - zero i succ";;
int_of_cnum zero;;
int_of_cnum one;;
int_of_cnum four;;

print_string "Dodawanie";;
int_of_cnum (add two four);;
int_of_cnum (add (cnum_of_int 42) (cnum_of_int 513));;

print_string "Mnożenie";;
int_of_cnum (mul two three);;
int_of_cnum (mul (cnum_of_int 111) (cnum_of_int 7));;

print_string "isZero";;
isZero zero;;
isZero one;;

