type 'a list_mutable =
    LMnil
  | LMcons of 'a * 'a list_mutable ref;;

let concat_copy l1 l2 =
  let rec aux l =
    match l with
      LMnil -> l2
    | LMcons(v, ltl) -> LMcons(v, ref (aux !ltl))
  in
  aux l1;;

let concat_share l1 l2 =
  let rec aux l =
    match l with
      LMnil -> l2
    | LMcons(_, ltl) -> (
        match !ltl with
          LMnil -> ltl := l2; l1
        | LMcons(_,_) -> aux !ltl
      )
  in
  aux l1;;

let cut l =
  match l with
    LMnil -> l
  | LMcons(_, ltl) -> ltl := LMnil;
    l;;

let l1 = LMcons(1, ref (LMcons(2, ref LMnil)));;
let l2 = LMcons(3, ref (LMcons(4, ref LMnil)));;

let lc = concat_copy l1 l2;;
l1;;
let ls = concat_share l1 l2;;
