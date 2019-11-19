type 'a llist = LNil
              | LCons of 'a * (unit -> 'a llist);;

let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

let lPi =
  let cmp s =
    if s<0.
    then (-.)
    else (+.) in
  let rec aux (f, s) = LCons(f, function() -> aux((f+.(1./.s), ((cmp s) s 2.)*. -1.))) in
  aux (1., -3.);;

let rec map3 f  = function
    LCons(x1, xf1) ->
    ( match xf1() with
        LCons(x2, xf2) ->
        ( match xf2() with
            LCons(x3, xf3) -> LCons(f x1 x2 x3, function () -> map3 f (xf3()) )
          | _ -> failwith "this should not happen"
        )
      | _ -> failwith "this should not happen"
    )
  | _ -> failwith "this should not happen";;

let eulersTransformation x y z =
  let sq a = a*.a in
  z -. (sq (y-.z))/.(x -. 2.*.y +. z);;

let fasterPi = map3 eulersTransformation lPi;;

type 'a llist2 = LNil2
               | LCons2 of 'a * 'a llist2 Lazy.t;;

let rec ltake2 = function(0, _) -> []
                      | (_, LNil2) -> []
                      | (n, LCons2(x,lazy xs)) -> x::ltake2(n-1,xs);;

let lPi2 =
  let cmp s =
    if s<0.
    then (-.)
    else (+.) in
  let rec aux (f, s) = LCons2(f, lazy(aux((f+.(1./.s), ((cmp s) s 2.)*. -1.)))) in
  aux (1., -3.);;

let rec ltake2 = function(0, _) -> []
                      | (_, LNil2) -> []
                      | (n, LCons2(x,lazy xs)) -> x::ltake2(n-1,xs);;

let rec map3_2 f = function
    LCons2(x1, lazy (LCons2(x2, lazy (LCons2(x3, lazy xs))))) -> LCons2((f x1 x2 x3), lazy (map3_2 f xs))
  | _ -> failwith "this should not happen";;

let fasterPi2 = map3_2 eulersTransformation lPi2;;

List.map (fun k -> 4.*.k) (ltake (10, lPi));;
List.map (fun k -> 4.*.k) (ltake (10, fasterPi));;

List.map (fun k -> 4.*.k) (ltake2 (10, lPi2));;
List.map (fun k -> 4.*.k) (ltake2 (10, fasterPi2));;
