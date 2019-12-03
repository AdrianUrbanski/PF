module SS = Set.Make(String)
module SSS = Set.Make(SS)

let powerset s =
  let rec aux ss s =
    if(SS.is_empty s)
    then ss
    else
      let elem = SS.min_elt s in
      aux (SSS.union ss (SSS.map (SS.add elem) ss)) (SS.remove elem s)
  in
  aux (SSS.singleton SS.empty) s

let ps = List.map SS.elements (SSS.elements (powerset (SS.of_list ["a"; "b"; "c"])))
