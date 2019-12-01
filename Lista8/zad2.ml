module SS = Set.Make(String)
module SSS = Set.Make(SS)

let powerset s =
  let rec aux ss strings =
    match strings with
      [] -> ss
    | (string::rest) -> aux (SSS.union ss (SSS.map (SS.add string) ss)) rest
  in
  aux (SSS.singleton SS.empty) (SS.elements s)

let ps = List.map SS.elements (SSS.elements (powerset (SS.of_list ["a"; "b"; "c"])))
