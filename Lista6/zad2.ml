type 'a llist =
    LNil
  | LCons of 'a * 'a llist Lazy.t;;

let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,lazy xs)) -> x::ltake(n-1,xs);;

let rec lfilter pred lxs =
  match lxs with
    LNil -> LNil
  | LCons(x, lazy xs) ->
    if pred x
    then LCons(x, lazy (lfilter pred xs))
    else lfilter pred xs;;

type move =
    FILL of int
  | DRAIN of int
  | TRANSFER of (int*int);;

let gen_moves n =
  let rec add_fills acc cnt =
    if cnt = n
    then acc
    else add_fills (FILL(cnt)::acc) (cnt+1)
  in
  let rec add_drains acc cnt =
    if cnt = n
    then acc
    else add_drains (DRAIN(cnt)::acc) (cnt+1)
  in
  let rec add_transfers acc (cnt1, cnt2) =
    match (cnt1, cnt2) with
      (c1, c2) when c1 = (n-1) && c2 = (n-1) -> acc
    | (c1, c2) when c1 = c2 -> add_transfers acc (cnt1, cnt2+1)
    | (c1, c2) when c2 = (n-1) -> add_transfers (TRANSFER(c1, (n-1))::acc) (cnt1+1, 0)
    | (c1, c2) -> add_transfers (TRANSFER(c1, c2)::acc) (cnt1, cnt2+1)
  in
  add_transfers (add_drains (add_fills [] 0) 0) (0, 0);;

let fill (glasses, volumes) n =
  let rec aux (acc1, acc2) (glasses, volumes) n =
    match (glasses, volumes) with
      (glass::gtl, vol::vtl) ->
      if n = 0
      then List.rev_append acc2 (glass::vtl)
      else aux (glass::acc1, vol::acc2) (gtl, vtl) (n-1)
    | (_, _) -> failwith "this should not happen"
  in
  aux ([], []) (glasses, volumes) n;;

let drain (glasses, volumes) n =
  let rec aux (acc1, acc2) (glasses, volumes) n =
    match (glasses, volumes) with
      (glass::gtl, vol::vtl) ->
      if n = 0
      then List.rev_append acc2 (0::vtl)
      else aux (glass::acc1, vol::acc2) (gtl, vtl) (n-1)
    | (_, _) -> failwith "this should not happen"
  in
  aux ([], []) (glasses, volumes) n;;


let transfer (glasses, volumes) (from_g, to_g) =
  let from_vol = List.nth volumes from_g in
  let to_vol = List.nth volumes to_g in
  let to_space = (List.nth glasses to_g) - to_vol in
  let trans_amount = min from_vol to_space  in
  let rec change (acc1, acc2) (glasses, volumes) n v =
    match (glasses, volumes) with
      (glass::gtl, vol::vtl) ->
      if n = 0
      then List.rev_append acc2 (v::vtl)
      else change (glass::acc1, vol::acc2) (gtl, vtl) (n-1) v
    | (_, _) -> failwith "this should not happen"
  in
  change ([],[]) (glasses, (change ([],[]) (glasses, volumes) to_g (to_vol + trans_amount))) from_g (from_vol - trans_amount);;

let empty_list len =
  let rec aux acc l =
    if l = 0
    then acc
    else aux (0::acc) (l-1)
  in
  aux [] len;;

(* strumień znajdujący kolejne ciągi kroków dla danych szklanek *)
let movesets glasses =
  let number_of_glasses = List.length glasses in
  let all_moves = gen_moves number_of_glasses in
  let movesets_queue = Queue.create () in
  let rec node current moves =
    match moves with
      [] -> node (Queue.take movesets_queue) all_moves
    | mv::tlmv -> (match current with
          (volumes, steps) ->
          (
            match mv with
              FILL(n) -> let (new_volumes, new_steps) = ((fill (glasses, volumes) n), mv::steps) in
              Queue.add (new_volumes, new_steps) movesets_queue;
              LCons((new_volumes, new_steps), lazy (node current tlmv))
            | DRAIN(n) -> let (new_volumes, new_steps) = ((drain (glasses, volumes) n), mv::steps) in
              Queue.add (new_volumes, new_steps) movesets_queue;
              LCons((new_volumes, new_steps), lazy (node current tlmv))
            | TRANSFER(n1, n2) -> let (new_volumes, new_steps) = ((transfer (glasses, volumes) (n1,n2)), mv::steps) in
              Queue.add (new_volumes, new_steps) movesets_queue;
              LCons((new_volumes, new_steps), lazy (node current tlmv))
          )
      )
  in
  node ((empty_list number_of_glasses), []) all_moves;;

let nsols (glasses, value) n =
  let sols = lfilter (fun (vols, moves) -> List.exists (fun v -> v = value) vols) (movesets glasses)
  in
  List.map (fun (vols, moves) -> List.rev moves) (ltake (n,sols));;

nsols ([4;9], 5) 5;;
