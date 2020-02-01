open Logic

type leaf =
  | Theorem of theorem
  | Hole of (string*formula) list * formula

type tree =
  | Leaf of leaf
  | Imp_E of tree * formula * tree
  | Imp_I of tree * formula
  | Bot_E of tree * formula

type step =
  | Left of formula * tree
  | Right of formula * tree
  | Down_Imp of formula
  | Down_Bot of formula

type proof = Proof of leaf * step list


let (|>) x f = f x;;

let goLeft (tree, path) =
  match tree with
  | Imp_E (l, f, r) -> (l, (Left (f, r))::path)
  | _ -> failwith "this should not happen"

let goRight (tree, path) =
  match tree with
  | Imp_E (l, f, r) -> (r, (Right (f, l))::path)
  | _ -> failwith "this should not happen"

let goDown (tree, path) =
  match tree with
  | Imp_I (t, f) -> (t, (Down_Imp f)::path)
  | Bot_E (t, f) -> (t, (Down_Bot f)::path)
  | _ -> failwith "this should never happen"

let goUp (tree, path) =
  match path with
  | (Left (f, r))::rp -> (Imp_E (tree, f, r), rp)
  | (Right (f, l))::rp -> (Imp_E (l, f, tree), rp)
  | (Down_Imp f)::rp -> (Imp_I (tree, f), rp)
  | (Down_Bot f)::rp -> (Bot_E (tree, f), rp)
  | [] -> failwith "this should never happen"

let proof g f =
  Proof(Hole (g, f), [])

let rec qed pf =
  match pf with
  | Proof(Theorem t, []) -> t
  |  _ -> failwith "not a proper proof"

let goal pf =
  match pf with
  | Proof (Theorem t, []) -> None
  | Proof (Hole (assm, phi), _) -> Some(assm, phi)
  | _ -> failwith "this should not happen_"

let rec merge tree =
  match tree with
  | Leaf (Theorem _) -> tree
  | Leaf (Hole (_,_)) -> tree
  | Imp_I (t, f) ->(
      match (merge t) with
      | Leaf(Theorem thm) -> Leaf (Theorem(imp_i f thm))
      | _ -> tree
    )
  | Bot_E (t, f) ->(
      match (merge t) with
      | Leaf(Theorem thm) -> Leaf (Theorem(bot_e f thm))
      | _ -> tree
    )
  | Imp_E (l, f, r) ->
    match (merge l, merge r) with
    | (Leaf(Theorem thm1), Leaf(Theorem thm2)) -> Leaf (Theorem(imp_e thm1 thm2))
    | (_,_) -> tree

let next pf =
  let rec findLeftmostHole (tree, path) =
    match tree with
    | Leaf (Hole (assm, phi)) -> Proof (Hole (assm, phi), path)
    | Imp_E (Leaf (Theorem _), _, _) -> (tree, path) |> goRight |> findLeftmostHole
    | Imp_E (_, _, _) -> (tree, path) |> goLeft |> findLeftmostHole
    | Imp_I (_, _) -> (tree, path) |> goDown |> findLeftmostHole
    | Bot_E (_, _) -> (tree, path) |> goDown |> findLeftmostHole
    | Leaf (Theorem t) -> failwith "this should never happen"
  in
  let rec climb (tree, path) =
    match path with
    | [] ->(
        let merged_tree = merge tree in
        match merged_tree with
        | Leaf(Theorem t) -> Proof(Theorem t, [])
        | Leaf(Hole(assm, phi)) -> Proof(Hole(assm, phi), [])
        | _ -> (merged_tree, path) |> findLeftmostHole
      )
    | (Left (f, r))::rp -> (
        let (tree_u, path_u) = (tree,path) |> goUp in
        let merged_tree = merge tree_u in
        match merged_tree with
        | Imp_E(_, _, (Leaf (Theorem t))) -> (merged_tree, path_u) |> climb
        | Imp_E(_, _, _) -> (merged_tree, path_u) |> goRight |> findLeftmostHole
        | Leaf(Hole (_, _)) -> failwith "this should never happen"
        | _ -> (merged_tree, path_u) |> climb
      )
    | _::rp ->
      let (tree_u, path_u) = (tree,path) |> goUp in
      (merge tree_u, path_u) |> climb
  in
  match pf with Proof(l, path) -> climb (Leaf l, path)

let intro name pf =
  match pf with
  | Proof(Hole(assm, phi), path) -> (
    match phi with
    | Implication(f1, f2) -> Proof(Hole((name, f1)::assm, f2), Down_Imp(f1)::path)
    | _ -> failwith "formula is not an implication"
  )
  | _ -> failwith "argument is not a hole"

let rec apply f pf =
  match (f, pf) with
  | (Implication(f1, f2), pf) ->(
      match apply f2 pf with
      | Proof(Hole(assm, phi), path) -> Proof(Hole(assm, f), Left(phi, Leaf(Hole(assm, f1)))::path)
      | Proof(Theorem(_), _) -> failwith "this should never happen"
    )
  | (Contradiction, Proof(Hole(assm,phi), path)) ->
    Proof(Hole(assm, Contradiction), Down_Bot(phi)::path)
  | (f, Proof(Hole(assm, phi), path)) ->(
    if f = phi
    then Proof(Hole(assm, f), path)
    else failwith "phi must be a consequence of formula"
  )
  | _ -> failwith "formula must be an implication or proof does not have a hole"

let fill thm pf =
  let f = consequence thm in
  match pf with
  | Proof(Hole(assm, phi), path) ->(
      if f = phi
      then Proof(Theorem thm, path)
      else failwith "this theorem is not very helpful"
    )
  | Proof(Theorem(_), _) -> failwith "argument is not a hole"

let apply_thm thm pf =
  let f = consequence thm in
  let p = fill thm (apply f pf) in
  next p

let apply_assm name pf =
  match pf with
  | Proof(Hole(assm, phi), path) -> let f = List.assoc name assm in
    apply_thm (by_assumption f) pf
  | _ -> failwith "argument is not a hole"

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()
