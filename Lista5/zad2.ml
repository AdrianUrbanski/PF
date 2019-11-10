type formula =
    Variable of char
  | Negation of formula
  | Conjunction of formula*formula
  | Disjunction of formula*formula;;

let rec evaluate f vals =
  match f with
    Variable c -> List.assoc c vals
  | Negation f -> not (evaluate f vals)
  | Conjunction(f1, f2) -> (evaluate f1 vals) && (evaluate f2 vals)
  | Disjunction(f1, f2) -> (evaluate f1 vals) || (evaluate f2 vals);;

let variables f =
  let rec aux f =
      match f with
        Variable c -> [c]
      | Negation f -> aux f
      | Conjunction(f1, f2) -> (aux f1)@(aux f2)
      | Disjunction(f1, f2) -> (aux f1)@(aux f2)
  in
  List.sort_uniq Char.compare (aux f);;

let allVals vars =
  let rec aux valss vars =
    match vars with
      [] -> valss
    | var::tl -> aux
                   ((List.map (fun xs -> ((var,true)::xs)) valss)@
                    (List.map (fun xs -> ((var,false)::xs)) valss))
                   tl
  in
  aux [[]] vars;;

let isTautology f =
  let rec aux valss =
    match valss with
      [] -> (true, [])
    | vals::tl ->
      if evaluate f vals
      then aux tl
      else (false, vals)
  in
  aux (allVals (variables f));;

(* ~p or p or q *)
let f1 = Disjunction(
    Negation(
      Variable 'q'
    ),
    Disjunction(
      Variable 'p',
      Variable 'q'
    )
  );;

isTautology f1;;

(* p and (~q or q)*)
let f2 = (Conjunction(
    Variable 'p',
    Disjunction(
      Negation(
        Variable 'q'
      ),
      Variable 'q'
    )
  ));;

isTautology f2;;

type nnf =
    Literal of char*bool
  | Conjunction of nnf*nnf
  | Disjunction of nnf*nnf;;

let nnf_of_formula f =
  let rec aux f negated =
    match f with
      Variable c -> Literal (c, (not negated))
    | Negation f -> aux f (not negated)
    | Conjunction(f1, f2) ->
      if negated
      then Disjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
      else Conjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
    | Disjunction(f1, f2) ->
      if negated
      then Conjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
      else Disjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
  in
  aux f false;;



(* ~(q and ~(r or s)) <=> ~q or (r or s)  *)
let f3 = Negation(
    Conjunction(
      Variable 'q',
      Negation(
        Disjunction(
          Variable 'r',
          Variable 's'
        )
      )
    )
  );;

nnf_of_formula f3;;

let rec cnf_of_nnf f =
  match f with
    Literal _ -> f
  | Disjunction(f1, Conjunction(f2, f3)) -> Conjunction(Disjunction(cnf_of_nnf f1, cnf_of_nnf f2), Disjunction(cnf_of_nnf f1, cnf_of_nnf f3))
  | Disjunction(Conjunction(f2, f3), f1) -> Conjunction(Disjunction(cnf_of_nnf f1, cnf_of_nnf f2), Disjunction(cnf_of_nnf f1, cnf_of_nnf f3))
  | Conjunction(f1, f2) -> Conjunction(cnf_of_nnf f1, cnf_of_nnf f2)
  | Disjunction(f1, f2) -> Disjunction(cnf_of_nnf f1, cnf_of_nnf f2);;

let nnf_f1 = Disjunction(
    Literal('p', true),
    Conjunction(
      Literal('q', true),
      Literal('r', true)
    )
  )
