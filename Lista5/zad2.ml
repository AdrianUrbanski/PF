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
    NLiteral of char*bool
  | NConjunction of nnf*nnf
  | NDisjunction of nnf*nnf;;

let nnf_of_formula f =
  let rec aux f negated =
    match f with
      Variable c -> NLiteral(c, (not negated))
    | Negation f -> aux f (not negated)
    | Conjunction(f1, f2) ->
      if negated
      then NDisjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
      else NConjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
    | Disjunction(f1, f2) ->
      if negated
      then NConjunction(
          (aux f1 negated),
          (aux f2 negated)
        )
      else NDisjunction(
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

type literal =
  Literal of char*bool

type cnf =
    Clause of literal list
  | CConjunction of cnf list;;

let mergeCnf c1 c2 =
  match c1, c2 with
    Clause(l1), Clause(l2) -> Clause(l1@l2)
  | Clause _, CConjunction l -> CConjunction (c1::l)
  | CConjunction l, Clause _ -> CConjunction (c2::l)
  | CConjunction l1, CConjunction l2 -> CConjunction(l1@l2);;

let rec cnf_of_nnf f =
  let rec make_cnf f =
    match f with
      NLiteral _ -> f
    | NDisjunction(f1, NConjunction(f2, f3)) -> NConjunction(NDisjunction(make_cnf f1, make_cnf f2), NDisjunction(make_cnf f1, make_cnf f3))
    | NDisjunction(NConjunction(f2, f3), f1) -> NConjunction(NDisjunction(make_cnf f1, make_cnf f2), NDisjunction(make_cnf f1, make_cnf f3))
    | NConjunction(f1, f2) -> NConjunction(make_cnf f1, make_cnf f2)
    | NDisjunction(f1, f2) -> NDisjunction(make_cnf f1, make_cnf f2)
  in
  let rec convert f =
    match f with
      NLiteral(l,b) -> Clause [Literal(l,b)]
    | NDisjunction(f1, f2)
    | NConjunction(f1, f2) -> mergeCnf (convert f1) (convert f2)
  in
  convert (make_cnf f);;

let nnf_f1 = NDisjunction(
    NLiteral('p', true),
    NConjunction(
      NLiteral('q', true),
      NLiteral('r', true)
    )
  )
