type formula =
  | Variable of char
  | Contradiction
  | Implication of formula*formula;;

let rec string_of_formula f =
  match f with
    Variable c -> Char.escaped c
  | Contradiction -> "⊥"
  | Implication (f1, f2) ->
    match f1 with
      Implication (_, _) -> "(" ^ string_of_formula f1 ^ ") ⇒ " ^ string_of_formula f2
    | _ -> string_of_formula f1 ^ " ⇒ " ^ string_of_formula f2;;

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem =
  | Consequence of formula
  | Assumption of formula * theorem;;

let rec assumptions thm =
  match thm with
    Consequence _ -> []
  | Assumption (a, t) -> a::(assumptions t);;


let rec consequence thm =
  match thm with
    Consequence c -> c
  | Assumption (_, t) -> consequence t;;

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
  Assumption (f, Consequence f);;

let rec imp_i f thm =
  match thm with
    Consequence c -> Consequence (Implication (f, c))
  | Assumption (a, t) -> if a = f
    then imp_i f t
    else Assumption(a, imp_i f t);;

let rec imp_e th1 th2 =
  match th1, th2 with
    Consequence c1, Consequence c2 ->
    (match c1, c2 with
       Implication(f1, f2), f -> if f1 = f
       then Consequence f2
       else failwith "invalid step"
     | _, _ -> failwith "invalid step")
  | Assumption (a, t), _ -> Assumption (a, imp_e t th2)
  | _, Assumption (a, t) -> Assumption (a, imp_e th1 t)

let rec bot_e f thm =
  match thm with
    Assumption (a, t) -> Assumption(a, bot_e f t)
  | Consequence c -> (match c with
        Contradiction -> Consequence f
      | _ -> failwith "invalid step")

let f1 = Implication(Implication(Variable 'p', Implication(Variable 'q', Variable 'r')),
Implication(Implication(Variable 'p', Variable 'q'), Implication(Variable 'p', Variable 'r')))

let p1 = imp_i (Variable 'p') (by_assumption (Variable 'p'))

let p2 = imp_i (Variable 'p') (imp_i (Variable 'q') (by_assumption (Variable 'p')) )

let p3 =
  imp_i
    (Implication (Variable 'p', Implication (Variable 'q', Variable 'r')))
    (imp_i
       (Implication(Variable 'p', Variable 'q'))
       (imp_i
          (Variable 'p')
          (imp_e
             (imp_e
                (by_assumption
                   (Implication(Variable 'p', Implication(Variable 'q', Variable 'r'))))
                (by_assumption
                   (Variable 'p'))
             )
             (imp_e
                (by_assumption
                   (Implication(Variable 'p', Variable 'q')))
                (by_assumption
                   (Variable 'p')))
          )
       )
    )

let p4 =
  imp_i
    (Contradiction)
    (bot_e
       (Variable 'p')
       (by_assumption (Contradiction)))
