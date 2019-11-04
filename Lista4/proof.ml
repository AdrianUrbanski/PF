open Logic

type proof (* = TODO: tu wpisz swoją definicję *)

let proof g f =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let qed pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let goal pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let next pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let intro name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply f pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

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
