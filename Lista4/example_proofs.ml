open Logic;;
open Proof;;

(**
#install_printer Logic.pp_print_formula ;;
#install_printer Logic.pp_print_theorem ;;
**)

let f1 = Implication(Variable 'p', Implication(Implication(Variable 'p', Variable 'q'), Variable 'q'))

let p1 = proof [] f1
        |> intro "H1"
        |> intro "H2"
        |> apply_assm "H2"
        |> apply_assm "H1"
        |> qed

let f2 = Implication(Implication(Variable 'p', Implication(Variable 'q', Variable 'r')),
                     Implication(Implication(Variable 'p', Variable 'q'), Implication(Variable 'p', Variable 'r')))

let p2 = proof [] f2
       |> intro "H1"
       |> intro "H2"
       |> intro "H3"
       |> apply_assm "H1"
       |> apply_assm "H3"
       |> apply_assm "H2"
       |> apply_assm "H3"
       |> qed

let f3 = Implication(Implication(Implication(Implication(Variable 'p', Contradiction), Variable 'p'), Variable 'p'),
                    Implication(Implication(Implication(Variable 'p', Contradiction), Contradiction), Variable 'p'))

let p3 = proof [] f3
         |> intro "H1"
         |> intro "H2"
         |> apply_assm "H1"
         |> intro "H3"
         |> apply_assm "H2"
         |> intro "H4"
         |> apply_assm "H3"
         |> apply_assm "H4"
         |> qed

let f4 = Implication(
    Implication(Implication(Implication(Variable 'p', Contradiction), Contradiction), Variable 'p'),
    Implication(Implication(Implication(Variable 'p', Contradiction), Variable 'p'), Variable 'p'))

let p4 = proof [] f4
         |> intro "H1"
         |> intro "H2"
         |> apply_assm "H1"
         |> intro "H3"
         |> apply_assm "H3"
         |> apply_assm "H2"
         |> intro "H4"
         |> apply_assm "H3"
         |> apply_assm "H4"
         |> qed
