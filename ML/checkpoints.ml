(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Checkpoints management *)
open Phases.Phases; (* [string_of_phase phase_of_string] *)

(* string encoding of a phase, used to transmit checkpoints in URLs *)
value rec phase_encode = fun
  [ Comp (ph,ph') prev form -> 
        "<{" ^ string_of_phase ph ^ "}{" ^
               string_of_phase ph' ^ "}{" ^
               Canon.decode prev ^ "}{" ^ Canon.decode form ^ "}>"
  | phase -> "{" ^ string_of_phase phase ^ "}" 
  ]
and bool_encode b = if b then "t" else "f"
;
value string_point (k,(phase,rword),select) =
  let segment = Canon.rdecode rword in
  string_of_int k ^ "," ^ phase_encode phase ^ ",{" ^ segment ^ 
                  "},{" ^ bool_encode select ^ "}"
;
value rec string_points = fun
  [ [] -> ""
  | [ last ] -> string_point last
  | [ first :: rest ] -> string_point first ^ "|" ^ string_points rest
  ]
;

open Bank_lexer;  
module Gram = Camlp4.PreCast.MakeGram Bank_lexer 
;
open Bank_lexer.Token
;
value cpts      = Gram.Entry.mk "cpts"
and lcpt        = Gram.Entry.mk "lcpt"
and phase_rword = Gram.Entry.mk "phase_rword"
and cpt         = Gram.Entry.mk "cpt"
and phase       = Gram.Entry.mk "phase"
and guess_morph = Gram.Entry.mk "guess_morph" (* for interface *)
;
EXTEND Gram 
  cpts:
    [ [ l = lcpt; `EOI -> l
      | lcpt -> failwith "Wrong checkpoints parsing\n"
    ] ] ;
  lcpt:
    [ [ l = LIST0 cpt SEP "|" -> l ] ] ;
  phase:
    [ [ "<"; p = TEXT; p' = TEXT  (* Preverbed *)
           ; pre = TEXT; form = TEXT ; ">" ->
       Comp (phase_of_string p, phase_of_string p') 
                   (Encode.code_string pre) (Encode.code_string form)
      | p = TEXT -> phase_of_string p 
    ] ] ;
  phase_rword:
    [ [ s = phase; ","; o = TEXT -> (s, Encode.rev_code_string o) ] ] ;
  cpt:
    [ [ m = INT; ","; p = phase_rword; ","; s = TEXT -> 
       (int_of_string m, p, s ="t") ] ] ;
  guess_morph:
    [ [ n = TEXT; ","; o = TEXT; `EOI -> (n,o) ] ] ;
END
;
value parse_cpts s =
  try Gram.parse_string cpts Loc.ghost s with
  [ _ -> raise (Control.Anomaly "parse_cpts") ] 
;
value parse_guess s =
  try Gram.parse_string guess_morph Loc.ghost s with
  [ _ -> raise (Control.Anomaly "parse_guess") ]
;

(* Parsing projections stream (Parser, Regression) *)
value projs = Gram.Entry.mk "projs"
and lproj   = Gram.Entry.mk "lproj" 
and proj    = Gram.Entry.mk "proj"
;
(* A stream of projections is encoded under the form [1,2|2,3|...] *)
EXTEND Gram
  projs:
    [ [ l = lproj; `EOI -> l
      | lproj -> failwith "Wrong projections parsing\n"
    ] ] ;
  lproj:
    [ [ l = LIST0 proj SEP "|" -> l ] ] ;
  proj:
    [ [ n = INT; ","; m = INT -> (int_of_string n,int_of_string m) ] ] ;
END
;
value parse_proj s =
  try Gram.parse_string projs Loc.ghost s with
  [ _ -> raise (Control.Anomaly "parse_proj") ]
;

