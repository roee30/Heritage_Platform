(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Amba Kulkarni                       *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Module [Scl_parser] used as interface with UoH dependency parser *)

open Html; 
open Web; (* ps pl etc. *)
open Morphology; (* inflected lemma morphology *)
open Phases; (* Phases *)
open Dispatcher; (* Dispatch *)
open SCLpaths; (* [scl_url scl_cgi default_output_font] *) 


module Prel = struct  
 value prelude () = Web.reader_prelude Web.reader_title;
 end (* Prel *)
;
module Lexer_control = struct
 value star = ref True;
 value transducers_ref = ref Load_transducers.dummy_transducer_vect;
end (* [Lexer_control] *)
;
(* Multi-phase lexer *)
module Lex = Lexer.Lexer Prel Lexer_control (* [print_scl_segment] *)
;
value print_scl_output output = 
  List.fold_left Lex.print_scl_segment 1 (List.rev output) 
;
value print_scl_solutions s =
  let _ = print_scl_output s in ()
;
(* Invocation of UoH's CSL parser for dependency graph display *)
value print_scl1 scl_font (solutions : list (Phases.phase * Word.word)) =
  let svg_interface_url = scl_cgi ^ "SHMT/" in do
  { ps ("<script type=\"text/javascript\" src=\"" ^ scl_url ^ "js_files/dragtable.js\"></script>")
  ; ps ("<form name=\"word-order\" method=\"POST\" action = \""
       ^ svg_interface_url ^ "prog/Word_order/call_heritage2anu.cgi\">\n")
  ; ps ("<table class=\"draggable\">")
  ; ps tr_begin
  ; print_scl_solutions solutions
  ; ps ("<td><input type=\"hidden\" name=\"DISPLAY\" value=\"" ^ scl_font ^"\"/></td>")
  ; ps tr_end
  ; ps table_end 
  ; ps (submit_input "Submit")
  } 
;
(* We restrict to the first solution - TEMPORARY *)
value print_scl scl_font sols = match sols with 
  [ [] -> failwith "No sol"
  | [ s :: _ ] -> print_scl1 scl_font s
  ]
;
(* end; *)
