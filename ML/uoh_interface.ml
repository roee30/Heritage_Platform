(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Amba Kulkarni                       *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Interface with UoH dependency parser *)

open Html; 
open Web; (* ps pl etc. [scl_url] *)
open Morphology; (* inflected lemma morphology *)
open Phases; (* Phases *)
open Dispatcher; (* Dispatch *)
open SCLpaths; (* [scl_url scl_cgi] *) 

value svg_interface_url = scl_cgi ^ "SHMT/" 
and nn_parser_url = scl_cgi ^ "NN/parser/generate.cgi"
and show_parses_path = "prog/interface/call_parser_summary.cgi"
;

module Prel = struct (* similar to Interface's lexer prelude *)

 value prelude () = do
  { pl http_header
  ; page_begin graph_meta_title 
  ; pl (body_begin Chamois_back)
  ; open_page_with_margin 15
  }
;
 end (* Prel *)
;
(* Service routines for morphological query, loading the morphology banks *)
module Lemmas = Load_morphs.Morphs Prel Phases 
;
open Lemmas (* [tags_of morpho] *)
;
open Phases; (* phase etc. *)

module UOH 
  (Lex: sig 
  module Disp :
      sig
      type transition;
      value color_of_phase: phase -> color; 
      type segment = (phase * Word.word * transition);
      end; 
  value print_ext_segment: int -> Disp.segment -> int; 
  end) = struct

(****************************************************************)
(* Interface with Amba Kulkarni's parser at UoH - Analysis mode *)
(****************************************************************)

(* UNUSED Delimitor for offline printing and piping into UoH's parser 
value delimitor = fun
  [ Iic | Iic2 | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif | Iiy -> "-"
  | Iiv | Iivv | Iivc -> "++"
  | Pv | Pvk | Pvkc | Pvkv -> failwith "No more Pv segments" 
  | _ -> " "
  ]
; *)

value print_ext_output (_,output) = 
  List.fold_left Lex.print_ext_segment 1 (List.rev output) 
;
value print_ext_solutions s =
   let _ = print_ext_output s in ()
;
(* Invocation of UoH's CSL parser for dependency graph display *)
value print_ext1 (solutions : (int * list Lex.Disp.segment)) = do
  { ps ("<script type=\"text/javascript\" src=\"" ^ scl_url ^ "js_files/dragtable.js\"></script>")
  ; ps ("<form name=\"word-order\" method=\"POST\" action = \""
       ^ svg_interface_url ^ "prog/Word_order/call_heritage2anu.cgi\">\n")
  ; ps ("<table class=\"draggable\">")
  ; ps tr_begin
  ; print_ext_solutions solutions
  ; ps tr_end
  ; ps table_end 
  ; ps (submit_input "Submit")
  } 
;
(* We restrict to the first solution - TEMPORARY *)
value print_ext sols = match sols with 
  [ [] -> failwith "No sol"
  | [ s :: _ ] -> print_ext1 s
  ]
;
end;
