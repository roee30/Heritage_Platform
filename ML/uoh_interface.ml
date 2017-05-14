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
open Phases.Phases; (* phase etc. *)
open Dispatcher; 

module UOH 
  (Lex: sig 
  module Disp : 
      sig
      type transition;
      value color_of_phase: phase -> color; 
      type segment = (phase * Word.word * transition);
      end; 
  value print_ext_segment: (string -> unit) -> Disp.segment -> unit; 
  end) = struct

(****************************************************************)
(* Interface with Amba Kulkarni's parser at UoH - Analysis mode *)
(****************************************************************)

(* Paths - to move to configuration time *)
value svg_interface_url = "http://localhost/cgi-bin/SCL/SHMT/"
and nn_parser_url = "http://localhost/cgi-bin/SCL/NN/parser/generate.cgi"
and show_parses_path = "prog/interface/call_parser_summary.cgi"
;

(* Delimitor for offline printing and piping into UoH's parser *)
value delimitor = fun
  [ Iic | Iic2 | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif | Iiy -> "-"
  | Iiv | Iivv | Iivc -> "++"
  | Pv | Pvk | Pvkc | Pvkv -> failwith "No more Pv segments" 
  | _ -> " "
  ]
; 
value print_ext_output cho (n,output) = 
  let ps = output_string cho in 
  let print_segment = Lex.print_ext_segment ps in do
  { ps (xml_begin_with_att "solution" [ ("num", string_of_int n) ])
  ; ps "\n"
  ; List.iter print_segment (List.rev output) 
  ; ps (xml_end "solution") 
  ; ps "\n"
  }
;

(* Temporary - should be set by SCL configure, as well as [Web.scl_url] *)
value scl_install_dir = "" 
and offline_dir = "/private/tmp/SKT_TEMP/" (* vamana *) 
and default_output_font = "ROMAN" (* could be "DEV" *)
;
value scl_dir = scl_install_dir ^ "SHMT/prog/"
and offline name = offline_dir ^ name (* problematic file output *)
;
value offline_file = offline "1.txt" (* owner [_www] Apache=httpd *)
and tmp_in = offline "tmp_in"
;

(* Prints a solution with its index, returns the bumped index ignoring sandhi *)
(* Prints on [std_out], as usual for a cgi. *)
value print_callback_solution counter solution =
  let print_pada rword = 
     let word = Morpho_html.visargify rword in
     ps (Canon.unidevcode word) in 
  let print_segment_cbk (phase,rword,_) = do (* follows [Lex.print_segment] *)
     { ps td_begin (* begin segment *)
     ; let solid = background (Lex.Disp.color_of_phase phase) in
       pl (table_begin solid)
     ; ps tr_begin 
     ; ps td_begin  
     ; print_pada rword
     ; ps td_end
     ; ps tr_end 
     ; ps table_end
     ; ps td_end (* end segment *)
     ; ps (delimitor phase) 
     } 
  and pid = string_of_int (Unix.getpid ()) (* process-id stamp *)
  and segmentations = string_of_int counter in do
  { ps tr_begin
  ; ps td_begin 
  (* TODO rewrite as [ps (let url= ... and link = ... in anchor_ref url link)] *)
  ; ps ("<a href=\"" ^ svg_interface_url ^ show_parses_path ^ 
        "?filename=./tmp_in") (* call-back to svg interface UoH *) 
  ; ps pid
  ; ps "&amp;outscript="
  ; ps default_output_font
  ; ps "&amp;rel=''"
  ; ps "&amp;sentnum="
  ; ps segmentations
  ; ps "&amp;save=no\""
  ; ps "&amp;translate=no\""
  ; ps (" onmouseover=\"Tip('<img src=" ^ scl_url ^ "DEMO/tmp_in")
  ; ps pid
  ; ps "/"
  ; ps segmentations
  ; ps ".1.svg height=100%; width=100%;>')\" onmouseout=\"UnTip()\">" 
  ; ps (html_latin12 "Solve dependencies ")
  ; ps (xml_end "a")
  ; List.iter print_segment_cbk solution
  ; ps td_end
  ; ps tr_end 
  ; counter+1
  }
;
value print_callback_output counter (_,output) = 
  let solution = List.rev output in
  print_callback_solution counter solution
;
(* Print solutions as call backs to the SCL dependency graph display cgi *) 
value print_callback = 
  List.fold_left print_callback_output 1  
;
value print_ext_solutions cho = 
  List.iter (print_ext_output cho) 
;
(* External call-back to Amba Kulkarni's parser (from [Reader.print_ext] *)
value amba_invoke pid = (* Experimental - assumes amrita configuration *)
  "mkdir -p " ^ tmp_in ^ pid ^ "; " ^ 
  scl_dir ^ "Heritage_morph_interface/Heritage2anusaaraka_morph.sh <" ^ 
  offline_file ^ " > " ^ tmp_in ^ pid ^ "/in" ^ pid ^ ".out; " ^
  scl_dir ^ "kAraka/shabdabodha.sh YES " ^ tmp_in ^ pid ^ " in" ^ 
  pid ^ ".out" ^ " in" ^ pid ^ ".kAraka " ^ default_output_font ^  
  " Full Prose NOECHO ND 2> " ^ offline ("err" ^ pid) ^ ";"
;

(* Prints all segmentations in [offline_file]  
   and prepares invocation of UoH's CSL parser for dependency graph display *)
value print_ext solutions =
  let cmd = "mkdir -p " ^ offline_dir in
  let _ = Sys.command cmd in (* call it *)
  let cho = open_out offline_file in do
  { print_ext_solutions cho solutions
  ; close_out cho 
    (* System call to Amba Kulkarni's parser - fragile *)
  ; ps table_end 
  ; ps (xml_begin "table") 
  ; let pid = string_of_int (Unix.getpid ()) in (* stamp with process id *)
    let cmd = amba_invoke pid in (* prepare cryptic UNIX command *) 
    let _ = Sys.command cmd in () (* call it *) 
  ; let _ = print_callback solutions in () (* print dependency graphs *) 
  ; ps table_end  
  }
;
(* Now for processing of navya-nyaaya compounds in Experimental mode *)
value print_nnparser_solution counter solution = 
let rec nnsegment acc solution = 
  match solution with
    [ [] -> acc
    | [ (phase,rword,_)] -> let word = Morpho_html.visargify rword in
                            acc ^ (Canon.unidevcode word)
    | [ (phase,rword,_) :: t ] -> 
                            let word = Morpho_html.visargify rword in
                            nnsegment (acc ^ (Canon.unidevcode word) ^ "-") t
                      ] in
  let nnstring = nnsegment "" solution in do
  { ps tr_begin
  ; ps td_begin 
  ; ps ("<a href=\"" ^ nn_parser_url ^ "?text=")
  ; ps nnstring
  ; ps ("&encoding=Unicode\">")
  ; ps (html_latin12 "NN Constituency Parser ")
  ; ps (xml_end "a")
  ; ps nnstring
  ; ps td_end
  ; ps tr_end 
  ; counter+1
  }
;
value print_nnparser_output counter (_,output) = 
  let solution = List.rev output in
  print_nnparser_solution counter solution
;
(* New: processing of NN compounds - called from Reader *)
 value print_nnparser solutions = let _ = 
  List.fold_left print_nnparser_output 1 solutions in ()
;
value print_nn solutions = do 
  { ps (xml_begin "table")
  ; print_nnparser solutions
  ; ps table_end
  }
;
end;
