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
open SCLpaths; (* [svg_interface_url] *) 

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
     } in do
  { ps tr_begin
  ; ps td_begin 
  ; List.iter print_segment_cbk solution
  ; ps td_end
  ; ps tr_end
  ; ps (html_latin12 "Verse Order")
  ; ps table_end
  ; ps ("<form name=\"word-order\" method=\"get\" action = \""
       ^ svg_interface_url ^ "prog/Word_order/call_heritage2anu.cgi\">\n")
  ; ps "<table>"
  ; ps tr_begin
  ; ps td_begin 
  ; ps (html_latin12 "Prose Order")
  ; ps (xml_begin_with_att "textarea"
      [ ("name","word-order"); ("rows","1"); ("cols","50") ] ^
       xml_end "textarea")
  ; ps (submit_input "Submit")
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
value print_ext_solutions cho = List.iter (print_ext_output cho) 
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
  ; let _ = print_callback solutions in () (* print dependency graphs *) 
  (*[; ps table_end ] (?) *)
  }
;

end;
