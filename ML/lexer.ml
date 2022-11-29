(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Sriram Krishnan                     *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Phrase Lexer - Used by Parser, and Rank for Reader. 
   Uses Phases from Dispatcher to define phase.
   Loads the transducers, calls Dispatch to create module Disp. 
   Calls Segment to build Viccheda, the Sanskrit lexer that undoes sandhi 
   in order to produce a padapaa.tha.
   Exports various print functions for the various modes. *)

open Transduction;
open Canon;
open Skt_morph; (* verbal *) 
open Auto.Auto; (* auto State *)
open Segmenter; (* Segment *)
open Dispatcher; (* Dispatch *) 
open Word; (* word length mirror patch *)

module Lexer (* takes its prelude and control arguments as module parameters *)
  (Prel: sig value prelude : unit -> unit; end) 
  (Lexer_control: sig 
    value star : ref bool; (* chunk = if star then word+ else word *)
    value transducers_ref : ref Load_transducers.transducer_vect;
    end) = struct 

open Html;
open Web; (* ps pl abort etc. *)
open Cgi;
open Phases; (* Phases *) 
open Phases; (* phase generative *) 

module Lemmas = Load_morphs.Morphs Prel Phases
;
open Lemmas; (* [morpho tag_sort tags_of] *)
open Load_transducers; (* [transducer_vect Trans] *)

module Transducers = Trans Prel;

module Machine = Dispatch Transducers Lemmas Lexer_control;
open Machine; (* [color_of_phase transition trim_tags] *) 

module Viccheda = Segment Phases Machine Lexer_control;
     (* [all_checks init_segment continue set_offset set_sa_contro resumption] *)

value all_checks = Viccheda.all_checks
and   set_offset = Viccheda.set_offset
and   set_sa_control = Viccheda.set_sa_control
;
value un_analyzable (chunk : word) = 
  ([ (Unknown,mirror chunk,Machine.Id) ],([]:Viccheda.resumption))
;

(* Printing *)

value table_morph_of phase = table_begin (background (color_of_phase phase)) 
;
value print_morph pvs cached seg_num gen form n tag = do
(* n is the index in the list of tags of an ambiguous form *)
  { tr_begin |> ps
  ; th_begin |> ps
  ; span_begin Latin12 |> ps
  ; Morpho_html.print_inflected_link pvs cached form (seg_num,n) gen tag 
  ; span_end |> ps
  ; th_end |> ps
  ; tr_end |> ps  
  ; n+1
  }
;
value print_tags pvs seg_num phase form tags =   
  let ptag = print_morph pvs (is_cache phase) seg_num (generative phase) form in 
  let _ = List.fold_left ptag 1 tags in ()
;
value rec scl_phase = fun
  [ Pv | Pvc | Pvv | Pvkc | Pvkv -> "pv"
  | Noun | Nouc | Nouv | Krid | Kriv | Kric | Lopak | Pron | Auxik 
         | Cache -> "noun"
  | Root | Lopa | Auxi -> "root"
  | Inde | Abso | Absv | Absc | Avy | Auxiinv -> "inde"
  | Iic | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif | Auxiick
        | Ai | Ani | Cachei -> "iic"
  | Iiv | Iivv | Iivc -> "iiv" 
  | Iiy -> "iiy" 
  | Peri -> "peri" 
  | Inftu -> "inftu" 
  | Kama -> "kama" 
  | Voca | Vocv | Vocc | Inv | Vok | Vokv | Vokc | Vocf -> "voca"
  | Ifc | Ifcv | Ifcc | Indifc -> "ifc"
  | Unknown -> "unknown"
  | Comp (_,ph) _ _ -> "preverbed " ^ scl_phase ph
  ]
;
value print_scl_morph pvs gen form tag = do
  { xml_begin "tag" |> ps
  ; Morpho_scl.print_scl_inflected pvs form gen tag  
  ; xml_end "tag" |> ps
  }
;
value print_scl_tags pvs phase form tags = 
  let table phase = 
      xml_begin_with_att "tags" [ ("phase",scl_phase phase) ] in do
  { table phase |> ps
  ; List.iter (print_scl_morph pvs (generative phase) form) tags 
  ; xml_end "tags" |> ps
  }
;
(* These definitions are for export to Parser.
   They betray a difficuly in the modular organisation, since Parser sees
   Lexer, but not [Load_morphs] or Dispatcher. Modules ought to be revised. *)
value tags_of = Lemmas.tags_of 
and trim_tags = Machine.trim_tags
;
(* Keeps only relevant tags with [trim_tags] *)(*i Cochonnerie i*)
value extract_lemma phase word = 
  match tags_of phase word with  
  [ Atomic tags -> tags 
  | Preverbed (_,phase) pvs form tags -> (* tags to be trimmed to [ok_tags] *)
     if pvs = [] then tags 
     else trim_tags (generative phase) form (Canon.decode pvs) tags 
  ]
; 
(* Returns the offset correction (used by SL interface) *)
value process_transition = fun  
  [ Euphony (w,u,v) ->   
    let off = if w=[] then 1 (* amui/lopa from Lopa/Lopak *)
                      else length w in
    off - (length u + length v) 
  | Id -> 0
  ]
;
value print_transition = fun
  [ Euphony (w,u,v) -> Morpho_html.print_sandhi u v w 
  | Id -> ()
  ]
;
value process_kridanta pvs seg_num phase form tags = do
  { th_begin |> ps
  ; table_morph_of phase |> pl          (* table begin *)
  ; let ok_tags = 
        if pvs = [] then tags 
        else trim_tags (generative phase) form (Canon.decode pvs) tags in do
        (* NB Existence of the segment warrants that [ok_tags] is not empty *)
  { print_tags pvs seg_num phase form ok_tags 
  ; table_end |> ps                     (* table end *) 
  ; th_end |> ps
  ; (phase, form, ok_tags) (* value used by [Parser.print_segment_roles] *)
  }}
; 
(* Same recursive structure as [Interface.print_morpho] *)
value print_morpho phase word = do  
  { table_morph_of phase |> pl          (* table begin *)  
  ; tr_begin |> ps
  ; th_begin |> ps
  ; span_begin Latin12 |> ps 
  ; let _ = 
       match tags_of phase word with 
       [ Atomic tags ->  
          process_kridanta [] 0 phase word tags
       | Preverbed (_,phase) pvs form tags -> 
          process_kridanta pvs 0 phase form tags
       ] in () 
  ; span_end |> ps
  ; th_end |> ps 
  ; tr_end |> ps
  ; table_end |> ps                      (* table end *)
  }
;
(* Segment printing with phonetics without semantics for Reader *)
value print_segment offset (phase,rword,transition) = do 
  { "[ " |> ps
  ; Morpho_html.print_signifiant_off rword offset
  ; print_morpho phase (mirror rword)
  (* Now we print the sandhi transition *)
  ; "&lang;" |> ps (* < *) 
  ; let correction = process_transition transition in do  
      { print_transition transition
      ; "&rang;]" |> pl (* >] *)
      ; html_break |> pl
      ; offset+correction+length rword
      }
  }
; 
(* Printing the segment with only the sentence 
   without the phase and transition details *)
value print_segment_words offset (phase,rword,transition) = do
  { Morpho_html.print_signifiant_off rword offset 
  ; if (ii_component phase) then ("-" |> ps)
    else (" " |> ps)
  ; let correction = process_transition transition in 
    offset+correction+(length rword)
  }
;
(* Similarly for [scl_plugin] mode (without offset and transitions) *)
(* Called from [Scl_parser.print_scl_output] *)
value print_scl_segment counter (phase,rword) =  
  let word = Morpho_html.visargify rword in do
  { let solid = background (Phases.color_of_phase phase) in
    td_begin_class solid |> pl
  ; let ic = string_of_int counter in
    "<input type=\"hidden\" name=\"field" ^ ic ^ "\" value='<form wx=\""
        ^ Canon.decode_WX word ^ "\"/>" |> ps
  ; match tags_of phase (mirror rword) with 
    [ Atomic tags ->
          print_scl_tags [] phase word tags
    | Preverbed (_,phase) pvs form tags -> 
         let ok_tags = 
           if pvs = [] then tags 
           else trim_tags (generative phase) form (Canon.decode pvs) tags in
         print_scl_tags pvs phase form ok_tags
    ] 
  ; "'>" |> ps (* closes <input *) 
  ; Canon.unidevcode word |> ps
  ; td_end |> ps
  ; "\n" |> ps
  ; counter+1
  } 
; 
(* Getting the form for the best segments *)
(* Called from [Scl_parser.post_best_segments_scl] *)
value best_segments_for_scl counter (phase,rword) =
  let word = Morpho_html.visargify rword in do 
  { "<form wx=\"" ^ Canon.decode_WX word ^ "\"/>" |> ps
  ; match tags_of phase (mirror rword) with 
    [ Atomic tags ->
          print_scl_tags [] phase word tags
    | Preverbed (_,phase) pvs form tags -> 
         let ok_tags = 
           if pvs = [] then tags 
           else trim_tags (generative phase) form (Canon.decode pvs) tags in
         print_scl_tags pvs phase form ok_tags
    ] 
  ; counter+1
  }
;
end;

