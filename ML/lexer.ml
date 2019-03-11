(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Phrase Lexer in 40 phases version. *)
(* Used by Parser, and Rank for Reader/Regression. 
   Uses Phases from Dispatcher to define phase.
   Loads the transducers, calls Dispatch to create Disp. 
   Calls Segment to build Viccheda, the Sanskrit lexer that undoes sandhi 
   in order to produce a padapatha.
   Exports various print functions for the various modes. *)

open Transduction;
open Canon;
open Skt_morph;
open Morphology; (* [inflected inflected_map] *)
open Auto.Auto; (* auto State *)
open Segmenter; (* Segment *)
open Dispatcher; (* [Dispatch transition phase_of_sort trim_tags] *) 
open Word; (* word length mirror patch *)

module Lexer (* takes its prelude and control arguments as module parameters *)
  (Prel: sig value prelude : unit -> unit; end) 
  (Control: sig value star : ref bool; (* chunk = if star then word+ else word *)
                value full : ref bool; (* all kridantas and nan cpds if full *)
                value out_chan : ref out_channel; (* output channel *)
            end) = struct 

open Html;
open Web; (* ps pl abort etc. *)
open Cgi;
open Phases; (* Phases *) 
open Phases; (* phase generative *) 

module Lemmas = Load_morphs.Morphs Prel Phases
;
open Lemmas (* [morpho tag_sort tags_of] *)
;
open Load_transducers; (* [transducer_vect Trans] *)

module Transducers = Trans Prel;

module Disp = Dispatch Transducers Lemmas;
open Disp (* [transducer initial accepting dispatch input color_of_phase 
              transition trim_tags] *) 
;
module Viccheda = Segment Phases Disp Control 
                  (* [init_segment continue set_offset] *)
;
value all_checks = Viccheda.all_checks
and   set_offset = Viccheda.set_offset
;
value un_analyzable (chunk : word) = 
  ([ (Unknown,mirror chunk,Disp.Id) ],Viccheda.finished)
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
  [ Pv | Pvk | Pvkc | Pvkv -> "pv"
  | Noun | Noun2 | Nouc | Nouv | Krid | Kriv | Kric | Lopak | Pron | Auxik 
    -> "noun"
  | Root | Lopa | Auxi -> "root"
  | Inde | Abso | Absv | Absc | Avy -> "inde"
  | Iic | Iic2 | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif | Auxiick
  | Ai | Ani -> "iic"
  | Iiv | Iivv | Iivc -> "iiv" 
  | Iiy -> "iiy" 
  | Peri -> "peri" 
  | Inftu -> "inftu" 
  | Kama -> "kama" 
  | Voca | Vocv | Vocc | Inv | Vok | Vokv | Vokc -> "voca"
  | Ifc | Ifc2 -> "ifc"
  | Unknown -> "unknown"
  | Cache -> "Cache" 
  | Comp (_,ph) _ _ -> "preverbed " ^ scl_phase ph
  ]
;
value print_scl_morph pvs gen form tag = do
  { ps (xml_begin "tag")
  ; Morpho_scl.print_scl_inflected pvs form gen tag  
  ; ps (xml_end "tag") 
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
value tags_of = Lemmas.tags_of (* For export to Parser *)
and trim_tags = Disp.trim_tags
;
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
        (* NB Existence of the segment guarantees that [ok_tags] is not empty *)
  { print_tags pvs seg_num phase form ok_tags 
  ; table_end |> ps                     (* table end *) 
  ; th_end |> ps
  ; (phase, form, ok_tags)
  }}
; 
(* Same structure as [Interface.print_morpho] *)
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
(* Similarly for [scl_plugin] mode (without offset and transitions) *)
(* Called from [Scl_parser.print_scl_output] *)
value print_scl_segment counter (phase,rword) =  
  let word = Morpho_html.visargify rword in do
  { let solid = background (Disp.color_of_phase phase) in
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

module Report_chan = struct 
value chan = Control.out_chan; (* where to report *)
end;

module Morpho_out = Morpho.Morpho_out Report_chan;

(* Structured entries with generative morphology *)
type gen_morph =
   [ Gen_krid of ((string * word) * (verbal * word))
   | Lexical of word
   | Preverbs_list of list word
   ]
;
value rec morph_list = fun
  [ [ a :: rest ] -> Morpho_string.string_morph a ^ " " ^ morph_list rest
  | [] -> ""
  ]
;
value rec decode_list = fun
  [ [ a :: rest ] -> Canon.decode_ref a ^ " " ^ decode_list rest
  | [] -> ""
  ]
;
value string_of_tag = fun
  [ (x,y,a,b) -> if y = Pv then "${" ^ Canon.decode_ref x ^ "}$&"
	         else "${" ^ Canon.decode_ref x ^ ":" ^ string_of_phase y
                      ^ "{ " ^ morph_list b  ^ "}" ^ "[" 
                      ^ match a with 
     [ Gen_krid ((z, c),(d, e)) -> 
        z ^ ":" ^ Canon.decode_ref c ^ " { " ^ Morpho_string.string_verbal d
        ^ " }[" ^ Canon.decode_ref e ^ "]"
     | Lexical c -> Canon.decode_ref c
     | Preverbs_list c -> decode_list c
     ]  ^ "]}$&"
  ]
;
value rec return_morph = fun
  [ [ a :: rest ] -> string_of_tag a ^ return_morph rest
  | [] -> ""
  ]
;
value generative_stem gen stem = 
   if gen then (* interpret stem as unique name *)
        let (homo,bare_stem) = Naming.homo_undo stem in
        let krid_infos = Deco.assoc bare_stem Naming.unique_kridantas in 
        let (vb,root) = Naming.look_up_homo homo krid_infos in 
        let look_up_stem =
            match Deco.assoc stem Naming.lexical_kridantas with
            [ [] (* not in lexicon *)        -> ("G",bare_stem)
            | _  (* stem is lexical entry *) -> ("L",stem)
            ] in
        Gen_krid (look_up_stem,(vb,root))
   else Lexical stem 
;
(* Applicative version of [Morpho.report_morph] *)
value lex_cat phase = phase (*i TEMPORARY - TODO i*)
;
value get_morph gen phase form (delta,morphs) =
  let stem = patch delta form in (* stem may have homo index *)
  (form, lex_cat phase, generative_stem gen stem, morphs)
;

end;

