(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
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
open Dispatcher; (* [generative Dispatch transition phase_of_sort trim_tags] *) 
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
open Phases; (* phase *) 

module Lemmas = Load_morphs.Morphs Prel Phases
;
open Lemmas (* [morpho tags_of] *)
;
open Load_transducers; (* [transducer_vect Trans] *)

module Transducers = Trans Prel;

module Disp = Dispatch Transducers Lemmas;
open Disp (* [transducer initial accepting dispatch input color_of_phase 
              transition] *) 
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
value rec color_of_role = fun (* Semantic role of lexical category *)
  [ Pv | Pvk | Pvkc | Pvkv | Iic | Iic2 | Iik | Voca | Inv | Iicv | Iicc 
  | Iikv | Iikc | Iiif | A | An | Vok | Vokv | Vokc | Vocv | Vocc | Iiy 
  | Iiv | Iivv | Iivc | Peri | Auxiick -> Grey 
  | Noun | Noun2 | Nouv | Nouc | Krid | Kriv | Kric | Pron | Ifc | Ifc2
  | Kama | Lopak | Auxik -> Cyan (* Actor or Predicate *)
  | Root | Lopa |  Auxi -> Pink (* abs-tvaa in Inde *) (* Process *) 
  | Abso | Absv | Absc | Inde | Avy | Ai | Ani | Inftu (* Circumstance *)
    -> Lavender 
  | Unknown | Cache -> Grey 
  | Comp (_,ph) _ _ | Tad (_,ph)  _ _ -> color_of_role ph
  | Sfx -> Cyan
  | Isfx -> Grey
  ]
;
value table_morph_of phase = table_begin (background (color_of_phase phase)) 
and table_role_of phase = table_begin (background (color_of_role phase)) 
and table_labels = table_begin (background Pink)
;
value print_morph pvs cached seg_num gen form n tag = do
(* n is the index in the list of tags of an ambiguous form *)
  { ps tr_begin
  ; ps th_begin 
  ; ps (span_begin Latin12)  
  ; Morpho_html.print_inflected_link pvs cached form (seg_num,n) gen tag 
  ; ps span_end 
  ; ps th_end   
  ; ps tr_end   
  ; n+1
  }
;
(* generalisation of [print_morph] to taddhitas *)
value print_morph_tad pvs cached seg_num gen stem sfx n tag = do
(* n is the index in the list of tags of an ambiguous form *)
  { ps tr_begin
  ; ps th_begin 
  ; ps (span_begin Latin12)  
  ; Morpho_html.print_inflected_link_tad pvs cached stem sfx (seg_num,n) gen tag 
  ; ps span_end 
  ; ps th_end  
  ; ps tr_end   
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
  | Sfx -> "suffix"
  | Isfx -> "iicsuffix"
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
  | Tad (ph,_)  _ _ -> "taddhita " ^ scl_phase ph
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
  { ps (table phase) 
  ; List.iter (print_scl_morph pvs (generative phase) form) tags 
  ; ps (xml_end "tags")
  }
;

(* Used in Parser *)
value extract_lemma phase word = 
 match tags_of phase word with  
 [ Atomic tags -> tags 
 | Preverbed (_,phase) pvs form tags -> (* tags to be trimmed to [ok_tags] *)
     if pvs = [] then tags 
     else trim_tags (generative phase) form (Canon.decode pvs) tags 
 | Taddhita  _ _ _ tags -> tags
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
value print_sfx_tags sfx = fun
  [ [ tag ] -> let _ = print_morph [] False 0 False sfx 1 tag in ()
  | _ -> failwith "Multiple sfx tag" 
  ]
;
value process_kridanta pvs seg_num phase form tags = do
  { ps th_begin 
  ; pl (table_morph_of phase)          (* table begin *)
  ; let ok_tags = 
        if pvs = [] then tags 
        else trim_tags (generative phase) form (Canon.decode pvs) tags in do
        (* NB Existence of the segment guarantees that [ok_tags] is not empty *)
  { print_tags pvs seg_num phase form ok_tags 
  ; ps table_end                       (* table end *) 
  ; ps th_end  
  ; (phase,form,ok_tags)
  }}
;
value process_taddhita pvs seg_num phase stem sfx_phase sfx sfx_tags = 
  let gen = generative phase 
  and cached = False in
  let ptag = print_morph_tad pvs cached seg_num gen stem sfx in do
  { ps th_begin 
  ; pl (table_morph_of sfx_phase)      (* table begin *)
  ; let _ = List.fold_left ptag 1 sfx_tags in ()
  ; ps table_end                       (* table end *) 
  ; ps th_end  
  ; (sfx_phase,sfx,sfx_tags)
  }
;
(* Same structure as [Interface.print_morpho] *)
value print_morpho phase word = do  
  { pl (table_morph_of phase)          (* table begin *)  
  ; ps tr_begin 
  ; ps th_begin 
  ; ps (span_begin Latin12)  
  ; let _ =
       match tags_of phase word with 
       [ Atomic tags ->  
          process_kridanta [] 0 phase word tags
       | Preverbed (_,phase) pvs form tags -> 
          process_kridanta pvs 0 phase form tags
       | Taddhita (ph,form) sfx sfx_phase sfx_tags -> 
            match tags_of ph form with 
            [ Atomic _ -> (* stem, tagged as iic *)
              process_taddhita [] 0 ph form sfx_phase sfx sfx_tags 
            | Preverbed _ pvs _ _ -> (* stem, tagged as iic *)
              process_taddhita pvs 0 ph form sfx_phase sfx sfx_tags 
            | _ -> failwith "Anomaly: taddhita recursion"
            ]
       ] in ()
  ; ps span_end  
  ; ps th_end  
  ; ps tr_end 
  ; ps table_end                       (* table end *)
  }
;
(* Segment printing with phonetics without semantics for Reader *)
value print_segment offset (phase,rword,transition) = do
  { ps "[ "
  ; Morpho_html.print_signifiant_off rword offset
  ; print_morpho phase (mirror rword)
  (* Now we print the sandhi transition *)
  ; ps "&lang;" (* < *) 
  ; let correction = process_transition transition in do  
      { print_transition transition
      ; pl "&rang;]" (* >] *)
      ; pl html_break
      ; offset+correction+length rword
      }
  }
; 
(* Similarly for [scl_plugin] mode (without offset and transitions) *)
(* Called from [Scl_parser.print_scl_output] *)
value print_scl_segment counter (phase,rword) =  
  let word = Morpho_html.visargify rword in do
  { let solid = background (Disp.color_of_phase phase) in
    pl (td_begin_class solid)
  ; let ic = string_of_int counter in
    ps ("<input type=\"hidden\" name=\"field" ^ ic ^ "\" value='<form wx=\""
        ^ Canon.decode_WX word ^ "\"/>")
  ; match tags_of phase (mirror rword) with 
    [ Atomic tags ->
          print_scl_tags [] phase word tags
    | Preverbed (_,phase) pvs form tags -> 
         let ok_tags = 
           if pvs = [] then tags 
           else trim_tags (generative phase) form (Canon.decode pvs) tags in
          print_scl_tags pvs phase form ok_tags
    | Taddhita (_,form) sfx sfx_phase sfx_tags ->
            let taddhitanta_phase = match sfx_phase with 
                [ Sfx -> Noun
                | Isfx -> Iic
                | _ -> failwith "Wrong taddhita structure"
                ] 
            and taddhitanta_stem = form @ sfx (* very experimental *) in
            print_scl_tags [] taddhitanta_phase taddhitanta_stem sfx_tags 
    ]
  ; ps "'>" (* closes <input *) 
  ; ps (Canon.unidevcode word)
  ; ps td_end
  ; ps "\n"
  ; counter+1
  } 
; 
value print_labels tags seg_num = do
    { ps th_begin  (* begin labels *) 
    ; pl table_labels
    ; let print_label n _ = do
        { ps (cell (html_red (string_of_int seg_num ^ "." ^ string_of_int n)))
        ; n+1
        } in 
      let _ = List.fold_left print_label 1 tags in () 
    ; ps table_end 
    ; ps th_end    (* end labels *)
    }
;
(* syntactico/semantical roles analysis, function of declension *)
value print_roles pr_sem phase tags form = do
    { ps th_begin 
    ; pl (table_role_of phase)
    ; let pr_roles (delta,sems) = do 
       { ps tr_begin 
       ; ps th_begin 
       ; let word = patch delta form in 
         pr_sem word sems 
       ; ps th_end
       ; ps tr_end 
       } in
      List.iter pr_roles tags  
    ; ps table_end 
    ; ps th_end  
    }
;
(* Segment printing without phonetics with semantics for Parser *)
value print_segment_roles print_sems seg_num (phase,rword,_) =  
  let word = mirror rword in do
  { Morpho_html.print_signifiant_yellow rword
  ; let (decl_phase,form,decl_tags) = match tags_of phase word with
       [ Atomic tags -> 
          process_kridanta [] seg_num phase word tags
       | Preverbed (_,phase) pvs form tags -> 
          process_kridanta pvs seg_num phase form tags 
       | Taddhita (ph,form) sfx sfx_phase sfx_tags ->  
            match tags_of ph form with 
            [ Atomic _ -> (* stem, tagged as iic *)
              process_taddhita [] seg_num ph form sfx_phase sfx sfx_tags 
            | Preverbed _ pvs _ _ -> (* stem, tagged as iic *)
              process_taddhita pvs seg_num ph form sfx_phase sfx sfx_tags 
            | _ -> failwith "taddhita recursion unavailable"
            ]
       ] in do
    { print_labels decl_tags seg_num
    ; print_roles print_sems decl_phase decl_tags form
    }
  } 
;
value project n list = List.nth list (n-1) (* Ocaml's nth starts at 0 *)
; 
value print_unitag pvs phase word multitags (n,m) = 
  let (delta,polytag) = project n multitags in
  let unitag = [ project m polytag ] in do
     { ps th_begin
     ; pl (table_morph_of phase) (* table of color of phase begins *)
     ; let _ = (* print unique tagging *)
       print_morph pvs False 0 (generative phase) word 0 (delta,unitag) in ()
     ; ps table_end              (* table of color of phase ends *)
     ; ps th_end
     }
;
value print_uni_taddhita pvs m phase stem sfx sfx_phase = fun
  [ [ (delta,polytag) ] -> (* we assume n=1 taddhita form unambiguous *)
    let unitag = [ project m polytag ] 
    and gen = generative phase 
    and cached = False in do
    { ps th_begin 
    ; pl (table_morph_of sfx_phase)      (* table begin *)
    ; let _ = print_morph_tad pvs cached 0 gen stem sfx 0 (delta,unitag) in ()
    ; ps table_end                       (* table end *) 
    ; ps th_end
    }
  | _ -> failwith "Multiple sfx tag"
  ]
;
value print_projection phase rword ((_,m) as index) = do
  { ps tr_begin             (* tr begins *)
  ; Morpho_html.print_signifiant_yellow rword
  ; let word = mirror rword in 
    match tags_of phase word with
    [ Atomic tags -> print_unitag [] phase word tags index 
    | Preverbed (_,phase) pvs form tags -> print_unitag pvs phase form tags index
    | Taddhita (ph,form) sfx sfx_phase sfx_tags -> 
        match tags_of ph form with
        [ Atomic _ -> print_uni_taddhita [] m phase form sfx sfx_phase sfx_tags
        | Preverbed _ pvs _ _ -> 
                      print_uni_taddhita pvs m phase form sfx sfx_phase sfx_tags
        | _ -> failwith "taddhita recursion unavailable" 
        ]
    ] 
  ; ps tr_end               (* tr ends *)
  }
;
value print_proj phase rword = fun 
   [ [] -> failwith "Projection missing"
   | [ n_m :: rest ] -> do
       { print_projection phase rword n_m 
       ; rest (* returns the rest of projections stream *)
       }
   ]
;

module Report_chan = struct 
value chan = Control.out_chan; (* where to report *)
end;

module Morpho_out = Morpho.Morpho_out Report_chan;

(* Recording of selected solution - used only in Regression *)
value record_tagging unsandhied mode_sent mode_trans all sentence output proj = 
  let report = output_string Control.out_chan.val in
  let print_proj1 phase rword proj prevs = do 
  (* adapted from [print_proj] *)
  { report "${"
  ; let form = mirror rword in do 
    { report (decode form)
    ; let res = match proj with 
           [ [] -> failwith "Projection missing"
           | [ (n,m) :: rest ] -> 
              let gen = generative phase in
              let polytag = extract_lemma phase form in
              let (delta,tags) = project n polytag in 
              let tagging = [ project m tags ] in do 
                { report ":"
                ; report (string_of_phase phase ^ "")
                ; Morpho_out.report_morph gen form (delta,tagging) 
                ; (rest,[]) (* returns the rest of projections stream *)
                }
           ] in 
      do { report "}$&"; res }
    }
  } in do
  { report (if Control.full.val then "[{C}] " else "[{S}] ")
  ; report (if unsandhied then "<{F}> " else "<{T}> ")
  ; report (if mode_sent then "|{Sent}| " else "|{Word}| ")
  ; report ("#{" ^ mode_trans ^ "}# ")
  ; report ("({" ^ sentence ^ "})")
  ; report (" [" ^ (string_of_int all) ^ "] ")
  ; let rec pp (proj,prevs) = fun
    [ [] -> match proj with 
            [ [] -> () (* finished, projections exhausted *)
            | _ -> failwith "Too many projections"
            ]
    | [ (phase,rword,_) :: rest ] -> (* sandhi ignored *)
        let proj_prevs = print_proj1 phase rword proj prevs in
        pp proj_prevs rest 
    ] in pp (proj,[]) output
  ; report "\n"
  ; close_out Report_chan.chan.val
  }
;
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
value return_tagging output projs = (* Used only in Regression *)
  let get_tags phase rword projs = (* adapted from [print_proj] *)
     let form = mirror rword in  
     match tags_of phase form with
     [ Atomic polytag -> match projs with 
           [ [] -> failwith "Projection missing"
           | [ (n,m) :: rest ] -> 
              let gen = generative phase in
              let (delta,tags) = project n polytag in
              let tagging = [ project m tags ] in 
              let entry = get_morph gen phase form (delta,tagging) in
              (rest, lex_cat phase, entry)
           ]
     | _ -> failwith "Not implemented yet" (*i TODO for Regression 
         [ (projs, lex_cat Pv, (form, lex_cat Pv, Preverbs_list prevs, []))] i*)
     ] in 
  let rec taggings accu projs = fun
     [ [] -> match projs with 
             [ [] -> accu
             | _ -> failwith "Too many projections"
             ]
     | [ (phase,rword,_) :: rest ] -> (* sandhi ignored *)
          let (new_projs,phase,tags) = get_tags phase rword projs in
          taggings [ tags :: accu ] new_projs rest 
     ] in 
  return_morph (List.rev (taggings [] projs output))
;

end;

