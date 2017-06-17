(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Prints morphological information, including derivative morphology.
   Used in [Morpho_html] and [Morpho_ext] *)

open Skt_morph;
open Morphology; 
  (* [inflected] and its constructors [Noun_form], ..., [homo_krid]  *)
open Naming; (* [homo_undo look_up_homo unique_kridantas lexical_kridantas] *) 
open Morpho_string (* [string_morph string_verbal] *);

module Morpho_out (Chan: sig value chan: ref out_channel; end)  
 = struct 

value ps s = output_string Chan.chan.val s 
; 
value pl s = ps (s ^ "\n")
;
value pr_word w = ps (Canon.decode w)
;
value print_morph m = ps (string_morph m)
and print_verbal vb = ps (string_verbal vb)
;
(*i Anomaly: Morpho should be independent of Html i*)
value select_morph (seg_num,sub,seg_count) morph = do 
  { let string_num = string_of_int seg_num  
    and seg = (string_of_int sub) ^ "," ^ (string_of_int seg_count) in
    let radio_cond = Html.radio_input_dft string_num seg "" in
    match (sub,seg_count) with 
    [ (1,1) -> ps (radio_cond True ^ " ") 
                  (* NB: only the first button is selected *)
    | _     -> ps (radio_cond False ^ " ")
    ]
  ; print_morph morph
  }
;
value rec select_morphs (seg_num,sub) seg_count = fun
  [ [] -> ()
  | [ last :: [] ] -> select_morph (seg_num,sub,seg_count) last 
  | [ first :: rest ] -> do
      { select_morph (seg_num,sub,seg_count) first
      ; ps " | "
      ; select_morphs (seg_num,sub) (seg_count+1) rest
      }
  ]
;
value print_morphs (seg_num,sub) morphs = match seg_num with
  [ 0 -> let bar () = ps " | " in
         List2.process_list_sep print_morph bar morphs
  | _ -> select_morphs (seg_num,sub) 1 morphs
  ]
; 
(* The following print functions insert in the HTML output links to entries 
   in the lexicon, also radio buttons and other marks for user choices. *)

(* [pe : word -> unit] is [Morpho_html.print_entry] with hyperlink,
   [pne : word -> unit] is [Morpho_html.print_stem],
   [pu : word -> unit] prints un-analysed chunks. *)
value print_inv_morpho pe pne pu form (seg_num,sub) generative (delta,morphs) = 
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { ps "{ "
    ; print_morphs (seg_num,sub) morphs 
    ; ps " }[" 
    ; if generative then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krit_infos = Deco.assoc bare_stem unique_kridantas in 
        try let (verbal,root) = look_up_homo homo krit_infos in do
        { match Deco.assoc bare_stem lexical_kridantas with
          [ [] (* not in lexicon *) -> pne bare_stem
          | entries (* bare stem is lexicalized *) -> 
              if List.exists (fun (_,h) -> h=homo) entries
                 then pe stem (* stem with exact homo is lexical entry *)
              else pne bare_stem
          ] 
        ; ps " { "; print_verbal verbal; ps " }["; pe root; ps "]"
        } with [ _ -> pu bare_stem ]
      else match morphs with
	   [ [ Unanalysed ] -> pu stem 
	   | _ -> pe stem 
	   ]
    ; ps "]"
    }
;
(* Used in [Morpho_html] *)
value print_inv_morpho_link pvs pe pne pu form =
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *)
           else pvs in
  let encaps print e = (* encapsulates prefixing with possible preverbs *)
     if pv = [] then print e else do { pe pvs; ps "-"; print e } in
  print_inv_morpho (encaps pe) (encaps pne) pu form
(* Possible overgeneration when derivative of a root non attested with pv 
   since only existential test in [Dispatcher.validate_pv]. Thus
   [anusandhiiyate] should show [dhaa#1], not [dhaa#2], [dhii#1] or [dhyaa] *)
;
value print_inv_morpho_tad pv pe pne pu stem sfx_form (seg_num,sub)  
                           generative (delta,morphs) = 
  let sfx = Word.patch delta sfx_form in do
    { ps "{ "
    ; print_morphs (seg_num,sub) morphs (* taddhitaanta declension *)
    ; ps " }["
    ; if generative then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krit_infos = Deco.assoc bare_stem unique_kridantas in 
        try let (verbal,root) = look_up_homo homo krit_infos in do
        { match Deco.assoc bare_stem lexical_kridantas with
          [ [] (* not in lexicon *) -> pne bare_stem 
          | entries (* bare stem is lexicalized *) -> 
              if List.exists (fun (_,h) -> h=homo) entries
                 then pe stem (* stem with exact homo is lexical entry *)
              else pne bare_stem
          ] 
        ; ps " { "; print_verbal verbal; ps " }["; pe root; ps "]"
        } with [ _ -> pu bare_stem ]
      else pe stem 
    ; pne sfx; ps "]" 
    }
;
(* variant with link for printing of taddhitaantas *)
value print_inv_morpho_link_tad pvs pe pne pu stem sfx_form  =
  let pv = if Phonetics.phantomatic stem then [ 2 ] (* aa- *) 
           else pvs in
  print_inv_morpho_tad pv pe pne pu stem sfx_form
;

(* Used in [Lexer.record_tagging] for regression analysis *)
value report_morph gen form (delta,morphs) =
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { ps "{ "
    ; print_morphs (0,0) morphs 
    ; ps " }[" 
    ; if gen then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krid_infos = Deco.assoc bare_stem unique_kridantas in 
        let (vb,root) = look_up_homo homo krid_infos in do
        { match Deco.assoc stem lexical_kridantas with
          [ [] (* not in lexicon *)        -> do { ps "G:"; pr_word bare_stem }
          | _  (* stem is lexical entry *) -> do { ps "L:"; pr_word stem }
          ]
        ; ps " { "; print_verbal vb; ps " }["; pr_word root; ps "]"
        }
      else pr_word stem
    ; ps "]"
    } 
;

end;

