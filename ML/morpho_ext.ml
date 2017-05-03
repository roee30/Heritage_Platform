(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Morpho_ext = struct i*)

(* Prints lists of inflected forms in XML for use by external Web services. *)
(* Adapted from [Morpho_xml] *)
(* Uses WX for transliteration output. *)

open Skt_morph;
open Morphology; (* [inflected] and its constructors [Noun_form], ... *)
open Naming; (* [look_up_homo homo_undo unique_kridantas lexical_kridantas] *)

value pr_ext_gana ps k = ps (string_of_int k) 
;
value print_ext_number ps = fun 
  [ Singular -> ps "<sg/>" 
  | Dual     -> ps "<du/>"
  | Plural   -> ps "<pl/>"
  ]
and print_ext_gender ps = fun 
  [ Mas -> ps "<m/>"
  | Neu -> ps "<n/>"
  | Fem -> ps "<f/>" 
  | Deictic _ -> ps "<d/>" 
  ]
and print_ext_case ps = fun 
  [ Nom -> ps "<nom/>"
  | Acc -> ps "<acc/>"
  | Ins -> ps "<ins/>"
  | Dat -> ps "<dat/>"
  | Abl -> ps "<abl/>"
  | Gen -> ps "<gen/>"
  | Loc -> ps "<loc/>"
  | Voc -> ps "<voc/>" 
  ] 
and print_ext_person ps = fun 
  [ First  -> ps "<fst/>" 
  | Second -> ps "<snd/>" 
  | Third  -> ps "<thd/>" 
  ] 
and print_ext_voice ps = fun 
  [ Active  -> ps "<ac/>" 
  | Middle  -> ps "<md/>" 
  | Passive -> ps "<ps/>"
  ] 
and print_ext_pr_mode ps = fun
  [ Present    -> ps "<pr gana="
  | Imperative -> ps "<imp gana="
  | Optative   -> ps "<opt gana="
  | Imperfect  -> ps "<impft gana="
  ]
and print_ext_pr_mode_ps ps = fun
  [ Present    -> ps "<prps/>"
  | Imperative -> ps "<impps/>"
  | Optative   -> ps "<optps/>"
  | Imperfect  -> ps "<impftps/>"
  ]
and print_ext_tense ps = fun
  [ Future       -> ps "<fut/>"
  | Perfect      -> ps "<pft/>"
  | Aorist k     -> do { ps "<aor gana="; pr_ext_gana ps k; ps "/>" }
  | Injunctive k -> do { ps "<inj gana="; pr_ext_gana ps k; ps "/>" }
  | Conditional  -> ps "<cond/>"
  | Benedictive  -> ps "<ben/>"
  ]
;
value print_ext_paradigm ps = fun
  [ Conjug t v    -> do { print_ext_tense ps t; print_ext_voice ps v }
  | Presenta k pr -> do { print_ext_pr_mode ps pr; pr_ext_gana ps k; 
                          ps "/><ac/>" }
  | Presentm k pr -> do { print_ext_pr_mode ps pr; pr_ext_gana ps k; 
                          ps "/><md/>" }
  | Presentp pr   -> print_ext_pr_mode_ps ps pr
  | Perfut v      -> ps "<perfut/>" (* TODO: mark voice *)
  ]
and print_ext_conjugation ps = fun 
  [ Primary      -> ()
  | Causative    -> ps "<ca/>"
  | Intensive    -> ps "<int/>"
  | Desiderative -> ps "<des/>"
  ]
and print_ext_nominal ps = fun
  [ Ppp     -> ps "<pp/>"
  | Pppa    -> ps "<ppa/>"
  | Ppra k  -> do { ps "<ppr gana="; pr_ext_gana ps k; ps "/>";
                    print_ext_voice ps Active }
  | Pprm k  -> do { ps "<ppr gana="; pr_ext_gana ps k; ps "/>";
                    print_ext_voice ps Middle }
  | Pprp    -> do { ps "<ppr/>"; print_ext_voice ps Passive }
  | Ppfta   -> do { ps "<ppf/>"; print_ext_voice ps Active }
  | Ppftm   -> do { ps "<ppf/>"; print_ext_voice ps Middle }
  | Pfuta   -> do { ps "<pfu/>"; print_ext_voice ps Active }
  | Pfutm   -> do { ps "<pfu/>"; print_ext_voice ps Middle }
  | Pfutp k -> do { ps "<pfp/>"; pr_ext_gana ps k }
  | _       -> ps "<act/>" (* action verbal nouns *)
  ]
and print_ext_invar ps = fun 
  [ Infi   -> ps "<inf/>" 
  | Absoya -> ps "<abs/>"
  | Perpft -> ps "<perpft/>"
  ]
and print_ext_kind ps = fun
  [ Part -> ps "<part/>"
  | Prep -> ps "<prep/>"
  | Conj -> ps "<conj/>"
  | Abs  -> ps "<abs/>"
  | Adv  -> ps "<adv/>"
  | _    -> ps "<und/>"
  ]
;
value print_ext_finite ps (c,p) = 
  do { print_ext_conjugation ps c; print_ext_paradigm ps p }
and   print_ext_verbal ps (c,n) = 
  do { print_ext_conjugation ps c; print_ext_nominal ps n }
and   print_ext_modal ps (c,i)  = 
  do { print_ext_conjugation ps c; print_ext_invar ps i }
;
value print_ext_morph ps = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> do
      { print_ext_case ps c
      ; print_ext_number ps n
      ; print_ext_gender ps g
      }
  | Bare_stem | Avyayai_form -> ps "<iic/>"
  | Verb_form f n p -> do
      { print_ext_finite ps f
      ; print_ext_number ps n
      ; print_ext_person ps p
      }
  | Ind_form k -> print_ext_kind ps k
  | Avyayaf_form -> ps "<avya/>"
  | Abs_root c -> do { print_ext_conjugation ps c; ps "<abs/>" }
  | Auxi_form -> ps "<iiv/>"
  | Ind_verb m -> print_ext_modal ps m
  | PV _ -> ps "<pv/>"
  | Unanalysed -> ps "<unknown/>" 
  ]
;
value print_ext_morphs ps = 
  let choice () = ps "</choice><choice>" in
  List2.process_list_sep (print_ext_morph ps) choice
;
value print_inv_morpho_ext ps pe pne form generative (delta,morphs) = 
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { ps "<morpho_infl><choice>"
    ; print_ext_morphs ps morphs
    ; ps "</choice></morpho_infl>"
    ; ps "<morpho_gen>"
    ; if generative then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krid_infos = Deco.assoc bare_stem unique_kridantas in 
        try let (verbal,root) = look_up_homo homo krid_infos in do
        { match Deco.assoc bare_stem lexical_kridantas with
          [ [] (* not in lexicon *) -> pne bare_stem
          | entries (* bare stem is lexicalized *) -> 
              if List.exists (fun (_,h) -> h=homo) entries
                 then pe stem (* stem with exact homo is lexical entry *)
              else pne bare_stem
          ]
        ; ps "<krid>"; print_ext_verbal ps verbal
        ; ps "</krid><root>"; pe root; ps "</root>"
        } with [ _ -> pne bare_stem ]
      else pe stem
    ; ps "</morpho_gen>"
    }
;
value print_inv_morpho_link_ext pvs ps pe pne form = 
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *) 
           else pvs in
  let encaps print e = if pv = [] then print e
  else do { ps (Canon.decode_WX pvs ^ "-"); print e } in
  print_inv_morpho_ext ps (encaps pe) (encaps pne) form 
and print_ext_entry ps w = (* ps offline in WX notation for UoH interface *)
  ps ("<entry wx=\"" ^ Canon.decode_WX w ^ "\"/>") 
;
(* Used in [Lexer.print_ext_morph] *)
value print_ext_inflected_link pvs ps = 
  print_inv_morpho_link_ext pvs ps (print_ext_entry ps) (print_ext_entry ps) 
;

(*i end; i*)
