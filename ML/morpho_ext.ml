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

value ps = print_string
;
value pr_ext_gana k = ps (string_of_int k) 
;
value print_ext_number = fun 
  [ Singular -> ps "<sg/>" 
  | Dual     -> ps "<du/>"
  | Plural   -> ps "<pl/>"
  ]
and print_ext_gender = fun 
  [ Mas -> ps "<m/>"
  | Neu -> ps "<n/>"
  | Fem -> ps "<f/>" 
  | Deictic _ -> ps "<d/>" 
  ]
and print_ext_case = fun 
  [ Nom -> ps "<nom/>"
  | Acc -> ps "<acc/>"
  | Ins -> ps "<ins/>"
  | Dat -> ps "<dat/>"
  | Abl -> ps "<abl/>"
  | Gen -> ps "<gen/>"
  | Loc -> ps "<loc/>"
  | Voc -> ps "<voc/>" 
  ] 
and print_ext_person = fun 
  [ First  -> ps "<fst/>" 
  | Second -> ps "<snd/>" 
  | Third  -> ps "<thd/>" 
  ] 
and print_ext_voice = fun 
  [ Active  -> ps "<ac/>" 
  | Middle  -> ps "<md/>" 
  | Passive -> ps "<ps/>"
  ] 
and print_ext_pr_mode = fun
  [ Present    -> ps "<pr gana="
  | Imperative -> ps "<imp gana="
  | Optative   -> ps "<opt gana="
  | Imperfect  -> ps "<impft gana="
  ]
and print_ext_pr_mode_ps = fun
  [ Present    -> ps "<prps/>"
  | Imperative -> ps "<impps/>"
  | Optative   -> ps "<optps/>"
  | Imperfect  -> ps "<impftps/>"
  ]
and print_ext_tense = fun
  [ Future       -> ps "<fut/>"
  | Perfect      -> ps "<pft/>"
  | Aorist k     -> do { ps "<aor gana="; pr_ext_gana k; ps "/>" }
  | Injunctive k -> do { ps "<inj gana="; pr_ext_gana k; ps "/>" }
  | Conditional  -> ps "<cond/>"
  | Benedictive  -> ps "<ben/>"
  ]
;
value print_ext_paradigm = fun
  [ Conjug t v    -> do { print_ext_tense t; print_ext_voice v }
  | Presenta k pr -> do { print_ext_pr_mode pr; pr_ext_gana k; 
                          ps "/><ac/>" }
  | Presentm k pr -> do { print_ext_pr_mode pr; pr_ext_gana k; 
                          ps "/><md/>" }
  | Presentp pr   -> print_ext_pr_mode_ps pr
  | Perfut v      -> ps "<perfut/>" (* TODO: mark voice *)
  ]
and print_ext_conjugation = fun 
  [ Primary      -> ()
  | Causative    -> ps "<ca/>"
  | Intensive    -> ps "<int/>"
  | Desiderative -> ps "<des/>"
  ]
and print_ext_nominal = fun
  [ Ppp     -> ps "<pp/>"
  | Pppa    -> ps "<ppa/>"
  | Ppra k  -> do { ps "<ppr gana="; pr_ext_gana k; ps "/>";
                    print_ext_voice Active }
  | Pprm k  -> do { ps "<ppr gana="; pr_ext_gana k; ps "/>";
                    print_ext_voice Middle }
  | Pprp    -> do { ps "<ppr/>"; print_ext_voice Passive }
  | Ppfta   -> do { ps "<ppf/>"; print_ext_voice Active }
  | Ppftm   -> do { ps "<ppf/>"; print_ext_voice Middle }
  | Pfuta   -> do { ps "<pfu/>"; print_ext_voice Active }
  | Pfutm   -> do { ps "<pfu/>"; print_ext_voice Middle }
  | Pfutp k -> do { ps "<pfp/>"; pr_ext_gana k }
  | _       -> ps "<act/>" (* action verbal nouns *)
  ]
and print_ext_invar = fun 
  [ Infi   -> ps "<inf/>" 
  | Absoya -> ps "<abs/>"
  | Perpft -> ps "<perpft/>"
  ]
and print_ext_kind = fun
  [ Part -> ps "<part/>"
  | Prep -> ps "<prep/>"
  | Conj -> ps "<conj/>"
  | Abs  -> ps "<abs/>"
  | Adv  -> ps "<adv/>"
  | _    -> ps "<und/>"
  ]
;
value print_ext_finite (c,p) = 
  do { print_ext_conjugation c; print_ext_paradigm p }
and   print_ext_verbal (c,n) = 
  do { print_ext_conjugation c; print_ext_nominal n }
and   print_ext_modal (c,i)  = 
  do { print_ext_conjugation c; print_ext_invar i }
;
value print_ext_morph = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> do
      { print_ext_case c
      ; print_ext_number n
      ; print_ext_gender g
      }
  | Bare_stem | Avyayai_form -> ps "<iic/>"
  | Verb_form f n p -> do
      { print_ext_finite f
      ; print_ext_number n
      ; print_ext_person p
      }
  | Ind_form k -> print_ext_kind k
  | Avyayaf_form -> ps "<avya/>"
  | Abs_root c -> do { print_ext_conjugation c; ps "<abs/>" }
  | Auxi_form -> ps "<iiv/>"
  | Ind_verb m -> print_ext_modal m
  | PV _ -> ps "<pv/>"
  | Unanalysed -> ps "<unknown/>" 
  ]
;
value print_ext_morphs = 
  let choice () = ps "</choice><choice>" in
  List2.process_list_sep print_ext_morph choice
;
value print_inv_morpho_ext pe pne form generative (delta,morphs) = 
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { ps "<morpho_infl><choice>"
    ; print_ext_morphs morphs
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
        ; ps "<krid>"; print_ext_verbal verbal
        ; ps "</krid><root>"; pe root; ps "</root>"
        } with [ _ -> pne bare_stem ]
      else pe stem
    ; ps "</morpho_gen>"
    }
;
value print_inv_morpho_link_ext pvs pe pne form = 
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *) 
           else pvs in
  let encaps print e = if pv = [] then print e
  else do { ps (Canon.decode_WX pvs ^ "-"); print e } in
  print_inv_morpho_ext (encaps pe) (encaps pne) form 
and print_ext_entry w = (* ps offline in WX notation for UoH interface *)
  ps ("<entry wx=\"" ^ Canon.decode_WX w ^ "\"/>") 
;
(* Used in [Lexer.print_ext_morph] *)
value print_ext_inflected_link pvs = 
  print_inv_morpho_link_ext pvs print_ext_entry print_ext_entry
;

(*i end; i*)
