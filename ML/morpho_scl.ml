(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Morpho_scl = struct i*)

(* Prints lists of inflected forms in XML for use by external Web services. *)
(* Adapted from [Morpho_xml] *)
(* Uses WX for transliteration output. *)

open Skt_morph;
open Morphology; (* [inflected] and its constructors [Noun_form], ... *)
open Naming; (* [look_up_homo homo_undo unique_kridantas lexical_kridantas
                 preverbs_structure] *)

value ps = print_string
;
value pr_scl_gana k = ps (string_of_int k) 
;
value print_scl_number = fun 
  [ Singular -> ps "<sg/>" 
  | Dual     -> ps "<du/>"
  | Plural   -> ps "<pl/>"
  ]
and print_scl_gender = fun 
  [ Mas -> ps "<m/>"
  | Neu -> ps "<n/>"
  | Fem -> ps "<f/>" 
  | Deictic _ -> ps "<d/>" 
  ]
and print_scl_case = fun 
  [ Nom -> ps "<nom/>"
  | Acc -> ps "<acc/>"
  | Ins -> ps "<ins/>"
  | Dat -> ps "<dat/>"
  | Abl -> ps "<abl/>"
  | Gen -> ps "<gen/>"
  | Loc -> ps "<loc/>"
  | Voc -> ps "<voc/>" 
  ] 
and print_scl_person = fun 
  [ First  -> ps "<fst/>" 
  | Second -> ps "<snd/>" 
  | Third  -> ps "<thd/>" 
  ] 
and print_scl_voice = fun 
  [ Active  -> ps "<ac/>" 
  | Middle  -> ps "<md/>" 
  | Passive -> ps "<ps/>"
  ] 
and print_scl_pr_mode = fun
  [ Present    -> ps "<pr gana="
  | Imperative -> ps "<imp gana="
  | Optative   -> ps "<opt gana="
  | Imperfect  -> ps "<impft gana="
  ]
and print_scl_pr_mode_ps = fun
  [ Present    -> ps "<prps/>"
  | Imperative -> ps "<impps/>"
  | Optative   -> ps "<optps/>"
  | Imperfect  -> ps "<impftps/>"
  ]
and print_scl_tense = fun
  [ Future       -> ps "<fut/>"
  | Perfect      -> ps "<pft/>"
  | Aorist k     -> do { ps "<aor gana="; pr_scl_gana k; ps "/>" }
  | Injunctive k -> do { ps "<inj gana="; pr_scl_gana k; ps "/>" }
  | Benedictive  -> ps "<ben/>"
  | Conditional  -> ps "<cond/>"
  | Subjunctive  -> ps "<subj/>"
  ]
;
value print_scl_paradigm = fun
  [ Conjug t v    -> do { print_scl_tense t; print_scl_voice v }
  | Presenta k pr -> do { print_scl_pr_mode pr; pr_scl_gana k; 
                          ps "/><ac/>" }
  | Presentm k pr -> do { print_scl_pr_mode pr; pr_scl_gana k; 
                          ps "/><md/>" }
  | Presentp pr   -> print_scl_pr_mode_ps pr
  | Perfut v      -> ps "<perfut/>" (* TODO: mark voice *)
  ]
and print_scl_conjugation = fun 
  [ Primary      -> ()
  | Causative    -> ps "<ca/>"
  | Intensive    -> ps "<int/>"
  | Desiderative -> ps "<des/>"
  ]
and print_scl_nominal = fun
  [ Ppp     -> ps "<pp/>"
  | Pppa    -> ps "<ppa/>"
  | Ppra k  -> do { ps "<ppr gana="; pr_scl_gana k; ps "/>";
                    print_scl_voice Active }
  | Pprm k  -> do { ps "<ppr gana="; pr_scl_gana k; ps "/>";
                    print_scl_voice Middle }
  | Pprp    -> do { ps "<ppr/>"; print_scl_voice Passive }
  | Ppfta   -> do { ps "<ppf/>"; print_scl_voice Active }
  | Ppftm   -> do { ps "<ppf/>"; print_scl_voice Middle }
  | Pfuta   -> do { ps "<pfu/>"; print_scl_voice Active }
  | Pfutm   -> do { ps "<pfu/>"; print_scl_voice Middle }
  | Pfutp k -> do { ps "<pfp/>"; pr_scl_gana k }
  | _       -> ps "<act/>" (* action verbal nouns *)
  ]
and print_scl_invar = fun 
  [ Infi   -> ps "<inf/>" 
  | Absoya -> ps "<abs/>"
  | Perpft -> ps "<perpft/>"
  ]
and print_scl_kind = fun
  [ Part -> ps "<part/>"
  | Prep -> ps "<prep/>"
  | Conj -> ps "<conj/>"
  | Abs  -> ps "<abs/>"
  | Adv  -> ps "<adv/>"
  | _    -> ps "<ind/>"
  ]
;
value print_scl_finite (c,p) = 
  do { print_scl_conjugation c; print_scl_paradigm p }
and   print_scl_verbal (c,n) = 
  do { print_scl_conjugation c; print_scl_nominal n }
and   print_scl_modal (c,i)  = 
  do { print_scl_conjugation c; print_scl_invar i }
;
value print_scl_morph = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> do
      { print_scl_case c
      ; print_scl_number n
      ; print_scl_gender g
      }
  | Bare_stem | Avyayai_form -> ps "<iic/>"
  | Verb_form f n p -> do
      { print_scl_finite f
      ; print_scl_number n
      ; print_scl_person p
      }
  | Ind_form k -> print_scl_kind k
  | Avyayaf_form -> ps "<avya/>"
  | Abs_root c   -> do { print_scl_conjugation c; ps "<abs/>" }
  | Gati    -> ps "<iiv/>"
  | Ind_verb m   -> print_scl_modal m
  | PV _         -> ps "<pv/>"
  | Unanalysed   -> ps "<unknown/>" 
  ]
;
value print_scl_morphs = 
  let choice () = ps "</choice><choice>" in
  List2.process_list_sep print_scl_morph choice
;
value print_inv_morpho_scl pe form generative (delta,morphs) = 
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { ps "<morpho_infl><choice>"
    ; print_scl_morphs morphs
    ; ps "</choice></morpho_infl>"
    ; ps "<morpho_gen>"
    ; if generative then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krid_infos = Deco.assoc bare_stem unique_kridantas in 
        try let (verbal,root) = look_up_homo homo krid_infos in do
        { pe bare_stem
        ; ps "<krid>"; print_scl_verbal verbal
        ; ps "</krid><root>"; pe root; ps "</root>"
        } with [ _ -> pe bare_stem ]
      else pe stem
    ; ps "</morpho_gen>"
    }
      ;
value print_scl_entry w = (* ps offline in WX notation for UoH interface *)
  ps ("<entry wx=\"" ^ Canon.decode_WX w ^ "\"/>")
;
(* Decomposes a preverb sequence into the list of its components *)
(* Similar to [Morpho.decomp_pvs] *)
value decomp_pvs pvs = 
  Deco.assoc pvs preverbs_structure
;
value print_inv_morpho_scl pvs form = 
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *) 
           else pvs in
  let encaps e = if pv = [] then print_scl_entry e
                 else let pv_list = decomp_pvs pvs in do 
                      { List.iter pr_pv pv_list 
                          where pr_pv pv = Canon.decode_WX pv ^ "_" |> ps
                      ; print_scl_entry e 
                      } in
  print_inv_morpho_scl encaps form 
;
(* Used in [Lexer.print_scl_morph] *)
value print_scl_inflected pvs = 
  print_inv_morpho_scl pvs 
;

(*i end; i*)
