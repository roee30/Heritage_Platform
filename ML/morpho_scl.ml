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
open Skt_morph
  
open Morphology
  
(* [inflected] and its constructors [Noun_form], ... *)
open Naming
  
(* [look_up_homo homo_undo unique_kridantas lexical_kridantas
                 preverbs_structure] *)
let ps = print_string
  
let pr_scl_gana k = ps (string_of_int k)
  
let print_scl_number =
  function
  | Singular -> ps "<sg/>"
  | Dual -> ps "<du/>"
  | Plural -> ps "<pl/>"
and print_scl_gender =
  function
  | Mas -> ps "<m/>"
  | Neu -> ps "<n/>"
  | Fem -> ps "<f/>"
  | Deictic _ -> ps "<d/>"
and print_scl_case =
  function
  | Nom -> ps "<nom/>"
  | Acc -> ps "<acc/>"
  | Ins -> ps "<ins/>"
  | Dat -> ps "<dat/>"
  | Abl -> ps "<abl/>"
  | Gen -> ps "<gen/>"
  | Loc -> ps "<loc/>"
  | Voc -> ps "<voc/>"
and print_scl_person =
  function
  | First -> ps "<fst/>"
  | Second -> ps "<snd/>"
  | Third -> ps "<thd/>"
and print_scl_voice =
  function
  | Active -> ps "<ac/>"
  | Middle -> ps "<md/>"
  | Passive -> ps "<ps/>"
and print_scl_pr_mode =
  function
  | Present -> ps "<pr gana="
  | Imperative -> ps "<imp gana="
  | Optative -> ps "<opt gana="
  | Imperfect -> ps "<impft gana="
and print_scl_pr_mode_ps =
  function
  | Present -> ps "<prps/>"
  | Imperative -> ps "<impps/>"
  | Optative -> ps "<optps/>"
  | Imperfect -> ps "<impftps/>"
and print_scl_tense =
  function
  | Future -> ps "<fut/>"
  | Perfect -> ps "<pft/>"
  | Aorist k -> (ps "<aor gana="; pr_scl_gana k; ps "/>")
  | Injunctive k -> (ps "<inj gana="; pr_scl_gana k; ps "/>")
  | Benedictive -> ps "<ben/>"
  | Conditional -> ps "<cond/>"
  | Subjunctive -> ps "<subj/>"
  
let print_scl_paradigm =
  function
  | Conjug (t, v) -> (print_scl_tense t; print_scl_voice v)
  | Presenta (k, pr) -> (print_scl_pr_mode pr; pr_scl_gana k; ps "/><ac/>")
  | Presentm (k, pr) -> (print_scl_pr_mode pr; pr_scl_gana k; ps "/><md/>")
  | Presentp pr -> print_scl_pr_mode_ps pr
  | Perfut v -> ps "<perfut/>"
and (* TODO: mark voice *) print_scl_conjugation =
  function
  | Primary -> ()
  | Causative -> ps "<ca/>"
  | Intensive -> ps "<int/>"
  | Desiderative -> ps "<des/>"
and print_scl_nominal =
  function
  | Ppp -> ps "<pp/>"
  | Pppa -> ps "<ppa/>"
  | Ppra k ->
      (ps "<ppr gana="; pr_scl_gana k; ps "/>"; print_scl_voice Active)
  | Pprm k ->
      (ps "<ppr gana="; pr_scl_gana k; ps "/>"; print_scl_voice Middle)
  | Pprp -> (ps "<ppr/>"; print_scl_voice Passive)
  | Ppfta -> (ps "<ppf/>"; print_scl_voice Active)
  | Ppftm -> (ps "<ppf/>"; print_scl_voice Middle)
  | Pfuta -> (ps "<pfu/>"; print_scl_voice Active)
  | Pfutm -> (ps "<pfu/>"; print_scl_voice Middle)
  | Pfutp k -> (ps "<pfp/>"; pr_scl_gana k)
  | _ -> ps "<act/>"
and (* action verbal nouns *) print_scl_invar =
  function
  | Infi -> ps "<inf/>"
  | Absoya -> ps "<abs/>"
  | Perpft -> ps "<perpft/>"
and print_scl_kind =
  function
  | Part -> ps "<part/>"
  | Prep -> ps "<prep/>"
  | Conj -> ps "<conj/>"
  | Abs -> ps "<abs/>"
  | Adv -> ps "<adv/>"
  | _ -> ps "<ind/>"
  
let print_scl_finite (c, p) = (print_scl_conjugation c; print_scl_paradigm p)
and print_scl_verbal (c, n) = (print_scl_conjugation c; print_scl_nominal n)
and print_scl_modal (c, i) = (print_scl_conjugation c; print_scl_invar i)
  
let print_scl_morph =
  function
  | Noun_form (g, n, c) | Part_form (_, g, n, c) ->
      (print_scl_case c; print_scl_number n; print_scl_gender g)
  | Bare_stem | Avyayai_form -> ps "<iic/>"
  | Verb_form (f, n, p) ->
      (print_scl_finite f; print_scl_number n; print_scl_person p)
  | Ind_form k -> print_scl_kind k
  | Avyayaf_form -> ps "<avya/>"
  | Abs_root c -> (print_scl_conjugation c; ps "<abs/>")
  | Gati -> ps "<iiv/>"
  | Ind_verb m -> print_scl_modal m
  | PV _ -> ps "<pv/>"
  | Unanalysed -> ps "<unknown/>"
  
let print_scl_morphs =
  let choice () = ps "</choice><choice>"
  in List2.process_list_sep print_scl_morph choice
  
let print_inv_morpho_scl pe form generative (delta, morphs) =
  let stem = Word.patch delta form
  in
    ((* stem may have homo index *)
     ps "<morpho_infl><choice>";
     print_scl_morphs morphs;
     ps "</choice></morpho_infl>";
     ps "<morpho_gen>";
     if generative
     then (* interpret stem as unique name *)
       (let (homo, bare_stem) = homo_undo stem in
        let krid_infos = Deco.assoc bare_stem unique_kridantas
        in
          try
            let (verbal, root) = look_up_homo homo krid_infos
            in
              (pe bare_stem;
               ps "<krid>";
               print_scl_verbal verbal;
               ps "</krid><root>";
               pe root;
               ps "</root>")
          with | _ -> pe bare_stem)
     else pe stem;
     ps "</morpho_gen>")
  
let print_scl_entry w = (* ps offline in WX notation for UoH interface *)
  ps ("<entry wx=\"" ^ ((Canon.decode_WX w) ^ "\"/>"))
  
(* Decomposes a preverb sequence into the list of its components *)
(* Similar to [Morpho.decomp_pvs] *)
let decomp_pvs pvs = Deco.assoc pvs preverbs_structure
  
let print_inv_morpho_scl pvs form =
  let pv = if Phonetics.phantomatic form then [ 2 ] else (* aa- *) pvs in
  let encaps e =
    if pv = []
    then print_scl_entry e
    else
      (let pv_list = decomp_pvs pvs
       in
         ((let pr_pv pv = (Canon.decode_WX pv) |> ps
           in List2.process_list_sep pr_pv (fun () -> ps "_") pv_list);
          print_scl_entry e))
  in print_inv_morpho_scl encaps form
  
(* Used in [Lexer.print_scl_morph] *)
let print_scl_inflected pvs = print_inv_morpho_scl pvs
  

