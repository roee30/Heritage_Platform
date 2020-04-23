(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_xml_data = struct  i*)

(* Prepares XML data banks from the databases of inflected forms in Resources,
   conformant to [WX_morph.dtd] or [SL_morph.dtd] according to transliteration
   CAUTION. Update the dtd files when changing the tags or adding new tags. *)

(* What follows merges previous [Print_inflected] and [Morpho_xml] modules *)

open Skt_morph;
open Morphology; (* [inflected] and its constructors [Noun_form] ,... *)

value read_inflected file =
  (Gen.gobble file : Morphology.inflected_map)
;
value abort mess = do
  { output_string stderr mess
  ; flush stderr
  ; failwith "Print_inflected"
  }
;
value read_nouns () = 
  try read_inflected Data.nouns_file with 
  [ Sys_error s -> 
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.nouns_file ^ "\n" in 
    abort mess
  ]
and read_pronouns () = 
  try read_inflected Data.pronouns_file with 
  [ Sys_error s -> 
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.nouns_file ^ "\n" in 
    abort mess
  ]
and read_roots () = 
  try read_inflected Data.roots_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_roots ***\n"
                 ^ " to create " ^ Data.roots_file ^ "\n" in 
    abort mess
  ]
and read_parts () = 
  try read_inflected Data.parts_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_parts ***\n"
                 ^ " to create " ^ Data.parts_file ^ "\n" in 
    abort mess
  ]
and read_indecls () = 
  try read_inflected Data.indecls_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_parts ***\n"
                 ^ " to create " ^ Data.indecls_file ^ "\n" in 
    abort mess
  ]
and read_absya () = 
  try read_inflected Data.absya_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_parts ***\n"
                 ^ " to create " ^ Data.absya_file ^ "\n" in 
    abort mess
  ]
and read_abstvaa () = 
  try read_inflected Data.abstvaa_file with 
  [ Sys_error s -> 
    let mess = s ^ "\n\n *** First call make_parts ***\n"
                 ^ " to create " ^ Data.abstvaa_file ^ "\n" in 
    abort mess
  ]
and read_iics () = 
  try read_inflected Data.iics_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.iics_file ^ "\n" in 
    abort mess
  ]
and read_voca () = 
  try read_inflected Data.vocas_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.vocas_file ^ "\n" in 
    abort mess
  ]
and read_ifcs () = 
  try read_inflected Data.ifcs_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.ifcs_file ^ "\n" in 
    abort mess
  ]
and read_iivs () = 
  try read_inflected Data.iivs_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.iivs_file ^ "\n" in 
    abort mess
  ]
and read_avyayafs () = 
  try read_inflected Data.avyayafs_file with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_nouns ***\n"
                 ^ " to create " ^ Data.avyayafs_file ^ "\n" in 
    abort mess
  ]
(* NB [auxis_file] not needed - its forms are included in [roots_file] 
   and similarly ayayais forms are included in indecls. *)
and read_prevs () = 
  try (Gen.gobble Data.preverbs_file : Deco.deco Word.word) with 
  [ Sys_error s ->
    let mess = s ^ "\n\n *** First call make_prevs ***\n"
                 ^ " to create " ^ Data.preverbs_file ^ "\n" in 
    abort mess
  ]
;

value ps = print_string
;
value pl s = ps (s ^ "\n")
;
(* Examples (in SL1 transliteration) 
<f form="AGAtAt"><na><abl/><sg/><mas/></na><s stem="AGAta"/></f> 
<f form="aham"><na><nom/><sg/><dei/></na><s stem="aham"/></f>
<f form="patati"><v><cj><prim/></cj><t><pr/><gn>1</gn><ac/></t><np><sg/><trd/></np></v><s stem="pat"/></f>
*)

(* parametrization of transliteration scheme *)
value decode = fun
  [ "SL" -> Canon.decode_SL
  | "WX" -> Canon.decode_WX
  | "VH" -> Canon.decode2 (* takes care of possible hiatus *)
  | s -> failwith ("Unknown transliteration scheme" ^ s)
  ]
 ;
(* Paradigm parameters *)
(* kind attributes: present class of primary conjug aka ga.na, from 1 to 11 (vn)
           aorist kind, from 1 to 7
           pfp kind (gerundive) 1 -ya <gya>   2 -iiya <iya>  3 -tavya <tav> *)
value kind_attr k = " gn=\"" ^ string_of_int k ^ "\""
;
(* present class aka ga.na, from 1 to 11 *)
value pg k = if k>11 || k=0 (* redundant with conjugation *) then () 
             else ps (kind_attr k)
;
value print_number = fun 
  [ Singular -> ps "<sg/>" 
  | Dual     -> ps "<du/>"
  | Plural   -> ps "<pl/>"
  ]
and print_gender = fun 
  [ Mas -> ps "<mas/>"
  | Neu -> ps "<neu/>"
  | Fem -> ps "<fem/>" 
  | Deictic _ -> ps "<dei/>" 
  ]
and print_case = fun 
  [ Nom -> ps "<nom/>"
  | Acc -> ps "<acc/>"
  | Ins -> ps "<ins/>"
  | Dat -> ps "<dat/>"
  | Abl -> ps "<abl/>"
  | Gen -> ps "<gen/>" 
  | Loc -> ps "<loc/>"
  | Voc -> ps "<voc/>" 
  ]
and print_person = fun  
  [ First  -> ps "<fst/>" 
  | Second -> ps "<snd/>" 
  | Third  -> ps "<trd/>" 
  ]
and print_voice = fun
  [ Active  -> ps "<para/>" 
  | Middle  -> ps "<atma/>"
  | Passive -> ps "<pass/>"
  ] 
and print_conjugation cg = do
  { ps "<cj>"
  ; match cg with 
       [ Primary      -> ps "<prim/>"
       | Causative    -> ps "<ca/>"
       | Intensive    -> ps "<int/>"
       | Desiderative -> ps "<des/>"
       ]
  ; ps "</cj>"
  }
and print_pr_mode pr = do
  { ps "<md>"
  ; match pr with 
       [ Present    -> ps "<pr/>"
       | Imperative -> ps "<ip/>"
       | Optative   -> ps "<op/>"
       | Imperfect  -> ps "<im/>"
       ]
  ; ps "</md>"
  }
and print_tense = fun
  [ Future       -> ps "<fut/>"
  | Perfect      -> ps "<prf/>"
  | Aorist k     -> do { ps "<aor"; ps (kind_attr k); ps "/>" }
  | Injunctive k -> do { ps "<inj"; ps (kind_attr k); ps "/>" }
  | Benedictive  -> ps "<ben/>"
  | Conditional  -> ps "<cnd/>"
  | Subjunctive  -> ps "<subj/>" 
  ] 
;
value pfutp_kind = fun
  [ 1 -> "<gya/>"
  | 2 -> "<iya/>"
  | 3 -> "<tav/>"
  | n -> failwith ("Unknown pfutp kind " ^ string_of_int n)
  ]
;
value print_nominal = fun
  [ Ppp     -> ps "<ppp/>"
  | Pppa    -> ps "<ppa/>"
  | Ppra k  -> do { ps "<ppr"; pg k; ps ">"; print_voice Active; ps "</ppr>" } 
  | Pprm k  -> do { ps "<ppr"; pg k; ps ">"; print_voice Middle; ps "</ppr>" } 
  | Pprp    -> ps "<pprp/>"
  | Ppfta   -> do { ps "<ppft>"; print_voice Active; ps "</ppft>" }
  | Ppftm   -> do { ps "<ppft>"; print_voice Middle; ps "</ppft>" }
  | Pfuta   -> do { ps "<pfut>"; print_voice Active; ps "</pfut>" }
  | Pfutm   -> do { ps "<pfut>"; print_voice Middle; ps "</pfut>" }
  | Pfutp k -> do { ps "<pfutp"; ps (pfutp_kind k); ps "/>" }
  | _       -> ps "<act/>" (* action verbal nouns *)
  ]
;
value print_system = fun
  [ Conjug t v    -> do { ps "<tp>"; print_tense t; print_voice v; ps "</tp>" }
  | Presenta k pr -> do { ps "<prs"; pg k; ps ">"; 
                          print_pr_mode pr; ps "<para/></prs>" }
  | Presentm k pr -> do { ps "<prs"; pg k; ps ">"; 
                          print_pr_mode pr; ps "<atma/></prs>" }
  | Presentp pr   -> do { ps "<pas>"; print_pr_mode pr; ps "</pas>" }
  | Perfut v      -> do { ps "<pef>"; print_voice v; ps "</pef>" }
  ]
and print_invar = fun
  [ Infi   -> ps "<inf/>"
  | Absoya -> ps "<abs/>"
  | Perpft -> ps "<per/>"
  ]
;
(* Next 3 functions print conjugation in different order than [Print_dict] *)
value print_finite (c,p) = do 
  { print_conjugation c 
  ; ps "<sys>"; print_system p; ps "</sys>"
  }
 and print_verbal (c,n) = do 
  { print_conjugation c
  ; ps "<no>"; print_nominal n; ps "</no>"
  } 
 and print_modal (c,t) = do 
  { print_conjugation c
  ; ps "<iv>"; print_invar t; ps "</iv>"
  }
;
value print_morph = fun
  [ Noun_form g n c -> do
      { ps "<na>"
      ; print_case c
      ; print_number n
      ; print_gender g
      ; ps "</na>"
      }
  | Part_form v g n c -> do
      { ps "<pa><na>"
      ; print_case c
      ; print_number n
      ; print_gender g
      ; ps "</na>"
      ; ps "<kr>"
      ; print_verbal v
      ; ps "</kr></pa>"
      }
  | Verb_form f n p -> do
      { ps "<v>"
      ; print_finite f
      ; ps "<np>"
      ; print_number n
      ; print_person p
      ; ps "</np></v>"
      }
  | Ind_form k -> do
      { ps "<uf>"
      ; match k with
           [ Adv | Avya | Default -> ps "<ind/>"
           | Interj -> ps "<interj/>"
           | Part -> ps "<parti/>"
           | Prep -> ps "<prep/>"
           | Conj -> ps "<conj/>"
           | Tas  -> ps "<tasil/>"
           | Abs  -> () (* redundant absolutive forms *)
           | Infl -> () (* redundant inflected form *)
           | Nota -> () (* skipped grammatical notation *)
           ]
      ; ps "</uf>"
      }
  | Avyayaf_form -> ps "<avya/>"
  | Abs_root c -> do { ps "<ab>"; print_conjugation c; ps "</ab>" }
  | Bare_stem | Avyayai_form -> ps "<iic/>"
  | Gati -> ps "<iiv/>"
  | Ind_verb m -> do { ps "<vu>"; print_modal m ; ps "</vu>" }
  | _ -> failwith "Anomaly print_morph"
  ]
;
value print_inverse_map_xml trans form (delta,morphs) = 
  let print_skt s = ps ("\"" ^ s ^ "\"") in
  (*i for diacritics in UTF-8: ps (Transduction.skt_to_html s) i*)
  if Phonetics.phantomatic form then ((* phantomatic forms skipped *)) else do
  { ps "<f form="
  ; print_skt (decode trans form)
  ; ps ">"
  ; List.iter print_morph morphs
  ; ps "<s stem="
  ; print_skt (decode trans (Word.patch delta form)) 
  ; ps "/></f>\n"
  }
;
(* Outputs an XML stream on stdout *)
value print_header trans = do
  { pl "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  ; pl ("<!DOCTYPE forms SYSTEM \"" ^ trans ^ "_morph.dtd\">")
  ; pl "<!-- Header"
  ; ps "<meta name=\"title\" content=\"Sanskrit Morphology\""; pl "\">"
  ; ps "<meta name=\"author\" content=\""; ps Html.author_name; pl "\">"
  ; ps "<meta name=\"date\" content=\""; ps Date.dico_date; pl "\">"
  ; ps "<meta name=\"copyright\" content=\""; ps Html.copyright; pl "\">"
  ; pl "<meta name=\"keywords\" content=\"sanskrit; morphology\"> -->"
  }
;
value print_xml trans inflected_map = do
  { print_header trans
  ; pl "<forms>"
  ; Deco.iter (print_inverse_map_xml trans) inflected_map
  ; pl "</forms>"
  }
;
(* For printing preverb lists *)
value print_xml_word trans (w,_) = do
  { ps "<pv form="
  ; pl ("\"" ^ (decode trans w) ^ "\"/>")
  }
;
value print_xml_list trans pairs prevs = do
  { print_header trans
  ; pl "<forms>"
  ; let print_segment inflected_map = 
        Deco.iter (print_inverse_map_xml trans) inflected_map in
    List.iter print_segment pairs
  ; List.iter (print_xml_word trans) (Deco.contents prevs)
  ; pl "</forms>"
  }
;

(* Prints big XML stream to stdout *)
value print_xml_morphology trans = 
  let nouns = read_nouns () in print_xml trans nouns 
and print_xml_pn trans = 
  let pronouns = read_pronouns () in print_xml trans pronouns
and print_xml_r trans = 
  let verbs = read_roots () in print_xml trans verbs
and print_xml_p trans = 
  let parts = read_parts () in print_xml trans parts
and print_xml_a trans = 
  let indecls = read_indecls () in print_xml trans indecls
and print_xml_f trans = 
  let abstva = read_abstvaa () 
  and absya = read_absya () 
  and iics = read_iics () 
  and voca = read_voca () 
  and ifcs = read_ifcs () 
  and avya = read_avyayafs ()
  and iivs = read_iivs () 
  and prevs = read_prevs () in
  print_xml_list trans [ abstva; absya; iics; ifcs; avya; voca; iivs ] prevs
;

(* Analyse the transliteration argument to command [make_xml_data] *)
try Arg.parse [ ("-trans", Arg.String print_xml_morphology, "") ]
              (fun s -> raise (Arg.Bad s))
              "Usage : make_xml_data -trans t (where t is SL WX or VH)"
with [ Failure _ -> () ]
;
(*i end; i*)
