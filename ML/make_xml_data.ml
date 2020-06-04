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
  try read_inflected Data.iivs_file with 
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
(****************************************)
(* Now printing in XML format on stdout *)
(****************************************)

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
             else kind_attr k |> ps
;
value print_number = fun 
  [ Singular -> "<sg/>" |> ps
  | Dual     -> "<du/>" |> ps
  | Plural   -> "<pl/>" |> ps
  ]
and print_gender = fun 
  [ Mas       -> "<mas/>" |> ps
  | Neu       -> "<neu/>" |> ps
  | Fem       -> "<fem/>" |> ps
  | Deictic _ -> "<dei/>" |> ps
  ]
and print_case = fun 
  [ Nom -> "<nom/>" |> ps
  | Acc -> "<acc/>" |> ps
  | Ins -> "<ins/>" |> ps
  | Dat -> "<dat/>" |> ps
  | Abl -> "<abl/>" |> ps
  | Gen -> "<gen/>" |> ps
  | Loc -> "<loc/>" |> ps
  | Voc -> "<voc/>" |> ps
  ]
and print_person = fun  
  [ First  -> "<fst/>" |> ps
  | Second -> "<snd/>" |> ps
  | Third  -> "<trd/>" |> ps
  ]
and print_voice = fun
  [ Active  -> "<para/>" |> ps
  | Middle  -> "<atma/>" |> ps
  | Passive -> "<pass/>" |> ps
  ] 
and print_conjugation cg = do
  { "<cj>" |> ps
  ; match cg with 
       [ Primary      -> "<prim/>" |> ps
       | Causative    -> "<ca/>"   |> ps
       | Intensive    -> "<int/>"  |> ps
       | Desiderative -> "<des/>"  |> ps
       ]
  ; "</cj>" |> ps
  }
and print_pr_mode pr = do
  { "<md>" |> ps
  ; match pr with 
       [ Present    -> "<pr/>" |> ps
       | Imperative -> "<ip/>" |> ps
       | Optative   -> "<op/>" |> ps
       | Imperfect  -> "<im/>" |> ps
       ]
  ; "</md>" |> ps
  }
and print_tense = fun
  [ Future       -> "<fut/>" |> ps
  | Perfect      -> "<prf/>" |> ps
  | Aorist k     -> do { "<aor" |> ps; kind_attr k |> ps; "/>" |> ps }
  | Injunctive k -> do { "<inj" |> ps; kind_attr k |> ps; "/>" |> ps }
  | Benedictive  -> "<ben/>" |> ps
  | Conditional  -> "<cnd/>" |> ps
  | Subjunctive  -> "<subj/>" |> ps
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
  [ Ppp     -> "<ppp/>" |> ps
  | Pppa    -> "<ppa/>" |> ps
  | Ppra k  -> do { "<ppr" |> ps; pg k; ">" |> ps; print_voice Active; "</ppr>" |> ps } 
  | Pprm k  -> do { "<ppr" |> ps; pg k; ">" |> ps; print_voice Middle; "</ppr>" |> ps } 
  | Pprp    -> "<pprp/>" |> ps
  | Ppfta   -> do { "<ppft>" |> ps; print_voice Active; "</ppft>" |> ps }
  | Ppftm   -> do { "<ppft>" |> ps; print_voice Middle; "</ppft>" |> ps }
  | Pfuta   -> do { "<pfut>" |> ps; print_voice Active; "</pfut>" |> ps }
  | Pfutm   -> do { "<pfut>" |> ps; print_voice Middle; "</pfut>" |> ps }
  | Pfutp k -> do { "<pfutp>" |> ps; pfutp_kind k |> ps; "</pfutp>" |> ps }
  | _       -> "<act/>" |> ps (* action verbal nouns *)
  ]
;
value print_system = fun
  [ Conjug t v    -> do { "<tp>" |> ps; print_tense t; print_voice v; "</tp>" |> ps }
  | Presenta k pr -> do { "<prs" |> ps; pg k; ">" |> ps; 
                          print_pr_mode pr; "<para/></prs>" |> ps }
  | Presentm k pr -> do { "<prs" |> ps; pg k; ">" |> ps; 
                          print_pr_mode pr; "<atma/></prs>" |> ps }
  | Presentp pr   -> do { "<pas>" |> ps; print_pr_mode pr; "</pas>" |> ps }
  | Perfut v      -> do { "<pef>" |> ps; print_voice v; "</pef>" |> ps }
  ]
and print_invar = fun
  [ Infi   -> "<inf/>" |> ps
  | Absoya -> "<abs/>" |> ps
  | Perpft -> "<per/>" |> ps
  ]
;
(* Next 3 functions print conjugation in different order than [Print_dict] *)
value print_finite (c,p) = do 
  { print_conjugation c 
  ; "<sys>" |> ps; print_system p; "</sys>" |> ps
  }
 and print_verbal (c,n) = do 
  { print_conjugation c
  ; "<no>" |> ps; print_nominal n; "</no>" |> ps
  } 
 and print_modal (c,t) = do 
  { print_conjugation c
  ; "<iv>" |> ps; print_invar t; "</iv>" |> ps
  }
;
value print_morph = fun
  [ Noun_form g n c -> do
      { "<na>" |> ps
      ; print_case c
      ; print_number n
      ; print_gender g
      ; "</na>" |> ps
      }
  | Part_form v g n c -> do
      { "<pa><na>" |> ps
      ; print_case c
      ; print_number n
      ; print_gender g
      ; "</na>" |> ps
      ; "<kr>" |> ps
      ; print_verbal v
      ; "</kr></pa>" |> ps
      }
  | Verb_form f n p -> do
      { "<v>" |> ps
      ; print_finite f
      ; "<np>" |> ps
      ; print_number n
      ; print_person p
      ; "</np></v>" |> ps
      }
  | Ind_form k -> do
      { "<uf>" |> ps
      ; match k with
           [ Adv | Avya | Default -> "<ind/>" |> ps
           | Interj -> "<interj/>" |> ps
           | Part -> "<parti/>" |> ps
           | Prep -> "<prep/>" |> ps
           | Conj -> "<conj/>" |> ps
           | Tas  -> "<tasil/>" |> ps
           | Abs  -> () (* redundant absolutive forms *)
           | Infl -> () (* redundant inflected form *)
           | Nota -> () (* skipped grammatical notation *)
           ]
      ; "</uf>" |> ps
      }
  | Avyayaf_form -> "<avya/>" |> ps
  | Abs_root c -> do { "<ab>" |> ps; print_conjugation c; "</ab>" |> ps }
  | Bare_stem | Avyayai_form -> "<iic/>" |> ps
  | Gati -> "<iiv/>" |> ps
  | Ind_verb m -> do { "<vu>" |> ps; print_modal m ; "</vu>" |> ps }
  | _ -> failwith "Anomaly print_morph"
  ]
;
value print_inverse_map_xml trans form (delta,morphs) = 
  let print_skt s = "\"" ^ s ^ "\"" |> ps in
  (*i for diacritics in UTF-8: Transduction.skt_to_html s |> ps i*)
  if Phonetics.phantomatic form then ((* phantomatic forms skipped *)) else do
  { "<f form=" |> ps
  ; print_skt (decode trans form)
  ; ">" |> ps
  ; List.iter print_morph morphs
  ; "<s stem=" |> ps
  ; print_skt (decode trans (Word.patch delta form)) 
  ; "/></f>\n" |> ps
  }
;
(* Outputs an XML stream on stdout *)
value print_xml_header trans = do
  { "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" |> pl
  ; "<!DOCTYPE forms SYSTEM \"" ^ trans ^ "_morph.dtd\">" |> pl
  ; "<!-- Header" |> pl
  ; "<meta name=\"title\" content=\"Sanskrit Morphology\"" |> ps; "\">" |> pl
  ; "<meta name=\"author\" content=\"" |> ps; Html.author_name |> ps; "\">" |> ps
  ; "<meta name=\"date\" content=\"" |> ps; Date.dico_date |> ps; "\">" |> pl
  ; "<meta name=\"copyright\" content=\"" |> ps; Html.copyright |> ps; "\">" |> pl
  ; "<meta name=\"keywords\" content=\"sanskrit; morphology\"> -->" |> pl
  }
;
value print_xml trans inflected_map = do
  { print_xml_header trans
  ; "<forms>" |> pl
  ; Deco.iter (print_inverse_map_xml trans) inflected_map
  ; "</forms>" |> pl
  }
;
(* For printing preverb lists *)
value print_xml_word trans (w,_) = do
  { "<pv form=" |> ps
  ; "\"" ^ (decode trans w) ^ "\"/>" |> pl
  }
;
value print_xml_list trans banks prevs = do
  { print_xml_header trans
  ; "<forms>" |> pl
  ; let print_bank inflected_map = 
        Deco.iter (print_inverse_map_xml trans) inflected_map in
    List.iter print_bank banks
  ; List.iter (print_xml_word trans) (Deco.contents prevs)
  ; "</forms>" |> pl
  }
;

(* Prints big XML stream to stdout *)
value print_xml_morphology trans = 
  let nouns = read_nouns () 
  and pronouns = read_pronouns () 
  and verbs = read_roots () 
  and parts = read_parts () 
  and indecls = read_indecls () 
  and abstva = read_abstvaa () 
  and absya = read_absya () 
  and iics = read_iics () 
  and voca = read_voca () 
  and ifcs = read_ifcs () 
  and avya = read_avyayafs ()
  and iivs = read_iivs () 
  and prevs = read_prevs () in
  print_xml_list trans [ nouns; pronouns; verbs; parts; indecls; abstva; absya;
                         iics; ifcs; avya; voca; iivs ] prevs
;

(* Analyse the transliteration argument to command [make_xml_data] *)
try Arg.parse [ ("-trans", Arg.String print_xml_morphology, "") ]
              (fun s -> raise (Arg.Bad s))
              "Usage : make_xml_data -trans t (where t is SL WX or VH)"
with [ Failure _ -> () ]
;
(*i end; i*)
