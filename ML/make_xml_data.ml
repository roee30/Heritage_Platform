(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Make_xml_data = struct  i*)
(* Prepares XML data banks from the databases of inflected forms in Resources,
   conformant to [WX_morph.dtd] or [SL_morph.dtd] according to transliteration
   CAUTION. Update the dtd files when changing the tags or adding new tags. *)
(* What follows merges previous [Print_inflected] and [Morpho_xml] modules *)
open Skt_morph
  
open Morphology
  
(* [inflected] and its constructors [Noun_form] ,... *)
let read_inflected file : Morphology.inflected_map = Gen.gobble file
  
let abort mess =
  (output_string stderr mess; flush stderr; failwith "Print_inflected")
  
let read_nouns () =
  try read_inflected Data.nouns_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.nouns_file ^ "\n")))
      in abort mess
and read_pronouns () =
  try read_inflected Data.pronouns_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.pronouns_file ^ "\n")))
      in abort mess
and read_roots () =
  try read_inflected Data.roots_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_roots ***\n" ^
             (" to create " ^ (Data.roots_file ^ "\n")))
      in abort mess
and read_parts () =
  try read_inflected Data.parts_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_parts ***\n" ^
             (" to create " ^ (Data.parts_file ^ "\n")))
      in abort mess
and read_indifcs () =
  try read_inflected Data.indifcs_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.indifcs_file ^ "\n")))
      in abort mess
and read_indecls () =
  try read_inflected Data.indecls_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.indecls_file ^ "\n")))
      in abort mess
and read_absya () =
  try read_inflected Data.absya_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_parts ***\n" ^
             (" to create " ^ (Data.absya_file ^ "\n")))
      in abort mess
and read_abstvaa () =
  try read_inflected Data.abstvaa_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_parts ***\n" ^
             (" to create " ^ (Data.abstvaa_file ^ "\n")))
      in abort mess
and read_iics () =
  try read_inflected Data.iics_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.iics_file ^ "\n")))
      in abort mess
and read_voca () =
  try read_inflected Data.vocas_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.vocas_file ^ "\n")))
      in abort mess
and read_ifcs () =
  try read_inflected Data.ifcs_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.ifcs_file ^ "\n")))
      in abort mess
and read_iivs () =
  try read_inflected Data.iivs_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.iivs_file ^ "\n")))
      in abort mess
and read_avyayafs () =
  try read_inflected Data.iivs_file
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_nouns ***\n" ^
             (" to create " ^ (Data.avyayafs_file ^ "\n")))
      in abort mess
and
  (* NB [auxis_file] not needed - its forms are included in [roots_file] 
   and similarly ayayais forms are included in indecls. *)
  read_prevs () =
  try (Gen.gobble Data.preverbs_file : Word.word Deco.deco)
  with
  | Sys_error s ->
      let mess =
        s ^
          ("\n\n *** First call make_prevs ***\n" ^
             (" to create " ^ (Data.preverbs_file ^ "\n")))
      in abort mess
  
(****************************************)
(* Now printing in XML format on stdout *)
(****************************************)
let ps = print_string
  
let pl s = ps (s ^ "\n")
  
(* Examples (in SL1 transliteration) 
<f form="AGAtAt"><na><abl/><sg/><mas/></na><s stem="AGAta"/></f> 
<f form="aham"><na><nom/><sg/><dei/></na><s stem="aham"/></f>
<f form="patati"><v><cj><prim/></cj><t><pr/><gn>1</gn><ac/></t><np><sg/><trd/></np></v><s stem="pat"/></f>
*)
(* parametrization of transliteration scheme *)
let decode =
  function
  | "SL" -> Canon.decode_SL
  | "WX" -> Canon.decode_WX
  | "VH" -> Canon.decode2
  | (* takes care of possible hiatus *) s ->
      failwith ("Unknown transliteration scheme" ^ s)
  
(* Paradigm parameters *)
(* kind attributes: present class of primary conjug aka ga.na, from 1 to 11 (vn)
           aorist kind, from 1 to 7
           pfp kind (gerundive) 1 -ya <gya>   2 -iiya <iya>  3 -tavya <tav> *)
let kind_attr k = " gn=\"" ^ ((string_of_int k) ^ "\"")
  
(* present class aka ga.na, from 1 to 11 *)
let pg k =
  if (k > 11) || (k = 0)
  then (* redundant with conjugation *) ()
  else (kind_attr k) |> ps
  
let print_number =
  function
  | Singular -> "<sg/>" |> ps
  | Dual -> "<du/>" |> ps
  | Plural -> "<pl/>" |> ps
and print_gender =
  function
  | Mas -> "<mas/>" |> ps
  | Neu -> "<neu/>" |> ps
  | Fem -> "<fem/>" |> ps
  | Deictic _ -> "<dei/>" |> ps
and print_case =
  function
  | Nom -> "<nom/>" |> ps
  | Acc -> "<acc/>" |> ps
  | Ins -> "<ins/>" |> ps
  | Dat -> "<dat/>" |> ps
  | Abl -> "<abl/>" |> ps
  | Gen -> "<gen/>" |> ps
  | Loc -> "<loc/>" |> ps
  | Voc -> "<voc/>" |> ps
and print_person =
  function
  | First -> "<fst/>" |> ps
  | Second -> "<snd/>" |> ps
  | Third -> "<trd/>" |> ps
and print_voice =
  function
  | Active -> "<para/>" |> ps
  | Middle -> "<atma/>" |> ps
  | Passive -> "<pass/>" |> ps
and print_conjugation cg =
  ("<cj>" |> ps;
   (match cg with
    | Primary -> "<prim/>" |> ps
    | Causative -> "<ca/>" |> ps
    | Intensive -> "<int/>" |> ps
    | Desiderative -> "<des/>" |> ps);
   "</cj>" |> ps)
and print_pr_mode pr =
  ("<md>" |> ps;
   (match pr with
    | Present -> "<pr/>" |> ps
    | Imperative -> "<ip/>" |> ps
    | Optative -> "<op/>" |> ps
    | Imperfect -> "<im/>" |> ps);
   "</md>" |> ps)
and print_tense =
  function
  | Future -> "<fut/>" |> ps
  | Perfect -> "<prf/>" |> ps
  | Aorist k -> ("<aor" |> ps; (kind_attr k) |> ps; "/>" |> ps)
  | Injunctive k -> ("<inj" |> ps; (kind_attr k) |> ps; "/>" |> ps)
  | Benedictive -> "<ben/>" |> ps
  | Conditional -> "<cnd/>" |> ps
  | Subjunctive -> "<subj/>" |> ps
  
let pfutp_kind =
  function
  | 1 -> "<gya/>"
  | 2 -> "<iya/>"
  | 3 -> "<tav/>"
  | n -> failwith ("Unknown pfutp kind " ^ (string_of_int n))
  
let print_nominal =
  function
  | Ppp -> "<ppp/>" |> ps
  | Pppa -> "<ppa/>" |> ps
  | Ppra k ->
      ("<ppr" |> ps; pg k; ">" |> ps; print_voice Active; "</ppr>" |> ps)
  | Pprm k ->
      ("<ppr" |> ps; pg k; ">" |> ps; print_voice Middle; "</ppr>" |> ps)
  | Pprp -> "<pprp/>" |> ps
  | Ppfta -> ("<ppft>" |> ps; print_voice Active; "</ppft>" |> ps)
  | Ppftm -> ("<ppft>" |> ps; print_voice Middle; "</ppft>" |> ps)
  | Pfuta -> ("<pfut>" |> ps; print_voice Active; "</pfut>" |> ps)
  | Pfutm -> ("<pfut>" |> ps; print_voice Middle; "</pfut>" |> ps)
  | Pfutp k -> ("<pfutp>" |> ps; (pfutp_kind k) |> ps; "</pfutp>" |> ps)
  | _ -> "<act/>" |> ps
  
(* action verbal nouns *)
let print_system =
  function
  | Conjug (t, v) ->
      ("<tp>" |> ps; print_tense t; print_voice v; "</tp>" |> ps)
  | Presenta (k, pr) ->
      ("<prs" |> ps;
       pg k;
       ">" |> ps;
       print_pr_mode pr;
       "<para/></prs>" |> ps)
  | Presentm (k, pr) ->
      ("<prs" |> ps;
       pg k;
       ">" |> ps;
       print_pr_mode pr;
       "<atma/></prs>" |> ps)
  | Presentp pr -> ("<pas>" |> ps; print_pr_mode pr; "</pas>" |> ps)
  | Perfut v -> ("<pef>" |> ps; print_voice v; "</pef>" |> ps)
and print_invar =
  function
  | Infi -> "<inf/>" |> ps
  | Absoya -> "<abs/>" |> ps
  | Perpft -> "<per/>" |> ps
  
(* Next 3 functions print conjugation in different order than [Print_dict] *)
let print_finite (c, p) =
  (print_conjugation c; "<sys>" |> ps; print_system p; "</sys>" |> ps)
and print_verbal (c, n) =
  (print_conjugation c; "<no>" |> ps; print_nominal n; "</no>" |> ps)
and print_modal (c, t) =
  (print_conjugation c; "<iv>" |> ps; print_invar t; "</iv>" |> ps)
  
let print_morph =
  function
  | Noun_form (g, n, c) ->
      ("<na>" |> ps;
       print_case c;
       print_number n;
       print_gender g;
       "</na>" |> ps)
  | Part_form (v, g, n, c) ->
      ("<pa><na>" |> ps;
       print_case c;
       print_number n;
       print_gender g;
       "</na>" |> ps;
       "<kr>" |> ps;
       print_verbal v;
       "</kr></pa>" |> ps)
  | Verb_form (f, n, p) ->
      ("<v>" |> ps;
       print_finite f;
       "<np>" |> ps;
       print_number n;
       print_person p;
       "</np></v>" |> ps)
  | Ind_form k ->
      ("<uf>" |> ps;
       (match k with
        | Adv | Avya | Default -> "<ind/>" |> ps
        | Interj -> "<interj/>" |> ps
        | Part -> "<parti/>" |> ps
        | Prep -> "<prep/>" |> ps
        | Conj -> "<conj/>" |> ps
        | Tas -> "<tasil/>" |> ps
        | Abs -> ()
        | (* redundant absolutive forms *) Infl -> ()
        | (* redundant inflected form *) Nota -> ());
       (* skipped grammatical notation *)
       "</uf>" |> ps)
  | Avyayaf_form -> "<avya/>" |> ps
  | Abs_root c -> ("<ab>" |> ps; print_conjugation c; "</ab>" |> ps)
  | Bare_stem | Avyayai_form -> "<iic/>" |> ps
  | Gati -> "<iiv/>" |> ps
  | Ind_verb m -> ("<vu>" |> ps; print_modal m; "</vu>" |> ps)
  | _ -> failwith "Anomaly print_morph"
  
let print_inverse_map_xml trans form (delta, morphs) =
  let print_skt s = ("\"" ^ (s ^ "\"")) |> ps
  in
    (*i for diacritics in UTF-8: Transduction.skt_to_html s |> ps i*)
    if Phonetics.phantomatic form
    then ()
    else (* phantomatic forms skipped *)
      ("<f form=" |> ps;
       print_skt (decode trans form);
       ">" |> ps;
       List.iter print_morph morphs;
       "<s stem=" |> ps;
       print_skt (decode trans (Word.patch delta form));
       "/></f>\n" |> ps)
  
(* Outputs an XML stream on stdout *)
let print_xml_header trans =
  ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" |> pl;
   ("<!DOCTYPE forms SYSTEM \"" ^ (trans ^ "_morph.dtd\">")) |> pl;
   "<!-- Header" |> pl;
   "<meta name=\"title\" content=\"Sanskrit Morphology\"" |> ps;
   "\">" |> pl;
   "<meta name=\"author\" content=\"" |> ps;
   Html.author_name |> ps;
   "\">" |> ps;
   "<meta name=\"date\" content=\"" |> ps;
   Date.dico_date |> ps;
   "\">" |> pl;
   "<meta name=\"copyright\" content=\"" |> ps;
   Html.copyright |> ps;
   "\">" |> pl;
   "<meta name=\"keywords\" content=\"sanskrit; morphology\"> -->" |> pl)
  
let print_xml trans inflected_map =
  (print_xml_header trans;
   "<forms>" |> pl;
   Deco.iter (print_inverse_map_xml trans) inflected_map;
   "</forms>" |> pl)
  
(* For printing preverb lists *)
let print_xml_word trans (w, _) =
  ("<pv form=" |> ps; ("\"" ^ ((decode trans w) ^ "\"/>")) |> pl)
  
let print_xml_list trans banks prevs =
  (print_xml_header trans;
   "<forms>" |> pl;
   (let print_bank inflected_map =
      Deco.iter (print_inverse_map_xml trans) inflected_map
    in List.iter print_bank banks);
   List.iter (print_xml_word trans) (Deco.contents prevs);
   "</forms>" |> pl)
  
(* Prints big XML stream to stdout *)
let print_xml_morphology trans =
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
  and indifcs = read_indifcs ()
  and avya = read_avyayafs ()
  and iivs = read_iivs ()
  and prevs = read_prevs ()
  in
    print_xml_list trans
      [ nouns; pronouns; verbs; parts; indecls; abstva; absya; iics; ifcs;
        indifcs; avya; voca; iivs ]
      prevs
  
(* Analyse the transliteration argument to command [make_xml_data] *)
let _ =
  try
    Arg.parse [ ("-trans", (Arg.String print_xml_morphology), "") ]
      (fun s -> raise (Arg.Bad s))
      "Usage : make_xml_data -trans t (where t is SL WX or VH)"
  with | Failure _ -> ()
  

