(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                     Gérard Huet & Sriram Krishnan                      *)
(*                                                                        *)
(* ©2024 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Linearizes morphological information as a string.
   Used in [Morpho, Morpho_tex, Lexer]. *)

open Skt_morph;
open Morphology; (* [inflected], [Noun_form], ... *)

value gana_str_roma k = 
  if k=11 then " [vn.]"
  else if k>10 (* redundant with conjugation *) then ""
  else if k=0 then failwith "gana_str_roma"
  else " ["  ^ string_of_int k ^ "]"
;
value str_voice_roma = fun 
  [ Active  -> " ac." 
  | Middle  -> " mo." 
  | Passive -> " ps." 
  ] 
and str_conjugation_roma = fun
  [ Primary      -> ""
  | Causative    -> "ca. "
  | Intensive    -> "int. "
  | Desiderative -> "des. "
  ]
and str_nominal_roma = fun
  [ Ppp     -> "pp."
  | Pppa    -> "ppa."
  | Ppra k  -> "ppr." ^ (gana_str_roma k) ^ " ac." 
  | Pprm k  -> "ppr." ^ (gana_str_roma k) ^ " mo." 
  | Pprp    -> "ppr." ^ " ps." 
  | Ppfta   -> "ppf." ^ " ac." 
  | Ppftm   -> "ppf." ^ " mo." 
  | Pfuta   -> "pfu." ^ " ac." 
  | Pfutm   -> "pfu." ^ " mo." 
  | Pfutp k -> "pfp." ^ (gana_str_roma k)
  | Action_noun -> "act."
  | Agent_noun  -> "agt."
  ]
and str_tense_roma = fun
  [ Future       -> "fut."
  | Future2      -> "per. fut."
  | Perfect      -> "pft."
  | Aorist k     -> "aor." ^ (gana_str_roma k)
  | Injunctive k -> "inj." ^ (gana_str_roma k)
  | Conditional  -> "cond."
  | Benedictive  -> "ben."
  | Subjunctive  -> "subj."
  ]
and str_case_roma = fun 
  [ Nom -> "nom."
  | Acc -> "acc."
  | Ins -> "i."
  | Dat -> "dat."
  | Abl -> "abl."
  | Gen -> "g."
  | Loc -> "loc."
  | Voc -> "voc." 
  ] 
and str_number_roma = fun 
  [ Singular -> " sg. " 
  | Dual     -> " du. "
  | Plural   -> " pl. "
  ]
and str_gender_roma = fun 
  [ Mas -> "m."
  | Neu -> "n."
  | Fem -> "f." 
  | Deictic _ -> "*" 
  ]
and str_pr_mode_roma = fun
  [ Present    -> "pr."
  | Imperative -> "imp."
  | Optative   -> "opt."
  | Imperfect  -> "impft."
  ] 
and str_person_roma = fun 
  [ First  -> "1" 
  | Second -> "2" 
  | Third  -> "3" 
  ] 
and str_ind_kind_roma = fun
  [ Part -> "part."
  | Prep -> "prep."
  | Conj -> "conj."
  | Abs  -> "abs." 
  | Adv  -> "adv."
  | Tas  -> "tasil"
  | _    -> "ind."
  ]
and str_invar_roma = fun
  [ Infi   -> "inf." 
  | Absoya | Absotvaa | Namul -> "abs." 
  | Perpft -> "per. pft."
  ]
;
value str_paradigm_roma = fun
  [ Conjug t v    -> (str_tense_roma t) ^ (str_voice_roma v)
  | Presenta k pr -> (str_pr_mode_roma pr) ^ (gana_str_roma k) ^ " ac."
  | Presentm k pr -> (str_pr_mode_roma pr) ^ (gana_str_roma k) ^ " mo."
  | Presentp pr   -> (str_pr_mode_roma pr ) ^ " ps."
(* OBS  | Perfut v      -> "per. fut." ^ (str_voice_roma v) *)
  ]
;
value str_finite_roma (c,p) = (str_conjugation_roma c) ^ (str_paradigm_roma p)
and   str_verbal_roma (c,n) = (str_conjugation_roma c) ^ (str_nominal_roma n)
and   str_modal_roma  (c,i) = (str_conjugation_roma c) ^ (str_invar_roma i)
;
value str_morph_roma = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> (str_gender_roma g) ^ (str_number_roma n) ^ (str_case_roma c) 
  | Bare_stem | Avyayai_form -> "iic."
  | Avyayaf_form -> "ind."
  | Verb_form f n p -> (str_finite_roma f) ^ (str_number_roma n) ^ (str_person_roma p)
  | Ind_form k -> str_ind_kind_roma k
  | Gati ->  "iiv."
  | Ind_verb m -> str_modal_roma m
  | Unanalysed -> "?"
  | PV pvs -> "pv." 
  ]
;
value gana_str_deva k =  
  if k=0 then failwith "gana_str_deva"
  else match k with
  [ 1 -> "भ्वादि"
  | 2 -> "अदादि"
  | 3 -> "जुहोत्यादि"
  | 4 -> "दिवादि"
  | 5 -> "स्वादि"
  | 6 -> "तुदाादि"
  | 7 -> "रुधादि"
  | 8 -> "तनादि"
  | 9 -> "क्र्यादि"
  | 10 -> "चुरादि"
  | 11 -> "नामधातु"
  | _  -> ""
  ] 
;
value pfp_type_deva k =
  if k=0 then failwith "pfp_type_deva"
  else match k with 
  [ 1 -> "यत्"
  | 2 -> "अनीयर्"
  | 3 -> "तव्य"
  | x -> "["  ^ string_of_int x ^ "]"
  ]
;
value aorist_type_deva k = 
  if k=0 then failwith "aorist_type_deva"
  else match k with 
  [ 1 -> "(सिच्-लुक्)" (* अभूः *)
  | 2 -> "(अ)" (* अगमम् *)
  | 3 -> "(चङ्)" (* अजीगमम् *)
  | 4 -> "(अनिट्-सिच्)" (* अश्रौषम् *)
  | 5 -> "(सेट्-सिच्)" (* अयाचिषम् *)
  | 6 -> "(सिष्)" (* अयासिषम् *)
  | 7 -> "(क्स)" (* आवृक्षम् *)
  | x -> "["  ^ string_of_int x ^ "]"
  ]
;

value str_voice_deva = fun 
  [ Active  -> "कर्तरि पप" 
  | Middle  -> "कर्तरि आप" 
  | Passive -> "कर्मणि आप" 
  ] 
and str_conjugation_deva = fun
  [ Primary      -> ""
  | Causative    -> "णिच् "
  | Intensive    -> "यङ् "
  | Desiderative -> "सन् "
  ]
and str_nominal_deva = fun
  [ Ppp     -> "क्त"
  | Pppa    -> "क्तवतु"
  | Ppra k  -> "शतृ_लट् " ^ (gana_str_deva k)
  | Pprm k  -> "शानच्_लट् " ^ (gana_str_deva k) 
  | Pprp    -> "शानच्_लट् "  ^ "कर्मणि"
  | Ppfta   -> "लिडादेश (क्वसु)" (* ^ " ac." (* क्वसु? *) *)
  | Ppftm   -> "लिडादेश (कानच्)"  (* ^ " mo." (* कानच्? *) *)
  | Pfuta   -> "शतृ_लृट्" 
  | Pfutm   -> "शानच्_लृट्" 
  | Pfutp k -> pfp_type_deva k
  | Action_noun -> "act."
  | Agent_noun  -> "agt."
  ]
and str_tense_deva = fun
  [ Future       -> "लृट्"
  | Future2      -> "लुट्"
  | Perfect      -> "लिट् (द्वित्व)"
  | Aorist k     -> "लुङ् " ^ (aorist_type_deva k)
  | Injunctive k -> "लुङ् " ^ (gana_str_deva k)
  | Conditional  -> "लृङ्"
  | Benedictive  -> "आशीर्लिङ्"
  | Subjunctive  -> "लेट्"
  ]
and str_case_deva = fun 
  [ Nom -> "1"
  | Acc -> "2"
  | Ins -> "3"
  | Dat -> "4"
  | Abl -> "5"
  | Gen -> "6"
  | Loc -> "7"
  | Voc -> "8" 
  ] 
and str_number_deva = fun 
  [ Singular -> "एक" 
  | Dual     -> "द्वि"
  | Plural   -> "बहु"
  ]
and str_gender_deva = fun 
  [ Mas -> "पुं"
  | Neu -> "नपुं"
  | Fem -> "स्त्री" 
  | Deictic _ -> "सर्व" 
  ]
and str_pr_mode_deva = fun
  [ Present    -> "लट्"
  | Imperative -> "लोट्"
  | Optative   -> "विधिलिङ्"
  | Imperfect  -> "लङ्"
  ] 
and str_person_deva = fun 
  [ First  -> "उ" (* t.rtiiya *)
  | Second -> "म" (* dvitiiya *)
  | Third  -> "प्र" (* prathama ! *)
  ] 
and str_ind_kind_deva = fun
  [ Part -> "अव्यय"
  | Prep -> "उपसर्ग"
  | Conj -> "अव्यय" (* or निपात *)
  | Adv  -> "अव्यय"
  | Tas  -> "तसिल्"
  | _    -> "अव्यय"
  ]
and str_invar_deva = fun
  [ Infi   -> "तुमुन्" 
  | Absoya -> "ल्यप्"
  | Absotvaa -> "क्त्वा"
  | Namul -> "णमुल्"
  | Perpft -> "लिट् (अनुप्रयोग)"
  ]
;
value str_paradigm_deva = fun
  [ Conjug t v    -> (str_tense_deva t) ^ " " ^ (str_voice_deva v)
  | Presenta k pr -> "कर्तरि " ^ (str_pr_mode_deva pr) ^ " पप " ^ (gana_str_deva k)
  | Presentm k pr -> "कर्तरि " ^ (str_pr_mode_deva pr) ^ " आप " ^ (gana_str_deva k)
  | Presentp pr   -> "कर्मणि " ^ (str_pr_mode_deva pr) ^ " आप"
(* OBS  | Perfut v      -> "लुट्" ^ (str_voice_deva v) *)
  ]
;
value str_finite_deva (c,p) = (str_conjugation_deva c) ^ (str_paradigm_deva p)
and   str_verbal_deva (c,n) = (str_conjugation_deva c) ^ (str_nominal_deva n)
and   str_modal_deva  (c,i) = (str_conjugation_deva c) ^ (str_invar_deva i)
;
value str_morph_deva = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> (str_gender_deva g) ^ " " ^ (str_case_deva c) ^ " " ^ (str_number_deva n)
  | Bare_stem | Avyayai_form -> "पूर्वपद"
  | Avyayaf_form -> "अव्यय"
  | Verb_form f n p -> (str_finite_deva f) ^ " " ^ (str_person_deva p) ^ " " ^ (str_number_deva n)
  | Ind_form k -> str_ind_kind_deva k
  | Gati -> "च्वि" 
  | Ind_verb m -> str_modal_deva m
  | Unanalysed -> "?"
  | PV pvs -> "उपसर्ग" 
  ]
;

(* end; *)

