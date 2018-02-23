(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Linearizes morphological information as a string.
   Used in [Morpho, Morpho_tex, Lexer]. *)

open Skt_morph;
open Morphology; (* [inflected], [Noun_form], ... *)

value gana_str k = 
  if k=11 then " [vn.]"
  else if k>10 (* redundant with conjugation *) then ""
  else if k=0 then failwith "gana_str"
  else " ["  ^ string_of_int k ^ "]"
;
value string_voice = fun 
  [ Active  -> " ac." 
  | Middle  -> " md." 
  | Passive -> " ps." 
  ] 
and string_conjugation = fun
  [ Primary      -> ""
  | Causative    -> "ca. "
  | Intensive    -> "int. "
  | Desiderative -> "des. "
  ]
and string_nominal = fun
  [ Ppp     -> "pp."
  | Pppa    -> "ppa."
  | Ppra k  -> "ppr." ^ (gana_str k) ^ " ac." 
  | Pprm k  -> "ppr." ^ (gana_str k) ^ " md." 
  | Pprp    -> "ppr." ^ " ps." 
  | Ppfta   -> "ppf." ^ " ac." 
  | Ppftm   -> "ppf." ^ " md." 
  | Pfuta   -> "pfu." ^ " ac." 
  | Pfutm   -> "pfu." ^ " md." 
  | Pfutp k -> "pfp." ^ (gana_str k)
  | Action_noun -> "act."
  ]
and string_tense = fun
  [ Future       -> "fut."
  | Perfect      -> "pft."
  | Aorist k     -> "aor." ^ (gana_str k)
  | Injunctive k -> "inj." ^ (gana_str k)
  | Conditional  -> "cond."
  | Benedictive  -> "ben."
  ]
and string_case = fun 
  [ Nom -> "nom."
  | Acc -> "acc."
  | Ins -> "i."
  | Dat -> "dat."
  | Abl -> "abl."
  | Gen -> "g."
  | Loc -> "loc."
  | Voc -> "voc." 
  ] 
and string_number = fun 
  [ Singular -> " sg. " 
  | Dual     -> " du. "
  | Plural   -> " pl. "
  ]
and string_gender = fun 
  [ Mas -> "m."
  | Neu -> "n."
  | Fem -> "f." 
  | Deictic _ -> "*" 
  ]
and string_pr_mode = fun
  [ Present    -> "pr."
  | Imperative -> "imp."
  | Optative   -> "opt."
  | Imperfect  -> "impft."
  ] 
and string_person = fun 
  [ First  -> "1" 
  | Second -> "2" 
  | Third  -> "3" 
  ] 
and string_ind_kind = fun
  [ Part -> "part."
  | Prep -> "prep."
  | Conj -> "conj."
  | Abs  -> "abs."
  | Adv  -> "adv."
  | Tas  -> "tasil"
  | _    -> "ind."
  ]
and string_invar = fun
  [ Infi   -> "inf." 
  | Absoya -> "abs." 
  | Perpft -> "per. pft."
  ]
;
value string_paradigm = fun
  [ Conjug t v    -> (string_tense t) ^ (string_voice v)
  | Presenta k pr -> (string_pr_mode pr) ^ (gana_str k) ^ " ac."
  | Presentm k pr -> (string_pr_mode pr) ^ (gana_str k) ^ " md."
  | Presentp pr   -> (string_pr_mode pr ) ^ " ps."
  | Perfut v      -> "per. fut." ^ (string_voice v)
  ]
;
value string_finite (c,p) = (string_conjugation c) ^ (string_paradigm p)
and   string_verbal (c,n) = (string_conjugation c) ^ (string_nominal n)
and   string_modal  (c,i) = (string_conjugation c) ^ (string_invar i)
;
value string_morph = fun
  [ Noun_form g n c 
  | Part_form _ g n c -> (string_case c) ^ (string_number n) ^ (string_gender g)
  | Bare_stem | Avyayai_form -> "iic."
  | Avyayaf_form -> "ind."
  | Verb_form f n p -> (string_finite f) ^ (string_number n) ^ (string_person p)
  | Ind_form k -> string_ind_kind k
  | Abs_root c -> (string_conjugation c) ^ "abs."
  | Auxi_form ->  "iiv."
  | Ind_verb m -> string_modal m
  | Unanalysed -> "?"
  | PV pvs -> "pv." 
  ]
;
(* end; *)

