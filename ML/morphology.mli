(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©20189 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Morphology interface *)

(* Used by [Inflected] for inflective morphology generation, 
   and by [Morpho] for further treatment *)

open Skt_morph;

(* module Morphology : sig *)

type inflexion_tag = (* vibhakti *)
  [ Noun_form of gender and number and case             (* declined nominal *)
  | Part_form of verbal and gender and number and case  (* declined participle *)
  | Bare_stem    (* iic forms *)
  | Avyayai_form (* iic forms of avyayiibhaava cpds *)
  | Avyayaf_form (* ifc forms of avyayiibhaava cpds *)
  | Verb_form of finite and number and person  (* finite conjugated root forms *)
  | Ind_form of ind_kind                 (* indeclinable forms: prep, adv, etc *)
  | Ind_verb of modal                    (* indeclinable inf abs-ya and perpft *)
  | Abs_root of conjugation              (* abs-tvaa *)
  | Auxi_form                            (* verbal auxiliaries forms *)
  | Unanalysed (* un-analysable segments *)
  | PV of list string (* Preverb sequences *)
  (* NB preverb sequences are collated separately by [Roots] module, and they do not 
     appear in solutions, removed by compression of [Dispatcher.validate]. *)
  ]
and inflexions = list inflexion_tag
;
type inflected_map = Lexmap.lexmap inflexions
and lemma = Lexmap.inverse inflexions
and lemmas = list lemma
;
type unitag = (Word.delta * inflexions)
and multitag = list unitag
;
type morphology = 
  { nouns    : inflected_map 
  ; nouns2   : inflected_map
  ; prons    : inflected_map  
  ; roots    : inflected_map
  ; krids    : inflected_map  
  ; voks     : inflected_map 
  ; lopas    : inflected_map 
  ; lopaks   : inflected_map 
  ; indes    : inflected_map 
  ; absya    : inflected_map 
  ; abstvaa  : inflected_map 
  ; iics2    : inflected_map
  ; iics     : inflected_map 
  ; iifs     : inflected_map
  ; iiks     : inflected_map 
  ; iivs     : inflected_map
  ; peris    : inflected_map 
  ; auxis    : inflected_map 
  ; auxiks   : inflected_map 
  ; auxiicks : inflected_map 
  ; vocas    : inflected_map
  ; invs     : inflected_map 
  ; ifcs     : inflected_map
  ; ifcs2    : inflected_map 
  ; inftu    : inflected_map
  ; kama     : inflected_map
  ; iiys     : inflected_map
  ; avys     : inflected_map 
  ; caches   : inflected_map
  }
;

(* end; *)

