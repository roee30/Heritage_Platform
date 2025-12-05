(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Morphology interface *)
(* Used by [Inflected] for inflective morphology generation, 
   and by [Morpho] for further treatment *)
open Skt_morph
  
(* module Morphology : sig *)
type inflexion_tag =
  (* vibhakti *)
  | Noun_form of gender * number * case
  | (* declined nominal *)
  Part_form of verbal * gender * number * case
  | (* declined participle *)
  Bare_stem
  | (* iic forms *)
  Avyayai_form
  | (* iic forms of avyayiibhaava cpds *)
  Avyayaf_form
  | (* ifc forms of avyayiibhaava cpds *)
  Verb_form of finite * number * person
  | (* finite conjugated root forms *)
  Ind_form of ind_kind
  | (* indeclinable forms: prep, adv, etc *)
  Ind_verb of modal
  | (* indeclinable inf abs-ya and perpft *)
  Abs_root of conjugation
  | (* abs-tvaa *)
  Gati
  | (* iiv verbal auxiliaries forms *)
  Unanalysed
  | (* un-analysable segments *)
  PV of string list
  and (* Preverb sequences *)
  (* NB preverb sequences are collated separately by [Roots] module, 
     and they do not appear in solutions, they are removed by compression 
     of [Dispatcher.validate]. *)
  inflexions =
  inflexion_tag list

type inflected_map =
  inflexions Lexmap.lexmap
  and lemma =
  inflexions Lexmap.inverse
  and lemmas =
  lemma list

type unitag = (Word.delta * inflexions) and multitag = unitag list

type morphology =
  { nouns : inflected_map; prons : inflected_map; roots : inflected_map;
    krids : inflected_map; voks : inflected_map; lopas : inflected_map;
    lopaks : inflected_map; indes : inflected_map; indifcs : inflected_map;
    absya : inflected_map; abstvaa : inflected_map; iics : inflected_map;
    iifs : inflected_map; iiks : inflected_map; iivs : inflected_map;
    peris : inflected_map; auxis : inflected_map; auxiinvs : inflected_map;
    auxiks : inflected_map; auxiicks : inflected_map; vocas : inflected_map;
    invs : inflected_map; ifcs : inflected_map; inftu : inflected_map;
    kama : inflected_map; vocaf : inflected_map; iiys : inflected_map;
    avys : inflected_map; caches : inflected_map; cacheis : inflected_map
  }


