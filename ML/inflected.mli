(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Inflected : sig i*)

open Skt_morph;
open Morphology;
open Naming;

value register_krid : Word.word -> homo_krid -> unit; 
value access_krid : Word.word -> list homo_krid; 

value admits_aa : ref bool;
value morpho_gen : ref bool
;
value nouns    : ref inflected_map;
value pronouns : ref inflected_map;
value vocas    : ref inflected_map;
value iics     : ref inflected_map;
value avyayais : ref inflected_map;
value avyayafs : ref inflected_map;
value piics    : ref inflected_map;
value iivs     : ref inflected_map;
value peri     : ref inflected_map;
value auxi     : ref inflected_map;
value auxik    : ref inflected_map;
value auxiick  : ref inflected_map;
value undecls  : ref inflected_map;
value invs     : ref inflected_map;
value absya    : ref inflected_map;
value abstvaa  : ref inflected_map;
value parts    : ref inflected_map;
value partvocs : ref inflected_map;
value roots    : ref inflected_map;
value lopas    : ref inflected_map;
value lopaks   : ref inflected_map;
value inftu    : ref inflected_map;
value kama     : ref inflected_map;
value preverbs : ref (Deco.deco Word.word);

value lexicalized_kridantas : ref deco_krid;
value unique_kridantas : ref deco_krid;

(* Inflectional categories *)
type nominal = 
  [ Noun (* lexicalized stem - noun, adjective or number *)
  | Pron (* lexicalized stem - pronoun *)
  | Krid of verbal and string (* kridantas of roots *)
  ]
;
type flexion =
  [ Declined of nominal and gender and list (number * list (case * Word.word))
  | Conju of finite and list (number * list (person * Word.word))
  | Undecl of und_kind and Word.word
  | Bare of nominal and Word.word
  | Avyayai of Word.word (* Iic of avyayiibhaava cpd *)
  | Avyayaf of Word.word (* Ifc of avyayiibhaava cpd *)
  | Cvi of Word.word  
  | Preverb of Word.word and list Word.word
  | Invar of modal and Word.word          (* inf abs-ya perpft *)
  | Inftu of conjugation and Word.word    (* infinitive in -tu *)
  | Absotvaa of conjugation and Word.word (* abs-tvaa *)
  ]
;
value enter1 : string -> flexion -> unit
;
value enter : string -> list flexion -> unit
;
value enter_form : Word.word -> flexion -> unit
;
value enter_forms : Word.word -> list flexion -> unit
;
value nominal_databases : unit -> 
  (inflected_map * inflected_map * inflected_map * inflected_map * inflected_map)
;
value reset_nominal_databases : unit -> unit
;
(*i end; i*)
