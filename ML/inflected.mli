(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Inflected : sig i*)
open Skt_morph
  
open Morphology
  
open Naming
  
val register_krid : Word.word -> homo_krid -> unit
  
val access_krid : Word.word -> homo_krid list
  
val admits_aa : bool ref
  
val morpho_gen : bool ref
  
val nouns : inflected_map ref
  
val pronouns : inflected_map ref
  
val vocas : inflected_map ref
  
val iics : inflected_map ref
  
val avyayais : inflected_map ref
  
val avyayafs : inflected_map ref
  
val piics : inflected_map ref
  
val iivs : inflected_map ref
  
val peri : inflected_map ref
  
val auxi : inflected_map ref
  
val auxiinv : inflected_map ref
  
val auxik : inflected_map ref
  
val auxiick : inflected_map ref
  
val indecls : inflected_map ref
  
val indifcs : inflected_map ref
  
val invs : inflected_map ref
  
val absya : inflected_map ref
  
val abstvaa : inflected_map ref
  
val parts : inflected_map ref
  
val partvocs : inflected_map ref
  
val roots : inflected_map ref
  
val lopas : inflected_map ref
  
val lopaks : inflected_map ref
  
val inftu : inflected_map ref
  
val kama : inflected_map ref
  
val preverbs : (Word.word Deco.deco) ref
  
val lexicalized_kridantas : deco_krid ref
  
val unique_kridantas : deco_krid ref
  
(* Inflectional categories *)
type nominal =
  | Noun
  | (* lexicalized stem - noun, adjective or number *)
  Pron
  | (* lexicalized stem - pronoun *)
  Krid of verbal * string

(* kridantas of roots *)
type flexion =
  | Declined of nominal * gender * (number * ((case * Word.word) list)) list
  | Conju of finite * (number * ((person * Word.word) list)) list
  | Indecl of ind_kind * Word.word
  | Indifc of ind_kind * Word.word
  | Bare of nominal * Word.word
  | Avyayai of Word.word
  | (* Iic of avyayiibhaava cpd *)
  Avyayaf of Word.word
  | (* Ifc of avyayiibhaava cpd *)
  Cvi of Word.word
  | Preverb of Word.word * Word.word list
  | Invar of modal * Word.word
  | (* inf abs-ya perpft *)
  Inftu of conjugation * Word.word
  | (* infinitive in -tu *)
  Absotvaa of conjugation * Word.word

(* abs-tvaa *)
val enter1 : string -> flexion -> unit
  
val enter : string -> flexion list -> unit
  
val enter_form : Word.word -> flexion -> unit
  
val enter_forms : Word.word -> flexion list -> unit
  
val nominal_databases :
  unit ->
    (inflected_map * inflected_map * inflected_map * inflected_map *
     inflected_map)
  
val reset_nominal_databases : unit -> unit
  

