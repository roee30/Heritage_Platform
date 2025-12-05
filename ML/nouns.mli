(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Nouns : sig i*)
open Skt_morph
  
open Morphology
  
(* [inflected_map] *)
type declension_class =
  | Gender of gender
  | (* declined substantive, adjective, number, pronoun *)
  Ind of ind_kind
  and (* indeclinable form *)
  nmorph =
  (string * declension_class)

exception Report of string
  
val compute_decls : Word.word -> nmorph list -> unit
  
val compute_extra_iic : string list -> unit
  
val compute_extra : string list -> unit
  
val enter_extra_ifcs : unit -> unit
  
val enter_indecl_ifcs : unit -> unit
  
val enter_extra_iifcs : unit -> unit
  
val fake_compute_decls :
  nmorph ->
    string ->
      (inflected_map * (* nouns *) inflected_map * (* pronouns *)
       inflected_map * (* vocas *) inflected_map * (* iics *) inflected_map)
  
(* adverbs ifcs *)
(* Used in Interface for User-aid *)
val extract_current_caches : string -> (inflected_map * inflected_map)
  

