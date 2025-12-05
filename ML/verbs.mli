(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Verbs : sig i*)
open Skt_morph
  
val compute_conjugs : Word.word -> Conj_infos.root_infos -> unit
  
val compute_conjugs_stems : string -> Conj_infos.root_infos -> unit
  
val compute_extra : unit -> unit
  
val fake_compute_conjugs : int -> (* [pr_class] *) string -> (* entry *) unit
  

