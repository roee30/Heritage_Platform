(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Verbs : sig i*)

open Skt_morph;
 
value compute_conjugs : Word.word -> Conj_infos.root_infos -> unit;
value compute_conjugs_stems : string -> Conj_infos.root_infos -> unit;
value compute_extra : unit -> unit;
value fake_compute_conjugs : int (* [pr_class] *) -> string (* entry *) -> unit; 

(*i end; i*)
