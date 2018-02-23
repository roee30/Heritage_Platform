(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Nouns : sig i*)

open Skt_morph;
open Morphology; (* [inflected_map] *)

type declension_class = 
  [ Gender of gender  (* declined substantive, adjective, number, pronoun *)
  | Ind of ind_kind   (* indeclinable form *)
  ]
and nmorph = (string * declension_class)
;
exception Report of string
;
value compute_decls : Word.word -> list nmorph -> unit; 
value compute_extra_iic : list string -> unit;
value compute_extra : list string -> unit;
value enter_extra_ifcs : unit -> unit; 
value enter_extra_iifcs : unit -> unit; 
value fake_compute_decls : 
     nmorph -> string -> ( inflected_map    (* nouns *)
                         * inflected_map    (* pronouns *)
                         * inflected_map    (* vocas *)
                         * inflected_map    (* iics *)
                         * inflected_map ); (* adverbs ifcs *)

value extract_current_cache : string -> inflected_map; (* used in Interface *)

(*i end; i*)
