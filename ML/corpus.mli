(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Operations on the corpus tree *)

module Heading : sig
  type t
  ;
  value label : t -> string
  ;
end
;
module Sentence : sig
  type t
  ;
  value make : int -> string -> list (string * string) -> t
  ;
  value id : t -> int
  ;
  value analyzer : t -> string
  ;
  value state : t -> list (string * string)
  ;
  (* TODO: Determine all the fields.  *)
  type metadata = { text : list Word.word }
  ;
end
;
module type Location = sig
  value path : string
  ;
end
;
module type S = sig
  (* Contents of a corpus subdirectory: either it is empty (constructor
     [Empty]), otherwise we are on leaves of the tree (constructor
     [Sentences]) or on branches (constructor [Headings]).  *)
  type contents =
    [ Empty
    | Headings of list Heading.t
    | Sentences of list Sentence.t
    ]
  ;
  (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Heading.compare] or
     [Sentence.compare] depending on the case.  Raise [Sys_error] when
     an operating system error occurs.  *)
  value contents : string -> contents
  ;
  (* Exception raised by [save_sentence] when the sentence to be saved
     already exists.  *)
  exception Sentence_already_exists
  ;
  (* Raise [Sentence_already_exists] if the sentence to be saved already
     exists and [force] is [False], [Failure "save_sentence"] if the
     given state is invalid and [Sys_error] when an operating system
     error occurs.  *)
  value save_sentence : bool -> string -> list (string * string) -> unit
  ;
  exception Heading_abbrev_already_exists of string
  ;
  (* Raise [Heading_abbrev_already_exists] if the given corpus directory
     already exists and [Unix.Unix_error] when an operating system error
     occurs.  *)
  value mkdir : string -> unit
  ;
  exception No_such_sentence
  ;
  (* Raise [No_such_sentence] if the requested sentence does not
     exist.  *)
  value sentence : string -> int -> Sentence.t
  ;
  value gobble_metadata : string -> Sentence.t -> Sentence.metadata
  ;
  value dump_metadata : string -> Sentence.t -> Sentence.metadata -> unit
  ;
end
;
module Make (Loc : Location) : S
;
