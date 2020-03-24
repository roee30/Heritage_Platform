(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Operations on the corpus tree *)

module Section : sig
  type t
  ;
  value label : t -> string
  ;
end
;
module Analyzer : sig
  type t = [ Graph ]
  ;
  value path : t -> string
  ;
  value relocatable_path : t -> string
  ;
end
;
module Analysis : sig
  type t
  ;
  value make : Analyzer.t -> Html.language -> string -> int (* Num.num *) -> t
  ;
  value analyzer : t -> Analyzer.t
  ;
  value lang : t -> Html.language
  ;
  value checkpoints : t -> string
  ;
  value nb_sols : t -> int (* Num.num *)
  ;
end
;
module Encoding : sig
  type t = [ Velthuis | WX | KH | SLP1 | Devanagari | IAST ]
  ;
  value to_string : t -> string
  ;
  value of_string : string -> t
  ;
  value encode : t -> string -> Word.word
  ;
  value decode : t -> Word.word -> string
  ;
end
;
module Sentence : sig
  type t
  ;
  value make : int -> list Word.word -> bool -> Analysis.t -> t
  ;
  value id : t -> int
  ;
  value text : Encoding.t -> t -> string
  ;
  value analysis : t -> Analysis.t
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
     [Sentences]) or on branches (constructor [Sections]).  *)
  type contents =
    [ Empty
    | Sections of list Section.t
    | Sentences of list Sentence.t
    ]
  ;
  (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Section.compare] or
     [Sentence.compare] depending on the case.  Raise [Sys_error] when
     an operating system error occurs.  *)
  value contents : string -> contents
  ;
  (* Exception raised by [save_sentence] when the sentence to be saved
     already exists.  *)
  exception Sentence_already_exists
  ;
  (* Raise [Sentence_already_exists] if the sentence to be saved already
     exists and [force] is [False] and [Sys_error] when an operating
     system error occurs.  *)
  value save_sentence :
    bool -> string -> int -> list Word.word -> bool -> Analysis.t -> unit
  ;
  exception Section_already_exists of string
  ;
  (* Raise [Section_already_exists] if the given corpus directory
     already exists and [Unix.Unix_error] when an operating system error
     occurs.  *)
  value mkdir : string -> unit
  ;
  exception No_such_sentence of int
  ;
  (* Raise [No_such_sentence i] if the sentence i does not exist.  *)
  value sentence : string -> int -> Sentence.t
  ;
  type permission = [ Reader | Annotator | Manager ]
  ;
  value default_permission : permission
  ;
  value string_of_permission : permission -> string
  ;
  value permission_of_string : string -> permission
  ;
  value url : string -> permission -> Sentence.t -> string
  ;
  value relocatable_url : string -> permission -> Sentence.t -> string
  ;
  (* [citation subdir id ]] returns an URL to the analysis
     of the sentence whose number is [id] in the corpus
     subdirectory [subdir]. Raise [Failure "citation"] if an
     error occurs.  *)
  value citation : string -> int -> string 
  ;
end
;
module Make (Loc : Location) : S
;
