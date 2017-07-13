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
  value id : t -> int
  ;
  value url : t -> string
  ;
  (* TODO: Determine all the fields.  *)
  type metadata = { text : list Word.word }
  ;
end
;
module type Location = sig
  value dir : string
  ;
end
;
module type S = sig
  (* Contents of a corpus subdirectory: either we are on leaves of the
     tree (constructor [Sentences]) or on branches (constructor
     [Headings]).  *)
  type contents =
    [ Headings of list Heading.t
    | Sentences of list Sentence.t
    ]
  ;
  (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Heading.compare] or
     [Sentence.compare] depending on the case.  *)
  value contents : string -> contents
  ;
  value save_sentence : string -> unit
  ;
  value mkdir : string -> unit
  ;
  value gobble_metadata : string -> Sentence.t -> Sentence.metadata
  ;
  value dump_metadata : string -> Sentence.t -> Sentence.metadata -> unit
  ;
end
;
module Make (Loc : Location) : S
;
