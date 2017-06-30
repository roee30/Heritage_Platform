(* Operations on the corpus tree *)

(* Content of a corpus subdirectory: either we are on leaves of the tree
   (constructor [Sentences]) or on branches (constructor
   [Sections]).  *)
type content =
  [ Sections of list string
  | Sentences of list string ]
;
(* List the content of the given corpus subdirectory.  *)
value content : string -> content
;
(* TODO: Determine all the fields.  *)
type sentence_metadata = { text : list Word.word }
;
value gobble_sentence_metadata : string -> string -> sentence_metadata
;
value dump_sentence_metadata : sentence_metadata -> string -> string -> unit
;
