(* Operations on the corpus tree *)

(* Content of a corpus subdirectory: either we are on leaves of the tree
   (constructor [Sentences]) or on branches (constructor
   [Sections]).  *)
type content =
  [ Sections of list string
  | Sentences of list string
  ]
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
(* Return the identifier of the sentence stored in the given file.  *)
value sentence_id : string -> int
;
value save_sentence :
  ~corpus_location:string -> ~query:string -> unit
(* ~corpus_dir:string -> ~sentence_no:int -> *)
  (* ~translit:string -> ~unsandhied:bool -> ~text:string -> unit *)
;
