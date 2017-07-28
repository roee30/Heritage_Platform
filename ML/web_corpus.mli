(**************************************************************************)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

include Corpus.S
;
(* type mode = [ Reader | Annotator | Manager ] *)
(* ; *)
(* value default_mode : mode *)
(* ; *)
(* value string_of_mode : mode -> string *)
(* ; *)
(* value mode_of_string : string -> mode *)
(* ; *)
(* value invalid_mode_page : mode -> mode -> unit *)
(* ; *)
value url : Web.corpus_mode -> Corpus.Sentence.t -> string
;
(* exception Citation_mismatch of string
; *)
(* [citation subdir id text editable] returns an URL to the analysis of
   the sentence [text] whose number is [id] in the corpus subdirectory
   [subdir].  [editable] is a flag to indicate if the sentence is editable
   or not.  Raise [Citation_mismatch expected_text] if the given [text]
   is different from the one stored in the sentence file
   ([expected_text]).  *)
value citation : string -> int -> string -> bool -> string
;
