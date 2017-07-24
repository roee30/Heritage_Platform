include Corpus.S
;
type mode = [ Reader | Annotator | Manager ]
;
value default_mode : mode
;
value string_of_mode : mode -> string
;
value mode_of_string : string -> mode
;
(* [invalid_mode_page expected_mode current_mode] generates an HTML on
   [Web.output_channel] to notify the user that the requested operation
   on the corpus is available only in [expected_mode] and not in
   [current_mode].  *)
value invalid_mode_page : mode -> mode -> unit
;
value url : mode -> Corpus.Sentence.t -> string
;
value citation : string -> int -> mode -> string
;
