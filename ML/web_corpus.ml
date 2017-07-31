(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

include Corpus.Make (struct value path = Web.corpus_dir; end)
;
(* [invalid_corpus_mode_page expected_mode current_mode] generates an HTML on
   [output_channel] to notify the user that the requested operation
   on the corpus is available only in [expected_mode] and not in
   [current_mode].  *)
value invalid_mode_page expected current =
  Web.error_page "Corpus Manager"
    "Invalid mode "
    ("Expected mode: " ^ string_of_mode expected ^
     " | Current mode: " ^ string_of_mode current)
;
