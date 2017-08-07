(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This program produces the pages corpus.html (Corpus interface).  *)

open Html;
open Web;

value mode_selection =
  let selection modes =
      List.map (fun mode ->
        let mode_str = Web_corpus.string_of_mode mode in
        (String.capitalize mode_str, mode_str, mode = Web_corpus.Reader)
      ) modes
  in
  let read_only_modes = [ Web_corpus.Reader ] in
  let other_modes = Web_corpus.[ Annotator; Manager ] in
  let all_modes = read_only_modes @ other_modes in
  selection (if corpus_read_only then read_only_modes else all_modes)
;
value make lang =
  let title_str = "Sanskrit Corpus" in
  do
  { open_html_file (corpus_page lang) (title title_str)
  ; body_begin Chamois_back |> pl
  ; open_page_with_margin 15
  ; h1_title title_str |> print_title (Some lang)
  ; center_begin |> pl
  ; cgi_begin corpus_manager_cgi "" ^
    "Mode: " ^
    option_select_default Params.corpus_mode mode_selection ^ " " ^
    submit_input "OK" ^
    cgi_end |> pl
  ; center_end |> pl
  ; close_page_with_margin ()
  ; close_html_file lang True
  }
;
value main =
  do
  { make English
  ; make French
  }
;
