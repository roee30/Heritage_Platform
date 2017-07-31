(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI script [mkdir_corpus] for creating a new corpus subdirectory.  *)

open Web;

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let dirname = Cgi.decoded_get Mkdir_corpus_params.dirname "" env in
  let parent_dir = Cgi.decoded_get Mkdir_corpus_params.parent_dir "" env in
  let mode =
    Cgi.decoded_get Mkdir_corpus_params.mode "" env
    |> Web_corpus.mode_of_string
  in
  let error_page = error_page "Corpus Manager" in
  match mode with
  [ Web_corpus.Manager ->
    try
      do
      { Web_corpus.mkdir (Filename.concat parent_dir dirname)
      ; Corpus_manager.mk_page parent_dir mode
      }
    with
    [ Web_corpus.Heading_abbrev_already_exists abbrev ->
      error_page "Already used heading abbreviation " abbrev
    | Unix.Unix_error (err, func, arg) ->
      let submsg =
        Printf.sprintf "'%s' failed on '%s': %s"
          func arg (Unix.error_message err)
      in
      error_page Control.sys_err_mess submsg
    | _ ->
      abort Html.default_language Control.fatal_err_mess "Unexpected anomaly"
    ]
  | Web_corpus.Reader | Web_corpus.Annotator ->
    let expected_mode = Web_corpus.(string_of_mode Manager) in
    let current_mode = Web_corpus.string_of_mode mode in
    invalid_corpus_mode_page expected_mode current_mode
  ]
;
