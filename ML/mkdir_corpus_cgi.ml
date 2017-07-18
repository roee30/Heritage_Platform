(* CGI script [mkdir_corpus] for creating a new corpus subdirectory.  *)

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let dirname = Cgi.decoded_get Mkdir_corpus_params.dirname "" env in
  let parent_dir = Cgi.decoded_get Mkdir_corpus_params.parent_dir "" env in
  let error_page = Web.error_page "Corpus Manager" in
  try
    do
    { Web_corpus.mkdir (parent_dir ^ dirname)
    ; Corpus_manager.make parent_dir
    }
  with
  [ Web_corpus.Heading_abbrev_already_exists abbrev ->
    error_page "Already used heading abbreviation " abbrev
  | Unix.Unix_error (err, func, arg) ->
    let submsg =
      Printf.sprintf "'%s' failed on '%s': %s" func arg (Unix.error_message err)
    in
    error_page Control.sys_err_mess submsg
  ]
;
