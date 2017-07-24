(* CGI script [manager] for corpus management, i.e. for listing and
   adding sentences of the corpus.  *)

value main =
  let env = Cgi.create_env (Cgi.query_string ()) in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let mode =
    Web_corpus.mode_of_string (Cgi.decoded_get Params.corpus_mode "" env)
  in
  Corpus_manager.mk_page corpdir mode
;
