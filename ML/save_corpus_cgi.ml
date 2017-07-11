(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let query = Cgi.decoded_get "q" "" env in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  do
  { Corpus.save_sentence ~corpus_location:Web.corpus_dir ~query:query
  ; Corpus_manager.make corpdir
  }
;
