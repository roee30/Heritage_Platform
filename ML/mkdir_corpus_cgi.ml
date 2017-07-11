(* CGI script [mkdir_corpus] for creating a new corpus subdirectory.  *)

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let dirname = Cgi.decoded_get Mkdir_corpus_params.dirname "" env in
  let parent_dir = Cgi.decoded_get Mkdir_corpus_params.parent_dir "" env in
  do
  { Corpus.mkdir ~corpus_location:Web.corpus_dir ~dirname:(parent_dir ^ dirname)
  ; Corpus_manager.make parent_dir
  }
;
