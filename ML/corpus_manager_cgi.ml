(* CGI script [manager] for corpus management, i.e. for listing and
   adding sentences of the corpus.  *)

value main =
  Cgi.query_string ()
  |> Cgi.create_env
  |> Cgi.decoded_get Params.corpus_dir ""
  |> Corpus_manager.make
;
