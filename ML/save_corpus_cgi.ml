(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let sentno = int_of_string (Cgi.decoded_get Params.sentence_no "" env) in
  let translit = Cgi.decoded_get "t" "" env in
  let unsandhied = Cgi.decoded_get "us" "" env = "t" in
  let text = Cgi.decoded_get "text" "" env in
  Save_corpus.make ~corpus_dir:corpdir ~sentence_no:sentno ~translit:translit
    ~unsandhied:unsandhied ~text:text
;
