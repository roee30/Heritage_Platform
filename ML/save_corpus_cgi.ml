(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let sentno =
    let sentno = Cgi.decoded_get Params.sentence_no "" env in
    try int_of_string sentno with
    [ Failure _ -> int_of_float (float_of_string sentno)
    ]
  in
  let translit = Cgi.decoded_get "t" "" env in
  let unsandhied = Cgi.decoded_get "us" "" env = "t" in
  let text = Cgi.decoded_get "text" "" env in
  do
  { Corpus.save_sentence
      ~corpus_dir:(Web.corpus_dir ^ corpdir)
      ~sentence_no:sentno
      ~translit:translit
      ~unsandhied:unsandhied
      ~text:text
  ; Corpus_manager.make corpdir
  }
;
