include Corpus.Make (struct value path = Web.corpus_dir; end)
;
type mode = [ Reader | Annotator | Manager ]
;
value default_mode = Reader
;
value string_of_mode = fun
  [ Reader -> "reader"
  | Annotator -> "annotator"
  | Manager -> "manager"
  ]
;
value mode_of_string = fun
  [ "annotator" -> Annotator
  | "manager" -> Manager
  | _ -> Reader
  ]
;
value invalid_mode_page expected current =
  Web.error_page "Corpus Manager"
    "Invalid mode "
    ("Expected mode: " ^ string_of_mode expected ^
     " | Current mode: " ^ string_of_mode current)
;
value url mode sentence =
  let env =
    [ (Params.corpus_mode, string_of_mode mode) ::
      (Corpus.Sentence.state sentence) ]
  in
  Cgi.url (Corpus.Sentence.analyzer sentence) ~query:(Cgi.query_of_env env)
;
value citation corpus_dir sentence_no mode = failwith "TODO"
;
