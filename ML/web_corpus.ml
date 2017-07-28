(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

include Corpus.Make (struct value path = Web.corpus_dir; end)
;
value url mode sentence =
  let env =
    [ (Params.corpus_mode, Web.string_of_corpus_mode mode) ::
      (Corpus.Sentence.state sentence) ]
  in
  Cgi.url (Corpus.Sentence.analyzer sentence) ~query:(Cgi.query_of_env env)
;
(* exception Citation_mismatch of string
; *)
value citation subdir id text editable =
  let mode = if editable then Web.Annotator else Web.Reader in
  let sentence =
    try sentence subdir id with
    [ No_such_sentence ->
      let init_state =
        [ (Params.corpus_dir, subdir)
        ; (Params.sentence_no, string_of_int id)
        ; ("t", Paths.default_transliteration)
        ; ("text", text)
        ]
      in
      Corpus.Sentence.make id Web.graph_cgi init_state
    ]
  in
  let expected_text =
    sentence
    |> Corpus.Sentence.state
    |> List.assoc "text"
  in
  if (* text = expected_text *) True then url mode sentence else
     (* raise (Citation_mismatch expected_text) *)
     failwith ("Citation mismatch: " ^ expected_text)
;
