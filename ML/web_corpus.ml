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
value url dir mode sentence =
  let analysis = Corpus.Sentence.analysis sentence in
  let env =
    [ (Params.corpus_mode, Web.string_of_corpus_mode mode)
    ; ("text", Corpus.Sentence.text Corpus.Encoding.Velthuis sentence)
    ; ("cpts", Checkpoints.string_points (Corpus.Analysis.checkpoints analysis))
    ; (Params.corpus_dir, dir)
    ; (Params.sentence_no, sentence |> Corpus.Sentence.id |> string_of_int)
    ]
  in
  let path =
    analysis
    |> Corpus.Analysis.analyzer
    |> Corpus.Analyzer.path
  in
  Cgi.url path ~query:(Cgi.query_of_env env)
;
(* exception Citation_mismatch of string
; *)
value citation subdir id text_str editable =
  let text = Sanskrit.read_VH False text_str in
  let mode = if editable then Web.Annotator else Web.Reader in
  let sentence =
    try sentence subdir id with
    [ No_such_sentence ->
      (* lang must be a param of citation? *)
      let analysis = Corpus.Analysis.make Corpus.Analyzer.Graph
          Html.default_language [] (Num.Int 0)
      in
      (* unsandhied or not ?  *)
      Corpus.Sentence.make id text False analysis
    ]
  in
  let expected_text = Corpus.Sentence.text Corpus.Encoding.Velthuis sentence in
  (* if text = expected_text then url subdir mode sentence else *)
  (*   raise (Citation_mismatch expected_text) *)
  (* in *)
  if (* text = expected_text *) True then url subdir mode sentence else
     (* raise (Citation_mismatch expected_text) *)
     failwith ("Citation mismatch: " ^ expected_text)
;
