(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

open Html;
open Web;

value confirmation_page query =
  let title_str = "Sanskrit Corpus" in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let corppermission = Cgi.decoded_get Params.corpus_permission "" env in
  let sentno = Cgi.decoded_get Params.sentence_no "" env in
  let confirmation_msg =
    Printf.sprintf "Confirm changes for sentence no. %s of %s ?" sentno corpdir
  in
  let specific_url path = Cgi.url path ~fragment:sentno in
  do
  { maybe_http_header ()
  ; page_begin (title title_str)
  ; body_begin Chamois_back |> pl
  ; open_page_with_margin 15
  ; h1_title title_str |> print_title (Some default_language)
  ; center_begin |> pl
  ; div Latin16 confirmation_msg |> pl
  ; html_break |> pl
  ; cgi_begin (specific_url save_corpus_cgi) "" |> pl
  ; hidden_input Save_corpus_params.state (escape query) |> pl
  ; hidden_input Save_corpus_params.force (string_of_bool True) |> pl
  ; submit_input "Yes" |> pl
  ; cgi_end |> pl
  ; html_break |> pl
  ; cgi_begin (specific_url corpus_manager_cgi) "" |> pl
  ; hidden_input Params.corpus_dir corpdir |> pl
  ; hidden_input Params.corpus_permission corppermission |> pl
  ; submit_input "No" |> pl
  ; cgi_end |> pl
  ; center_end |> pl
  ; close_page_with_margin ()
  ; page_end default_language True
  }

;
value analysis_of_env env =
  let lang =
    env
    |> Cgi.decoded_get "lex" Paths.default_lexicon
    |> Html.language_of_string
  in
  let cpts =
    env
    |> Cgi.decoded_get "cpts" ""
    (* [|> Checkpoints.parse_cpts] *)
  in
  let nb_sols =
    env
    |> Cgi.decoded_get Save_corpus_params.nb_sols "0"
    |> int_of_string
  in
  Corpus.Analysis.make Corpus.Analyzer.Graph lang cpts nb_sols
;
value error_page = error_page "Corpus Manager"
;
(***************)
(* Entry point *)
(***************)
value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let query = Cgi.decoded_get Save_corpus_params.state "" env in
  try
    let force =
      env
      |> Cgi.decoded_get Save_corpus_params.force (string_of_bool False)
      |> bool_of_string
    in
    let env = Cgi.create_env query in
    let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
    let sentno =
      env
      |> Cgi.decoded_get Params.sentence_no ""
      |> float_of_string
      |> int_of_float
    in
    let text = Cgi.decoded_get "text" "" env in
    let unsandhied = Cgi.decoded_get "us" "f" env = "t" in 
    let permission =
      Web_corpus.permission_of_string (Cgi.decoded_get Params.corpus_permission "" env)
    in
    match permission with
    [ Web_corpus.Annotator ->
      let read_skt = Sanskrit.read_raw_sanskrit_corpus (* NEW *)
        (* WAS if unsandhied then Sanskrit.read_raw_sanskrit
           else Sanskrit.read_sanskrit *)
      in
      let encode =
        Cgi.decoded_get "t" Paths.default_transliteration env
        |> Corpus.Encoding.of_string
        |> Corpus.Encoding.encode
      in do
      { Web_corpus.save_sentence force corpdir sentno
          (read_skt encode text) unsandhied (analysis_of_env env)
      ; Corpus_manager.mk_page corpdir permission 
      }
    | Web_corpus.Reader | Web_corpus.Manager ->
      let expected_permission = Web_corpus.string_of_permission Annotator in
      let current_permission = Web_corpus.string_of_permission permission in
      invalid_corpus_permission_page expected_permission current_permission
    ]
  with
  [ Web_corpus.Sentence_already_exists -> confirmation_page query
  | Sys_error msg -> error_page Control.sys_err_mess msg
  | Failure msg -> error_page Control.fatal_err_mess msg
  | _ -> abort default_language Control.fatal_err_mess "Unexpected anomaly"
  ]
;
