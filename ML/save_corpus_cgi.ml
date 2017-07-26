(**************************************************************************)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

value confirmation_page query =
  let title = "Sanskrit Corpus" in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let sentno = Cgi.decoded_get Params.sentence_no "" env in
  let confirmation_msg =
    Printf.sprintf "Confirm changes for sentence no. %s of %s ?" sentno corpdir
  in
  let specific_url path = Cgi.url path ~fragment:sentno in
  do
  { Web.maybe_http_header ()
  ; Web.page_begin (Html.title title)
  ; Html.body_begin Html.Chamois_back |> Web.pl
  ; Web.open_page_with_margin 15
  ; Html.h1_title title |> Web.print_title (Some Html.default_language)
  ; Html.center_begin |> Web.pl
  ; Html.div Html.Latin16 confirmation_msg |> Web.pl
  ; Html.html_break |> Web.pl
  ; Web.cgi_begin (specific_url Web.save_corpus_cgi) "" |> Web.pl
  ; Html.hidden_input Save_corpus_params.state (Html.escape query) |> Web.pl
  ; Html.hidden_input Save_corpus_params.force (string_of_bool True) |> Web.pl
  ; Html.submit_input "Yes" |> Web.pl
  ; Web.cgi_end |> Web.pl
  ; Html.html_break |> Web.pl
  ; Web.cgi_begin (specific_url Web.corpus_manager_cgi) "" |> Web.pl
  ; Html.hidden_input Params.corpus_dir corpdir |> Web.pl
  ; Html.submit_input "No" |> Web.pl
  ; Web.cgi_end |> Web.pl
  ; Html.center_end |> Web.pl
  ; Web.close_page_with_margin ()
  ; Web.page_end Html.default_language True
  }

;
(***************)
(* Entry point *)
(***************)
value main =
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let query = Cgi.decoded_get Save_corpus_params.state "" env in
  let force =
    env
    |> Cgi.decoded_get Save_corpus_params.force (string_of_bool False)
    |> bool_of_string
  in
  let env = Cgi.create_env query in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let corpmode =
    Web.corpus_mode_of_string (Cgi.decoded_get Params.corpus_mode "" env)
  in
  let error_page = Web.error_page "Corpus Manager" in
  try
    let state =
      env
      |> List.remove_assoc Params.corpus_mode
      |> List.map (fun (k, v) -> (k, Cgi.decode_url v))
    in
    do
    { Web_corpus.save_sentence force Web.graph_cgi state
    ; Corpus_manager.mk_page corpdir corpmode
    }
  with
  [ Web_corpus.Sentence_already_exists -> confirmation_page query
  | Sys_error msg -> error_page Control.sys_err_mess msg
  | Failure msg -> error_page Control.fatal_err_mess msg
  ]
;
