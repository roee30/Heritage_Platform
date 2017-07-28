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
  let corpmode = Cgi.decoded_get Params.corpus_mode "" env in
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
  ; hidden_input Params.corpus_mode corpmode |> pl
  ; submit_input "No" |> pl
  ; cgi_end |> pl
  ; center_end |> pl
  ; close_page_with_margin ()
  ; page_end default_language True
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
    corpus_mode_of_string (Cgi.decoded_get Params.corpus_mode "" env)
  in
  let error_page = error_page "Corpus Manager" in
  try
    let state =
      env
      |> List.remove_assoc Params.corpus_mode
      |> List.map (fun (k, v) -> (k, Cgi.decode_url v))
    in
    do
    { Web_corpus.save_sentence force graph_cgi state
    ; Corpus_manager.mk_page corpdir corpmode
    }
  with
  [ Web_corpus.Sentence_already_exists -> confirmation_page query
  | Sys_error msg -> error_page Control.sys_err_mess msg
  | Failure msg -> error_page Control.fatal_err_mess msg
  ]
;
