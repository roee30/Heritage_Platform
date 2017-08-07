(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI script [manager] for corpus management, i.e. for listing and
   adding sentences of the corpus.  *)

value main =
  let env = Cgi.create_env (Cgi.query_string ()) in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env in
  let mode =
    Web_corpus.mode_of_string (Cgi.decoded_get Params.corpus_mode "" env)
  in
  try
    Corpus_manager.mk_page corpdir mode
  with
  [ Sys_error msg -> Web.abort Html.default_language Control.sys_err_mess msg
  | _ ->
    Web.abort Html.default_language Control.fatal_err_mess
      "Unexpected anomaly"
  ]
;
