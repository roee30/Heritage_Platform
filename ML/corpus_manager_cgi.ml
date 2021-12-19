(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI script [manager] for corpus management, i.e. for listing and
   adding sentences of the corpus.  *)

value main =
  let env = Cgi.create_env (Cgi.query_string ()) in
  let corpdir = Cgi.decoded_get Params.corpus_dir "" env 
  and corpperm = Cgi.decoded_get Params.corpus_permission "" env 
  and font = Cgi.decoded_get Params.corpus_font Paths.default_display_font env 
  and lex = Cgi.decoded_get Params.corpus_lex Paths.default_lexicon env in
  let permission = Web_corpus.permission_of_string corpperm in
  let lang =  Html.default_language in
  try
    Corpus_manager.mk_page corpdir permission font lex
  with
  [ Sys_error msg -> Web.abort lang Control.sys_err_mess msg
  | _ -> Web.abort lang Control.fatal_err_mess "Unexpected anomaly"
  ]
;
