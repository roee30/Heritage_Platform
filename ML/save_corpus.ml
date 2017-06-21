(* CGI script [save_corpus] for saving a sentence into the corpus.  *)

value make () =
  let style = Html.background Html.Chamois in
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let outdir = Cgi.decoded_get InterfaceParams.outdir env "" in
  let outfile = Cgi.decoded_get InterfaceParams.outfile env "" in
  let file = Web.corpus_dir ^ outdir ^ outfile ^ ".html" in
  let title = "File " ^ file ^ " created." in
  let meta_title = Html.title title in
  let write_file file =
    do
    { Unix.putenv "QUERY_STRING" query
    ; Web.output_channel.val := open_out file
    ; Interface.Interface.safe_engine ()
    ; close_out Web.output_channel.val
    ; Web.output_channel.val := stdout }
  in
  let mk_page () =
    do
    { Web.http_header |> Web.pl
    ; Web.page_begin meta_title
    ; Html.body_begin style |> Web.pl
    ; Html.h1_title title |> Web.pl
    ; Web.page_end Html.default_language True }
  in
  do
  { write_file file
  ; mk_page () }
;

(***************)
(* Entry point *)
(***************)
value main = make ()
;
