(* CGI script [manager] for corpus management, i.e. for listing
   sentences of the corpus and calling [add_corpus] to add a sentence to
   the corpus.  *)

(*i module Manager = struct i*)

value dir_key = "q"
;
value sentence_links dir =
  let to_anchor_ref file = Html.anchor_ref (Web.corpus_url ^ dir ^ file) file in
  let files = Dir.files_with_ext "html" (Web.corpus_dir ^ dir) in
  List.map to_anchor_ref files
;
value subdir_selection dir subdirs =
  let prefixed_subdirs = List.map (fun x -> dir ^ x ^ "/") subdirs in
  Html.option_select_label dir_key (List.combine prefixed_subdirs subdirs)
;
value body dir subdirs =
  match subdirs with
  [ [] ->
    do
    { sentence_links dir |> List.iter Web.pl
    ; Web.cgi_begin Web.add_corpus_cgi "" |> Web.pl
    ; Html.submit_input "Add" |> Web.pl
    ; Web.cgi_end |> Web.pl }
  | _ ->
    do
    { Web.cgi_begin Web.manager_cgi "" |> Web.pl
    ; subdir_selection dir subdirs |> Web.pl
    ; Html.submit_input "Submit" |> Web.pl
    ; Web.cgi_end |> Web.pl }]
;
value make () =
  let title = "Corpus Manager" in
  let meta_title = Html.title title in
  let style = Html.background Html.Chamois in
  let query = Cgi.query_string () in
  let env = Cgi.create_env query in
  let dir = Cgi.decode_url (Cgi.get dir_key env "") in
  let subdirs = Dir.subdirs (Web.corpus_dir ^ dir) in
  do
  { Web.http_header |> Web.pl
  ; Web.page_begin meta_title
  ; Html.body_begin style |> Web.pl
  ; Html.h1_title title |> Web.pl
  ; body dir subdirs
  ; Web.page_end Html.default_language True }
;

(***************)
(* Entry point *)
(***************)
value main = make ()
;

(*i end; i*)
