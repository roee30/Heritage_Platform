(* CGI script [manager] for corpus management, i.e. for listing
   sentences of the corpus and calling [add_corpus] to add a sentence to
   the corpus.  *)

(*************)
(* Utilities *)
(*************)
value int_of_file file =
  let chop_extension file =
    try Filename.chop_extension file with [ Invalid_argument _ -> file ]
  in
  file
  |> Filename.basename
  |> chop_extension
  |> int_of_string
;
value cmp_section_file file file' =
  compare (int_of_file file) (int_of_file file')
;
value split file = Str.split (Str.regexp Filename.dir_sep) file
;
value rec first_gap = fun
  [ [] -> 1
  | [ h ] -> h
  | [ x ; y :: t ] -> if y = x + 1 then first_gap [ y :: t ] else x + 1
  ]
;

(******************)
(* CGI parameters *)
(******************)
value dir_param = "q"
;

(*******************)
(* Page generation *)
(*******************)
value sentence_links dir files =
  let to_anchor_ref file =
    Html.anchor_ref (Web.corpus_url ^ dir ^ file) (Filename.chop_extension file)
  in
  List.map to_anchor_ref files
;
value subdir_selection dir subdirs =
  let prefixed_subdirs =
    List.map (fun x -> dir ^ x ^ Filename.dir_sep) subdirs
  in
  Html.option_select_label dir_param (List.combine prefixed_subdirs subdirs)
;
value body dir =
  let subdirs =
    let cmp_subdir =
      let section path = List.length (split path) > 1 in
      if section dir then cmp_section_file else String.compare
    in
    Dir.subdirs (Web.corpus_dir ^ dir) cmp_subdir
  in
  match subdirs with
  [ [] ->
    let files =
      Dir.files_with_ext "html" (Web.corpus_dir ^ dir) cmp_section_file
    in
    let attrs =
      let first_missing_sentence files =
        first_gap (List.map int_of_file files)
      in
      { Html.step = 1;
        Html.min = 1;
        Html.max = max_int;
        Html.default = first_missing_sentence files }
    in
    do
    { sentence_links dir files |> List.iter Web.pl
    ; Web.cgi_begin (Web.cgi_bin "skt_heritage") "" |> Web.pl
    ; Html.hidden_input InterfaceParams.outdir dir |> Web.pl
    ; Html.int_input' InterfaceParams.outfile (Some attrs) |> Web.pl
    ; Html.submit_input "Add sentence" |> Web.pl
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
  let dir = Cgi.decode_url (Cgi.get dir_param env "") in
  do
  { Web.http_header |> Web.pl
  ; Web.page_begin meta_title
  ; Html.body_begin style |> Web.pl
  ; Html.h1_title title |> Web.pl
  ; Html.center_begin |> Web.pl
  (* add links at the top to navigate quickly in the corpus *)
  (* ; split dir |> List.iter Web.pl *)
  ; body dir
  ; Html.center_end |> Web.pl
  ; Web.page_end Html.default_language True }
;

(***************)
(* Entry point *)
(***************)
value main = make ()
;
