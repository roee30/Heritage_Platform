(*************)
(* Utilities *)
(*************)
value rec first_gap = fun
  [ [] -> 1
  | [ h ] -> h + 1
  | [ x ; y :: t ] -> if y = x + 1 then first_gap [ y :: t ] else x + 1
  ]
;
(*******************)
(* Page generation *)
(*******************)
value sentence_links dir files =
  let to_anchor_ref file =
    let chunks =
      (Gen.gobble (Web.corpus_dir ^ dir ^ "." ^ Filename.chop_extension file) 
       : list Word.word)
    in
    let chunks = List.map (
        match Multilingual.font_of_string (Paths.default_display_font) with
        [ Multilingual.Deva -> Canon.unidevcode
        | Multilingual.Roma -> Canon.uniromcode ])
        chunks
    in
    let sentence = String.concat " " chunks in
  (Filename.chop_extension file) ^ ". " ^
  Html.anchor_ref (Web.corpus_url ^ dir ^ file) sentence
  in
  List.map to_anchor_ref files
;
value subdir_selection dir subdirs =
  let options =
    let prefixed_subdirs =
      List.map (fun x -> dir ^ x ^ Filename.dir_sep) subdirs
    in
    List.combine prefixed_subdirs subdirs
  in
  Html.option_select_label Params.corpus_dir options
;
value body dir =
  match Corpus.content dir with
  [ Corpus.Files files ->
    (* let default = *)
    (*   let first_missing_sentence files = *)
    (*     first_gap (List.map Corpus.int_of_file files) *)
    (*   in *)
    (*   first_missing_sentence files *)
    (* in *)
    do
    { sentence_links dir files |> List.iter Web.pl
    ; Web.cgi_begin (Web.cgi_bin "skt_heritage") "" |> Web.pl
    ; Html.hidden_input Params.corpus_dir dir |> Web.pl
    ; Html.int_input ~name:Params.sentence_no ~step:1 ~min:1 ~max:max_int
        ~val:1 ~id:""
      |> Web.pl
    ; Html.submit_input "Add sentence" |> Web.pl
    ; Web.cgi_end |> Web.pl }

  | Corpus.Dirs subdirs ->
    do
    { Web.cgi_begin Web.corpus_manager_cgi "" |> Web.pl
    ; subdir_selection dir subdirs |> Web.pl
    ; Html.submit_input "Submit" |> Web.pl
    ; Web.cgi_end |> Web.pl }]
;
value make dir =
  let title = "Corpus Manager" in
  let meta_title = Html.title title in
  let style = Html.background Html.Chamois in
  do
  { Web.maybe_http_header ()
  ; Web.page_begin meta_title
  ; Html.body_begin style |> Web.pl
  ; Html.h1_title title |> Web.pl
  ; Html.center_begin |> Web.pl
  (* ; Html.center_begin |> Web.pl *)
  (* add links at the top to navigate quickly in the corpus *)
  (* ; split dir |> List.iter Web.pl *)
  ; body dir
  ; Html.center_end |> Web.pl
  (* ; Html.center_end |> Web.pl *)
  ; Web.page_end Html.default_language True }
;
