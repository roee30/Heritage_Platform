(*************)
(* Utilities *)
(*************)
value rec first_gap = fun
  [ [] -> 1
  | [ h ] -> h + 1
  | [ x ; y :: t ] -> if y = x + 1 then first_gap [ y :: t ] else x + 1
  ]
;
value until_nth n l =
  if n < 0 then invalid_arg "until_nth" else aux n l
    where rec aux n = fun
      [ [] -> []
      | [ h :: t ] -> if n = 0 then [ h ] else [ h :: aux (n - 1) t ] ]
;
(*******************)
(* Page generation *)
(*******************)
value top_link = Html.anchor_ref Web.corpus_manager_cgi "Top"
;
value link dir =
  let url =
    let corpdir =
      match dir with
      [ None -> ""
      | Some dir -> "?corpdir=" ^ Dir.url_encode dir ^ Dir.url_encoded_dir_sep]
    in
    Web.corpus_manager_cgi ^ corpdir
  in
  Html.anchor_ref url ".." |> Html.span2_center
;
value uplink dir =
  if dir = "" then None else
    let updir = Filename.dirname dir in
    let updir =
      if updir = Filename.current_dir_name then None else Some updir
    in
    Some (link updir)
;
value string_of_uplink = fun
  [ None -> ""
  | Some uplink -> uplink ^ " / "]
;
(* value quick_links dir = *)
(*    let updirs = Dir.split dir in *)
(*    let updirs = *)
(*      List.mapi (fun i x -> *)
(*          String.concat Filename.dir_sep (until_nth i updirs) *)
(*        ) updirs *)
(*    in *)
(*    List.map link updirs *)
(* ; *)
(* Display sentences with format "sentence || sentno" like in citations
   file.  *)
value sentence_links dir files =
  let to_anchor_ref file =
    let metadata = Corpus.gobble_sentence_metadata dir file in
    let words =
      List.map (
        match Multilingual.font_of_string (Paths.default_display_font) with
        [ Multilingual.Deva -> Canon.unidevcode
        | Multilingual.Roma -> Canon.uniromcode ]
      ) metadata.Corpus.text
    in
    let sentence = String.concat " " words in
    Html.anchor_ref (Web.corpus_url ^ dir ^ file) (Html.span2_center sentence)
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
type gap = { start : int; stop : int }
;
value rec first_group = fun
  [ [ x :: ([ y :: _ ] as t) ] ->
    let idx = Corpus.sentence_id x in
    let idy = Corpus.sentence_id y in
    if idy = idx + 1 then
      let ((group, gap), rest) = first_group t in
      (([ x :: group ], gap), rest)
    else
      (([ x ], { start = idx + 1; stop = idy - 1 }), t)
  | [] -> (([], { start = 1; stop = max_int }), [])
  | [ x ] as l ->
    ((l, { start = Corpus.sentence_id x + 1; stop = max_int }), []) ]
;
value very_first_group = fun
  [ [ ([ x :: _ ], _) :: _ ] ->
    let idx = Corpus.sentence_id x in
    if idx <> 1 then
      Some ([], { start = 1; stop = idx - 1 })
    else
      None
  | _ -> None ]
;
value groups l =
  let rec aux l =
    let (group, rest) = first_group l in
    match rest with
    [ [] ->  [ group ]
    | _ -> [ group :: aux rest ] ]
  in
  let groups = aux l in
  match very_first_group groups with
  [ None -> groups
  | Some x -> [ x :: groups ] ]
;
value add_sentence_form dir gap =
  Web.cgi_begin (Web.cgi_bin "skt_heritage") "" ^
  string_of_uplink (uplink dir) ^
  Html.hidden_input Params.corpus_dir dir ^
  Html.int_input ~name:Params.sentence_no ~step:1 ~min:gap.start ~max:gap.stop
                 ~val:gap.start ~id:Params.sentence_no ^
  Html.submit_input "Add sentence" ^
  Web.cgi_end
;
value htmlify_group dir (group, gap) =
  let ol =
    match group with
    [ [] -> ""
    | [ h :: _ ] ->
      Html.ol ~start:(Corpus.sentence_id h) (sentence_links dir group) ]
  in
  ol ^ add_sentence_form dir gap
;
value body dir =
  match Corpus.content dir with
  [ Corpus.Sentences files ->
    do
    { List.map (htmlify_group dir) (groups files) |> List.iter Web.pl }

  | Corpus.Sections subdirs ->
    do
    { Web.cgi_begin Web.corpus_manager_cgi "" |> Web.pl
    ; uplink dir |> string_of_uplink |> Web.pl
    ; subdir_selection dir subdirs |> Web.pl
    (* Submit button or links to subdirs?  *)
    ; Html.submit_input "Select" |> Web.pl
    ; Web.cgi_end |> Web.pl } ]
;
value make dir =
  let title = "Corpus Manager" in
  let meta_title = Html.title title in
  let style = Html.background Html.Chamois in
  do
  { Web.maybe_http_header ()
  ; Web.page_begin meta_title
  ; Html.body_begin style |> Web.pl
  ; Html.h1_title title |> Web.print_title (Some Html.default_language)
  ; Html.center_begin |> Web.pl
  ; body dir
  ; Html.center_end |> Web.pl
  ; Web.page_end Html.default_language True }
;
