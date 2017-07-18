(*************)
(* Utilities *)
(*************)
value until_nth n l =
  if n < 0 then invalid_arg "until_nth" else aux n l
    where rec aux n = fun
      [ [] -> []
      | [ h :: t ] -> if n = 0 then [ h ] else [ h :: aux (n - 1) t ]
      ]
;
(* Type representing interval of missing integers in a sorted list.  *)
type gap = { start : int; stop : int }
;
(* The following functions assume that the given list is sorted in
   increasing order and represents a subset of positive integers.  In
   particular, the lowest bound of a gap is at least [1] and the
   greatest at most [max_int]).  We call "group" a list of consecutive
   integers.  *)

value max_gap = { start = 1; stop = max_int }
;
value string_of_gap gap =
  let right_bound =
    if gap.stop = max_int then "..." else string_of_int gap.stop
  in
  Printf.sprintf "%d - %s" gap.start right_bound
;
(* Return a triple [(g, gap, rest)] where [g] is the first group of the
   given list, [gap] the gap to the next group and [rest] the given
   list without its first group.  *)
value rec first_group = fun
  [ [ x :: ([ y :: _ ] as t) ] ->
    if y = x + 1 then
      let (group, gap, rest) = first_group t in
      ([ x :: group ], gap, rest)
    else
      ([ x ], { start = x + 1; stop = y - 1 }, t)
  | [] -> ([], max_gap, [])
  | [ x ] as l ->
    (l, { start = x + 1; stop = max_int }, [])
  ]
;
value groups_with_gaps l =
  let rec aux l =
    let (group, gap, rest) = first_group l in
    let group_gap = (group, gap) in
    match rest with
    [ [] ->  [ group_gap ]
    | _ -> [ group_gap :: aux rest ]
    ]
  in
  aux l
;
value add_init_gap groups =
  let init_gap = fun
    [ [ ([ x :: _ ], _) :: _ ] ->
      if x <> 1 then Some { start = 1; stop = x - 1 } else None
    | _ -> None
    ]
  in
  match init_gap groups with
  [ None -> groups
  | Some gap -> [ ([], gap) :: groups ]
  ]
;
(*******************)
(* Page generation *)
(*******************)
value link dir =
  let url =
    Web.corpus_manager_cgi ^ "?corpdir=" ^ Dir.url_encode dir ^
    Dir.url_encoded_dir_sep
  in
  let label = Filename.basename dir in
  Html.anchor_ref url label
;
value uplinks dir =
  let aux dir =
   let updirs = Dir.split dir in
   let updirs =
     List.mapi (fun i x ->
         String.concat Filename.dir_sep (until_nth i updirs)
       ) updirs
   in
   List.map link updirs
  in
  dir
  |> aux
  |> String.concat " / "
;
(* Display sentences with format "sentence || sentno" like in citations
   file.  *)
value sentence_links dir sentences =
  let to_anchor_ref sentence =
    let metadata =
      Web_corpus.gobble_metadata (Web.corpus_dir ^ dir) sentence
    in
    let font = Multilingual.font_of_string Paths.default_display_font in
    let words =
      List.map (
        match font with
        [ Multilingual.Deva -> Canon.unidevcode
        | Multilingual.Roma -> Canon.uniromcode
        ]
      ) metadata.Corpus.Sentence.text
    in
    let display =
      match font with
      [ Multilingual.Deva -> Html.deva16_blue
      | Multilingual.Roma -> Html.span Html.Trans16
      ]
    in
    let sentence_str = String.concat " " words in
    Html.anchor_ref (Html.escape (Corpus.Sentence.url sentence)) sentence_str
    |> display
  in
  List.map to_anchor_ref sentences
;
value heading_selection dir headings =
  let options =
    let prefixes =
      List.map (fun x -> dir ^ x ^ Filename.dir_sep) headings
    in
    List.combine prefixes headings
  in
  Html.option_select_label Params.corpus_dir options
;
value add_sentence_form dir gap =
  Web.cgi_begin (Web.cgi_bin "skt_heritage") "" ^
  Html.h3_begin Html.B3 ^
  uplinks dir ^ " / Sentence no. " ^
  Html.hidden_input Params.corpus_dir dir ^
  Html.int_input
    ~name:Params.sentence_no
    ~step:1
    ~min:gap.start
    ~max:gap.stop
    ~val:gap.start
    ~id:Params.sentence_no ^ " " ^
  Html.submit_input "Add" ^
  Html.h3_end ^
  Web.cgi_end
;
value htmlify_group dir (group, gap) =
  let (ol, group_id) =
    match group with
    [ [] -> ("", "")
    | [ h :: _ ] ->
      let id = Corpus.Sentence.id h in
      (Html.ol ~start:id ~items:(sentence_links dir group), string_of_int id)
    ]
  in
  let div_id = "group" ^ group_id in
  let add_sentence_form =
    Html.button
      ~id:"add_sentence"
      ~cl:Html.Center_
      ~onclick:
        { Html.js_funid = "hideShowElement"
        ; Html.js_funargs = [ div_id ]
        }
      ~label:("Hide/Show form to fill gap " ^ string_of_gap gap) ^
    Html.elt_begin_attrs [ ("id", div_id) ] "div" Html.Hidden_ ^
    Html.html_paragraph ^
    add_sentence_form dir gap ^
    Html.div_end
  in
  ol ^ if not Web.corpus_read_only then add_sentence_form else ""

;
value group_sentences dir sentences =
  let ids = List.map Corpus.Sentence.id sentences in
  let dict = List.combine ids sentences in
  let groups = ids |> groups_with_gaps |> add_init_gap in
  List.map (fun (x, y) -> (List.map (fun x -> List.assoc x dict) x, y)) groups
;
value new_heading_form dir =
  Web.cgi_begin Web.mkdir_corpus_cgi "" ^
  Html.h3_begin Html.B3 ^
  "New heading: " ^
  Html.hidden_input Mkdir_corpus_params.parent_dir dir ^
  Html.text_input "new_heading" Mkdir_corpus_params.dirname ^ " " ^
  Html.submit_input "Create" ^
  Html.h3_end ^
  Web.cgi_end
;
value body dir =
  match Web_corpus.contents (Web.corpus_dir ^ dir) with
  [ Web_corpus.Empty ->
    if not Web.corpus_read_only then
      do
        { Html.center_begin |> Web.pl
        ; add_sentence_form dir max_gap |> Web.pl
        ; new_heading_form dir |> Web.pl
        ; Html.center_end |> Web.pl
        }
    else
      do
      { Html.h2_begin Html.B2 |> Web.pl
      ; "Empty corpus" |> Web.pl
      ; Html.h2_end |> Web.pl
      }
  | Web_corpus.Sentences sentences ->
    let groups = group_sentences dir sentences in
    do
    { Html.h2_begin Html.B2 |> Web.pl
    ; uplinks dir |> Web.pl
    ; Html.h2_end |> Web.pl
    ; groups |> List.map (htmlify_group dir) |> List.iter Web.pl
    ; Html.html_break |> Web.pl
    }
  | Web_corpus.Headings headings ->
    let selection_prompt =
      "Explore " ^
      heading_selection dir (List.map Corpus.Heading.label headings)  ^ " " ^
      Html.submit_input "Go"
    in
    do
    { Html.center_begin |> Web.pl
    ; Html.h2_begin Html.B2 |> Web.pl
    ; uplinks dir |> Web.pl
    ; Html.h2_end |> Web.pl
    ; Web.cgi_begin Web.corpus_manager_cgi "" |> Web.pl
    ; Html.h3_begin Html.B3 |> Web.pl
    ; selection_prompt |> Web.pl
    ; Html.h3_end |> Web.pl
    ; Web.cgi_end |> Web.pl
    ; if not Web.corpus_read_only then new_heading_form dir |> Web.pl else ()
    ; Html.center_end |> Web.pl
    }
  ]
;
value make dir =
  let title = "Sanskrit Corpus" in
  try
    do
    { Web.maybe_http_header ()
    ; Web.page_begin (Html.title title)
    ; Html.body_begin Html.Chamois_back |> Web.pl
    ; Web.open_page_with_margin 15
    ; Html.h1_title (Html.anchor_ref Web.corpus_manager_cgi title)
      |> Web.print_title (Some Html.default_language)
    ; body dir
    ; Web.close_page_with_margin ()
    ; Web.page_end Html.default_language True
    }
  with
  [ Sys_error msg -> Web.abort Html.default_language Control.sys_err_mess msg
  ]
;
