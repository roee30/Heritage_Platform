(**************************************************************************)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*************)
(* Utilities *)
(*************)

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
value big text = Html.div Html.Latin16 text
;
value link mode dir =
  let url =
    let query =
      Cgi.query_of_env
        [ (Params.corpus_dir, dir)
        ; (Params.corpus_mode, Web.string_of_corpus_mode mode)
        ]
    in
    Cgi.url Web.corpus_manager_cgi ~query |> Html.escape
  in
  let label = Filename.basename dir in
  Html.anchor_ref url label
;
value uplinks dir mode =
  let aux dir =
   let updirs = Dir.split dir in
   let updirs =
     List.mapi (fun i x ->
         String.concat Filename.dir_sep (List2.take_prefix (i + 1) updirs)
       ) updirs
   in
   List.map (link mode) updirs
  in
  dir
  |> aux
  |> String.concat " / "
;
(* Display sentences with format "sentence || sentno" like in citations
   file.  *)
value sentence_links dir mode sentences =
  let to_anchor_ref sentence =
    let metadata =
      Web_corpus.gobble_metadata dir sentence
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
    sentence_str
    |> Html.anchor_ref (sentence |> Web_corpus.url mode |> Html.escape)
    |> display
  in
  List.map to_anchor_ref sentences
;
value heading_selection dir headings =
  let options =
    let prefixes =
      List.map (fun x -> Filename.concat dir x) headings
    in
    List.combine prefixes headings
  in
  Html.option_select_label Params.corpus_dir options
;
value add_sentence_form dir mode gap =
  Web.cgi_begin (Web.cgi_bin "skt_heritage") "" ^
  "Add sentence: " ^ uplinks dir mode ^ " / " ^
  Html.hidden_input Params.corpus_dir dir ^
  Html.hidden_input Params.corpus_mode (Web.string_of_corpus_mode mode) ^
  Html.int_input Params.sentence_no
    ~step:1
    ~min:gap.start
    ~max:gap.stop
    ~val:gap.start
    ~id:Params.sentence_no ^ " " ^
  Html.submit_input "Add"
  ^
  Web.cgi_end
  ;
value htmlify_group dir mode (group, gap) =
  let (ol, group_id) =
    match group with
    [ [] -> ("", "")
    | [ h :: _ ] ->
      let id = Corpus.Sentence.id h in
      let group_id = string_of_int id in
      (Html.ol ~li_id_prefix:"" ~start:id (sentence_links dir mode group),
       group_id)
    ]
  in
  let div_id = "group" ^ group_id in
  let add_sentence_form =
    Html.button
      ~id:"add_sentence"
      ~onclick:
        { Html.js_funid = "hideShowElement"
        ; Html.js_funargs = [ div_id ]
        }
    ("Hide/Show form to fill gap " ^ string_of_gap gap) ^
    Html.elt_begin_attrs [ ("id", div_id) ] "div" Html.Hidden_ ^
    Html.html_paragraph ^
    add_sentence_form dir mode gap ^
    Html.div_end
  in
  ol ^ if mode = Web.Annotator then add_sentence_form else ""

;
value group_sentences dir sentences =
  let ids = List.map Corpus.Sentence.id sentences in
  let dict = List.combine ids sentences in
  let groups = ids |> groups_with_gaps |> add_init_gap in
  List.map (fun (x, y) -> (List.map (fun x -> List.assoc x dict) x, y)) groups
;
value new_heading_form dir mode =
  let uplinks = uplinks dir mode in
  Web.cgi_begin Web.mkdir_corpus_cgi "" ^
  "New heading: " ^ uplinks ^ (if uplinks <> "" then " / " else "") ^
  Html.hidden_input Mkdir_corpus_params.parent_dir dir ^
  Html.hidden_input Mkdir_corpus_params.mode (Web.string_of_corpus_mode mode) ^
  Html.text_input "new_heading" Mkdir_corpus_params.dirname ^ " " ^
  Html.submit_input "Create"
  ^
  Web.cgi_end
  ;
value heading_selection_form dir mode headings =
  let selection_prompt =
    let uplinks = uplinks dir mode in
    let submit_button_label =
      match mode with
      [ Web.Reader -> "Read"
      | Web.Annotator -> "Annotate"
      | Web.Manager -> "Manage" ]
    in
    uplinks ^ (if uplinks <> "" then " / " else "") ^
    heading_selection dir (List.map Corpus.Heading.label headings)  ^ " " ^
    Html.submit_input submit_button_label
  in
  Web.cgi_begin Web.corpus_manager_cgi "" ^
  big (
    selection_prompt ^
    Html.hidden_input Params.corpus_mode (Web.string_of_corpus_mode mode)
  ) ^
  Web.cgi_end
;
value body dir mode =
  match Web_corpus.contents dir with
  [ Web_corpus.Empty ->
    do
    { uplinks dir mode |> big |> Web.pl
    ; Web.open_page_with_margin 30
    ; match mode with
        [ Web.Reader -> "Empty corpus"
        | Web.Annotator -> add_sentence_form dir mode max_gap
        | Web.Manager -> new_heading_form dir mode
        ]
      |> Web.pl
    ; Web.close_page_with_margin ()
    }


  | Web_corpus.Sentences sentences ->
    let groups = group_sentences dir sentences in
    do
    { uplinks dir mode |> big |> Web.pl
    ; Web.open_page_with_margin 30
    ; groups |> List.map (htmlify_group dir mode) |> List.iter Web.pl
    ; Web.close_page_with_margin ()
    }

  | Web_corpus.Headings headings ->
    do
    { Html.center_begin |> Web.pl
    ; heading_selection_form dir mode headings |> Web.pl
    ; Html.html_break |> Web.pl
    ; if mode = Web.Manager then
        new_heading_form dir mode |> Web.pl
      else ()
    ; Html.center_end |> Web.pl
    }
  ]
;
value mk_page dir mode =
  let title =
    "Sanskrit Corpus " ^
    (mode |> Web.string_of_corpus_mode |> String.capitalize)
  in
  let clickable_title =
    let query =
      Cgi.query_of_env [ (Params.corpus_mode, Web.string_of_corpus_mode mode) ]
    in
    title
    |> Html.anchor_ref (Cgi.url Web.corpus_manager_cgi ~query)
    |> Html.h1_title
  in
  try
    do
    { Web.maybe_http_header ()
    ; Web.page_begin (Html.title title)
    ; Html.body_begin Html.Chamois_back |> Web.pl
    ; Web.open_page_with_margin 15
    ; clickable_title |> Web.print_title (Some Html.default_language)
    ; body dir mode
    ; Web.close_page_with_margin ()
    ; Web.page_end Html.default_language True
    }
  with
  [ Sys_error msg -> Web.abort Html.default_language Control.sys_err_mess msg
  ]
;
