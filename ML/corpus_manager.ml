(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

open Html;
open Web;

(*************)
(* Utilities *)
(*************)

(* Type representing interval of missing integers in a sorted list.  *)
type gap = { start : int; stop : int }
;
(* The following functions assume that the given list is sorted in
   increasing order and represents a subset of positive integers. 
   In particular, the lowest bound of a gap is at least [1] and the
   greatest at most [max_int]). We call "group" a list of consecutive
   integers.  *)

value max_gap = { start = 1; stop = max_int }
;
value string_of_gap gap =
  if gap.stop = max_int then
    Printf.sprintf "> %d" (gap.start - 1)
  else
    Printf.sprintf "%d - %d" gap.start gap.stop
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
value big text = div Latin16 text
;
value link permission dir =
  let url =
    let query =
      Cgi.query_of_env
        [ (Params.corpus_dir, dir)
        ; (Params.corpus_permission, Web_corpus.string_of_permission permission)
        ]
    in
    Cgi.url corpus_manager_cgi ~query |> escape
  in
  let label = Filename.basename dir in
  anchor_ref url label
;
value uplinks dir permission =
  let aux dir =
   let updirs = Dir.split dir in
   let updirs =
     List.mapi (fun i x ->
         String.concat Filename.dir_sep (List2.take_prefix (i + 1) updirs)
       ) updirs
   in
   List.map (link permission) updirs
  in
  let uplinks_str =
    dir
    |> aux
    |> String.concat " / "
  in
  let final_sep = if uplinks_str <> "" then " / " else "" in
  uplinks_str ^ final_sep

;
(* Display sentences with format "sentence || sentno" like in citations
   file.  *)
value sentence_links dir permission sentences =
  let to_anchor_ref sentence =
    let font = Multilingual.font_of_string Paths.default_display_font in
    let encoding =
        match font with
        [ Multilingual.Deva -> Corpus.Encoding.Devanagari
        | Multilingual.Roma -> Corpus.Encoding.IAST
        ] in
    let text = Corpus.Sentence.text encoding sentence in
    let display =
      match font with
      [ Multilingual.Deva -> deva16_blue
      | Multilingual.Roma -> span Trans16
      ] in 
    text
    |> anchor_ref (sentence |> Web_corpus.url dir permission |> escape)
    |> display in
  List.map to_anchor_ref sentences
;
value section_selection dir sections =
  let options =
    let prefixes =
      List.map (fun x -> Filename.concat dir x) sections in
    List.combine prefixes sections in
  option_select_label Params.corpus_dir options
;
value add_sentence_form dir permission gap =
  cgi_begin (cgi_bin "skt_heritage") "" ^
  "Add sentence: " ^ uplinks dir permission ^
  hidden_input Params.corpus_dir dir ^
  hidden_input Params.corpus_permission (Web_corpus.string_of_permission permission) ^
  int_input Params.sentence_no
    ~step:1
    ~min:gap.start
    ~max:gap.stop
    ~val:gap.start
    ~id:Params.sentence_no ^ " " ^
  submit_input "Add"
  ^
  cgi_end
  ;
value htmlify_group dir permission (group, gap) =
  let (ol, group_id) =
    match group with
    [ [] -> ("", "")
    | [ h :: _ ] ->
      let id = Corpus.Sentence.id h in
      let group_id = string_of_int id in
      (ol ~li_id_prefix:"" ~start:id (sentence_links dir permission group),
       group_id)
    ]
  in
  let div_id = "group" ^ group_id in
  let add_sentence_form =
    button
      ~id:"add_sentence"
      ~onclick:{ js_funid = "hideShowElement" ; js_funargs = [ div_id ] }
      (string_of_gap gap) ^
    elt_begin_attrs [ ("id", div_id) ] "div" Hidden_ ^
    html_paragraph ^
    add_sentence_form dir permission gap ^
    div_end
  in
  ol ^ if permission = Web_corpus.Annotator then add_sentence_form else ""

;
value group_sentences dir sentences =
  let ids = List.map Corpus.Sentence.id sentences in
  let dict = List.combine ids sentences in
  let groups = ids |> groups_with_gaps |> add_init_gap in
  List.map (fun (x, y) -> (List.map (fun x -> List.assoc x dict) x, y)) groups
;
value new_section_form dir permission =
  cgi_begin mkdir_corpus_cgi "" ^
  "New section: " ^ uplinks dir permission ^
  hidden_input Mkdir_corpus_params.parent_dir dir ^
  hidden_input Mkdir_corpus_params.permission (Web_corpus.string_of_permission permission) ^
  text_input "new_section" Mkdir_corpus_params.dirname ^ " " ^
  submit_input "Create"
  ^
  cgi_end
  ;
value section_selection_form dir permission sections =
  let selection_prompt =
    let submit_button_label = Web_corpus.(
      match permission with
      [ Reader -> "Read"
      | Annotator -> "Annotate"
      | Manager -> "Manage"
      ]
    )
    in
    uplinks dir permission ^
    section_selection dir (List.map Corpus.Section.label sections)  ^ " " ^
    submit_input submit_button_label
  in
  cgi_begin corpus_manager_cgi "" ^
  big (
    selection_prompt ^
    hidden_input Params.corpus_permission (Web_corpus.string_of_permission permission)
  ) ^
  cgi_end
;
value body dir permission =
  match Web_corpus.contents dir with
  [ Web_corpus.Empty ->
    do
    { uplinks dir permission |> big |> pl
    ; open_page_with_margin 30
    ; match permission with
        [ Web_corpus.Reader -> "Empty corpus"
        | Web_corpus.Annotator -> add_sentence_form dir permission max_gap
        | Web_corpus.Manager -> new_section_form dir permission
        ]
      |> pl
    ; close_page_with_margin ()
    }
  | Web_corpus.Sentences sentences ->
    let groups = group_sentences dir sentences in
    do
    { uplinks dir permission |> big |> pl
    ; open_page_with_margin 30
    ; if permission = Web_corpus.Manager then
        "No action available." |> pl
      else
        groups |> List.map (htmlify_group dir permission) |> List.iter pl
    ; close_page_with_margin ()
    }
  | Web_corpus.Sections sections ->
    do
    { center_begin |> pl
    ; section_selection_form dir permission sections |> pl
    ; html_break |> pl
    ; if permission = Web_corpus.Manager then
        new_section_form dir permission |> pl
      else ()
    ; center_end |> pl
    }
  ]
;
value mk_page dir permission =
  let title_str =
    "Sanskrit Corpus " ^
    (permission |> Web_corpus.string_of_permission |> String.capitalize)
  in
  let clickable_title =
    let query =
      Cgi.query_of_env [ (Params.corpus_permission, Web_corpus.string_of_permission permission) ]
    in
    title_str
    |> anchor_ref (Cgi.url corpus_manager_cgi ~query)
    |> h1_title
  in
  do
  { maybe_http_header ()
  ; page_begin (title title_str)
  ; body_begin Chamois_back |> pl
  ; open_page_with_margin 15
  ; clickable_title |> print_title (Some default_language)
  ; body dir permission
  ; close_page_with_margin ()
  ; page_end default_language True
  }
;
