(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
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
        ] in
    Cgi.url corpus_manager_cgi ~query |> escape in
  let label = Filename.basename dir in
  anchor_ref url label
;
value uplinks dir permission =
  let aux dir =
   let updirs = Dir.split dir in
   let process i x = 
     String.concat Filename.dir_sep (List2.take_prefix (i + 1) updirs) in
   List.map (link permission) (List.mapi process updirs) in
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
value sentence_links dir permission skt_font lex sentences =
  let to_anchor_ref sentence =
    let font = font_of_string skt_font in
    let encoding =
        match font with
        [ Deva -> Corpus.Encoding.Devanagari
        | Roma -> Corpus.Encoding.IAST
        ] in
    let text = Corpus.Sentence.text encoding sentence 
    and url = Web_corpus.url dir permission sentence skt_font lex in
    let display =
      match font with
      [ Deva -> deva16_blue
      | Roma -> span Trans16
      ] in 
    text
    |> anchor_ref (url |> escape)
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
  cgi_begin (cgi_bin "skt_heritage") "" ^ (* fakes [mk_reader_page] *)
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
value htmlify_group dir permission font lex (group, gap) =
  let (ol, group_id) =
    match group with
    [ [] -> ("", "")
    | [ h :: _ ] ->
      let id = Corpus.Sentence.id h in
      let group_id = string_of_int id in
      (ol ~li_id_prefix:"" ~start:id (sentence_links dir permission font lex group),
       group_id)
    ] in
  let div_id = "group" ^ group_id in
  let add_sentence_form =
    button
      ~id:"add_sentence"
      ~onclick:{ js_funid = "hideShowElement" ; js_funargs = [ div_id ] }
      (string_of_gap gap) ^
    elt_begin_attrs [ ("id", div_id) ] "div" Hidden_ ^
    html_paragraph ^
    add_sentence_form dir permission gap ^
    div_end in
  ol ^ if permission = Web_corpus.Annotator then add_sentence_form else ""

;
value group_sentences dir sentences =
  let ids = List.map Corpus.Sentence.id sentences in
  let dict = List.combine ids sentences in
  let groups = ids |> groups_with_gaps |> add_init_gap in
  List.map (fun (x, y) -> (List.map (fun x -> List.assoc x dict) x, y)) groups
;
value new_section_form dir permission =
  let perm = Web_corpus.string_of_permission permission in
  cgi_begin mkdir_corpus_cgi "" ^
  "New section: " ^ uplinks dir permission ^
  hidden_input Mkdir_corpus_params.parent_dir dir ^
  hidden_input Mkdir_corpus_params.permission perm ^
  text_input "new_section" Mkdir_corpus_params.dirname ^ " " ^
  submit_input "Create" ^ cgi_end
;
value section_selection_form dir permission sections font lex =
  let selection_prompt =
    let submit_button_label = Web_corpus.(
      match permission with
      [ Reader -> "Read"
      | Annotator -> "Annotate"
      | Manager -> "Manage"
      ]) in
    uplinks dir permission ^
    section_selection dir (List.map Corpus.Section.label sections)  ^ " " ^
    submit_input submit_button_label
  and corpus_permission = Web_corpus.string_of_permission permission in
  cgi_begin corpus_manager_cgi "" ^
  big (
    selection_prompt ^
    hidden_input Params.corpus_permission corpus_permission ^
    hidden_input Params.corpus_font font ^
    hidden_input Params.corpus_lex lex 
      ) ^
  cgi_end
;
value body dir permission font lex = Web_corpus.(
  match contents dir with
  [ Empty ->
    do
    { uplinks dir permission |> big |> pl
    ; open_page_with_margin 30
    ; match permission with
        [ Reader -> "Empty corpus"
        | Annotator -> add_sentence_form dir permission max_gap
        | Manager -> new_section_form dir permission
        ]
      |> pl
    ; close_page_with_margin ()
    }
  | Sentences sentences ->
    let groups = group_sentences dir sentences in
    do
    { uplinks dir permission |> big |> pl
    ; open_page_with_margin 30
    ; if permission = Manager then
        "No action available." |> pl
      else
        groups |> List.map (htmlify_group dir permission font lex) |> List.iter pl
    ; close_page_with_margin ()
    }
  | Sections sections ->
    do
    { center_begin |> pl
    ; section_selection_form dir permission sections font lex |> pl
    ; html_break |> pl
    ; if permission = Web_corpus.Manager then
        new_section_form dir permission |> pl
      else ()
    ; center_end |> pl
    }
  ])
;
value capitalize_ascii str = (* Oct 2020 [String.capitalize_ascii] deprecated *)
  str |> Bytes.of_string |> Bytes.capitalize_ascii |> Bytes.to_string
;
value mk_page dir permission font lex =
  let str_permission = Web_corpus.string_of_permission permission in
  let title_str =
    "Sanskrit Corpus " ^ (str_permission |> capitalize_ascii)
  and env = 
    [ (Params.corpus_permission, str_permission) 
    ; (Params.corpus_lex, lex)
    ; (Params.corpus_font, font)
    ] in
  let clickable_title =
    let query = Cgi.query_of_env env in
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
  ; body dir permission font lex
  ; close_page_with_margin ()
  ; page_end default_language True
  }
;
