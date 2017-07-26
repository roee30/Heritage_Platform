(**************************************************************************)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This program produces the pages corpus.html (Corpus interface).  *)

value mode_selection =
  List.map (fun mode ->
    let mode_str = Web.string_of_corpus_mode mode in
    (String.capitalize mode_str, mode_str, mode = Web.Reader)
  ) Web.[ Reader; Annotator; Manager ]
;
value make lang =
  let title =
    if Web.corpus_toggle then "Sanskrit Corpus" else "No corpus available"
  in
  do
  { Web.open_html_file (Web.corpus_page lang) (Html.title title)
  ; Html.body_begin Html.Chamois_back |> Web.pl
  ; Web.open_page_with_margin 15
  ; Html.h1_title title |> Web.print_title (Some lang)
  ; Html.center_begin |> Web.pl
  ; if Web.corpus_toggle then
      Web.cgi_begin Web.corpus_manager_cgi "" ^
      "Mode: " ^
      Html.option_select_default Params.corpus_mode mode_selection ^ " " ^
      Html.submit_input "OK" ^
      Web.cgi_end |> Web.pl
    else ()
  ; Html.center_end |> Web.pl
  ; Web.close_page_with_margin ()
  ; Web.close_html_file lang True
  }
;
value main =
  do
  { make Html.English
  ; make Html.French
  }
;
