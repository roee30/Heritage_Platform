(* This program produces the pages corpus.html (Corpus interface).  *)

value make lang =
  let title = "Sanskrit Corpus" in
  do
  { Web.open_html_file (Web.corpus_page lang) (Html.title title)
  ; Html.body_begin Html.Chamois_back |> Web.pl
  ; Web.open_page_with_margin 15
  ; Html.h1_title title |> Web.print_title (Some lang)
  ; Html.anchor_ref (Web.corpus_manager_cgi) "Explore corpus" |> Web.pl
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
