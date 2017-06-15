(* CGI script [add_corpus] for adding a sentence to the corpus.  *)

(*i module Add_corpus = struct i*)

value make () =
  let title = "TODO" in
  let meta_title = Html.title title in
  let style = Html.background Html.Chamois in
  do
  { Web.http_header |> Web.pl
  ; Web.page_begin meta_title
  ; Html.body_begin style |> Web.pl
  ; Html.h1_title title |> Web.pl
  ; Web.page_end Html.default_language True }
;

(***************)
(* Entry point *)
(***************)
value main = make ()
;

(*i end; i*)
