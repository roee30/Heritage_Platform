(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* This program produces the pages corpus.html (Corpus interface).  *)
open Html
  
open Web
  
let permission_selection =
  let selection permissions =
    let select permission =
      let permission_str = Web_corpus.string_of_permission permission
      in
        ((String.capitalize_ascii permission_str), permission_str,
         (permission = Web_corpus.Reader))
    in List.map select permissions in
  let read_only_permissions = [ Web_corpus.Reader ] in
  let other_permissions = let open Web_corpus in [ Annotator; Manager ] in
  let all_permissions = read_only_permissions @ other_permissions
  in
    selection
      (if corpus_read_only then read_only_permissions else all_permissions)
  
let make lang =
  let title_str = "Sanskrit Corpus"
  in
    (open_html_file (corpus_page lang) (title title_str);
     (body_begin Chamois_back) |> pl;
     open_page_with_margin 15;
     (h1_title title_str) |> (print_title (Some lang));
     center_begin |> pl;
     ((cgi_begin corpus_manager_cgi "") ^
        ("Capacity: " ^
           ((option_select_default Params.corpus_permission
               permission_selection)
              ^ (" " ^ ((submit_input "OK") ^ cgi_end)))))
       |> pl;
     center_end |> pl;
     close_page_with_margin ();
     close_html_file lang true)
  
let main = (make English; make French)
  

