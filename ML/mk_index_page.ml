(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This stand-alone program produces the page [indexer_page.html] used as
   index interface to the Sanskrit Heritage dictionary. *)

(*i module Mk_index_page = struct i*)
open Html;
open Web; (* ps pl abort etc. *)

value deva = (Paths.default_display_font="deva") 
;
value print_index_help lang = 
  if narrow_screen then () else do
  { par_begin G2 |> pl
  ; "Search for an entry matching an initial pattern:" |> ps
  ; html_break |> pl 
  ; print_transliteration_help lang
  ; par_end |> pl (* G2 *)
  }
;
value print_dummy_help_en () = 
  if narrow_screen then () else do
  { par_begin G2 |> pl
  ; "The simplified interface below allows search without diacritics" |> ps
  ; html_break |> pl 
  ; "Proper names may be entered with an initial capital" |> pl
  ; par_end |> pl (* G2 *)
  }
;
value print_query lang cgi = do
  { cgi_begin cgi "convert" |> pl
  ; print_lexicon_select (lexicon_of lang)
  ; html_break |> pl
  ; text_input "focus" "q" |> pl  
  ; print_transliteration_switch "trans" 
  ; html_break |> pl
  ; submit_input "Search" |> pl
  ; reset_input "Reset" |> pl
  ; cgi_end |> pl
  }
;
value print_query_dummy lang cgi = do
  { cgi_begin cgi "convert" |> pl
  ; hidden_input "lex" (lexicon_of lang) |> pl
  ; text_input "unused" "q" |> pl
  ; "ASCII" |> pl
  ; html_break |> pl
  ; submit_input "Search" |> pl  
  ; reset_input "Reset" |> pl
  ; cgi_end |> pl
  }
;
value print_query_lemma lang cgi = do
  { cgi_begin cgi "convert1" |> pl
  ; hidden_input "lex" (lexicon_of lang) |> pl
  ; text_input "focus1" "q" |> pl
  ; print_transliteration_switch "trans1"
  ; html_break |> pl
  ; option_select_default "c" 
        [ (" Noun ","Noun",True)  (* default Noun *)
        ; (" Pron ","Pron",False) 
        ; (" Verb ","Verb",False) 
        ; (" Part ","Part",False) 
        ; (" Inde ","Inde",False) 
        ; (" Absya ","Absya",False) 
        ; (" Abstvaa ","Abstvaa",False) 
        ; (" Voca ","Voca",False) 
        ; (" Iic " ,"Iic", False) 
        ; (" Ifc " ,"Ifc", False) 
        ; (" Iiv " ,"Iiv", False) 
        ; (" Piic ","Piic",False) 
        ] |> pl
  ; html_break |> pl 
  ; submit_input "Search" |> pl
  ; reset_input "Reset" |> pl
  ; cgi_end |> pl
  }
;
value indexer lang = do (* Not yet in xhtml validated form *)
  { open_html_file (indexer_page lang) heritage_dictionary_title 
  ; body_begin (background Chamois) |> pl (* closed by [close_html_file] *)
  ; print_title (Some lang) (dico_title lang)
  ; center_begin |> pl (* closed at the end *)
     (* Sankskit index section *)
  ; print_index_help lang
  ; print_query lang index_cgi
  ; html_paragraph |> pl
  ; hr |> pl
     (* Sankskrit made easy section (Sanskrit for dummies) *)
  ; anchor_def "easy" "" |> pl 
  ; dummy_title_en |> pl
  ; print_dummy_help_en ()
  ; print_query_dummy lang dummy_cgi
  ; html_paragraph |> pl
  ; hr |> pl
     (* Stemmer section *)
  ; stem_title_en |> pl
  ; anchor_def "stemmer" "" |> pl (* for access from dock link *)
  ; print_stemmer_help_en ()
  ; print_query_lemma lang lemmatizer_cgi
  ; html_break |> pl
  ; center_end |> pl
  ; close_html_file lang True
  }
;
indexer French
;
indexer English
;
(*i end; i*)
