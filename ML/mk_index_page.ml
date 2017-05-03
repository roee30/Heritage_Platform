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

value print_query lang cgi = do
  { pl (cgi_begin cgi "convert")
  ; print_lexicon_select (lexicon_of lang)
  ; pl html_break
  ; pl (text_input "focus" "q")
  ; print_transliteration_switch "trans"
  ; pl html_break
  ; pl (submit_input "Search")  
  ; pl (reset_input "Reset")
  ; pl cgi_end
  }
;
value print_query_dummy lang cgi = do
  { pl (cgi_begin cgi "convert")
  ; pl (hidden_input "lex" (lexicon_of lang))
  ; pl (text_input "unused" "q")
  ; ps "ASCII"
  ; pl html_break
  ; pl (submit_input "Search")  
  ; pl (reset_input "Reset")
  ; pl cgi_end
  }
;
value print_query_lemma lang cgi = do
  { pl (cgi_begin cgi "convert1")
  ; pl (hidden_input "lex" (lexicon_of lang))
  ; pl (text_input "focus1" "q")
  ; print_transliteration_switch "trans1"
  ; pl html_break 
  ; pl (option_select_default "c" 
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
        ])
  ; pl html_break 
  ; pl (submit_input "Search")
  ; pl (reset_input "Reset")
  ; pl cgi_end
  }
;
value indexer lang = do (* Not yet in xhtml validated form *)
  { open_html_file (indexer_page lang) heritage_dictionary_title 
  ; pl (body_begin (background Chamois))
    (* will be closed by [close_html_file] *)
  ; print_title (Some lang) (dico_title lang)
  ; pl center_begin (* closed at the end *)
     (* Sankskit index section *)
  ; print_index_help lang
  ; print_query lang index_cgi
  ; pl html_paragraph
  ; pl hr
     (* Sankskrit made easy section (Sanskrit for dummies) *)
  ; pl (anchor_def "easy" "") 
  ; pl dummy_title_en
  ; print_dummy_help_en ()
  ; print_query_dummy lang dummy_cgi
  ; pl html_paragraph
  ; pl hr
     (* Stemmer section *)
  ; pl stem_title_en
  ; pl (anchor_def "stemmer" "") (* for access from dock link *)
  ; print_stemmer_help_en ()
  ; print_query_lemma lang lemmatizer_cgi
  ; pl html_break
  ; pl center_end
  ; close_html_file lang True
  }
;
indexer French
;
indexer English
;
(*i end; i*)
