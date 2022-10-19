(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This program produces the page grammar.html (Grammarian interface) *)

(*i executable module Mk_grammar_page = struct i*)

open Html;
open Web; (* ps pl abort etc. *)

value title = h1_title "The Sanskrit Grammarian"
and subtitle_d = h1_title "Declension"
and subtitle_c = h1_title "Conjugation"
and meta_title = title "Sanskrit Grammarian Query"
;
value deva = (Paths.default_display_font="deva") 
;
value print_declension_help lang =
  if narrow_screen then () else do
    { ps (par_begin G2)
    ; ps "Submit stem and gender for declension:"
    ; pl html_break 
    ; ps "(Use Any for deictic pronouns and numbers)"
    ; pl par_end (* G2 *)
    }
;
value print_conjugation_help lang =
  if narrow_screen then () else do
    { ps (par_begin G2)
    ; ps "Submit root and present class"
    ; pl html_break 
    ; ps "(Use 0 for roots with no present forms)"
    ; pl par_end (* G2 *)
    }
;
value print_output_font () = do
  { pl html_break 
  ; ps "Output font "
  ; pl (option_select_default "font" 
        [ (" Roman","roma",not deva)  (* default roma - Computer *)
        ; (" Devanagari","deva",deva) (* default deva - Simputer *)
        ])
  ; pl html_break 
  ; pl (submit_input "Send")
  ; pl (reset_input "Reset")
  ; pl cgi_end
  }
;
value grammarian lang = do
  { open_html_file (grammar_page lang) meta_title 
  ; pl (body_begin (background Chamois))
  ; print_title (Some lang) title 
  ; pl center_begin
  ; pl subtitle_d 
  ; print_declension_help lang
  ; pl (cgi_begin decls_cgi "convert")
  ; pl (hidden_input "lex" (lexicon_of lang))
  ; pl (text_input "focus" "q")
  ; print_transliteration_switch "trans" 
  ; pl html_break
  ; ps "Gender "
  ; pl (option_select_default "g" 
        [ (" Mas ","Mas",True)  (* default Mas *)
        ; (" Fem ","Fem",False) 
        ; (" Neu ","Neu",False)  
        ; (" Any ","Any",False) (* deictic pronouns and numbers *)
        ])
  ; print_output_font ()
  ; pl html_break 
  ; pl subtitle_c
  ; pl (xml_empty_with_att "a" [ ("name","roots") ]) (* for portal ref *)
  ; print_conjugation_help lang
  ; pl (cgi_begin conjs_cgi "convert1")
  ; pl (hidden_input "lex" (lexicon_of lang))
  ; pl (text_input "focus1" "q")
  ; print_transliteration_switch "trans1" 
  ; pl html_break 
  ; ps "Present class "
  ; pl (option_select_default "c" (* gana = present class *)
        [ (" 1 ", "1",  True)  (* default 1 *)
        ; (" 2 ", "2",  False) 
        ; (" 3 ", "3",  False)  
        ; (" 4 ", "4",  False)  
        ; (" 5 ", "5",  False)  
        ; (" 6 ", "6",  False)
        ; (" 7 ", "7",  False)  
        ; (" 8 ", "8",  False)  
        ; (" 9 ", "9",  False)  
        ; (" 10", "10", False)  
        ; (" 11", "11", False) (* denominative verbs *)
        ; (" 0", "0", False)  (* secondary conjugations *)
        ])
  ; print_output_font ()
  ; pl center_end
  ; close_html_file lang True
  }
;
grammarian French
;
grammarian English
;
(*i end; i*)
