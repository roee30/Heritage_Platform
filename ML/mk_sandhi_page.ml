(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This stand-alone program produces the page [sandhi_page.html] used as
   sandhi computation interface to the Sandhi Engine. *)

(*i module Mk_sandhi_page = struct i*)

open Html;
open Web; (* ps pl abort etc. *)

value title = h1_title "The Sandhi Engine"
and meta_title = title "Sanskrit Sandhi Engine"
and back_ground = background Chamois
  (*[ obs if narrow_screen then background Chamois else Pict_geo ]*)
;
value sandhier lang = do
  { open_html_file (sandhi_page lang) meta_title 
  ; pl (body_begin back_ground)
  ; print_title None title
  ; pl center_begin
  ; pl (cgi_begin sandhier_cgi "convert2")
    (* following necessary to transmit the lexicon choice of the session *)
  ; pl (hidden_input "lex" (lexicon_of lang))
  ; pl (text_input "focus1" "l") 
  ; pl (text_input "focus2" "r")
  ; print_transliteration_switch "trans"
  ; pl html_break 
  ; pl (option_select_default "k" 
         [ (" External ","external",True)  (* default external *)
         ; (" Internal ","internal",False) 
         ])
  ; pl html_break 
  ; pl (submit_input "Send")
  ; pl (reset_input "Reset")
  ; pl cgi_end
  ; pl html_break 
  ; pl center_end
  ; close_html_file lang True
  }
;
sandhier French
;
sandhier English
;
(*i end; i*)
