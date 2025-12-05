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
open Html
  
open Web
  
(* ps pl abort etc. *)
let title = h1_title "The Sanskrit Grammarian"
and subtitle_d = h1_title "Declension"
and subtitle_c = h1_title "Conjugation"
and meta_title = title "Sanskrit Grammarian Query"
  
let deva = Paths.default_display_font = "deva"
  
let print_declension_help lang =
  if narrow_screen
  then ()
  else
    (ps (par_begin G2);
     ps "Submit stem and gender for declension:";
     pl html_break;
     ps "(Use Any for deictic pronouns and numbers)";
     pl par_end)
  
(* G2 *)
let print_conjugation_help lang =
  if narrow_screen
  then ()
  else
    (ps (par_begin G2);
     ps "Submit root and present class";
     pl html_break;
     ps "(Use 0 for secondary conjugations)";
     pl par_end)
  
(* G2 *)
let print_output_font () =
  (pl html_break;
   ps "Output font ";
   pl
     (option_select_default "font"
        [ (" Roman", "roma", (not deva)); (* default roma - Computer *)
          (" Devanagari", "deva", deva) ]);
   (* default deva - Simputer *)
   pl html_break;
   pl (submit_input "Send");
   pl (reset_input "Reset");
   pl cgi_end)
  
let grammarian lang =
  (open_html_file (grammar_page lang) meta_title;
   pl (body_begin (background Chamois));
   print_title (Some lang) title;
   pl center_begin;
   pl subtitle_d;
   print_declension_help lang;
   pl (cgi_begin decls_cgi "convert");
   pl (hidden_input "lex" (lexicon_of lang));
   pl (text_input "focus" "q");
   print_transliteration_switch "trans";
   pl html_break;
   ps "Gender ";
   pl
     (option_select_default "g"
        [ (" Mas ", "Mas", true); (* default Mas *) (" Fem ", "Fem", false);
          (" Neu ", "Neu", false); (" Any ", "Any", false) ]);
   (* deictic pronouns and numbers *)
   print_output_font ();
   pl html_break;
   pl subtitle_c;
   pl (xml_empty_with_att "a" [ ("name", "roots") ]);
   (* for portal ref *)
   print_conjugation_help lang;
   pl (cgi_begin conjs_cgi "convert1");
   pl (hidden_input "lex" (lexicon_of lang));
   pl (text_input "focus1" "q");
   print_transliteration_switch "trans1";
   pl html_break;
   ps "Present class ";
   pl
     (option_select_default "c" (* gana = present class *)
        [ (" 1 ", "1", true); (* default 1 *) (" 2 ", "2", false);
          (" 3 ", "3", false); (" 4 ", "4", false); (" 5 ", "5", false);
          (" 6 ", "6", false); (" 7 ", "7", false); (" 8 ", "8", false);
          (" 9 ", "9", false); (" 10", "10", false); (" 11", "11", false);
          (* denominative verbs *) (" 0", "0", false) ]);
   (* secondary conjugations *)
   print_output_font ();
   pl center_end;
   close_html_file lang true)
  
let _ = grammarian French
  
let _ = grammarian English
  

