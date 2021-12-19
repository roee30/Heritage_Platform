(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Common parameters of different CGIs related to the reader *)

(* Parameter for specifying the corpus subdirectory when the corpus mode
   is enabled.  *)
value corpus_dir : string
;
(* Parameter for specifying the sentence number when the corpus mode is
   enabled.  *)
value sentence_no : string
;
(* Parameter for specifying the permission of the corpus user: 
   ["reader"], ["annotator"] or ["manager"].  *)
value corpus_permission : string
;
(* Parameter for specifying the font for printing sentences and lemmas *)
value corpus_font : string
;
(* Parameter for specifying the lexicon for stem look-up in lemmas *)
value corpus_lex : string
;
