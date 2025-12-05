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
val corpus_dir : string
  
(* Parameter for specifying the sentence number when the corpus mode is
   enabled.  *)
val sentence_no : string
  
(* Parameter for specifying the permission of the corpus user: 
   ["reader"], ["annotator"] or ["manager"].  *)
val corpus_permission : string
  

