(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Amba Kulkarni & Sriram Krishnan             *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_freq = struct  i*)

(* Prepares the decorated trie structures of the word-freq tables
   from the text files in Resources/DATA  *)
   
(* The frequency tables are generated using a parallel corpus obtained by mapping DCS Sentences and the analysis of the Heritage Reader. 
   These are stored in txt files in Resources/DATA, and retrieved here to generate rem files for the same *)
   
(* This list of files is converted to  here:
   a. [comp_freq.txt] -> list of compound components (ii-s) and their frequencies
   b. [pada_freq.txt] -> list of words (which are not part of any compound) and their frequencies
   c. [word_freq.txt] -> list of both the comp and pada together and their frequencies
*)

open Deco; (* [ empty ] *)
open Word; (* [ diff ] *)
open Lexmap; (* [ lexmaps addl ] *)

type freq = int
;
type freqs = list freq 
;
type wfreq = Lexmap.lexmap freqs
;
value word_freq = ref (Deco.empty : wfreq)
;
value add_word_freq w d f =
  word_freq.val:= Lexmap.addl word_freq.val w (d w, f)
;        
value diff_str str w = Word.diff w (Encode.code_string_WX str)
;
value split2 str fmt =  Scanf.sscanf str fmt (fun x y -> (x,y))
;
value deco_of_word_freq m = 
  match m with
  [ (frm,freq) -> 
    let word = Transduction.code_raw_WX (frm) 
    and delta = diff_str frm in
    add_word_freq word delta freq
  ]
;
(* To make the decorated trie structure which stores all the words and their frequencies, minimize the trie *)
value rec process_file chin =
  try let line = input_line chin in
  let (a,b) = Scanf.sscanf line "%s@\t%d" (fun x y -> (x,y)) in 
  do { 
    deco_of_word_freq (a,b);
    process_file chin
  }
  with [ End_of_file -> (* [let lexicon = (word_freq.val : Trie.trie) in
                            let mini = Mini.minimize lexicon  in
                            mini] *)
                        word_freq.val
       ]
;
value make_deco txt_file = 
  let chin = open_in txt_file in
  let output = process_file chin in
  output_value stdout output
;
value main = 
  make_deco Sys.argv.(1)
;
