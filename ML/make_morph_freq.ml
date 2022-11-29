(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Amba Kulkarni & Sriram Krishnan             *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_morph_freq = struct  i*)

(* Prepares the decorated trie structures of the 
   (stem, inflectional_morph, base, derivational_morph, freq) tables
   from the text files in Resources/DATA  *)
   
(* The frequency tables are generated using a parallel corpus obtained by 
   mapping DCS Sentences and the analysis of the Heritage Reader. 
   These are stored in tsv files in Resources/DATA in WX Notation, 
   and retrieved here to generate rem files for the same *)
      
(* This list of files is converted to decorated tries here:
   a. [comp_morph_freq.tsv] -> list of compound components' (ii-s) 
                         stem and morph analysis and their frequencies
   b. [pada_morph_freq.tsv] -> list of stem and morph analysis of
                         words (which are not part of any compound) 
                         and their frequencies
*)

open Deco; (* [ empty ] *)
open Word; (* [ diff ] *)
open Lexmap; (* [ lexmaps addl ] *)

type stem = string
and morph = string
and base = string
and base_morph = string
and freq = int;

type freqs = list freq
;
type morph_frq = list (morph * base * base_morph * freqs)
;
type sm_freq = Lexmap.lexmap morph_frq
;

value morph_freq = ref (Deco.empty : sm_freq)
;

value add_morph_freq w d m =
  morph_freq.val := Lexmap.addl morph_freq.val w (d w, m)
;

value diff_str str w = Word.diff w (Encode.code_string_WX str)
;

(* Add entry to the morph_freq decorated trie structure *)
value deco_of_sm_freq (stm,mrph,bs,bs_mrph,frq) = 
  let word = Transduction.code_raw_WX (stm) 
  and delta = diff_str stm in
  add_morph_freq word delta (mrph,bs,bs_mrph,[ frq ])
;
(* To make the decorated trie structure which stores all the stems and their 
   possbile inflectional morph, base and derivational morph and frequencies, 
   and optionally minimize the trie *)
value rec process_file chin =
  try let line = input_line chin in
  let (a,b,c,d,e) = 
    Scanf.sscanf line "%s@\t%s@\t%s@\t%s@\t%d" (fun v w x y z -> (v,w,x,y,z)) in 
  do { 
    deco_of_sm_freq (a,b,c,d,e);
    process_file chin
  }
  with [ End_of_file -> (* [let lexicon = (morph_freq.val : Trie.trie) in
                            let mini = Mini.minimize lexicon  in
                            mini] *)
                        morph_freq.val
       ]
;

(* Constructing the decorated trie structure and pushing it to STD_OUT *)
value make_deco txt_file = 
  let chin = open_in txt_file in
  let output = process_file chin in
  output_value stdout output
;

value main = 
  make_deco Sys.argv.(1)
;
