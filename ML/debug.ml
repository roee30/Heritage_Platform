(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(****************************************************************)
(* Beware - remove foo.mli and make depend before debugging foo *)
(* Better yet - remove *.mli (saving them) to get transparency. *) 
(****************************************************************)

(* debugging context building - to adapt according to needs *)
(* call with "make debug" to get interactive Ocaml loop and text *)

#load "../ZEN/list2.cmo"; 
#load "../ZEN/gen.cmo";
open Gen;
#load "../ZEN/word.cmo"; open Word;
#load "../ZEN/share.cmo";    
#load "../ZEN/trie.cmo";    
#load "../ZEN/deco.cmo"; open Deco;   
#load "../ZEN/lexmap.cmo"; open Lexmap;   
#load "../ZEN/zen_lexer.cmo";  

#load "paths.cmo"; 
#load "version.cmo";
open Version;
#load "date.cmo"; 
(* #load "html.cmo"; *)
(* #load "web.cmo";
open Web;
#load "cgi.cmo"; *)
#load "canon.cmo";
open Canon;
#load "phonetics.cmo";
open Phonetics;   

open List;
#load "min_lexer.cmo"; 
#load "transduction.cmo"; 
open Transduction;
#load "encode.cmo"; 
open Encode;

#load "sandhi.cmo"; 
open Sandhi;


#load "chunker.cmo"; 
#load "skt_lexer.cmo";  
#load "sanskrit.cmo"; 
open Sanskrit;  

#load "int_sandhi.cmo"; 
open Int_sandhi; 
#load "order.cmo"; 
#load "sandhi.cmo"; 
#load "inflected.cmo"; 

#load "parts.cmo";  

#load "control.cmo";  

#load "pada.cmo";  

#load "verbs.cmo";  
open Verbs; 

(* 
#load "int_sandhi.cmo"; 
open Int_sandhi; 
#load "order.cmo"; 
#load "sandhi.cmo"; 
#load "inflected.cmo"; 
#load "make_preverbs.cmo"; 
open Make_preverbs;
all_preverbs_morph;
*)
(* 
#load "dispatch.cmo"; 
#load "bank_lexer.cmo"; 
#load "graph.cmo"; 
open Graph;
*)

(* OBS
#load "fr_lexer.cmo";  
#load "dico_lexer.cmo"; 
#load "french.cmo"; 
open French;  
#load "order.cmo";  
#load "naming.cmo"; 
open Naming;


#load "morpho_string.cmo"; 
#load "morpho.cmo"; 
open Morpho; 

 
#load "inflected.cmo"; 
open Inflected; 
#load "index.cmo";  
 
(* #load "subst.cmo"; open Subst; *)

#load "bank_lexer.cmo"; 
#load "nouns.cmo"; 
open Nouns; 

#load "parts.cmo";  
open Parts;  
#load "pada.cmo";  
open Pada; 

(* first remove verbs.mli in order to export all definitions *)
#load "verbs.cmo";  
open Verbs; 
*)
print_string "done"; 

(* Morpho debug 

(* #load "../ZEN/mini.cmo"; *)
(* #load "make_inflected.cmo"; open Make_inflected; -- costly *)
(* #load "morpho_debug.cmo"; open Morpho_debug; *)

(* Debug de Roots
value vl = (Gen.gobble Data.verblinks_file : list Roots.preverbs_tree);
List.assoc (string_to_skt "h.r#1") vl;
List.assoc "vih.r" vl;
List.assoc "pravih.r" vl;
List.assoc "sa.mpravih.r" vl;
*)

(* Test de déclinaisons 
value test_decls () = (Gen.gobble Data.nouns_file : inflected_map);

value d = test_decls (); 
(* value c = contents_map d; (* Stack overflow *) *)
value (i,m) = match d with [ Map (i,m) -> (i,m) ]; 
value (n,da) = hd m; 
value ca = contents_map da; 
value (w1,im1) = hd ca;
print_inverse_map [ 1 :: w1 ] im1; 
(* a"m"sa <= { iic. | voc. sg. m. }[a"m"sa] *)

*)

(*
#load "constraints.cmo"; 
open Constraints;
*)

(* Testing Parse_tree etc *)

#load "dispatch11.cmo"; 
#load "stemmer.cmo"; 
open Stemmer;

#load "bank_lexer.cmo";
#load "parse_tree.cmo"; 
open Parse_tree;

value parse = parse_tree;

(* Example where a unit solution is not found first (for word vadanti)
parse "[S [ADV tat ] [ADV kim ] 
       [NP6 asya ] 
       [NP1s manu.syaa.h ] 
       [VP [NP2 __ [AP2 raak.sasiim ] vaacam ] vadanti ] ]";
*)

parse "[S [INJ haa ] [ADV katham ] [NP1s [NP6 vi.s.no.h ] 
       (dharma<daaraa.h) ] [VP 0 [NP1 (priya<sakhii) [NP6 me ] 
       lalitaa ] ] ]";
*)
