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

#load "inflected.cmo"; 
open Inflected;

#load "int_sandhi.cmo"; 
open Int_sandhi;

#load "parts.cmo"; 
open Parts;
#load "control.cmo"; 
open Control;
#load "pada.cmo"; 
open Pada;
#load "verbs.cmo"; 
open Verbs;

(* 
#load "int_sandhi.cmo"; 
open Int_sandhi; 

#load "sandhi.cmo"; 
open Sandhi; 

#load "pv_sandhi.cmo"; 
open Pv_sandhi; 

#load "data.cmo";

(* testing geminations 
#load "test_gem.cmo";

open Test_gem;

filter_gem [ 49 ];
*)

#load "automaton.cmo"; 
open Automaton;
*) 
