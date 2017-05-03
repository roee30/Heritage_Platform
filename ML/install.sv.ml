(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Module Install reads localisation parameters from paths.ml, 
   created by "make configure" in main directory, called by configure script.
   Describes all installation parameters and resources *)

(*i module Install = struct i*)

(* Heritage used as the generative lexicon *)
value lexicon = "Heritage" (* for lexicon version in Parser.stamp *) 
;
(* Configuration of the platform *)

(* truncation is the maximum number of solutions computed by the lexer.
   Too small a truncation limit will miss solutions, too large a truncation 
   limit will provoke un unrecoverable choking server failure. This is relevant
   only for the parser (deprecated) mode. The graph interface has no limit. *)
value truncation = 10000 
;

(*i end; i*)
