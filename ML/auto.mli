(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* The auto structure *)

module Auto : sig

type rule = (Word.word * Word.word * Word.word); 
(* [(w,u,v)] such that [(rev u)|v -> w] *)

type auto = [ State of (bool * deter * choices) ]
(* [bool] is [True] for accepting states *)
(* Possible refinement - order choices by right-hand sides of sandhi rules *)
and deter = list (Word.letter * auto)
and choices = list rule;

type stack = list choices;  (* choice points stack *)

end;

