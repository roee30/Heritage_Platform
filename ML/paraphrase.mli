(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* English paraphrase of semantic analysis *)

(*i module Paraphrase : sig i*)

value print_sem : string -> Morphology.inflexion_tag -> unit;
value print_role : ((int * int * int) * Constraints.pos) -> unit;

(*i end; i*)
