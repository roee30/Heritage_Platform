(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Gérard Huet & Pawan Goyal                       *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Sanskrit Reader Summarizing interface. *)
(* Similar design to Segmenter and Lexer, but records recognized segments
  represented in a shared graph with their offset with respect to the input 
  sentence. *)
module Interface : sig val safe_engine : unit -> unit
                          end
  

