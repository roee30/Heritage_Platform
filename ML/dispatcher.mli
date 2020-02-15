(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Dispatcher: Sanskrit Engine in 55 phases automaton (plus 2 fake ones) *)

(* The Dispatch functor maps a transducer vector of 39 aums into     *)
(*  - a dispatch automaton implementing a regular description over   *)
(*    45 phases of lexical analysis                                  *)
(*  - an initial vector of initial resumptions                       *)
(*  - a final test for lexical acceptance                            *)
(*  - a consistency check of the output of the segmenting transducer *)

(* Dispatch, instantiated by Transducers, is used as parameter of the 
   Segment functor from Segmenter or Interface.   *)

open Auto.Auto; 
open Load_transducers; (* [transducer_vect] *)
open Morphology; (* [inflexion_tag Verb_form pada_tag morphology] *)
open Phases.Phases; (* phase etc. *)

module Dispatch : functor 
  (Trans: sig value roots_usage : Deco.deco string; end) -> functor
  (Lem: sig value morpho : morphology; end) -> functor
  (Segment_control: sig value transducers_ref : ref transducer_vect; end) -> sig

value transducer : phase -> auto  
; 
value initial: bool -> phases 
;
value dispatch: bool -> Word.word -> phase -> phases 
;
value accepting: phase -> bool 
;
type input = Word.word (* input sentence represented as a word *) 
and transition = (* Reflexive relation *) 
    [ Euphony of rule (* [(w,rev u,v)] such that [u|v -> w] *)
    | Id              (* identity or no sandhi *)
    ]
and segment = (phase * Word.word * transition)
and output = list segment;

value valid_morpho : 
  bool -> string -> Word.word -> Morphology.inflexion_tag -> bool
;
value trim_tags : 
  bool -> Word.word -> string -> Morphology.multitag -> Morphology.multitag 
;
value validate : output -> output (* consistency check and glueing *) 
;
value terminal_sa : output -> option output 
;
value color_of_phase : phase -> Html.color
;  
end;

