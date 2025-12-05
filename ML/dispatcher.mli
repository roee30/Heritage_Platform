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
open Auto.Auto
  
open Load_transducers
  
(* [transducer_vect] *)
open Morphology
  
(* [inflexion_tag Verb_form pada_tag morphology] *)
open Phases.Phases
  
(* phase etc. *)
module Dispatch
  (Trans : sig val roots_usage : string Deco.deco
                  end)
  (Lem : sig val morpho : morphology
                end)
  (Segment_control : sig val transducers_ref : transducer_vect ref
                            end) :
  sig
    val transducer : phase -> auto
      
    val initial : phases
      
    val dispatch : Word.word -> phase -> phases
      
    val accepting : phase -> bool
      
    type input =
      Word.
      word
      and (* input sentence represented as a word *)
      transition =
      (* Reflexive relation *)
      | Euphony of rule | (* [(w,rev u,v)] such that [u|v -> w] *) Id
      and (* identity or no sandhi *)
      segment =
      (phase * Word.word * transition)
      and output =
      segment list
    
    val valid_morpho :
      bool -> string -> Word.word -> Morphology.inflexion_tag -> bool
      
    val trim_tags :
      bool ->
        Word.word -> string -> Morphology.multitag -> Morphology.multitag
      
    val validate : output -> output
      
    (* consistency check and glueing *)
    val sanitize_sa : bool -> output -> output
      
  end
  

