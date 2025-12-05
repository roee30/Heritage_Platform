(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Sanskrit Phrase Lexer *)
open Morphology
  
(* [inflexions lemma morphology] *)
open Phases
  
open Dispatcher
  
open Load_transducers
  
(* [transducer_vect morpho] *)
module Lexer
  (Prel : (* takes its prelude and iterator control as parameters *)
    sig val prelude : unit -> unit
           end)
  (Lexer_control :
    sig
      val star : bool ref
        
      (* chunk = if star then word+ else word *)
      val transducers_ref : Load_transducers.transducer_vect ref
        
    end) :
  sig
    module Transducers : sig val mk_transducers : unit -> transducer_vect
                                end
      
    module Machine :
      sig
        val accepting : Phases.phase -> bool
          
        type input =
          Word.
          word
          and transition
          and segment =
          (Phases.phase * Word.word * transition)
          and output =
          segment list
        
      end
      
    module Viccheda :
      sig
        type resumption
        
        val continue : resumption -> (Machine.output * resumption) option
          
        val init_segment : Machine.input -> resumption
          
        type check = (int * (Phases.phase * Word.word) * bool)
        
        val all_checks : (check list) ref
          
        val set_offset : (int * (check list)) -> unit
          
        val set_sa_control : bool -> unit
          
      end
      
    val extract_lemma : Phases.phase -> Word.word -> lemma list
      
    val print_segment : int -> Machine.segment -> int
      
    (* Exported for Parser *)
    val process_kridanta :
      Word.word ->
        int ->
          Phases.phase ->
            Word.word ->
              Morphology.multitag ->
                (Phases.phase * Word.word * Morphology.multitag)
      
    val table_morph_of : Phases.phase -> string
      
    val print_morph :
      Word.word ->
        bool -> int -> bool -> Word.word -> int -> Morphology.unitag -> int
      
    val trim_tags :
      bool ->
        Word.word -> string -> Morphology.multitag -> Morphology.multitag
      
    (* END Exported for Parser *)
    val all_checks : (Viccheda.check list) ref
      
    val un_analyzable :
      Word.word -> ((Machine.segment list) * Viccheda.resumption)
      
    val set_offset : (int * (Viccheda.check list)) -> unit
      
    val print_scl_segment : int -> (Phases.phase * Word.word) -> int
      
    val tags_of :
      Phases.phase -> Word.word -> Load_morphs.Morphs(Prel)(Phases).tag_sort
      
  end
  

