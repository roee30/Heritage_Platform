(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Sriram Krishnan                     *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Phrase Lexer *)

open Morphology; (* [inflexions lemma morphology] *)
open Phases;
open Dispatcher; 
open Load_transducers; (* [transducer_vect morpho] *)

module Lexer : functor (* takes its prelude and iterator control as parameters *)
  (Prel: sig value prelude : unit -> unit; end) -> functor
    (Lexer_control: sig 
        value star : ref bool; (* chunk = if star then word+ else word *)
        value transducers_ref : ref Load_transducers.transducer_vect;
        end) -> sig
  
  module Transducers : sig value mk_transducers : unit -> transducer_vect; end;

  module Machine : sig
      value accepting : Phases.phase -> bool;
      type input = Word.word
      and transition
      and segment = (Phases.phase * Word.word * transition)
      and output = list segment;
      end; 

  module Viccheda : sig 
      type resumption;
      value continue : resumption -> option (Machine.output * resumption);
      value init_segment : Machine.input -> resumption;
      type check = (int * (Phases.phase * Word.word) * bool);
      value all_checks : ref (list check);
      value set_offset : (int * list check) -> unit;
      value set_sa_control : bool -> unit;
      end;

  value extract_lemma : Phases.phase -> Word.word -> list lemma;
  value print_segment : int -> Machine.segment -> int;
(* Exported for Parser *)
  value process_kridanta: Word.word -> int -> Phases.phase -> Word.word ->
        Morphology.multitag -> (Phases.phase * Word.word * Morphology.multitag);
  value table_morph_of : Phases.phase -> string; 
  value print_morph : Word.word -> bool -> int -> bool -> Word.word -> int -> 
        Morphology.unitag -> int;
  value trim_tags : bool ->
        Word.word -> string -> Morphology.multitag -> Morphology.multitag;
(* END Exported for Parser *)
  value all_checks : ref (list Viccheda.check); 
  value un_analyzable : Word.word -> (list Machine.segment * Viccheda.resumption); 
  value set_offset : (int * list Viccheda.check) -> unit;
  value print_scl_segment : int -> (Phases.phase * Word.word) -> int;
  value tags_of : Phases.phase -> Word.word -> 
                  (Load_morphs.Morphs Prel Phases).tag_sort; (* ugly *)
  value prioritize : (list (int * list (Machine.segment))) -> list (int * float * list (Machine.segment) * string ) ;
  value print_segment_words : int -> Machine.segment -> int;
  value print_segment_to_file : out_channel -> Machine.segment -> unit;
  value get_sandhi_word: (Machine.segment) -> string;
  value assign_freq_info: unit;
end;

