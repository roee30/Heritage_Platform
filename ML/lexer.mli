(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Phrase Lexer *)

open Morphology; (* [inflexions lemma morphology] *)
open Phases;
open Dispatcher; 
open Load_transducers; (* [transducer_vect morpho] *)

module Lexer : functor (* takes its prelude and iterator control as parameters *)
  (Prel: sig value prelude : unit -> unit; end) -> functor
  (Control: sig value star : ref bool; (* chunk= if start then word+ else word *)
                value full : ref bool; (* all kridantas and nan cpds if full *)
                value out_chan : ref out_channel;
            end) -> sig
  
  module Transducers : sig value transducers : transducer_vect; end;

  module Disp : sig
      value accepting : Phases.phase -> bool;
      type input = Word.word
      and transition
      and segment = (Phases.phase * Word.word * transition)
      and output = list segment;
      value color_of_phase : Phases.phase -> Html.color;
      end; 

  module Viccheda : sig 
      type resumption;
      value continue : resumption -> option (Disp.output * resumption);
      value init_segment : Disp.input -> resumption;
      value finished : resumption;
      type check = (int * (Phases.phase * Word.word) * bool);
      value all_checks : ref (list check);
      value set_offset : (int * list check) -> unit;
      end;

  value extract_lemma : Phases.phase -> Word.word -> list lemma;
  value print_segment : int -> Disp.segment -> int;
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
  value un_analyzable : Word.word -> (list Disp.segment * Viccheda.resumption); 
  value set_offset : (int * list Viccheda.check) -> unit;
  value print_scl_segment : int -> (Phases.phase * Word.word) -> int;
  value tags_of : Phases.phase -> Word.word -> 
                  (Load_morphs.Morphs Prel Phases).tag_sort; (* ugly *)
end;

