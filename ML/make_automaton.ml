(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Compiles a trie of inflected forms into a tagging automaton structure  *)
(*i module Make_automaton = struct i*)
(* Reads on standard input a trie of inflected forms.
   Prints on standard output an fsm automaton usable by the segmenter. 
   This executable is invoked in DATA/Makefile. 
   It is executed at morphology/automata construction time. *)
(* Instrumentation flag for printing file [automaton_stats] *)
let stats = ref false
  
(* We add to the stack arrays a deco rewrite set *)
(* A rewrite deco maps revu to a list of rules (w,revu,v) *)
type rewrite_set = (Word.word * Word.word * Word.word) Deco.deco

(* phantom processing with extra rules *)
let extra_phantom_rules : (* persistent precompiled by [Compile_sandhi] *)
  rewrite_set = Gen.gobble Data.sandhis_ph_file
  
let make_transducer inflected =
  let flag = false
  in
    (* no monitoring *)
    try Automata.compile flag extra_phantom_rules inflected
    with
    | (* return transducer *) Sys_error s ->
        let mess = s ^ "\n\n *** First call make_inflected ***\n"
        in (Gen.notify_error mess; exit 1)
    | Automata.Overlap ->
        (Gen.notify_error "Conflict lexicon/sandhi\n"; exit 1)
  

