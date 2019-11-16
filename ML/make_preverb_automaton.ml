(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_preverb_automaton = struct i*)

(* Compiles a trie of inflected forms into a tagging automaton structure  
   -- case of preverb automata, for which special sandhi applies. *)

(* We add to the stack arrays a deco rewrite set *)
(* A rewrite deco maps revu to a list of rules (w,revu,v) *)
type rewrite_set = Deco.deco (Word.word * Word.word * Word.word) 
;
(* preverb glueing may incur retroflexion *)
value extra_preverbs_rules = (* persistent precompiled by [Compile_sandhi] *)
  (Gen.gobble Data.sandhis_pv_file:rewrite_set)
;

value make_transducer inflected =
  let flag = False in (* no monitoring *)
  try Automata.compile_pv flag extra_preverbs_rules inflected 
  with 
  [ Sys_error s -> 
     let mess = s ^ "\n\n *** First call make_inflected ***\n" in do
     { Gen.notify_error mess
     ; exit 1
     }
  | Automata.Overlap -> do 
     { Gen.notify_error "Conflict lexicon/sandhi\n"
     ; exit 1
     }
  ]
;

(*i end; i*)
