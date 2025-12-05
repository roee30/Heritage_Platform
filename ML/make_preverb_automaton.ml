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
type rewrite_set = (Word.word * Word.word * Word.word) Deco.deco

(* preverb glueing may incur retroflexion *)
let extra_preverbs_rules : (* persistent precompiled by [Compile_sandhi] *)
  rewrite_set = Gen.gobble Data.sandhis_pv_file
  
let make_transducer inflected =
  let flag = false
  in
    (* no monitoring *)
    try Automata.compile_pv flag extra_preverbs_rules inflected
    with
    | Sys_error s ->
        let mess = s ^ "\n\n *** First call make_inflected ***\n"
        in (Gen.notify_error mess; exit 1)
    | Automata.Overlap ->
        (Gen.notify_error "Conflict lexicon/sandhi\n"; exit 1)
  

