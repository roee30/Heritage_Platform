(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_transducers = struct  i*)

(* Prepares the transducers from the databases of inflected forms in Resources *)

(* The general scheme is that Resources morphology creates a revmap [nouns],
its underlying minimized trie is used for constructing a segmenting
transducer [transn], and [nouns] is used for lemmatizing, for instance
to give the tags of the segments.

A. One-automaton logic for recognizing noun phrases (segmenter,tagger):
  (This corresponds to our first prototype, with only one phase)
  1. [make_nouns] creates [nouns_file] from [genders_file] (* Resources *)
  2. [Make_automaton.make transducer] creates a shared trie from [nouns_file]
     and compiles it into a transducer dumped in Resources DATA directory
  3. make segmenter uses [transn_file] for segmenting, [nouns_file] for tagging

B. The multi-automata logic, used for more general sentences, does instead:
  1. [make_nouns] creates [nouns_file],[pronouns_file],[iics_file],[iivs_file],
     [invars_file],[voca_file], [inv_file] and [ifcs_file] from [genders_file]
  2. [make_roots] creates [roots_file],[parts_file],[piics_file],[piivs_file], 
     [abstvaa_file],[absya_file] and [eorts_file] 
  3. [make_preverbs] creates [preverbs_file] from [verblinks_file]
     All these files contain decos with morphological lemmas 
  4. [Make_automaton.make transducer] creates corresponding shared tries 
     and compiles it into transducers dumped in Resources DATA directory
     but preverbs uses [Make_preverb_automaton.make transducer] instead 
  5. make segmenter uses [transn_file], [transr_file], ... for segmenting, and
        [nouns_file], [roots_file], ... for tagging/lemmatizing.
*)

value make_inflected inflected_file = 
  try let inflected = (Gen.gobble inflected_file : Morphology.inflected_map) in
      Mini.minimize (Deco.forget_deco inflected) 
  with 
     [ Sys_error s -> do
        { let mess = s ^ "\n\n *** Inflected file missing ***\n" 
                       ^ inflected_file in
          output_string stderr mess
        ; flush stderr
        ; failwith "Make_inflected"
        }
     ]
;
value make_preverbs preverbs_file = 
  try let preverbs = (Gen.gobble preverbs_file : Deco.deco Word.word) in
      (* minimize as dag *)
      Mini.minimize (Deco.forget_deco preverbs) 
  with 
     [ Sys_error s -> do
        { let mess = s ^ "\n\n *** Preverbs file missing ***\n" 
                       ^ preverbs_file in
          output_string stderr mess
        ; flush stderr
        ; failwith "Make_inflected"
        }
     ]
;

(* create all transducers files *)
let inflected = make_inflected Data.nouns_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transn_file 
;
let inflected = make_inflected Data.nouns2_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transn2_file 
;
let inflected = make_inflected Data.kama_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transkama_file 
;
let inflected = make_inflected Data.pronouns_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transpn_file 
;
Mini.reset () (* since little overlap of finals in subantas and tinantas *)
;
let inflected = make_inflected Data.roots_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transr_file 
;
let inflected = make_inflected Data.lopas_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.translopa_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.parts_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transpa_file 
;
let inflected = make_inflected Data.lopaks_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.translopak_file 
;
let inflected = make_inflected Data.partvocs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transpav_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.iics_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transic_file 
;
let inflected = make_inflected Data.iics2_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transic2_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.avyayais_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transiiy_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.avyayafs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transavy_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.vocas_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transvoca_file 
;
Mini.reset ()
;
let inflected = make_inflected Data.invs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transinv_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.piics_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transpic_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.ifcs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transif_file 
;
let inflected = make_inflected Data.ifcs2_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transif2_file 
;
let inflected = make_inflected Data.iifcs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transiif_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.indecls_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transinde_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.inftu_file in
let transducer = Make_preverb_automaton.make_transducer inflected in
Gen.dump transducer Data.transinftu_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.absya_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transabsya_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.abstvaa_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transabstvaa_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.iivs_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transiv_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.peris_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transperi_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.auxis_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transauxi_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.auxiks_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transauxik_file 
;
Mini.reset () 
;
let inflected = make_inflected Data.auxiicks_file in
let transducer = Make_automaton.make_transducer inflected in
Gen.dump transducer Data.transauxiick_file 
;
Mini.reset () 
;

(* Special treatment for [preverbs_file] created by [Make_preverbs] *)
let deco = make_preverbs Data.preverbs_file in
let transducer = Make_preverb_automaton.make_transducer deco in
Gen.dump transducer Data.transp_file 
;

(*i end; i*)
