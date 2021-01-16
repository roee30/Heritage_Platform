(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_transducers = struct  i*)

(* Prepares the transducers from the morphology banks databases of 
   inflected forms in Resources/DATA  *)

(* The general scheme is that Resources morphology creates a revmap [nouns],
its underlying minimized trie is used for constructing a segmenting
transducer [transn], and [nouns] is used for lemmatizing, for instance
to give the tags of the segments.

A. One-automaton logic for recognizing noun phrases (segmenter,tagger):
  (This corresponds to our historical prototype, with a unique phase)
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

open Auto.Auto; (* [auto State ] *) 
open Automata_vector; (* [transducers_datatype] *) 

(* Takes a morphology map and contracts it to its underlying trie *)
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

(* Creates the transducers files *)
(* For each lexical category, the trie obtained by forgetting the lemmas is then 
   decorated as transducer of type auto *)
value transducer_of_lemmas deco = 
  make_inflected deco |> Make_automaton.make_transducer
and transducer_of_preverbs = (* special sandhi rules *)
  make_preverbs Data.preverbs_file |> Make_preverb_automaton.make_transducer
and empty_trans = State (False,[],[]) (* dummy empty transducer *)
;
value make_transducers = 
  (* Remark. We could interleave with calls of [Mini.reset ()] for minimize speedup *)
  let nouns    = transducer_of_lemmas Data.nouns_file 
  and kama     = transducer_of_lemmas Data.kama_file 
  and pronouns = transducer_of_lemmas Data.pronouns_file 
  and roots    = transducer_of_lemmas Data.roots_file 
  and lopas    = transducer_of_lemmas Data.lopas_file 
  and parts    = transducer_of_lemmas Data.parts_file 
  and lopaks   = transducer_of_lemmas Data.lopaks_file 
  and partvocs = transducer_of_lemmas Data.partvocs_file 
  and iics     = transducer_of_lemmas Data.iics_file 
  and iifcs    = transducer_of_lemmas Data.iifcs_file 
  and avyayais = transducer_of_lemmas Data.avyayais_file 
  and avyayafs = transducer_of_lemmas Data.avyayafs_file 
  and vocas    = transducer_of_lemmas Data.vocas_file 
  and invs     = transducer_of_lemmas Data.invs_file 
  and piics    = transducer_of_lemmas Data.piics_file 
  and ifcs     = transducer_of_lemmas Data.ifcs_file 
  and indecls  = transducer_of_lemmas Data.indecls_file 
  and indifcs  = transducer_of_lemmas Data.indifcs_file 
  and inftu    = transducer_of_lemmas Data.inftu_file 
  and absya    = transducer_of_lemmas Data.absya_file 
  and abstvaa  = transducer_of_lemmas Data.abstvaa_file 
  and iivs     = transducer_of_lemmas Data.iivs_file 
  and peris    = transducer_of_lemmas Data.peris_file 
  and auxis    = transducer_of_lemmas Data.auxis_file 
  and auxiinvs = transducer_of_lemmas Data.auxiinvs_file 
  and auxiks   = transducer_of_lemmas Data.auxiks_file 
  and auxiicks = transducer_of_lemmas Data.auxiicks_file 
  and preverbs = transducer_of_preverbs in
  let (transducers_data : transducers_datatype) =
  { nouns    = nouns 
  ; kama     = kama 
  ; pronouns = pronouns 
  ; roots    = roots 
  ; lopas    = lopas 
  ; parts    = parts 
  ; lopaks   = lopaks 
  ; partvocs = partvocs 
  ; iics     = iics 
  ; iifcs    = iifcs 
  ; avyayais = avyayais 
  ; avyayafs = avyayafs 
  ; vocas    = vocas 
  ; invs     = invs 
  ; piics    = piics 
  ; ifcs     = ifcs 
  ; indecls  = indecls 
  ; indifcs  = indifcs
  ; inftu    = inftu 
  ; absya    = absya 
  ; abstvaa  = abstvaa 
  ; iivs     = iivs 
  ; peris    = peris 
  ; auxis    = auxis 
  ; auxiinvs = auxiinvs 
  ; auxiks   = auxiks 
  ; auxiicks = auxiicks 
  ; preverbs = preverbs 
  } 
 in Gen.dump transducers_data Data.transducers_file 
;

(*i end; i*)
