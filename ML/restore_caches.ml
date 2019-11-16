(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* [Restore caches] *)
(* Restore the cache databases from master cache file *)

(* Cache management (from Interface) *)
(* [ (Morphology.inflected_map * Morphology.inflected_map) -> unit] *)
value make_cache_transducers (cache,cachei) =
  let deco_cache = Mini.minimize (Deco.forget_deco cache) 
  and deco_cachei = Mini.minimize (Deco.forget_deco cachei) in
  let auto_cache = Automaton.compile Deco.empty deco_cache 
  and auto_cachei = Automaton.compile Deco.empty deco_cachei in do
  { Gen.dump cache Data.public_cache_file (* for [Load_morphs] *)
  ; Gen.dump cachei Data.public_cachei_file (* id *)
  ; Gen.dump auto_cache Data.public_trans_cache_file (* for [Load_transducers] *)
  ; Gen.dump auto_cachei Data.public_trans_cachei_file (* id *)
  }
;
value restore_caches () = 
let cache_txt_file = Data.public_cache_txt_file in
let caches = Nouns.extract_current_caches cache_txt_file in
make_cache_transducers caches
;
restore_caches ()
;

