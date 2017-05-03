(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* [Reset_caches] *)
(* Used for initializing or resetting the cache databases *)
(* Caution. Execution of this program erases the contents of the caches *)

open Morphology;
open Auto;

value empty_inflected_map = (Deco.empty : inflected_map) (* dummy morpho bank *)
and empty_trans = Auto.State(False,[],[]) (* dummy empty transducer *)
;

Gen.dump empty_inflected_map Web.public_cache_file
;
Gen.dump empty_trans Web.public_transca_file
;

Unix.system (":>" ^ Web.public_cache_txt_file) (* resets the master text cache *)
;
