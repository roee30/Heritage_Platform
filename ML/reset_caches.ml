(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* [Reset_caches] *)
(* Used for initializing or resetting the cache databases *)
(* Caution. Execution of this program erases the contents of the caches *)
open Morphology
  
open Auto
  
let empty_inflected_map : inflected_map = Deco.empty
and (* dummy morpho bank *) empty_trans = Auto.State ((false, [], []))
  
(* dummy empty transducer *)
let _ = Gen.dump empty_inflected_map Data.public_cache_file
  
let _ = Gen.dump empty_inflected_map Data.public_cachei_file
  
let _ = Gen.dump empty_trans Data.public_trans_cache_file
  
let _ = Gen.dump empty_trans Data.public_trans_cachei_file
  
let _ = Unix.system (":>" ^ Data.public_cache_txt_file)
  

