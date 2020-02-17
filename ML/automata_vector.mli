(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* The vector of automata constructed by [Make_transducers] at make time
   and loaded by [Load_transducers] at cgi running time *)

(* This bizarre datatype is a sum type of transducers used by one of the
   modes Simple or Full. Thus nouns2, iics2 and ifcs2 are used only
   in mode Simple. This should be cleaned up, or the Simple mode deprecated *)
open Auto.Auto; (* auto *) 

type transducers_datatype = 
  { nouns : auto
  ; nouns2 : auto
  ; kama : auto
  ; pronouns : auto
  ; roots : auto
  ; lopas : auto
  ; parts : auto
  ; lopaks : auto
  ; partvocs : auto
  ; iics : auto
  ; iics2 : auto
  ; iifcs : auto
  ; avyayais : auto
  ; avyayafs : auto
  ; vocas : auto
  ; invs : auto
  ; piics : auto
  ; ifcs : auto
  ; ifcs2 : auto
  ; indecls : auto
  ; inftu : auto
  ; absya : auto
  ; abstvaa : auto
  ; iivs : auto
  ; peris : auto
  ; auxis : auto
  ; auxiinvs : auto
  ; auxiks : auto
  ; auxiicks : auto
  ; preverbs : auto
  }
;
