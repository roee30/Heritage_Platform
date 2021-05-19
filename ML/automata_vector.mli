(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* The vector of automata constructed by [Make_transducers] at make time
   and loaded by [Load_transducers] at cgi running time *)

open Auto.Auto; (* auto *) 

type transducers_datatype = 
  { nouns : auto
  ; kama : auto
  ; pronouns : auto
  ; roots : auto
  ; lopas : auto
  ; parts : auto
  ; lopaks : auto
  ; partvocs : auto
  ; iics : auto
  ; iifcs : auto
  ; avyayais : auto
  ; avyayafs : auto
  ; vocas : auto
  ; vocaf : auto
  ; invs : auto
  ; piics : auto
  ; ifcs : auto
  ; indecls : auto
  ; indifcs : auto
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
