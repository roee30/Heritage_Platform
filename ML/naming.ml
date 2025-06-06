(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Unique naming mechanism. *)

(*i module Naming = struct i*)

(* Kridanta names management: namespace data structures *)

(* The problem is to find the lexical entry, if any, that matches a stem
and an etymology, corresponding to the morphological structure of a
generated stem. For instance k.rta has etymology pp(k.r\#1). 
It does not produce forms, and is skipped by the morphology generator,
since the pp participal stem is a productive taddhita construction,
that will indeed generate stem k.rta from its root k.r\#1.
The problem for the morphology generator is to display forms of k.rta
with a link to k.rta in the hypertext lexicon. It is non-trivial, since
homonymies occur. Thus homophony indexes associated with generators
and consistent with possible lexicalisations must be registered.
A first pass of recording builds [lexical_kridantas] as a [deco_krid] deco
indexing the stems with a pair (morphology,homo). Then the morphology
generator from Inflected extends it as [unique_kridantas], accessed as
[Inflected.acccess_krid] and [Inflected.register_krid], and used
by [Parts.gen_stem].  *)

(* Unique naming of kridantas *)
(* associates to a pair (verbal,root) a homophony index for unique naming *)
type homo_krid = ((Skt_morph.verbal * Word.word) * int)
and deco_krid = Deco.deco homo_krid
;
value homo_undo w = Encode.decompose (Word.mirror w)
;
value look_up_homo homo = look_rec
  where rec look_rec = fun
  [ [] -> failwith "look_up_homo" 
  | [ (morpho,n) :: rest ] -> if n=homo then morpho else look_rec rest
  ] 
;
value unique_kridantas =  
  try (Gen.gobble Web.public_unique_kridantas_file : deco_krid) 
  with [ _ -> failwith "unique_kridantas" ] 
and lexical_kridantas =  
  try (Gen.gobble Web.public_lexical_kridantas_file : deco_krid) 
  with [ _ -> failwith "lexical_kridantas" ] 
;
(* This mechanism is used by [Make_roots] at morphology generation time,
and by [Morpho.print_inv_morpho] and [Morpho_ext.print_inv_morpho_ext]
at segmenting time. *)

(*i end; i*)
