(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Conj_infos : sig i*)

(* NB no module value, [Conj_infos] is a purely defining types signature *)

type vmorph =
  [ Prim of int and bool and Word.word  (* primary conjugation *)
         (* gana    pada     form of present 3rd sg for checking *)
         (* pada=True Paradmaipada pada=False AAtmanepada   *)
  | Causa of Word.word          (* causative 3rd sg form    *) 
  | Inten of Word.word          (* intensive 3rd sg form    *)
  | Desid of Word.word          (* desiderative 3rd sg form *)
  ]
;
type root_infos = (vmorph * bool) (* [True] means root admits preverb aa- *)
; (* NB could be (list vmorph * bool) for better factorisation *)

(*i end; i*)
