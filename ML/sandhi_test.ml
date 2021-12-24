(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Functional tests for sandhi. *)

(*i module Sandhi_test = struct i*)

open Sandhi;
open Int_sandhi;

assert (e_sandhi "vane" "iva" = "vana_iva");
assert (e_sandhi "na" "chinatti" = "nacchinatti"); 
assert (e_sandhi "tat" "zariiram" = "tacchariiram");
assert (e_sandhi "azvas" "asti" = "azvo'sti");
assert (e_sandhi "azvas" "iva" = "azva_iva");
assert (e_sandhi "punar" "iva" = "punariva");
assert (e_sandhi "punar" "janman" = "punarjanman");
assert (e_sandhi "api" "avagacchasi" = "apyavagacchasi");
assert (e_sandhi "nanu" "upavizaama.h" = "nanuupavizaama.h");
assert (e_sandhi "ubhau" "aagacchata.h" = "ubhaavaagacchata.h");
assert (e_sandhi "katham" "smarati" = "katha.msmarati");
assert (e_sandhi "tat" "jayati" = "tajjayati");
assert (e_sandhi "dvi.t" "hasati" = "dvi.d.dhasati");
assert (e_sandhi "ud" "h.r" = "uddh.r");
assert (e_sandhi "tat" "hema" = "taddhema");
assert (e_sandhi "taan" "tu" = "taa.mstu");
assert (e_sandhi "nara.h" "rak.sati" = "narorak.sati");
assert (e_sandhi "punar" "rak.sati" = "punaarak.sati");
assert (e_sandhi "gaayan" "aagacchati" = "gaayannaagacchati");
assert (e_sandhi "vaak" "me" = "vaafme");
assert (e_sandhi "vaag" "hasati" = "vaagghasati");
assert (e_sandhi ".sa.t" "naam" = ".sa.nnaam"); (* and not ".sa.n.naam" *)
assert (e_sandhi "tat" "namas" = "tannamas"); (* but "tadnamas" also correct *)
assert (e_sandhi "tat" "mitram" = "tanmitram");
assert (e_sandhi "devaan" "z.r.noti" = "devaa~nch.r.noti");
assert (external_sandhi "sas" "gaja.h" = "sa gaja.h");
assert (external_sandhi "sas" "aacaarya.h" = "sa_aacaarya.h");
assert (external_sandhi "sas" "azva.h" = "so'zva.h");
assert (external_sandhi "sas" "" = "sa.h");
assert (after_dual_sandhi "tephale" "icchaama.h" = "tephale_icchaama.h");
assert (ortho "nisanna" = "ni.sa.n.na");
assert (ortho "pranamati" = "pra.namati");
assert (ortho "parinindati" = "pari.nindati"); (* could be "parinindati" *)
assert (ortho "gurusu" = "guru.su");
assert (ortho "visarpati" = "vi.sarpati"); (* should be "visarpati" *)
assert (ortho "kusuma" = "ku.suma"); (* should be "kusuma" *)
assert (ortho "pustaka" = "pu.s.taka"); (* should be "pustaka" *)
assert (internal_sandhi "ne" "ati" = "nayati");
assert (internal_sandhi "budh" "ta" = "buddha");
assert (internal_sandhi "pustak" "a" = "pustaka");
assert (internal_sandhi "d.rz" "ta" = "d.r.s.ta");
assert (internal_sandhi "dvi.s" "ta" = "dvi.s.ta");
assert (internal_sandhi "dvi.s" "dhvam" = "dvi.d.dhvam"); 
assert (internal_sandhi "han" "si" = "ha.msi");
assert (internal_sandhi "yaj" "na" = "yaj~na");
assert (internal_sandhi "han" "ka" = "hanka"); 
assert (internal_sandhi "gam" "va" = "ganva");
assert (internal_sandhi "lih" "ta" = "lii.dha");
assert (internal_sandhi "manas" "su" = "mana.hsu");
assert (internal_sandhi "manas" "bhis" = "manobhis");
assert (internal_sandhi "bhas" "ya" = "bhasya");
assert (internal_sandhi "bho" "ya" = "bhavya");
assert (internal_sandhi "sraj" "su" = "srak.su");
assert (internal_sandhi "yuj" "ta" =  "yukta"); (* not "yu.s.ta" *)
assert (internal_sandhi "yu~nj" "te" = "yufkte"); 
assert (internal_sandhi "tad" "" = "tat"); 
assert (internal_sandhi "nis" "rasa" = "niirasa"); 
assert (internal_sandhi "hi.ms" "aa" = "hi.msaa"); (* not hi.m.saa *)

(*i end; i*)
