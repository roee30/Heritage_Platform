(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2015 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Functional tests for sandhi. *)
(*i module Sandhi_test = struct i*)
open Sandhi
  
open Int_sandhi
  
let _ = assert ((e_sandhi "vane" "iva") = "vana_iva")
  
let _ = assert ((e_sandhi "na" "chinatti") = "nacchinatti")
  
let _ = assert ((e_sandhi "tat" "zariiram") = "tacchariiram")
  
let _ = assert ((e_sandhi "azvas" "asti") = "azvo'sti")
  
let _ = assert ((e_sandhi "azvas" "iva") = "azva_iva")
  
let _ = assert ((e_sandhi "punar" "iva") = "punariva")
  
let _ = assert ((e_sandhi "punar" "janman") = "punarjanman")
  
let _ = assert ((e_sandhi "api" "avagacchasi") = "apyavagacchasi")
  
let _ = assert ((e_sandhi "nanu" "upavizaama.h") = "nanuupavizaama.h")
  
let _ = assert ((e_sandhi "ubhau" "aagacchata.h") = "ubhaavaagacchata.h")
  
let _ = assert ((e_sandhi "katham" "smarati") = "katha.msmarati")
  
let _ = assert ((e_sandhi "tat" "jayati") = "tajjayati")
  
let _ = assert ((e_sandhi "dvi.t" "hasati") = "dvi.d.dhasati")
  
let _ = assert ((e_sandhi "ud" "h.r") = "uddh.r")
  
let _ = assert ((e_sandhi "tat" "hema") = "taddhema")
  
let _ = assert ((e_sandhi "taan" "tu") = "taa.mstu")
  
let _ = assert ((e_sandhi "nara.h" "rak.sati") = "narorak.sati")
  
let _ = assert ((e_sandhi "punar" "rak.sati") = "punaarak.sati")
  
let _ = assert ((e_sandhi "gaayan" "aagacchati") = "gaayannaagacchati")
  
let _ = assert ((e_sandhi "vaak" "me") = "vaafme")
  
let _ = assert ((e_sandhi "vaag" "hasati") = "vaagghasati")
  
let _ = assert ((e_sandhi ".sa.t" "naam") = ".sa.nnaam")
  
(* and not ".sa.n.naam" *)
let _ = assert ((e_sandhi "tat" "namas") = "tannamas")
  
(* but "tadnamas" also correct *)
let _ = assert ((e_sandhi "tat" "mitram") = "tanmitram")
  
let _ = assert ((e_sandhi "devaan" "z.r.noti") = "devaa~nch.r.noti")
  
let _ = assert ((external_sandhi "sas" "gaja.h") = "sagaja.h")
  
let _ = assert ((external_sandhi "sas" "aacaarya.h") = "sa_aacaarya.h")
  
let _ = assert ((external_sandhi "sas" "azva.h") = "so'zva.h")
  
let _ = assert ((external_sandhi "sas" "") = "sa.h")
  
let _ =
  assert ((after_dual_sandhi "tephale" "icchaama.h") = "tephale_icchaama.h")
  
let _ = assert ((ortho "nisanna") = "ni.sa.n.na")
  
let _ = assert ((ortho "pranamati") = "pra.namati")
  
let _ = assert ((ortho "parinindati") = "pari.nindati")
  
(* could be "parinindati" *)
let _ = assert ((ortho "gurusu") = "guru.su")
  
let _ = assert ((ortho "visarpati") = "vi.sarpati")
  
(* should be "visarpati" *)
let _ = assert ((ortho "kusuma") = "ku.suma")
  
(* should be "kusuma" *)
let _ = assert ((ortho "pustaka") = "pu.s.taka")
  
(* should be "pustaka" *)
let _ = assert ((internal_sandhi "ne" "ati") = "nayati")
  
let _ = assert ((internal_sandhi "budh" "ta") = "buddha")
  
let _ = assert ((internal_sandhi "pustak" "a") = "pustaka")
  
let _ = assert ((internal_sandhi "d.rz" "ta") = "d.r.s.ta")
  
let _ = assert ((internal_sandhi "dvi.s" "ta") = "dvi.s.ta")
  
let _ = assert ((internal_sandhi "dvi.s" "dhvam") = "dvi.d.dhvam")
  
let _ = assert ((internal_sandhi "han" "si") = "ha.msi")
  
let _ = assert ((internal_sandhi "yaj" "na") = "yaj~na")
  
let _ = assert ((internal_sandhi "han" "ka") = "hanka")
  
let _ = assert ((internal_sandhi "gam" "va") = "ganva")
  
let _ = assert ((internal_sandhi "lih" "ta") = "lii.dha")
  
let _ = assert ((internal_sandhi "manas" "su") = "mana.hsu")
  
let _ = assert ((internal_sandhi "manas" "bhis") = "manobhis")
  
let _ = assert ((internal_sandhi "bhas" "ya") = "bhasya")
  
let _ = assert ((internal_sandhi "bho" "ya") = "bhavya")
  
let _ = assert ((internal_sandhi "sraj" "su") = "srak.su")
  
let _ = assert ((internal_sandhi "yuj" "ta") = "yukta")
  
(* not "yu.s.ta" *)
let _ = assert ((internal_sandhi "yu~nj" "te") = "yufkte")
  
let _ = assert ((internal_sandhi "tad" "") = "tat")
  
let _ = assert ((internal_sandhi "nis" "rasa") = "niirasa")
  
let _ = assert ((internal_sandhi "hi.ms" "aa") = "hi.msaa")
  

