(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* [Load_morphs] *)
(* Used for loading the (huge) morphology databanks. *)

open Morphology; (* lemmas *)

module Morphs (* takes its prelude and control arguments as parameters *) 
  (Prel: sig value prelude : unit -> unit; end) 
  (Phases: sig type phase = (* Phases.phase *)
  [ Noun | Noun2
  | Pron 
  | Root  
  | Inde 
  | Absv | Absc | Abso 
  | Voca 
  | Inv 
  | Iic | Iic2 
  | Iiif 
  | Iiv | Iivv | Iivc
  | Auxi | Auxik | Auxiick
  | Ifc | Ifc2
  | Peri (* periphrastic perfect *)
  | Lopa (* e/o conjugated root forms with lopa *) 
  | Lopak (* e/o kridantas forms with lopa *)
  | Pv (* Preverb optional before Root or Lopa or mandatory before Abso *)
  | Pvk | Pvkc | Pvkv (* Preverb optional before Krid or Iik or Lopak *) 
  | A | An | Ai | Ani | Iicv | Iicc | Nouv | Nouc (* privative nan-compounds *)
  | Krid (* K.ridaantaas - used to be called Parts *) 
  | Vok (* K.ridaanta vocatives *) 
  | Iik (* K.ridaantaas as left component - used to be called Piic *) 
  | Iikv | Iikc | Kriv | Kric | Vocv | Vocc | Vokv | Vokc
  | Iiy | Avy | Inftu | Kama
  | Sfx | Isfx
  | Cache (* Cached lexicon acquisitions *)
  | Unknown (* Unrecognized chunk *)
  | Comp of (phase * phase) and (* pv *) Word.word and (* root form *) Word.word
  | Tad  of (phase * phase) and (* nominal *) Word.word and (* sfx *) Word.word 
  ]; end)
 = struct 

open Phases (* phase *)
;

(* Somewhat weird classification of segments accoding to their construction
by Dispatcher. Preverbed segments may be finite verb forms or kridantas. *)
type tag_sort =
  [ Atomic of lemmas 
  | Preverbed of (phase * phase) and (* pv *) Word.word and Word.word and lemmas 
  | Taddhita of (phase * Word.word) and (* sfx *) Word.word and phase and lemmas 
  ]
;
(* Fake tags of nan prefixes *)
value nan_prefix = Bare_stem 
;
value a_tag = [ ((0,[]),[ nan_prefix ]) ]
and an_tag = [ ((0,[ 51 ]),[ nan_prefix ]) ] (* since lexicalized as an\#1 *)
(* [an_tag] has delta = (0,[51]) since an\#1 is the relevant entry. Such values
will have to be parameters of the specific lexicon used. *)(*i TODO i*)
;
value ai_tag = a_tag (* special for privative abs-tvaa eg akritvaa *)
and ani_tag = an_tag
;
value unknown_tag = [ ((0,[]),[ Unanalysed ]) ] 
;
value give_up cat = 
  let mess = "Missing " ^ cat ^ " morphology bank" in do
  { Web.abort Html.default_language
                      "System error - please report - " mess
(*  ; exit 0 (* installation problem -- executing process fails *) *)
  ; Deco.empty 
  }
;
value load_morpho file = 
  try (Gen.gobble file : inflected_map) 
  with [ _ ->  do { Prel.prelude (); give_up file } ]
and  load_morpho_cache file = 
  try (Gen.gobble file : inflected_map) 
  with [ _ -> Deco.empty ] (* dummy empty morpho lexmap *)
;
(* Loads all morphological databases; Used in Reader, Parser. *)
(* NB both Noun and Noun2 are loaded whether full or not - TODO improve *)
value load_morphs () = 
  { nouns = load_morpho Web.public_nouns_file
  ; nouns2 = load_morpho Web.public_nouns2_file
  ; prons = load_morpho Web.public_pronouns_file
  ; roots = load_morpho Web.public_roots_file
  ; krids = load_morpho Web.public_parts_file
  ; voks  = load_morpho Web.public_partvocs_file
  ; peris = load_morpho Web.public_peris_file
  ; lopas = load_morpho Web.public_lopas_file
  ; lopaks = load_morpho Web.public_lopaks_file
  ; indes = load_morpho Web.public_inde_file
  ; absya = load_morpho Web.public_absya_file
  ; abstvaa = load_morpho Web.public_abstvaa_file
  ; iics  = load_morpho Web.public_iics_file
  ; iics2 = load_morpho Web.public_iics2_file
  ; iifs  = load_morpho Web.public_iifcs_file
  ; iiks  = load_morpho Web.public_piics_file
  ; iivs  = load_morpho Web.public_iivs_file
  ; iiys  = load_morpho Web.public_avyayais_file
  ; avys  = load_morpho Web.public_avyayafs_file
  ; auxis = load_morpho Web.public_auxis_file
  ; auxiks = load_morpho Web.public_auxiks_file
  ; auxiicks = load_morpho Web.public_auxiicks_file
  ; vocas = load_morpho Web.public_vocas_file
  ; invs  = load_morpho Web.public_invs_file
  ; ifcs  = load_morpho Web.public_ifcs_file
  ; ifcs2 = load_morpho Web.public_ifcs2_file
  ; inftu = load_morpho Web.public_inftu_file
  ; kama = load_morpho Web.public_kama_file
  ; sfxs  = load_morpho Web.public_sfxs_file
  ; isfxs = load_morpho Web.public_isfxs_file
  ; caches = load_morpho_cache Web.public_cache_file
  } 
;
value morpho = load_morphs () (* costly *)
;
value morpho_tags = fun
    [ Noun | Nouv | Nouc -> morpho.nouns
    | Pron               -> morpho.prons
    | Root               -> morpho.roots
    | Peri               -> morpho.peris
    | Lopa               -> morpho.lopas
    | Lopak              -> morpho.lopaks
    | Inde               -> morpho.indes
    | Absv | Absc        -> morpho.abstvaa
    | Abso               -> morpho.absya
    | Auxi               -> morpho.auxis
    | Auxik              -> morpho.auxiks
    | Auxiick            -> morpho.auxiicks
    | Voca | Vocv | Vocc -> morpho.vocas
    | Inv                -> morpho.invs
    | Ifc                -> morpho.ifcs
    | Iic | Iicv | Iicc  -> morpho.iics
    | Iiv | Iivv | Iivc  -> morpho.iivs 
    | Iiif               -> morpho.iifs
    | Iiy                -> morpho.iiys
    | Avy                -> morpho.avys
    | Krid | Kriv | Kric -> morpho.krids 
    | Vok  | Vokv | Vokc -> morpho.voks
    | Iik  | Iikv | Iikc -> morpho.iiks
    | Noun2              -> morpho.nouns2
    | Iic2               -> morpho.iics2
    | Ifc2               -> morpho.ifcs2
    | Inftu              -> morpho.inftu
    | Kama               -> morpho.kama
    | Sfx                -> morpho.sfxs
    | Isfx               -> morpho.isfxs 
    | Cache              -> morpho.caches
    | _ -> raise (Control.Anomaly "morpho_tags") 
    ]
;
(* Used in Lexer, Reader, Parser, Interface *) 
value tags_of phase word = 
  match phase with
  [ Pv | Pvk | Pvkc | Pvkv -> failwith "Preverb in tags_of" 
    (* all preverbs ought to have been captured by [Dispatcher.validate] *)
  | A | Ai   -> Atomic a_tag 
  | An | Ani -> Atomic an_tag 
  | Unknown -> Atomic unknown_tag 
  | Comp ((_,ph) as sort) pv form -> 
      let tag = Deco.assoc form (morpho_tags ph) in
      Preverbed sort pv form tag
(* NB [Preverbed] comprises tin verbal forms of verbs with preverbs as well 
   as sup kridanta forms with preverbs. The preverbs are packed in pv. *)
  | Tad (ph,sfx_ph) form sfx -> (* tag inherited from fake suffix entry *)
      let sfx_tag = Deco.assoc sfx (morpho_tags sfx_ph) in
(*   [let stem_tag = Deco.assoc sfx (morpho_tags ph) in] - possible extension *)
      Taddhita (ph,form) [ 0 :: sfx ] sfx_ph sfx_tag (* 0 = "-" *)
  | _ -> Atomic (Deco.assoc word (morpho_tags phase)) 
    (* NB Atomic comprises tin verbal forms of roots as well as sup atomic forms
       and all the pure stems collections Iic Iiv etc. *)
  ]
;

end;
