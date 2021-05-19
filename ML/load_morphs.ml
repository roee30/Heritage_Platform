(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* [Load_morphs] *)
(* Used for loading the (huge) morphology databanks. *)
(* Caution. This is an executable, that actually loads the lemmas at link time.*)

open Morphology; (* lemmas *)

(* Morph functor takes its prelude and control arguments as parameters *) 
module Morphs 
  (Prel: sig value prelude : unit -> unit; end) 
  (Phases: sig type phase = (* Phases.phase *)
  [ Noun 
  | Pron 
  | Root  
  | Inde 
  | Absv | Absc | Abso 
  | Voca 
  | Inv 
  | Iic 
  | Iiif 
  | Iiv | Iivv | Iivc
  | Auxi | Auxiinv | Auxik | Auxiick
  | Ifc 
  | Indifc 
  | Peri (* periphrastic perfect *)
  | Lopa (* e/o conjugated root forms with lopa *) 
  | Lopak (* e/o kridantas forms with lopa *)
  | Pv (* Preverb optional before Root or Lopa or mandatory before Abso *)
  | Pvc | Pvv (* privative Abso *)
  | Pvkc | Pvkv (* Preverb optional before Krid or Iik or Lopak *) 
  | A | An | Ai | Ani (* privative nan-compounds *)
  | Iicv | Iicc | Ifcv | Ifcc | Nouv | Nouc 
  | Krid (* K.ridaantaas - used to be called Parts *) 
  | Vok (* K.ridaanta vocatives *) 
  | Iik (* K.ridaantaas as left component - used to be called Piic *) 
  | Iikv | Iikc | Kriv | Kric | Vocv | Vocc | Vokv | Vokc | Vocf
  | Iiy | Avy | Inftu | Kama
  | Cache (* Cached lexicon acquisitions *) 
  | Cachei (* Cached iic lexicon acquisitions *) 
  | Unknown (* Unrecognized chunk *) 
  | Comp of (phase * phase) and (* pv *) Word.word and (* root form *) Word.word
  ]; end)
 = struct 

open Phases (* phase *)
;
(* Somewhat weird classification of segments according to their construction
by Dispatcher. Preverbed segments may be finite verb forms or kridantas. *)
type tag_sort =
  [ Atomic of lemmas 
  | Preverbed of (phase * phase) and (* pv *) Word.word and Word.word and lemmas 
  ]
; 
(* Fake tags of nan prefixes *)
value nan_prefix = Bare_stem 
;
value a_tag = [ ((0,[]),[ nan_prefix ]) ]
and an_tag = [ ((0,[ 51 ]),[ nan_prefix ]) ] (* since lexicalized as an\#1 *)
(* [an_tag] has delta = (0,[51]) since an\#1 is the relevant entry. Such values
ought to be parameters of the specific lexicon used. *)(*i TODO i*)
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
value load_morphs () = 
  { nouns = load_morpho Data.public_nouns_file
  ; prons = load_morpho Data.public_pronouns_file
  ; roots = load_morpho Data.public_roots_file
  ; krids = load_morpho Data.public_parts_file
  ; voks  = load_morpho Data.public_partvocs_file
  ; peris = load_morpho Data.public_peris_file
  ; lopas = load_morpho Data.public_lopas_file
  ; lopaks = load_morpho Data.public_lopaks_file
  ; indes = load_morpho Data.public_inde_file
  ; indifcs = load_morpho Data.public_indifcs_file
  ; absya = load_morpho Data.public_absya_file
  ; abstvaa = load_morpho Data.public_abstvaa_file
  ; iics  = load_morpho Data.public_iics_file
  ; iifs  = load_morpho Data.public_iifcs_file
  ; iiks  = load_morpho Data.public_piics_file
  ; iivs  = load_morpho Data.public_iivs_file
  ; iiys  = load_morpho Data.public_avyayais_file
  ; avys  = load_morpho Data.public_avyayafs_file
  ; auxis = load_morpho Data.public_auxis_file
  ; auxiinvs = load_morpho Data.public_auxiinvs_file
  ; auxiks = load_morpho Data.public_auxiks_file
  ; auxiicks = load_morpho Data.public_auxiicks_file
  ; vocas = load_morpho Data.public_vocas_file
  ; invs  = load_morpho Data.public_invs_file
  ; ifcs  = load_morpho Data.public_ifcs_file
  ; inftu = load_morpho Data.public_inftu_file
  ; kama = load_morpho Data.public_kama_file
  ; vocaf = load_morpho Data.public_vocaf_file
  ; caches = load_morpho_cache Data.public_cache_file
  ; cacheis = load_morpho_cache Data.public_cachei_file
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
    | Indifc             -> morpho.indifcs
    | Absv | Absc        -> morpho.abstvaa
    | Abso               -> morpho.absya
    | Auxi               -> morpho.auxis
    | Auxiinv            -> morpho.auxiinvs
    | Auxik              -> morpho.auxiks
    | Auxiick            -> morpho.auxiicks
    | Voca | Vocv | Vocc -> morpho.vocas
    | Vocf               -> morpho.vocaf
    | Inv                -> morpho.invs
    | Ifc | Ifcv | Ifcc  -> morpho.ifcs
    | Iic | Iicv | Iicc  -> morpho.iics
    | Iiv | Iivv | Iivc  -> morpho.iivs 
    | Iiif               -> morpho.iifs
    | Iiy                -> morpho.iiys
    | Avy                -> morpho.avys
    | Krid | Kriv | Kric -> morpho.krids 
    | Vok  | Vokv | Vokc -> morpho.voks
    | Iik  | Iikv | Iikc -> morpho.iiks
    | Inftu              -> morpho.inftu
    | Kama               -> morpho.kama
    | Cache              -> morpho.caches 
    | Cachei             -> morpho.cacheis 
    | _ -> raise (Control.Anomaly "morpho_tags") 
    ]
;

(* Used in Lexer/Reader/Parser and Interface *) 
value tags_of phase word = 
  match phase with
  [ Pv | Pvkc | Pvkv -> failwith "Preverb in tags_of" 
    (* all preverbs ought to have been captured by [Dispatcher.validate] *)
  | A | Ai   -> Atomic a_tag 
  | An | Ani -> Atomic an_tag 
  | Unknown -> Atomic unknown_tag 
  | Comp ((_,ph) as sort) pv form -> 
      let tag = Deco.assoc form (morpho_tags ph) in
      Preverbed sort pv form tag
    (* NB [Preverbed] comprises tin verbal forms of verbs with preverbs as well 
      as sup kridanta forms with preverbs. The preverbs are packed in pv. *)
  | _ -> Atomic (Deco.assoc word (morpho_tags phase))  
    (* NB Atomic comprises tin verbal forms of roots as well as sup atomic forms
       and all the pure stems collections Iic Iiv etc. *)
  ]
; 

end;
