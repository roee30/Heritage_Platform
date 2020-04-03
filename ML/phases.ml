(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

module Phases = struct 
(* Lexical sorts as phases, i.e. states of the modular transducer *)
type phase = 
  [ Noun | Noun2
  | Pron 
  | Root 
  | Inde (* indeclinable forms *)
  | Absv (* vowel-initial abs-tvaa *) 
  | Absc (* consonant-initial abs-tvaa *) 
  | Abso (* abs in -ya *)
  | Voca (* vocatives *)
  | Inv (* invocations *)
  | Iic | Iic2 (* first part of compounds *)
  | Iiif (* iic of ifc, atteinable from previous iic eg -vartin iic -varti- *) 
  | Iiv | Iivv | Iivc (* inchoatives - cvi verbal compounds *)
  | Auxi | Auxiinv | Auxik | Auxiick (* forms of auxiliary verbs as bhuu k.r *)
  | Ifc | Ifc2 (* second part of compounds *)
  | Peri (* periphrastic perfect *)
  | Lopa (* e/o conjugated root forms with lopa *) 
  | Lopak (* e/o kridantas forms with lopa *) 
  | Pv (* Preverb optional before Root or Lopa or mandatory before Abso *)
  | Pvc | Pvv (* privative Abso *)
  | Pvkc | Pvkv (* Preverb optional before Krid or Iik or Lopak *)
  | A | An (* privative nan-compounds formations in a- or -an *)
  | Ai | Ani (* initial privative nan-compounds *)
  | Iicv | Iicc (* split of Iic by first letter resp. vowel or consonant *)
  | Ifcv | Ifcc (* idem for Ifc *)
  | Nouv | Nouc (* idem for Noun *)
  | Krid (* Kridantas eg participles *) 
  | Vok (* Kridanta vocatives *)  
  | Iik (* Kridanta iics *)
  | Iikv | Iikc | Kriv | Kric | Vocv | Vocc | Vokv | Vokc
  | Iiy | Avy (* Avyayiibhaavas *)
  | Inftu | Kama (* vaktukaama cpds *) 
  | Cache | Cachei (* Lexicon acquisition *) 
  | Unknown (* Unrecognized chunk *)
  (* now pseudo phase tagging root/kridanta forms with preverbs *)
  | Comp of tag and (* pv *) Word.word and (* root/krid in tag *) Word.word 
  ] 
and tag = (phase * phase) (* preverb phase and root/taddhita phase *)
(* NB. It is essential to keep both phases to identify transition checkpoints *)
and phases = list phase
(* NB. In Simplified mode, we use only 10 phases:
   [[ Noun2; Pron; Iic2; Ifc2; Root; Inde; Pv; Iiv; Abso; Lopa ]] *)
;
(* Marshalling for cgi invocations *)
value rec string_of_phase = fun 
  [ Noun  -> "Noun"
  | Noun2 -> "Noun2" 
  | Pron  -> "Pron"
  | Root  -> "Root" 
  | Inde  -> "Inde"
  | Absv  -> "Absv" 
  | Absc  -> "Absc" 
  | Abso  -> "Abso" 
  | Voca  -> "Voca"
  | Inv   -> "Inv" 
  | Iic   -> "Iic"
  | Iic2  -> "Iic2" 
  | Iiif  -> "Iiif"
  | Iiv   -> "Iiv"
  | Iivv  -> "Iivv"
  | Iivc  -> "Iivc"
  | Auxi  -> "Auxi" 
  | Auxiinv -> "Auxiinv" 
  | Auxik -> "Auxik" 
  | Auxiick -> "Auxiick" 
  | Ifc   -> "Ifc"
  | Ifc2  -> "Ifc2" 
  | Lopa  -> "Lopa"
  | Lopak -> "Lopak"
  | Pv    -> "Pv" 
  | Pvc   -> "Pvc" 
  | Pvv   -> "Pvv" 
  | Pvkc  -> "Pvkc" 
  | Pvkv  -> "Pvkv"
  | A     -> "A"
  | An    -> "An" 
  | Ai    -> "Ai"
  | Ani   -> "Ani" 
  | Iicv  -> "Iicv"
  | Iicc  -> "Iicc" 
  | Ifcv  -> "Ifcv"
  | Ifcc  -> "Ifcc" 
  | Nouv  -> "Nouv"
  | Nouc  -> "Nouc"  
  | Krid  -> "Krid"
  | Vok   -> "Vok" 
  | Vokv  -> "Vokv" 
  | Vokc  -> "Vokc" 
  | Iik   -> "Iik"
  | Iikv  -> "Iikv" 
  | Iikc  -> "Iikc"
  | Iiy   -> "Iiy" 
  | Avy   -> "Avya" 
  | Kriv  -> "Kriv"
  | Kric  -> "Kric" 
  | Vocv  -> "Vocv" 
  | Vocc  -> "Vocc"
  | Peri  -> "Peri"
  | Inftu -> "Inftu"
  | Kama  -> "Kama" 
  | Cache -> "Cache" 
  | Cachei -> "Cachei" 
  | Unknown -> "Unknown"
  | _ -> failwith "string_of_phase"
  ] 
and phase_of_string = fun (* unsafe *)  
  [ "Noun"  -> Noun
  | "Noun2" -> Noun2 
  | "Pron"  -> Pron
  | "Root"  -> Root
  | "Inde"  -> Inde
  | "Abso"  -> Abso
  | "Absv"  -> Absv
  | "Absc"  -> Absc
  | "Voca"  -> Voca
  | "Inv"   -> Inv
  | "Iic"   -> Iic
  | "Iic2"  -> Iic2
  | "Iiif"  -> Iiif
  | "Iiv"   -> Iiv
  | "Iivv"  -> Iivv
  | "Iivc"  -> Iivc
  | "Auxi"  -> Auxi
  | "Auxiinv" -> Auxiinv
  | "Auxik" -> Auxik 
  | "Auxiick" -> Auxiick
  | "Ifc"   -> Ifc
  | "Ifc2"  -> Ifc2
  | "Lopa"  -> Lopa
  | "Lopak" -> Lopak
  | "Pv"    -> Pv
  | "Pvv"   -> Pvv
  | "Pvc"   -> Pvc
  | "Pvkc"  -> Pvkc
  | "Pvkv"  -> Pvkv
  | "A"     -> A
  | "An"    -> An
  | "Ai"    -> Ai
  | "Ani"   -> Ani
  | "Iicv"  -> Iicv
  | "Iicc"  -> Iicc
  | "Ifcv"  -> Ifcv
  | "Ifcc"  -> Ifcc
  | "Nouv"  -> Nouv
  | "Nouc"  -> Nouc
  | "Krid"  -> Krid
  | "Vokv"  -> Vokv
  | "Vokc"  -> Vokc
  | "Iik"   -> Iik
  | "Iikv"  -> Iikv
  | "Iikc"  -> Iikc
  | "Iiy"   -> Iiy
  | "Avya"  -> Avy
  | "Kriv"  -> Kriv
  | "Kric"  -> Kric
  | "Vocv"  -> Vocv
  | "Vocc"  -> Vocc
  | "Peri"  -> Peri 
  | "Inftu" -> Inftu
  | "Kama"  -> Kama
  | "Unknown" -> Unknown
  | "Cache" -> Cache
  | "Cachei" -> Cachei
  | s -> failwith ("Unknown phase " ^ s)
  ]
;
value unknown = Unknown
and aa_phase = fun (* phase of preverb "aa" according to following phase *)
    [ Root | Abso | Peri | Inftu -> Pv | _ -> Pvkv ]
and un_lopa = fun (* phase of origin of lopa *)
    [ Lopa -> Root | Lopak -> Kriv | _ -> failwith "un_lopa" ] 
and preverb_phase = fun 
    [ Pv | Pvv | Pvc | Pvkc | Pvkv -> True | _ -> False ]
and krid_phase = fun [ Krid | Kric | Kriv -> True | _ -> False ]
and ikrid_phase = fun [ Iik | Iikc | Iikv -> True | _ -> False ]
and vkrid_phase = fun [ Vokc | Vokv -> True | _ -> False ]
and ii_phase = fun [ Iicv | Iicc | Iikv | Iikc | A | An -> True | _ -> False ]
and is_cache phase = (phase = Cache) || (phase = Cachei)
;
(* Needed as argument of [Morpho.print_inv_morpho] *)
value rec generative = fun
  [ Krid | Kriv | Kric | Vokv | Vokc | Iik | Iikv | Iikc | Auxik -> True
  | Comp (_,ph) _ _ -> generative ph
  | _ -> False
  ]
;

open Html;
value rec color_of_phase = fun
  [ Noun | Noun2 | Lopak | Nouc | Nouv | Kriv | Kric | Krid | Auxik | Kama
         | Cache -> Deep_sky 
  | Pron -> Light_blue
  | Root | Auxi | Lopa -> Carmin  
  | Inde | Abso | Absv | Absc | Auxiinv | Ai | Ani -> Mauve
  | Iiy -> Lavender
  | Avy -> Magenta
  | Inftu -> Salmon 
  | Iic | Iic2 | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif 
        | Auxiick | Cachei -> Yellow
  | Peri | Iiv | Iivv | Iivc -> Orange
  | Voca | Vocv | Vocc | Inv | Vok | Vokv | Vokc -> Lawngreen
  | Ifc | Ifcv | Ifcc | Ifc2 -> Cyan
  | Unknown -> Grey
  | Comp (_,ph) _ _ -> color_of_phase ph 
  | Pv | Pvv | Pvc | Pvkc | Pvkv -> failwith "Illegal preverb segment" 
(*[| _ -> raise (Control.Anomaly "Unexpected color")] *)
(*i NB: unused background colors: Pink Green Aquamarine Chamois i*)
  ]
; 

end; (* Phases *)
