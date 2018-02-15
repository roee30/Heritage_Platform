(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* [Load_transducers] *)
(* Used for loading the transducers as well as root informations *)
(* Caution. This is an executable, that actually loads the transducers at
   link time. It also has some redundancy with [Load_morphs]. *)
open Morphology; 
open Auto.Auto; (* auto State *) 

type transducer_vect = 
  { nouv : auto (* vowel-initial nouns *)
  ; nouc : auto (* consonant-initial nouns *)
(*; noun : auto (* declined nouns and undeclinables *) *)
  ; noun2 : auto (* idem in mode non gen *)
  ; pron : auto (* declined pronouns *)
  ; root : auto (* conjugated root forms *)
(*; krid : auto (* kridantas forms *) *)
  ; lopa : auto (* e/o conjugated root forms with lopa *)
  ; lopak : auto (* e/o kridantas forms with lopa *) 
  ; inde : auto (* indeclinables + infinitives *)
  ; abso : auto (* abso-ya *)
  ; absv : auto (* vowel-initial abso-tvaa *)
  ; absc : auto (* consonant-initial abso-tvaa *)
  ; peri : auto (* periphrastic perfect *)
  ; vokv : auto (* kridanta vocatives *)
  ; vokc : auto (* id *)
  ; inv  : auto (* invocations *) 
(*; iic  : auto (* iic stems *) *)
  ; iic2 : auto (* iic stems in mode non gen *)
  ; iifc : auto (* iic forms of ifc stems *) 
(*; iik  : auto (* iik stems *) *)
  ; iiv  : auto (* iiv periphrastic stems *)
  ; auxi : auto (* their k.r and bhuu finite forms supports *) 
  ; auxik : auto (* their k.r and bhuu kridanta forms supports *) 
  ; auxiick : auto (* their k.r and bhuu iic kridanta forms supports *) 
  ; ifc  : auto (* ifc forms *)
(*  ; ifcv : auto (* vowel-initial ifc *)
  ; ifcc : auto (* consonant-initial ifc *) *)
  ; ifc2 : auto (* ifc forms in mode non gen *)
  ; iiy  : auto (* iic avyayiibhava *) 
  ; avya : auto (* ifc avyayiibhava *)
  ; inftu : auto (* infinitives in -tu *)
  ; kama : auto (* forms of kaama *)
  ; prev : auto (* preverb sequences *)
  ; pvc  : auto (* preverb sequences starting with consonant *)
  ; pvv  : auto (* preverb sequences starting with vowel *)
  ; a    : auto (* privative a *)
  ; an   : auto (* privative an *)
  ; iicv : auto (* vowel-initial iic *)
  ; iicc : auto (* consonant-initial iic *)
  ; iivv : auto (* vowel-initial iiv *)
  ; iivc : auto (* consonant-initial iiv *)
  ; vocv : auto (* vowel-initial vocatives *)
  ; vocc : auto (* consonant-initial vocatives *)
  ; iikv : auto (* vowel-initial iik *)
  ; iikc : auto (* consonant-initial iik *)
  ; kriv : auto (* vowel-initial krids *)
  ; kric : auto (* consonant-initial krids *)
  ; sfx  : auto (* taddhita suffixes *)
  ; isfx : auto (* taddhita suffixes for iic stems *)
  ; cache : auto (* user-defined supplement to noun *)
  }
;

module Trans (* takes its prelude and control arguments as parameters *)
  (Prel: sig value prelude : unit -> unit; end) 
 = struct 

value abort cat = 
  let mess = "Missing " ^ cat ^ " database" in 
  raise (Control.Anomaly mess)
;
value empty_trans = State(False,[],[]) (* dummy empty transducer *)
;
(* Load persistent transducer automaton of given phase (lexical category). *)
(* These files have been copied from their development version [transn_file]
   etc. created by [Make_inflected] followed by [Make_automaton]. *)
value load_transducer cat = 
  let file = match cat with 
      [ "Noun"    -> Web.public_transn_file
      | "Noun2"   -> Web.public_transn2_file
      | "Pron"    -> Web.public_transpn_file
      | "Verb"    -> Web.public_transr_file
      | "Krid"    -> Web.public_transpa_file
      | "Vok"     -> Web.public_transpav_file
      | "Peri"    -> Web.public_transperi_file
      | "Lopa"    -> Web.public_translopa_file
      | "Lopak"   -> Web.public_translopak_file
      | "Inde"    -> Web.public_transinde_file
      | "Iic"     -> Web.public_transic_file
      | "Iic2"    -> Web.public_transic2_file
      | "Iiif"    -> Web.public_transiif_file
      | "Iik"     -> Web.public_transpic_file
      | "Iiv"     -> Web.public_transiv_file
      | "Ifc"     -> Web.public_transif_file
      | "Ifc2"    -> Web.public_transif2_file
      | "Iiy"     -> Web.public_transiiy_file
      | "Avya"    -> Web.public_transavy_file
      | "Abstvaa" -> Web.public_transabstvaa_file
      | "Absya"   -> Web.public_transabsya_file
      | "Inftu"   -> Web.public_transinftu_file
      | "Kama"    -> Web.public_transkama_file
      | "Auxi"    -> Web.public_transauxi_file
      | "Auxik"   -> Web.public_transauxik_file 
      | "Auxiick" -> Web.public_transauxiick_file
      | "Voca"    -> Web.public_transvoca_file
      | "Inv"     -> Web.public_transinv_file 
      | "Prev"    -> Web.public_transp_file
      | "Sfx"     -> Web.public_transsfx_file
      | "Isfx"    -> Web.public_transisfx_file
      | "Cache"   -> Web.public_transca_file 
      | _ -> failwith ("Unexpected category: " ^ cat) 
      ] in 
  try (Gen.gobble file : auto) 
  with [ _ -> if cat="Cache" (* uninitialized cache *)
                 then empty_trans (* initialised to empty transducer *)
              else do { Prel.prelude (); abort cat } ]
; 
(* privative prefixes automata *)
value a_trans = State(False,[(1,State(True,[],[cch]))],[])
  where cch = (([ 22; 23 ],[],[ 23 ]) : rule) (* a-ch \R acch *)
and an_trans  = let n_trans = State(False,[(36,State(True,[],[]))],[]) in 
                State(False,[(1,n_trans)],[])
;
(* Splitting an automaton into vowel-initial and consonant-initial solutions *)
(* with maximum sharing. Assumes deter is in increasing order of phonemes.   *)
value split deter = 
  let (rv,c) = split_rec [] deter 
    where rec split_rec vow con = match con with 
    [ [] -> (vow,[])
    | [ ((c,_) as arc) :: rest ] -> 
        if c>16 then (vow,con) else split_rec [ arc :: vow ] rest
    ] in 
  (List.rev rv,c)
;
value split_auto = fun 
  [ State (False,det,[]) -> 
      let (vow,con) = split det in
      (State (False,vow,[]),State (False,con,[]))
    (* This assumes no non-determinism at the top node *)
  | State (False,det,rules) -> 
      let (vow,con) = split det in
      (State (False,vow,rules),State (False,con,[]))
    (* This assumes non-determinism at the top node, and is needed for the 
       preverb automaton. It assumes that the rules concern the vowel part. *)
  | _ -> failwith "Split_auto"
  ]
;
value transducers = 
  let transn  = load_transducer "Noun"
  and transi  = load_transducer "Iic" 
  and transf  = load_transducer "Ifc" 
  and transk  = load_transducer "Krid"
  and transik = load_transducer "Iik" 
  and transv  = load_transducer "Voca" 
  and vok     = load_transducer "Vok"
  and iiv     = load_transducer "Iiv"
  and abstvaa = load_transducer "Abstvaa"
  and pv = load_transducer "Prev" in
  (* now we split the subanta stems and forms starting with vowel or consonant *)
  let (transnv,transnc) = split_auto transn
  and (transiv,transic) = split_auto transi  
  and (kriv,kric) = split_auto transk
  and (iikv,iikc) = split_auto transik  
  and (iivv,iivc) = split_auto iiv
  and (vocv,vocc) = split_auto transv
  and (vokv,vokc) = split_auto vok
  and (absv,absc) = split_auto abstvaa
  and (pvkv,pvkc) = split_auto pv in
  { noun2 = load_transducer "Noun2"
  ; root = load_transducer "Verb"
  ; pron = load_transducer "Pron"
  ; peri = load_transducer "Peri" 
  ; lopa = load_transducer "Lopa" 
  ; lopak = load_transducer "Lopak"
  ; inde = load_transducer "Inde"
  ; abso = load_transducer "Absya"
  ; iic2 = load_transducer "Iic2" 
  ; iifc = load_transducer "Iiif"  
  ; ifc  = transf
  ; ifc2  = load_transducer "Ifc2"
  ; iiv = iiv 
  ; auxi  = load_transducer "Auxi"
  ; auxik = load_transducer "Auxik"
  ; auxiick = load_transducer "Auxiick"
  ; inv  = load_transducer "Inv" 
  ; iiy  = load_transducer "Iiy" 
  ; avya = load_transducer "Avya" 
  ; inftu = load_transducer "Inftu" 
  ; kama = load_transducer "Kama" 
  ; prev = pv
  ; pvc = pvkc
  ; pvv = pvkv
  ; a = a_trans
  ; an = an_trans
  ; iicv = transiv
  ; iicc = transic
  ; iivv = iivv
  ; iivc = iivc
  ; nouv = transnv
  ; nouc = transnc
  ; vocv = vocv
  ; vocc = vocc
  ; vokv = vokv
  ; vokc = vokc
  ; kriv = kriv
  ; kric = kric
  ; iikv = iikv
  ; iikc = iikc
  ; absv = absv
  ; absc = absc
  ; sfx  = load_transducer "Sfx" 
  ; isfx = load_transducer "Isfx" 
  ; cache = load_transducer "Cache"
  }
;

(* Lexicalized root informations needed for Dispatcher *)
value roots_usage = (* attested preverb sequences *)
  try (Gen.gobble Web.public_roots_usage_file : Deco.deco string) 
  with [ _ ->  do { Prel.prelude (); abort "RU" } ]
; 

end (* Trans *)
;
