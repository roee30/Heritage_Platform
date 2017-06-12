(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Dispatcher: Sanskrit Engine in 53 phases automaton (plus 2 fake ones) *)

(* The Dispatch functor maps a transducer vector of 39 aums into     *)
(*  - a dispatch automaton implementing a regular description over   *)
(*    45 phases of lexical analysis                                  *)
(*  - an initial vector of initial resumptions                       *)
(*  - a final test for lexical acceptance                            *)
(*  - a consistency check of the output of the segmenting transducer *)

(* Dispatch, instantiated by Transducers, is used as parameter of the 
   Segment functor from Segmenter or Interface.   *)

open Auto.Auto; 
open Load_transducers; (* [transducer_vect Trans roots_morpho krids_morpho] *)
open Skt_morph;
open Morphology; (* [inflected inflected_map Verb_form morphology] *)
open Naming; (* [homo_undo look_up_homo unique_kridantas] *)  
open Phases.Phases; (* phase etc. *)

module Dispatch 
  (* To be instantiated by Transducers from Lexer or Interface *) 
  (Trans: sig value transducers : transducer_vect;   
              value roots_usage : Deco.deco string; end) 
  (Lem: sig value morpho : morphology; end) = struct  
open Trans; 
open Lem;

(* [ transducer : phase -> auto ] *)
value transducer = fun
  [ Nouv -> transducers.nouv (* vowel-initial noun *)
  | Nouc -> transducers.nouc (* consonant-initial noun *)
  | Noun2 -> transducers.noun2 (* idem in mode non gen *) 
  | Pron -> transducers.pron (* declined pronouns *) 
  | Root -> transducers.root (* conjugated root forms and infinitives *) 
  | Vokv -> transducers.vokv (* vowel-initial vocative k.rdaantas *)  
  | Vokc -> transducers.vokc (* consonant-initial vocative k.rdaantas *)  
  | Inde -> transducers.inde (* indeclinables, particles  *) 
  | Absv -> transducers.absv (* vowel-initial absolutives in -tvaa *) 
  | Absc -> transducers.absc (* consonant-initial absolutives in -tvaa *) 
  | Abso -> transducers.abso (* absolutives in -ya *) 
  | Iic2 -> transducers.iic2 (* idem in mode non gen *) 
  | Iiif -> transducers.iifc (* fake iic of ifc stems *) 
  | Iiv  -> transducers.iiv  (* in initio verbi nominal stems, perpft *) 
  | Inv  -> transducers.inv  (* invocations *)   
  | Auxi -> transducers.auxi       (* k.r and bhuu finite forms *)
  | Auxik -> transducers.auxik     (* k.r and bhuu kridanta forms *) 
  | Auxiick -> transducers.auxiick (* k.r and bhuu kridanta bare forms *)
  | Peri -> transducers.peri (* periphrastic perfect *)
  | Lopa -> transducers.lopa (* e/o root forms *)
  | Lopak -> transducers.lopak (* e/o kridanta forms *)
  | Ifc  -> transducers.ifc  (* in fine composi forms *)
  | Ifc2 -> transducers.ifc2 (* idem in mode non gen *)
  | Pv   -> transducers.prev (* preverbs *) 
  | Pvkc -> transducers.pvc  (* preverbs starting with consonant *) 
  | Pvkv -> transducers.pvv  (* preverbs starting with vowel *) 
  | A | Ai   -> transducers.a    (* privative a *)
  | An | Ani -> transducers.an   (* privative an *)
  | Iicv -> transducers.iicv (* vowel-initial iic *)
  | Iicc -> transducers.iicc (* consonant-initial iic *)
  | Iikv -> transducers.iikv (* vowel-initial iic k.rdaanta *)
  | Iikc -> transducers.iikc (* consonant-initial iic k.rdaanta *)
  | Iivv -> transducers.iivv (* vowel-initial iiv (cvi) *)
  | Iivc -> transducers.iivc (* consonant-initial iiv (cvi) *)
  | Kriv -> transducers.kriv (* vowel-initial krid *)
  | Kric -> transducers.kric (* consonant-initial krid *) 
  | Vocv -> transducers.vocv (* vowel-initial vocatives *)
  | Vocc -> transducers.vocc (* consonant-initial vocatives *)
  | Iiy  -> transducers.iiy  (* iic avyayiibhava *)
  | Avy  -> transducers.avya (* ifc avyayiibhava *)
  | Inftu -> transducers.inftu (* infinitives in -tu *)
  | Kama -> transducers.kama (* forms of kaama *)
  | Sfx  -> transducers.sfx  (* ifc taddhita suffixes *)
  | Isfx -> transducers.isfx (* iifc taddhita suffixes *)
  | Cache -> transducers.cache (* cached forms *)
  | Noun | Iic | Iik | Voca | Krid | Pvk | Vok
    -> raise (Control.Anomaly "composite phase")
  | Unknown -> raise (Control.Anomaly "transducer - Unknown")
  | _ -> raise (Control.Anomaly "no transducer for fake phase") 
  ]
; 
(* Tests whether a word starts with a phantom phoneme (precooked aa-prefixed
   finite or participial or infinitive or abs-ya root form *)
value phantomatic = fun
  [ [ c :: _ ] -> c<(-2)
  | _ -> False
  ]
(* Amuitic forms start wiih -2 = [-] which elides preceding -a or -aa from Pv *)
and amuitic= fun
  [ [ -2 :: _ ] -> True 
  | _ -> False
  ]
;
(* We recognize $S = (Subst + Pron + Verb + Inde + Voca)^+$\\
   with $Verb = (1 + Pv).Root + Pv.Abso + Iiv.Auxi$,\\
       $Subst = Noun + Iic.Ifc + Iic.Subst + Iiv.Auxik$,\\
       $Noun = Nounv + Nounc$ and
       $Iic = Iicv + Iicc$\\
NB. $Abso$ = absolutives in -ya, 
    $Inde$ contains absolutives in -tvaa and infinitives,
    $Voca = Vocv + Vocc$ (vocatives),
    $Auxi$ = finite forms of bhuu and k.r.\\
The following is obtained from the above recursion equation by Brzozowski's 
derivatives like in Berry-Sethi's translator. *)

value cached = (* potentially cached lexicon acquisitions *)
  if Web.cache_active.val="t" then [ Cache ] else []
;
(* initial1, initial2: phases *)
value initial1 =
   (* All phases but Ifc, Abso, Auxi, Auxik, Auxiick, Lopa, Lopak, Sfx, Isfx. *)
   [ Inde; Iicv; Iicc; Nouv; Nouc; Pron; A; An; Root; Kriv; Kric; Iikv; Iikc
   ; Peri; Pv; Pvkv; Pvkc; Iiv; Iivv; Iivc; Iiy; Inv; Ai; Ani; Absv; Absc; Inftu
   ; Vocv; Vocc; Vokv; Vokc ] @ cached
and initial2 =  (* simplified segmenter with less phases, no generation *)
   [ Inde; Iic2; Noun2; Pron; Root; Pv; Iiv; Absv; Absc ] 
;
value initial full = if full then initial1 else initial2
;
(* dispatch1: Word.word -> phase -> phases *)
value dispatch1 w = fun (* w is the current input word *)
  [ Nouv | Nouc | Pron | Inde | Abso | Auxi | Auxik | Kama | Ifc 
  | Kriv | Kric | Absv | Absc | Avy | Lopak | Sfx | Root | Lopa ->
       if phantomatic w then [ Root; Kriv; Kric; Iikv; Iikc; Abso ] (* aa- pv *) 
       else initial1
  | A -> if phantomatic w then []
         else [ Iicc; Nouc; Iikc; Kric; Pvkc; Iivc; Vocc; Vokc ]
  | An -> if phantomatic w then [] 
          else [ Iicv; Nouv; Iikv; Kriv; Pvkv; Iivv; Vocv; Vokv
          ; A (* eg anak.sara *) ; An (* attested ? *) ]
  | Ai -> [ Absc ]
  | Ani -> [ Absv ]
    (* This assumes that privative prefixes cannot prefix Ifc forms 
       justified by \Pan{2,2,6} a-x only if x is a subanta. *)
  | Iicv | Iicc | Iikv | Iikc | Iiif | Auxiick -> (* Compounding *)
       [ Iicv; Iicc; Nouv; Nouc; A; An; Ifc; Iikv; Iikc; Kriv; Kric
       ; Pvkv; Pvkc; Iiif; Iivv; Iivc; Vocv; Vocc; Vokv; Vokc ] @ 
       [ Sfx; Isfx ] @ cached
  | Pv -> if phantomatic w then [] else 
          if amuitic w then [ Lopa ] else [ Root; Abso; Peri; Inftu ]
  | Pvkc | Pvkv -> if phantomatic w then [] else 
          if amuitic w then [ Lopak ] else [ Iikv; Iikc; Kriv; Kric; Vokv; Vokc ]
  | Iiv -> [ Auxi ] (* as bhuu and k.r finite forms *)
  | Iivv | Iivc -> [ Auxik; Auxiick ] (* bhuu and k.r kridanta forms *)
  | Iiy -> [ Avy ]
  | Isfx -> (* Compounding with taddhita *)
       [ Iicv; Iicc; Nouv; Nouc; A; An; Ifc; Iikv; Iikc; Kriv; Kric
       ; Pvkv; Pvkc; Iiif; Iivv; Iivc; Vocv; Vocc; Vokv; Vokc ] @ cached
  | Peri -> [ Auxi ] (* overgenerates, should be only perfect forms *)
  | Inftu -> [ Kama ] 
  | Vocc | Vocv | Vokv | Vokc | Cache -> [] 
      (* only chunk-final vocatives so no Iic overlap *) 
  | Inv -> [ Vocv; Vocc; Vokv; Vokc ] (* invocations before vocatives *) 
(* Privative prefixes A and An are not allowed to prefix Ifc like a-dhii *)
  | Noun | Iic | Iik | Voca | Krid | Noun2 | Iic2 | Ifc2 | Pvk | Vok
  | Unknown -> failwith "Dispatcher anomaly"
  | _ -> failwith "Dispatcher fake phase" 
  ]
and dispatch2 w = fun (* simplified segmenter *)
  [ Noun2 | Pron | Inde | Abso | Absv | Absc | Auxi | Ifc2 ->
       if phantomatic w then [ Root; Abso ]
       else initial2
  | Root | Lopa -> if phantomatic w then [] (* no consecutive verbs in chunk *)
                   else [ Inde; Iic2; Noun2; Pron ]
  | Iic2 -> if phantomatic w then []
            else [ Iic2; Noun2; Ifc2 ]  
  | Pv -> if phantomatic w then [] else  
          if amuitic w then [ Lopa ] else [ Root; Abso ]
  | Iiv -> [ Auxi ] 
  | _ -> failwith "Dispatcher anomaly"
  ]
;
(* dispatch: bool -> Word.word -> phase -> phases *)
value dispatch full = if full then dispatch1 else dispatch2
;
value terminal = (* Accepting phases *)
   [ Nouv; Nouc; Noun2
   ; Pron
   ; Root 
   ; Kriv 
   ; Kric 
   ; Inde
   ; Abso; Absv; Absc
   ; Ifc; Ifc2
   ; Auxi; Auxik
   ; Vocc; Vocv; Vokv; Vokc; Inv 
   ; Lopa; Lopak
   ; Avy; Kama
   ; Sfx
   ; Cache
   ]
;

(* accepting: phase -> bool *)
value accepting phase = List.mem phase terminal
;
(* Segmenter control *)
type input = Word.word (* input sentence represented as a word *)
and transition = (* Reflexive relation *)
    [ Euphony of rule (* [(w,rev u,v)] such that [u|v -> w] *)
    | Id                   (* identity or no sandhi *)
    ]
and segment = (phase * Word.word * transition)
and output = list segment
;
(* Now consistency check - we check that preverbs usage is consistent with
   root px declaration in lexicon *)

value assoc_word word deco =
   let infos = Deco.assoc word deco in
   if infos = [] then failwith ("Unknown form: " ^ Canon.decode word)
   else infos
;
value autonomous root = (* root form allowed without preverb *)
  let infos = assoc_word root roots_usage in
  match infos with 
    [ [ "" :: _ ] -> True 
    | _ -> False
    ]
and attested prev root = (* prev is attested preverb sequence for root *)
  let pvs = assoc_word root roots_usage in
  List.mem prev pvs (* NB attested here means lexicalized entry *)
;
(* Now we retrieve finer discrimination for verbs forms preceded by preverbs.
   This is experimental, and incurs too many conversions betweeen strings
   and words, suggesting a restructuring of preverbs representation. *)
value preverbs_structure =
  try (Gen.gobble Web.public_preverbs_file : Deco.deco Word.word) 
                  (*i should probably be rather : [Deco.deco string] i*)
  with [ _ -> failwith "preverbs_structure" ]
;
value gana_o = fun 
  [ None -> 0 (* arbitrary *)
  | Some g -> g (* only used for "tap" *)
  ] 
and voice_o v = fun
  [ None -> True
  | Some voice -> voice = v
  ]
;
(* pvs is a list of preverb words *)
(* upasarga closest to the root form *)
value main_preverb pvs = List2.last pvs 
;
value main_preverb_string pv = 
  Canon.decode (main_preverb (assoc_word pv preverbs_structure))
;
value attested_verb (o_gana,o_voice) pv root = attested pv root && 
  let gana = gana_o o_gana in
  let upasarga = main_preverb_string (Encode.code_string pv) in 
  try let pada = Pada.voices_of_pv upasarga gana (Canon.decode root) in
      match pada with
      [ Pada.Ubha -> True
      | _ -> voice_o pada o_voice 
      ] 
  with [ Pada.Unattested -> False ]
;
(* Similarly for root forms used without preverb *)
value autonomous_root (o_gana,o_voice) root = autonomous root &&  
  let gana = gana_o o_gana in 
  try let pada = Pada.voices_of_pv "" (*i glitch i*) gana (Canon.decode root) in 
      match pada with
      [ Pada.Ubha -> True
      | _ -> voice_o pada o_voice 
      ] 
  with [ Pada.Unattested -> False ]
;
value pada_of_voice = fun
  [ Active -> Some Pada.Para
  | Middle -> Some Pada.Atma
  | _ -> None 
  ] 
;
exception Unvoiced
;
value extract_gana_pada = fun
  [ Verb_form (conj,paradigm) _ _ -> 
       let (o_gana,voice) = match paradigm with
           [ Presenta g _ -> (Some g,Active)
           | Presentm g _ -> (Some g,Middle)
           | Presentp _   -> (None,Passive)
           | Conjug _ v | Perfut v -> (None,v)
           ] in
       (conj,(o_gana,pada_of_voice voice))
  | Ind_verb _ _ -> raise Unvoiced (* could be refined *)
  | _ -> failwith "Unexpected root form" 
  ]
and extract_gana_pada_k krit =
    let (o_gana,voice) = match krit with
       [ Ppp | Pprp | Pfutp _ -> (None,Passive)
       | Pppa | Ppfta | Pfuta -> (None,Active)
       | Ppftm | Pfutm -> (None,Middle)
       | Ppra g -> (Some g,Active)
       | Pprm g -> (Some g,Middle)
       | _ -> raise Unvoiced (* could be refined *)
       ] in
    (o_gana,pada_of_voice voice)
;
value fail_inconsistency form =
  raise (Control.Anomaly ("Unknown root form: " ^ Canon.decode form))
;
value valid_morph_pv pv root (morph : Morphology.inflexion_tag) = try
  let (conj,gana_pada) = extract_gana_pada morph in
  if conj=Primary then attested_verb gana_pada pv root else attested pv root
  with [ Unvoiced -> attested pv root ]
and valid_morph_aut root (morph : Morphology.inflexion_tag) = try
  let (conj,gana_pada) = extract_gana_pada morph in
  if conj=Primary then autonomous_root gana_pada root 
                  else autonomous root (* eg. kalpaya Para ca. while k.lp Atma *)
  with [ Unvoiced -> autonomous root ]
;
value valid_morph_pv_k pv krit_stem morph = (* morph of form [Part_form] *)
  let (homo,bare_stem) = homo_undo krit_stem in  
  let krit_infos = assoc_word bare_stem unique_kridantas in  
  let ((conj,krit),root) = look_up_homo homo krit_infos in try
  (* Asymmetry of treatment: conj is deduced from [krit_stem], not from morph *)
  let gana_pada = extract_gana_pada_k krit in 
  if conj=Primary then attested_verb gana_pada pv root else attested pv root 
  with [ Unvoiced -> attested pv root ]
;
value validate_pv pv root_form = (* generalizes [roots_of] *) 
  match Deco.assoc root_form morpho.roots with
    [ [] -> fail_inconsistency root_form
    | tags -> List.exists valid tags 
              (* NB later on the lexer will refine in filtering validity *)
              where valid (delta,morphs) = 
                let root = Word.patch delta root_form in  
                List.exists (valid_morph_pv pv root) morphs
    ]  
;
value validate_pv_tu pv root_form = (* special case infinitive forms in -tu *) 
  match Deco.assoc root_form morpho.inftu with
    [ [] -> fail_inconsistency root_form
    | tags -> List.exists valid tags 
              (* NB later on the lexer will refine in filtering validity *)
              where valid (delta,morphs) = 
                let root = Word.patch delta root_form in  
                List.exists (valid_morph_pv pv root) morphs
    ]  
;
value validate_pv_k pv krit_form (delta,_) = (* see [Morpho.print_inv_morpho] *)
  let krit_stem = Word.patch delta krit_form in 
  let (homo,bare_stem) = homo_undo krit_stem in 
  let krit_infos = assoc_word bare_stem unique_kridantas in 
  let ((conj,krit),root) = look_up_homo homo krit_infos in try
  let gana_pada = extract_gana_pada_k krit in  
  if conj=Primary then attested_verb gana_pada pv root else attested pv root
  with [ Unvoiced -> attested pv root ]
;
(* We should verify aa- validation for phantomatic forms *)
value autonomous_form root_form = 
  match Deco.assoc root_form morpho.roots with
    [ [] -> fail_inconsistency root_form
    | tags -> List.exists valid tags (* Lexer will filter later on *)
      where valid (delta,morphs) = 
        let root = Word.patch delta root_form in
        List.exists (valid_morph_aut root) morphs 
    ]  
;
(* This allows to rule out ifc only kridantas even when root autonomous *)
value filter_out_krit krit root = match Canon.decode root with
  [ "i" | "dagh" -> krit = Ppp (* -ita -daghna *)
  | _ -> False
  ] 
;
(* We should verify aa- validation for phantomatic forms *)
value autonomous_form_k krid_form (delta,_) =
  let stem = Word.patch delta krid_form in
  let (homo,bare_stem) = homo_undo stem in
  let krid_infos = assoc_word bare_stem unique_kridantas in 
  let ((conj,krit),root) = look_up_homo homo krid_infos in try
  let gana_pada = extract_gana_pada_k krit in  
  if conj=Primary then if filter_out_krit krit root then False
                       else autonomous_root gana_pada root else True
  with [ Unvoiced -> autonomous root ]
;
(* Checks whether a verbal or participial form is attested/validated *)
value valid_morpho gen = 
  if gen then valid_morph_pv_k else valid_morph_pv 
;
(* This inspects a multitag in order to filter out pv-inconsistent taggings. *)
(* It is used by Interface and Lexer for Reader and Parser *)
value trim_tags gen form pv tags = List.fold_right trim tags []
      where trim (delta,morphs) acc = (* tags : Morphology.multitag *)  
        let stem = Word.patch delta form in (* root or kridanta *)
        let valid_pv = valid_morpho gen pv stem in 
        let ok_morphs = List.filter valid_pv morphs in  
        if ok_morphs = [] then acc else [ (delta,ok_morphs) :: acc ] 
;
(* Preventing overgeneration of forms "sa" and "e.sa" \Pan{6,1,132} *)
value not_sa_v = fun (* Assumes next pada starts with a vowel *)
  [ [ (Pron,[ 1; 48 ],_) :: _ ] (* sa *)
  | [ (Pron,[ 1; 47; 10 ],_) :: _ ] (* e.sa *) -> False 
  | _ -> True
  ]
and sa_before_check form = fun (* Next pada should start with a consonant *)
  [ [ (Pron,[ 1; 48 ],_) :: _ ] (* sa *)
  | [ (Pron,[ 1; 47; 10 ],_) :: _ ] (* e.sa *) -> Phonetics.consonant_initial form
  | _ -> True
  ]
;
(* Similar to [List2.subtract] but raises Anomaly exception *)
value rec chop word = fun  
  [ [] -> word
  | [ c :: r ] -> match word with 
     [ [ c' :: r' ] when c'=c -> chop r' r
     | _ -> raise (Control.Anomaly "Wrong transition between segments")
     ]
  ]
; 
value sfx_phase = fun [ Sfx | Isfx -> True | _ -> False ]
and iic_phase = fun 
  [ Iicv | Iicc | Iikv | Iikc 
  | Comp (_,Iikv) _ _ | Comp (_,Iikc) _ _ -> True
  | _ -> False ]
;
value apply_sandhi rleft right = fun
    [ Euphony (w,ru,v) -> 
       let rl = chop rleft ru
       and r =  chop right v in List2.unstack rl (w @ r)
    | Id -> List2.unstack rleft right
    ]
;
(* [validate : output -> output] - dynamic consistency check in Segmenter.
   It refines the regular language of dispatch by contextual conditions
   expressing that preverbs are consistent with the following verbal form. 
   The forms are then compounded, otherwise rejected. *)
(* Things would be much simpler if we generated forms of verbs and kridantas
 with (only valid) preverbs attached, since this check would be unnecessary.
 On the other hand, we would have to solve the ihehi problem. *)
(* A similar kind of aggregation is effected for a few generative taddhitas,
   but this is still experimental. *)
value validate out = match out with
  [ [] -> []
  | [ (Root,rev_root_form,s) :: [ (Pv,prev,sv) :: r ] ] ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and root_form = Word.mirror rev_root_form in
      if validate_pv pv_str root_form then  
         let form = apply_sandhi prev root_form sv in
         let verb_form = Word.mirror form in
         (* We glue the two segments with a composite tag keeping information *)
         [ (Comp (Pv,Root) pv root_form,verb_form,s) :: r ] 
      else []
  | [ (Root,rev_root_form,_) :: next ] ->
      let root_form = Word.mirror rev_root_form in 
      if autonomous_form root_form && sa_before_check root_form next
      then out else [] 
  | [ (Lopa,rev_lopa_form,s) :: [ (Pv,prev,sv) :: r ] ] -> 
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and lopa_form = Word.mirror rev_lopa_form in
      let root_form = match lopa_form with 
                      [ [ -2 :: rf ] -> rf | _ -> failwith "Wrong lopa form" ] in
      if validate_pv pv_str root_form then
        let form = apply_sandhi prev lopa_form sv in
        let verb_form = Word.mirror form in
        [ (Comp (Pv,Lopa) pv lopa_form,verb_form,s) :: r ] 
      else []
  | [ (Lopa,rev_lopa_form,_) :: next ] ->
      let lopa_form = Word.mirror rev_lopa_form in 
      if autonomous_form lopa_form
      && sa_before_check lopa_form next
      then out else []
  | (* infinitives in -tu with preverbs *)
    [ (Inftu,rev_root_form,s) :: [ (Pv,prev,sv) :: r ] ] ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and root_form = Word.mirror rev_root_form in
      if validate_pv_tu pv_str root_form then  
         let form = apply_sandhi prev root_form sv in
         let verb_form = Word.mirror form in
         (* We glue the two segments with a composite tag keeping information *)
         [ (Comp (Pv,Inftu) pv root_form,verb_form,s) :: r ] 
      else []
   | (* kridanta forms with preverbs *)
     [ (phk,rev_krid_form,s) :: [ (ph,prev,sv) :: r ] ] 
         when krid_phase phk && preverb_phase ph ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.krids with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (validate_pv_k pv_str krid_form) tags then
                   let form = apply_sandhi prev krid_form sv in
                   let cpd_form = Word.mirror form in
                   [ (Comp (ph,phk) pv krid_form,cpd_form,s) :: r ] 
                else []
      ]
  | [ (Kriv,rev_krid_form,_) :: next ] ->
      let krid_form = Word.mirror rev_krid_form in
      if phantomatic krid_form then failwith "Kriv phantom" else (* PB *)
      match Deco.assoc krid_form morpho.krids with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form) 
      | tags -> if List.exists (autonomous_form_k krid_form) tags && not_sa_v next
                then out else []
      ]
  | [ (Kric,rev_krid_form,_) :: _ ] ->
      let krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.krids with
      [ [] -> failwith ("Unknown krid_form: " ^ (Canon.decode krid_form))
      | tags -> if List.exists (autonomous_form_k krid_form) tags
                then out else []
      ]
   | (* iic kridanta forms with preverbs *)
     [ (phk,rev_ikrid_form,s) :: [ (ph,prev,sv) :: r ] ] 
         when ikrid_phase phk && preverb_phase ph ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and ikrid_form = Word.mirror rev_ikrid_form in
      match Deco.assoc ikrid_form morpho.iiks with
      [ [] -> failwith ("Unknown ikrid_form: " ^ Canon.decode ikrid_form)
      | tags -> if List.exists (validate_pv_k pv_str ikrid_form) tags then
                   let form = apply_sandhi prev ikrid_form sv in
                   let cpd_form = Word.mirror form in
                   [ (Comp (ph,phk) pv ikrid_form,cpd_form,s) :: r ] 
                else []
      ]
  | [ (Iikv,rev_krid_form,_) :: next ] ->
      let krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.iiks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (autonomous_form_k krid_form) tags && not_sa_v next
                then out else []
      ]
  | [ (Iikc,rev_krid_form,_) :: _ ] ->
      let krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.iiks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (autonomous_form_k krid_form) tags
                then out else []
      ]
  | (* vocative kridanta forms with preverbs *)
    [ (phk,rev_krid_form,s) :: [ (ph,prev,sv) :: r ] ] 
         when vkrid_phase phk && preverb_phase ph ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.voks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (validate_pv_k pv_str krid_form) tags then
                   let form = apply_sandhi prev krid_form sv in
                   let cpd_form = Word.mirror form in
                   [ (Comp (ph,phk) pv krid_form,cpd_form,s) :: r ] 
                else []
      ]
  | [ (Vokv,rev_krid_form,_) :: next ] ->
      let krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.voks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (autonomous_form_k krid_form) tags && not_sa_v next
                then out else []
      ]
  | [ (Vokc,rev_krid_form,_) :: _ ] ->
      let krid_form = Word.mirror rev_krid_form in
      match Deco.assoc krid_form morpho.voks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (autonomous_form_k krid_form) tags
                then out else []
      ]
  | [ (Lopak,rev_lopak_form,s) :: [ (ph,prev,sv) :: r ] ]  
        when preverb_phase ph ->
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and lopak_form = Word.mirror rev_lopak_form in
      let krid_form = match lopak_form with 
                      [ [ -2 :: rf ] -> rf | _ -> failwith "Wrong lopa form" ] in
      match Deco.assoc krid_form morpho.lopaks with
      [ [] -> failwith ("Unknown krid_form: " ^ Canon.decode krid_form)
      | tags -> if List.exists (validate_pv_k pv_str krid_form) tags then
                   let form = apply_sandhi prev krid_form sv in
                   let cpd_form = Word.mirror form in
                   [ (Comp (ph,Lopak) pv krid_form,cpd_form,s) :: r ]
                else []
      ] (*i TODO Lopak without preverb i*)
  | [ (Peri,rev_peri_form,s) :: [ (Pv,prev,sv) :: r ] ] -> 
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and peri_form = Word.mirror rev_peri_form in
      match Deco.assoc peri_form morpho.peris with
      [ [] -> failwith ("Unknown peri_form: " ^ Canon.decode peri_form)
      | tags -> let valid (delta,morphs) = 
                   let root = Word.patch delta peri_form in
                   attested pv_str root in 
                if List.exists valid tags then
                   let form = apply_sandhi prev peri_form sv in 
                   let cpd_form = Word.mirror form in
                   [ (Comp (Pv,Peri) pv peri_form,cpd_form,s) :: r ]
                else []
      ]
  | [ (Abso,rev_abso_form,s) :: [ (Pv,prev,sv) :: r ] ] ->
      (* Takes care of absolutives in -ya and of infinitives with preverbs *)
      let pv = Word.mirror prev in 
      let pv_str = Canon.decode pv 
      and abso_form = Word.mirror rev_abso_form in
      match Deco.assoc abso_form morpho.absya with
      [ [] -> failwith ("Unknown abs_form: " ^ Canon.decode abso_form)
      | tags -> let valid (delta,morphs) = 
                   let root = Word.patch delta abso_form in
                   attested pv_str root in 
                if List.exists valid tags then
                   let form = apply_sandhi prev abso_form sv in 
                   let cpd_form = Word.mirror form in
                   [ (Comp (Pv,Abso) pv abso_form,cpd_form,s) :: r ]
                else []
      ]
    (* We now prevent overgeneration of forms "sa" and "e.sa" \Pan{6,1,132} *)
    (*i TODO: similar test for dual forms i*)
  | [ (ph,form,_) :: [ (Pron,[ 1; 48 ],_) :: _ ] ]  (* sa *)
  | [ (ph,form,_) :: [ (Pron,[ 1; 47; 10 ],_) :: _ ] ] (* e.sa *) -> 
      if Phonetics.consonant_initial (Word.mirror form) 
      then out else [] 
(* Alternative: put infinitives in Root rather than Indecl+Abso 
 [| [ (Absc,_,_) :: _ ] 
  | [ (Absv,_,_) :: _ ] -> check root is autonomous 
  idem for infinitives, but they need their own phase/color *)
(* Finally we glue taddita suffix "forms" to the previous (iic) segment *)
(* NB This cumulates with the preverb glueing but not with itself *)
  | [ (sfxph,sfx,s) :: [ (ph,rstem,sv) :: r ] ] when sfx_phase sfxph 
                                                  && iic_phase ph ->
      let sfx_form = Word.mirror sfx in 
      let stem = Word.mirror rstem in
      let tad_form = Word.mirror (apply_sandhi rstem sfx_form sv) in
      [ (Tad (ph,sfxph) stem sfx_form,tad_form,s) :: r ]
  | [ (phase,_,_) :: [ (pv,_,_) :: _ ] ] when preverb_phase pv -> 
      let m = "validate: " ^ string_of_phase pv ^ " " ^ string_of_phase phase in 
      raise (Control.Anomaly m) (* all preverbs ought to have been processed *)
(* [ | [ (pv,_,_) :: _ ] when preverb_phase pv -> out ] noop
This pv is not terminal, and should be chopped off by the next item *) 
  | [ _ :: [ (_,w,_) :: _ ] ] when phantomatic (Word.mirror w) -> 
    raise (Control.Anomaly "Bug phantomatic segment")
  | _ -> out (* default identity *)
  ]
;

open Html;
value rec color_of_phase = fun
  [ Noun | Noun2 | Lopak | Nouc | Nouv | Kriv | Kric | Krid | Auxik | Kama
         | Cache -> Deep_sky 
  | Pron -> Light_blue
  | Root | Auxi | Lopa -> Carmin  
  | Inde | Abso | Absv | Absc | Ai | Ani -> Mauve
  | Iiy -> Lavender
  | Avy -> Magenta
  | Inftu -> Salmon 
  | Iic | Iic2 | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif 
        -> Yellow
  | Auxiick | Iivv | Iivc | Peri | Iiv -> Orange
  | Voca | Vocv | Vocc | Inv | Vok | Vokv | Vokc -> Lawngreen
  | Ifc | Ifc2 -> Cyan
  | Unknown -> Grey
  | Comp (_,ph) _ _ -> color_of_phase ph 
  | Tad (_,ph)  _ _ -> if ph=Sfx then Deep_sky else Yellow
  | Pv | Pvk | Pvkc | Pvkv -> failwith "Illegal preverb segment"
  | Sfx -> Deep_sky (* necessary for [Lexer.print_segment2] *)
  | Isfx -> Yellow  (* idem *)
(*i NB: unused background colors: Pink Green Aquamarine Chamois i*)
  ]
; 

end;
