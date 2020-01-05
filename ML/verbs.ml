(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Verbs defines the conjugation paradigms, and computes conjugated forms *)
(* Computed forms comprise finite verbal forms of roots, but also derived
   nominal forms (participles), infinitives and absolutives *)
(* Terminology. record functions will build the forms needed by Conjugation
and Stemming. After change of this file, and "make releasecgi", these tables
are updated. But the Reader/Parser needs a full pass of generation, with 
"make scratch" from Dictionary, in order to rebuild the full automata. *)

(*i module Verbs = struct i*)

open List; (* map, length, rev *)
open Phonetics; (* [vowel, homonasal, duhify, mrijify, nahify, light, nasal, 
                    gana, mult, aug, trunc_a, trunc_u, trunc_aa] *)
open Skt_morph;
open Inflected; (* [Conju, Invar, Inftu, roots, enter1, morpho_gen, admits_aa] *)
open Parts; (* [memo_part, record_part, cau_gana, fix, fix_augment, rfix,
                compute_participles] *)
(* This module also uses modules [List2 Word Control Canon Encode Int_sandhi] 
   and interface [Conj_infos] *)
open Pada; (* [voices_of_gana] *)

(* In the grinding phase, we record for each root entry its class and its stem
   for 3rd present. In the declination phase, we compute the inflected forms and
   we record them with a pair [(entry,conjugs)] in verbs.rem, parts.rem, etc. *)

exception Not_attested (* No attested form *)
;
(* Present system - we give [vmorph] info [Prim root_class pada third_conjug] 
   where [third_conjug] is a word, used for checking the 3rd sg Para *)
value present  = Present 
and imperfect  = Imperfect
and optative   = Optative
and imperative = Imperative
;
(* Paradigms *)
value vpa cl = Presenta cl Present
and vpm cl =   Presentm cl Present
and vpp =      Presentp Present
and via cl =   Presenta cl Imperfect
and vim cl =   Presentm cl Imperfect
and vip =      Presentp Imperfect
and voa cl =   Presenta cl Optative
and vom cl =   Presentm cl Optative
and vop =      Presentp Optative
and vma cl =   Presenta cl Imperative
and vmm cl =   Presentm cl Imperative
and vmp =      Presentp Imperative
and vfa =    Conjug Future Active
and vfm =    Conjug Future Middle
and vca =    Conjug Conditional Active
and vcm =    Conjug Conditional Middle
and vfp =    Conjug Future Passive
and vpfa =   Conjug Perfect Active
and vpfm =   Conjug Perfect Middle
and vpfp =   Conjug Perfect Passive
and vbena =  Conjug Benedictive Active
and vbenm =  Conjug Benedictive Middle
and vaa cl = Conjug (Aorist cl) Active
and vam cl = Conjug (Aorist cl) Middle
and vja cl = Conjug (Injunctive cl) Active
and vjm cl = Conjug (Injunctive cl) Middle
and vap1 =   Conjug (Aorist 1) Passive     (* passive of root aorist *)
and vjp1 =   Conjug (Injunctive 1) Passive (* passive of root injunctive *)
;
(* Finite verbal forms of roots *)
value fpresa cl conj = (conj,vpa cl)
and fpresm cl conj =   (conj,vpm cl)
and fpresp conj =      (conj,vpp)
and fimpfta cl conj =  (conj,via cl)
and fimpftm cl conj =  (conj,vim cl)
and fimpftp conj =     (conj,vip)
and fopta cl conj =    (conj,voa cl)
and foptm cl conj =    (conj,vom cl)
and foptp conj =       (conj,vop)
and fimpera cl conj =  (conj,vma cl)
and fimperm cl conj =  (conj,vmm cl)
and fimperp conj =     (conj,vmp) 
and ffutura conj =     (conj,vfa)
and ffuturm conj =     (conj,vfm)
and fconda conj =      (conj,vca)
and fcondm conj =      (conj,vcm)
and fperfa conj =      (conj,vpfa)
and fperfm conj =      (conj,vpfm)
and fbenea conj =      (conj,vbena)
and fbenem conj =      (conj,vbenm)
and faora cl conj =    (conj,vaa cl)
and faorm cl conj =    (conj,vam cl)
and finja cl conj =    (conj,vja cl)
and finjm cl conj =    (conj,vjm cl)
and faorp1 conj =      (conj,vap1)
and finjp1 conj =      (conj,vjp1)
;
(* Primary finite verbal forms of roots *)
value presa cl = fpresa cl Primary
and presm cl   = fpresm cl Primary
and impfta cl  = fimpfta cl Primary
and impftm cl  = fimpftm cl Primary
and opta cl    = fopta cl Primary
and optm cl    = foptm cl Primary
and impera cl  = fimpera cl Primary
and imperm cl  = fimperm cl Primary
and futura  = ffutura Primary
and futurm  = ffuturm Primary
and perfa   = fperfa Primary
and perfm   = fperfm Primary
and aora cl = faora cl Primary
and aorm cl = faorm cl Primary
and aorp1   = faorp1 Primary
and benea   = fbenea Primary
and benem   = fbenem Primary
and inja cl = finja cl Primary
and injm cl = finjm cl Primary
and injp1   = finjp1 Primary
;
(* Participial forms *)
value pra k = Ppra k 
and prm k = Pprm k 
and prp   = Pprp
and pfta  = Ppfta
and pftm  = Ppftm
and futa  = Pfuta
and futm  = Pfutm
(* Also in Part: Ppp, Pppa, Ger=Pfut Passive, Inf *)
;
(* Verbal forms of roots *)
value vppra k conj = (conj,pra k)
and vpprm k conj = (conj,prm k)
and vppfta conj = (conj,pfta)
and vppftm conj = (conj,pftm)
and vpfuta conj = (conj,futa)
and vpfutm conj = (conj,futm)
and vpprp  conj = (conj,prp)
(* Also in Part: Ppp, Pppa, Ger=Pfut Passive, Inf *)
;
(* Verbal forms of roots *)
value ppra k = vppra k Primary
and pprm k = vpprm k Primary
and ppfta  = vppfta Primary
and ppftm  = vppftm Primary
and pfuta  = vpfuta Primary
and pfutm  = vpfutm Primary
and pprp   = vpprp Primary
;
(* Derived verbal forms *)
value causa = fpresa cau_gana Causative
and pcausa  = vppra cau_gana Causative
and causm   = fpresm cau_gana Causative
and pcausm  = vpprm cau_gana Causative
and causp   = fpresp Causative
and causfa  = ffutura Causative
and pcausfa = vpfuta Causative
and causfm  = ffuturm Causative
and pcausfm = vpfutm Causative
and caaora cl = faora cl Causative
and caaorm cl = faorm cl Causative
and intensa = fpresa int_gana Intensive
and pinta   = vppra int_gana Intensive
and intensm = fpresm int_gana Intensive
and pintm   = vpprm int_gana Intensive
and desida  = fpresa des_gana Desiderative
and pdesa   = vppra des_gana Desiderative
and desidm  = fpresm des_gana Desiderative
and pdesm   = vpprm des_gana Desiderative
and despfa  = fperfa Desiderative
and despfm  = fperfm Desiderative
;
value intimpfta = fimpfta int_gana Intensive
and intopta     = fopta   int_gana Intensive
and intimpera   = fimpera int_gana Intensive
;
value  code = Encode.code_string (* normalized *)
and revcode = Encode.rev_code_string (* reversed *)
and revstem = Encode.rev_stem (* stripped of homo counter *)
;
(* Checking consistency of computed form with witness from lexicon.      *)
(* Discrepancies are noted on a warnings log, written on stderr.         *)
(* NB currently log dumped in (D)STAT/warnings.txt by "make roots.rem".  *)
value emit_warning s =
  if morpho_gen.val then output_string stderr (s ^ "\n") else ((* cgi *))
;
value report entry gana listed computed =
  let s1 = Canon.decode computed
  and s2 = Canon.decode listed in
  let message = entry ^ " [" ^ string_of_int gana ^ "] wrong 3rd pr "
                      ^ s1 ^ " for " ^ s2 in
  emit_warning message 
;
(* third is attested from Dico, form is generated by morphology *)
value check entry gana third ((_,form) as res) = do
  { if third=[] (* no checking *) || third=form then () 
    else match entry with 
         [ "a~nc" | "kalu.s" | "kram" | "grah" | "cam" | "tul" | "t.rr"
         | "manth" | "v.r#1" | "huu" | "putr" 
           (* these roots have multiple ga.nas, i.e. different entries in DP *)
             -> () (* 2 forms - avoids double warning *)
         | _ -> report entry gana third form
         ]
  ; res (* Note that the computed form has priority over the listed one. *)
        (* Log inspection leads to correction of either Dico or Verbs.   *)
  }
;
value warning message = 
  failwith (message ^ "\n")
and error_empty n = 
  failwith ("empty stem " ^ string_of_int n)
and error_suffix n = 
  failwith ("empty suffix " ^ string_of_int n)
and error_vowel n = 
  failwith ("no vowel in root " ^ string_of_int n)
;

(*****  Conjugation of verbal stems *****)

(* Suffixing uses [Int_sandhi.sandhi] (through Parts.fix) for thematic
   conjugation and conjugation of roots of ganas 5,7,8 and 9, and the following 
   sandhi function for athematic conjugation of roots of ganas 2 and 3 (through
   respectively fix2 and fix3w). *)

(* This sandhi restores initial aspiration if final one is lost -- Gonda§4 note.
   This concerns root syllables with initial g- d- b- and final -gh -dh -bh -h
   where aspiration is shifted forwards. The corresponding problem is dealt in 
   [Nouns.build_root] by [Phonetics.finalize], so there is some redundancy. 
   It is related to Grassmann's law and Bartholomae's law in IE linguistics. *)
value sandhi revstem wsuff = 
  let aspirate w = match w with
    [ [] -> w
    | [ c :: rest ] -> match c with (* uses arithmetic encoding for aspiration *)
       [ 19 | 34 | 39 (* g d b *) -> [ c+1 :: rest ] (* aspiration *)
       | _ -> w
       ]
    ] 
  and lost = match wsuff with 
    [ [] -> False
    | [ c :: _ ] -> match c with (* Gonda§4 note *)
      [ 48 (* s *) -> (* 32 | 33 | 35 | 49 (* t th dh h *) ? *) 
        match revstem with
        [ [ 20 :: _ ] | [ 35 :: _ ] | [ 40 :: _ ] | [ 49 :: _ ] 
          (* gh           dh            bh            h      *)
        | [ 149 :: _ ] | [ 249 :: _ ] 
          (* h'            h''     *) 
            -> True
        | _ -> False
        ]
      | _ -> False
      ] 
    ]
  and result = Int_sandhi.int_sandhi revstem wsuff in
  if lost then aspirate result else result
;

(* Theoretical general conjugational scheme : 
   Given the stem value, let conjug person suff = (person,fix stem suff) 
                             ([fix_augment] instead of [fix] for preterit) 
   We enter in the roots lexicon an entry:
  [ (Conju verbal 
     [ (Singular, 
        [ conjug First  suff_s1
        ; conjug Second suff_s2
        ; conjug Third  suff_s3
        ])
     ; (Dual,
        [ conjug First  suff_d1
        ; conjug Second suff_d2
        ; conjug Third  suff_d3
        ])
     ; (Plural,
        [ conjug First  suff_p1
        ; conjug Second suff_p2
        ; conjug Third  suff_p3
        ])
     ]) ]
  Remark. More general patterns such as above could have been used, in Paninian
  style, but at the price of complicating internal sandhi, for instance for
  dropping final a of the stem in [conjug First suff_s1] (Goldman§4.22).
  Here instead of st-a+e -> st-e we compute st-e with a shortened stem. 
  Similarly st-a+ete -> st-ete -> in Dual, see [compute_thematic_presentm] etc. 
*)

(* Returns the reverse of [int_sandhi] of reversed prefix and reversed stem *)
(* But [int_sandhi] may provoke too much retroflexion, such as *si.sarti 
   instead of sisarti for root s.r in redup3 below. 
   Same pb to avoid *pu.sphora as perfect of sphur, instead of pusphora. 
   Thus need of the boolean argument retr in the following: *)
value revaffix retr revpref rstem = 
  let glue = if retr then Int_sandhi.int_sandhi else List2.unstack in
  rev (glue revpref (rev rstem)) (*i too many revs - ugly i*)
;
(* Computation of verbal stems from root *)

value  final_guna v w = List2.unstack (guna v) w
and final_vriddhi v w = List2.unstack (vriddhi v) w 
;
(* Strong form of reversed stem *)
value strong = fun (* follows Phonetics.gunify *)
  [ [] -> error_empty 1
  | [ v :: rest ] when vowel v -> final_guna v rest 
  | [ c :: [ v :: rest ] ] when short_vowel v -> [ c :: final_guna v rest ]
  | s -> s
  ]
;
(* Lengthened form of reversed stem *)
value lengthened = fun
  [ [] -> error_empty 2
  | [ v :: rest ] when vowel v -> final_vriddhi v rest 
  | [ c :: [ v :: rest ] ] when short_vowel v -> [ c :: final_vriddhi v rest ]
  | s -> s
  ]
;
value strengthen_10 rstem = fun
  [ "m.r.d" | "sp.rh" -> rstem (* exceptions with weak stem *)
  | "k.sal" -> lengthened rstem (* v.rddhi *)     
  | _ -> strong rstem  (* guna *) 
  ] 
;
(* .r -> raa (Whitney§882a, Macdonell§144.4) *)
value long_metathesis = fun (* .r penultimate -> raa *)
  [ [ c :: [ 7 (* .r *) :: rest ] ] -> [ c :: [ 2 :: [ 43 :: rest ] ] ]
  | _ -> failwith "long_metathesis"
  ]
;
(* truncates an rstem eg bh.rjj -> bh.rj *)
value truncate = fun 
  [ [] -> error_empty 3
  | [ _ :: r ] -> r
  ]
;
value strong_stem entry rstem = (* rstem = revstem entry *)
  match entry with 
    [ "am" -> revcode "amii" (* amiiti *)
    | "dah#1" | "dih" | "duh#1" | "druh#1" | "muh" | "snih#1" | "snuh#1"
               -> duhify (strong rstem)
    | "nah"    -> nahify (strong rstem)
    | "m.rj"   -> mrijify (revcode "maarj") (* maar.s.ti [long_metathesis] *)
    | "yaj#1" | "vraj" | "raaj#1" | "bhraaj" | "s.rj#1" 
               -> mrijify (strong rstem)
    | "bh.rjj" -> mrijify (strong (truncate rstem)) (* bh.rsj Pan{8,2,29} *)
    | "nij"    -> revcode "ni~nj" (* nasalisation for gana 2 *) 
    | "zrath"  -> revcode "zranth"
    | "diiv#1" -> revcode "dev"
    | _ -> strong rstem
    ]
;
value weak_stem entry rstem = (* rstem = revstem entry *)
  match entry with 
    [ "dah#1" | "dih" | "duh#1" | "druh#1" | "muh" | "snih#1" | "snuh#1"
               -> duhify rstem
    | "nah"    -> nahify rstem
    | "m.rj" | "yaj#1" | "vraj" | "raaj#1" | "bhraaj" | "s.rj#1" 
               -> mrijify rstem
    | "bh.rjj" -> mrijify (truncate rstem)
    | "nij"    -> revcode "ni~nj" (* nasalisation *)
    | "vaz"    -> revcode "uz" (* but not vac ! *)
    | "zaas"   -> revcode "zi.s" 
    | "myak.s" -> revcode "mik.s" 
    | _ -> rstem
    ]
;
(* samprasaara.na correction - weak strong and long rev stem words of root.    *)
(* Concerns 4 roots, lexicalized under their strong rather than weak stem.     *)
(* Beware. The sampra correction must be effected separately when [weak_stem]
   and [strong_stem] are invoked directly, rather than as components of stems. *)
value stems root = 
  let rstem = revstem root in
  let sampra substitute = 
      let lstem = lengthened rstem in
      (revstem substitute,rstem,lstem) in
  match root with (* This shows what ought to be the root name, its weak form *)
     [ "grah"   -> sampra "g.rh" (* \Pan{6,1,15} *) 
     | "vyadh"  -> sampra "vidh" (* \Pan{6,1,15} *) 
     | "spardh" -> sampra "sp.rdh"
     | "svap"   -> sampra "sup" (* \Pan{6,1,15} *) 
     (*  note "vac", "yaj" etc not concerned although having samprasaara.na *)
     | _ -> let weak   = weak_stem root rstem 
            and strong = strong_stem root rstem in
            let long = lengthened weak in
            (weak,strong,long)
     ]
;
value drop_penultimate_nasal = fun
  [ [ c :: [ n :: s ] ] when nasal n -> [ c :: s ] 
  | _ -> failwith "No penultimate nasal"
  ]
;
value passive_stem entry rstem = (* Panini yak (k : no guna, samprasaara.na) *)
  let weak = match entry with 
  (* [weak] same as first component of [stems], except praz vac etc and bh.rjj *)
    [ "dah#1" | "dih" | "duh#1" | "druh#1" | "muh" | "snih#1" | "snuh#1"
              -> duhify rstem
    | "nah"   -> nahify rstem
    | "m.rj" | "vraj" | "raaj#1" | "bhraaj" | "s.rj#1" | "bh.rjj" 
              -> mrijify rstem
    | "yaj#1" -> mrijify (revcode "ij") (* samprasaara.na ya-x \R i-x \Pan{6,1,15} *)
    | "vyadh" -> revcode "vidh"  (* id *)
    | "grah"  -> revcode "g.rh"  (* samprasaara.na ra-x \R .r-x  \Pan{6,1,16} *) 
    | "vrazc" -> revcode "v.rzc" (* id *)
    | "praz"  -> revcode "p.rcch" (* id *)
    | "svap"  -> revcode "sup"   (* samprasaara.na va-x \R u-x \Pan{6,1,15} *) 
    | "vaz" | "vac" | "vap" | "vap#1" | "vap#2" | "vad" | "vas#1" | "vas#4" 
    | "vah#1" (* idem - specific code for va-x roots *)
              -> match rstem with 
                 [ [ 48 :: _ ] -> [ 47 ; 5 (* u *) ] (* vas  \R u.s *)
                 | [ c :: _ ] -> [ c ; 5 (* u *) ]   (* va-x \R u-x *)
                 | [] -> failwith "Anomalous passive_stem"
                 ]
    | "vaa#3" -> revcode "uu" (* \Pan{6,1,15} *) 
    | "zaas"  -> revcode "zi.s" (* ambiguous zi.s.ta, zi.syate *)
    | "zii#1" -> revcode "zay" (* \Pan{7,4,22} *) 
    | "pyaa"  -> revcode "pyaay" (* pyaa=pyai *)
    | "indh" | "und" | "umbh" | "gumph" | "granth" | "da.mz" | "dhva.ms"  
    | "bandh" | "bhra.mz" | "za.ms" | "zrambh" | "skambh" | "skand" 
      (* above roots have penultimate nasal and do not have [i_it] marker *)
    | "ba.mh" | "ma.mh" | "manth" | "stambh" 
      (* these four roots are listed in dhatupathas as bahi, mahi, mathi, stabhi
         and thus appear here even though they admit [i_it] marker *)
              -> drop_penultimate_nasal rstem
    | _ -> match rstem with 
         (* -a~nc -aa~nc va~nc a~nj sa~nj [drop_penultimate_nasal] *)
         (* doubt for pi~nj and gu~nj since they admit [i_it] marker *)
         [ [ 22 :: [ 26 :: r ] ] (* -~nc *) -> [ 22 :: r ] (* -ac *)
         | [ 24 :: [ 26 :: r ] ] (* -~nj *) -> [ 24 :: r ] (* -aj *)
         | w -> w
         ]
    ] in 
  match weak with
    [ [ c :: rst ] -> match c with
        [ 2 (* aa *) -> match rst with
            [ [ 42 (* y *) ] (* yaa1 *)
            | [ 42 (* y *); 18 (* kh *) ] (* khyaa *) 
            | [ 42 (* y *); 35 (* dh *) ] (* dhyaa *) -> weak
            | [ 42 (* y *) :: r ] -> [ 4 (* ii *) :: r ] (* ziiyate stiiyate *)
            | _ -> match entry with
               [ "j~naa#1" | "bhaa#1" | "mnaa" | "laa" | "zaa" | "haa#2" 
                   -> weak
               | _ -> [ 4 (* ii *) :: rst ]
               ]
            ]
        | 3 (* i *) -> [ 4 (* ii *) :: rst ]
        | 5 (* u *) -> match entry with
            [ "k.su" | "k.s.nu" | "plu" | "sru" -> weak
            | _ -> [ 6 (* uu *) :: rst ]
            ]
        | 7 (* .r *) -> match rst with
            [ [ _ ] -> [ 3 :: [ 43 :: rst ] ] (* ri- *)
            | _ (* 0 or 2 consonants *) -> [ 43 :: [ 1 :: rst ] ] (* ar- *)
            ]
        | 8 (* .rr *) -> match rst with
            [ [ d :: _ ] -> 
              if labial d then [ 43 :: [ 6 :: rst ] ] (* puuryate *)
                          else [ 43 :: [ 4 :: rst ] ] (* kiiryate ziiryate *)
            | _ -> error_empty 4
            ] 
        | _ -> if c>9 && c<14 (* e ai o au *) then match entry with
            [ "dhyai" -> [ 2 :: rst ] (* dhyaa in Dico *)
            | "hve" -> revcode "huu" (* huu in Dico, just for convenience *)
            | _ -> [ 4 (* ii *) :: rst ]
            ]
               else weak
        ]
    | [] -> error_empty 5 
    ] 
;
(* Reduplication for third class present: redup3 takes the root string  
   and its (reversed) stem word, and returns a triple [(s,w,b)] 
   where [s] is the (reversed) strong stem word, 
         [w] is the (reversed) weak stem word, 
         [b] is a boolean flag for special aa roots *)
value redup3 entry rstem = 
  match mirror rstem with 
    [ [] -> failwith "Empty root"
    | [ 7 (* .r *) ] -> (* Whitney§643d *) (revstem "iyar",revstem "iy.r",False)
    | [ c1 :: r ] -> if vowel c1 then failwith "Attempt reduplicating vowel root"
                     else 
      let v = lookvoy r
         where rec lookvoy = fun
           [ [] -> failwith "Attempt to reduplicate root with no vowel"
           | [ c2 :: r2 ] -> if vowel c2 then c2 else lookvoy r2
           ] 
      and iflag = match entry with (* special flag for some aa roots *)
           [ "gaa#1" | "ghraa" | "maa#1" | "zaa" | "haa#2" -> True
           | _ -> False 
           ] 
      and iflag2 = match entry with (* special flag for some other roots *)
           [ "maa#3" | "vac" | "vyac" -> True
           | _ -> False 
           ] in 
      let c = if sibilant c1 then match r with 
       (* c is reduplicating consonant candidate *)
                 [ [] -> failwith "Reduplicated root with no vowel"
                 | [ c2 :: _ ] -> if vowel c2 || nasal c2 then c1
                                  else if stop c2 then c2
                                  else (* semivowel c2 *) c1
                 ] 
              else c1 in
      let rv = (* rv is reduplicating vowel *)
        if entry="v.rt#1" then 1 (* a *) else
        if rivarna v || iflag || iflag2 then 3 (* i *)
        else if entry="nij" then 10 (* e *) (* Whitney says intensive! *)
        else short v (* reduplicated vowel is short *)
      and rc = match c with (* rc is reduplicating consonant *)
        [ 17 | 18 (* k kh *) -> 22 (* c *)
        | 19 | 20 | 49 (* g gh h *) -> 24 (* j *) 
        | 149 | 249 (* h' h2 *) -> failwith "Weird root of class 3"
        | 23 | 25 | 28 | 30 | 33 | 35 | 38 | 40 -> c-1 (* aspiration loss *)
        | _ -> c
        ] 
      and iiflag = iflag || entry = "haa#1" in 
      let (strong,weak) = 
           if iiflag then match rstem with
              [ [ 2 :: rest ] -> (rstem,[ 4 :: rest ]) (* aa \R ii *)
              | _ -> failwith "Anomaly Verbs"
              ]
           else let wstem = match entry with
                [ "daa#1" | "dhaa#1"  -> match rstem with
                   [ [ 2 :: rest ] -> rest (* drop final aa *)
                   | _ -> failwith "Anomaly Verbs"
                   ]
                | _ -> rstem 
                ] in 
      (strong rstem,wstem)
      and glue = match entry with
          [ "s.r" -> revaffix False [ rv; rc ] (* no retroflexion: sisarti *)
          | _ -> revaffix True [ rv; rc ] 
          ] in (glue strong,glue weak,iiflag) 
    ]
;

(* Dhatupatha it markers (from AK's listing) *)
(* NB Use of these markers should progressively replace lists of exceptions *)
value aa_it = fun
  [ (* "muurch" | WRONG ? *) 
    "phal" | "zvit" | "svid#2" | "tvar" | "dh.r.s" -> True
  | _ -> False
  ]
and i_it = fun (* unused but subset of set in intercalates *)
  [ "vand" | "bhand" | "mand#1" | "spand" | "indh" | "nind" 
  | "nand" | "cand" | "zafk" | "iifkh" | "lafg" | "afg" | "ifg" 
  | "gu~nj" | "laa~nch" | "vaa~nch" | "u~nch" | "ku.n.d" | "ma.n.d" | "ku.n.th" 
  | "lu.n.th" | "kamp" | "lamb" | "stambh" | "j.rmbh" | "cumb" | "inv" | "jinv"
  | "ba.mh" | "ma.mh" | "ghu.s" | "kaafk.s" | "ra.mh" | "tvar" 
  | "pi~nj" | "rud#1" | "hi.ms" | "chand" | "lafgh" -> True
(* NB. other roots admitting set:
[ "a~nc" | "an#2" | "arh" | "av" | "az#1" | "az#2" | "as#2" | "aas#2"
| "i.s#1" | "i.s#2" | "iik.s" | "ii.d" | "iiz#1" | "uc" | "umbh" | "uuh" 
| ".rc#1" | ".rj" | ".rdh" | "edh" | "kafk" | "kam" | "ka.s" |  "kup" | "krand"
| "krii.d" | "khan" | "khaad" | "gam" | "ghaat" | "ghuur.n" | "cit#1" 
| "jak.s" | "jap" | "jalp" | "tak" | "tan#1" | "tan#2" | "tark" | "dagh" 
| "dabh" | "dham" | "dhva.ms" | "dhvan" | "pa.th" | "pat#1" | "piz" 
| "bhaa.s" | "bhraaj" | "mad#1" | "mlecch" | "yat#1" | "yaac" | "rak.s" 
| "raaj#1" | "ruc#1" | "lag" | "lap" | "la.s" | "lok" | "loc" | "vad" 
| "vam" | "vaz" | "vaaz" | "vip" | "ven" | "vyath" | "vraj" | "vrii.d"
| "za.ms" | "zas" | "zaas" | "zuc#1" | "san#1" | "skhal" | "spardh" | "sp.rh" 
| "sphu.t" | "svan" | "has" ] *) 
  | _ -> False
  ]
and ii_it = fun
  [ "hlaad" | "yat#1" | "cit#1" | "vas#4" | "jabh#1" | "kan" | "puuy" | "sphaa"
  | "pyaa" | "jan" | "n.rt" | "tras" | "diip" | "mad#1" | ".r.s" | "ju.s#1" 
  | "vij" | "d.rbh" | "gur" | "k.rt#1" | "indh" | "und" | "v.rj" | "p.rc" 
      -> True
  | _ -> False
  ]
and u_it = fun
  [ "sidh#2" | "a~nc" | "va~nc" | "zrambh" | "stubh" | "kam" | "cam" | "jam"
  | "kram" | ".s.thiiv" | "dhaav#1" | "gras" | "mi.s" | "p.r.s" | "v.r.s" 
  | "gh.r.s" | "zas" | "za.ms" | "sra.ms" | "dhva.ms" | "v.rt" | "v.rdh#1" 
  | "bhram" | "ram" | "m.rdh" | "khan" | "zaas" | "diiv#1" | "siiv" | "sidh#1"
  | "zam#1" | "tam" | "dam#1" | "zram" | "as#2" | "yas" | "jas" | "das" 
  | "bhra.mz" | ".rdh" | "g.rdh" | "dambh" | "i.s#1" | "t.rd" | "tan#1"
  | "k.san" -> True
  | _ -> False
  ]
and uu_it = fun (* perstems \Pan{7,2,44} *)
  [ "trap" | "k.sam" | "gaah" | "ak.s" | "tak.s" | "tvak.s" | "syand" | "k.rp" 
  | "guh" | "m.rj" | "klid" | "az#1" | "vrazc" | "b.rh#2" | "v.rh" | "a~nj"
  | "kli.s" | "ta~nc" -> True 
  | _ -> False
  ]
and o_it = fun (* these roots have ppp in -na \Pan{8,2,45} - unused here *)
  [ "zuu" | "haa#1" | "haa#2" | "vij" | "vrazc" | "bhuj#1" | "bha~nj" | "lag" 
 (* | "iir" | "und" | "k.rr" | "klid" | "k.sii" | "k.sud" | "k.svid" | "khid"
    | "g.rr#1" | "glai" | "chad#1" | "chid#1" | "ch.rd" | "j.rr" | ".dii"
    | "tud#1" | "t.rd" | "t.rr" | "dagh" | "d.rr" | "dev" | "draa#1" | "draa#2"
    | "nud" | "pad#1" | "pii" | "p.rr" | "pyaa" | "bhid#1" | "majj" | "man"
    | "mid" | "mlaa" | "ri" | "lii" | "luu#1" | "vid#2" | "vlii" | "zad" | "z.rr"
    | "sad#1" | "skand" | "st.rr" | "styaa" | "syand" | "svid#2" | "had" *)
 (* also "suu#2" suuna and "vrii" vrii.na and "k.saa" k.saa.na *)
      -> True 
  | _ -> False
  ]
;
(******************)
(* Present system *)
(******************)

(* In all such functions, [(stem : word)] is the code of the reversed stem. *)
(* Exemple pour cyu: stem=strong=guna=cyo et cyo+ati=cyavati par [int_sandhi] *)
value compute_thematic_presenta cl conj stem entry third = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 entry (Conju (fpresa cl conj)
   [ (Singular, 
        [ conjug First  "aami"
        ; conjug Second "asi"
        ; check entry cl third (conjug Third "ati") 
        ])
   ; (Dual,
        [ conjug First  "aavas"
        ; conjug Second "athas"
        ; conjug Third  "atas"
        ])
   ; (Plural,
        [ conjug First  "aamas"
        ; conjug Second "atha"
        ; conjug Third  "anti"
        ])
   ])
  ; let m_stem = match entry with (* Whitney§450 *)
        [ "b.rh#1" -> revcode "b.rh" (* not b.r.mh *)
        | _ -> stem 
        ] in
    let f_stem = match entry with (* Whitney§450f *)
        [ "j.rr" | "p.r.s" | "b.rh#1" (* | "mah" *) | "v.rh" -> rfix m_stem "at" 
        | _ -> rfix m_stem "ant" 
        ] in 
    if cl=4 && entry="daa#2" || entry="mah" then () (* to avoid dyat mahat *)
    else record_part (Ppra_ cl conj m_stem f_stem entry)
  }
;
value compute_thematic_presentm cl conj stem entry third = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (fpresm cl conj)
   [ (Singular, 
        [ conjug First  "e"
        ; conjug Second "ase"
        ; check entry cl third (conjug Third "ate")
        ])
   ; (Dual,
        [ conjug First  "aavahe"
        ; conjug Second "ethe"
        ; conjug Third  "ete"
        ])
   ; (Plural,
        [ conjug First  "aamahe"
        ; conjug Second "adhve"
        ; conjug Third  "ante"
        ])
   ])
;
value thematic_preterit_a conjug = 
   [ (Singular, 
        [ conjug First  "am"
        ; conjug Second "as"
        ; conjug Third  "at"
        ])
   ; (Dual,
        [ conjug First  "aava"
        ; conjug Second "atam"
        ; conjug Third  "ataam"
        ])
   ; (Plural, 
        [ conjug First  "aama"
        ; conjug Second "ata"
        ; conjug Third  "an"
        ])
   ]
;
value compute_thematic_impfta cl conj stem entry =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (fimpfta cl conj) (thematic_preterit_a conjug))
;
value thematic_preterit_m conjug = 
   [ (Singular, 
        [ conjug First  "e"
        ; conjug Second "athaas"
        ; conjug Third  "ata"
        ])
   ; (Dual,
        [ conjug First  "aavahi"
        ; conjug Second "ethaam"
        ; conjug Third  "etaam"
        ])
   ; (Plural,
        [ conjug First  "aamahi"
        ; conjug Second "adhvam"
        ; conjug Third  "anta"
        ])
   ]
;
value compute_thematic_impftm cl conj stem entry =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (fimpftm cl conj) (thematic_preterit_m conjug))
;
value compute_thematic_optativea cl conj stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (fopta cl conj)
   [ (Singular, 
        [ conjug First  "eyam"
        ; conjug Second "es"
        ; conjug Third  "et"
        ])
   ; (Dual,
        [ conjug First  "eva"
        ; conjug Second "etam"
        ; conjug Third  "etaam"
        ])
   ; (Plural,
        [ conjug First  "ema"
        ; conjug Second "eta"
        ; conjug Third  "eyur"
        ])
   ])
;
value compute_thematic_optativem cl conj stem entry =
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (foptm cl conj)
   [ (Singular, 
        [ conjug First  "eya"
        ; conjug Second "ethaas"
        ; conjug Third  "eta"
        ])
   ; (Dual,
        [ conjug First  "evahi"
        ; conjug Second "eyaathaam"
        ; conjug Third  "eyaataam"
        ])
   ; (Plural,
        [ conjug First  "emahi"
        ; conjug Second "edhvam"
        ; conjug Third  "eran"
        ])
   ])
;
value compute_thematic_imperativea cl conj stem entry =
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (fimpera cl conj)
   [ (Singular, 
        [ conjug First  "aani"
        ; conjug Second "a"
        ; conjug Third  "atu"
        ])
   ; (Dual,
        [ conjug First  "aava"
        ; conjug Second "atam"
        ; conjug Third  "ataam"
        ])
   ; (Plural,
        [ conjug First  "aama"
        ; conjug Second "ata"
        ; conjug Third  "antu"
        ])
   ])
;
value compute_thematic_imperativem cl conj stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (fimperm cl conj)
   [ (Singular, 
        [ conjug First  "ai"
        ; conjug Second "asva"
        ; conjug Third  "ataam"
        ])
   ; (Dual,
        [ conjug First  "aavahai"
        ; conjug Second "ethaam"
        ; conjug Third  "etaam"
        ])
   ; (Plural,
        [ conjug First  "aamahai"
        ; conjug Second "adhvam"
        ; conjug Third  "antaam"
        ])
   ])
; 
value record_part_m (conj,part_kind) stem entry = match part_kind with 
  [ Pprm k -> record_part (Pprm_ k conj stem entry)
  | Pprp   -> record_part (Pprp_ conj stem entry)
  | Ppfta  -> record_part (Ppfta_ conj stem entry)
  | Ppftm  -> record_part (Ppftm_ conj stem entry)
  | Pfutm  -> record_part (Pfutm_ conj stem entry)
  | _ -> failwith "Unexpected participle"
  ]
;
value record_part_m_th verbal stem entry =  
  match entry with
  [ "cint" -> let pprm = Pprm_ 10 Primary (revcode "cintayaan") entry in
              record_part pprm (* irregular *)
  | "muc#1" | "sp.rz#1" -> 
         let mid_stem = rfix stem "aana" in (* Whitney§752 *)
         record_part_m verbal mid_stem entry 
  | _ -> let mid_stem = trunc_a (rfix stem "amaana") (* -maana *) in
         (* [trunc_a] needed because possible retroflexion in amaa.na *)
         record_part_m verbal mid_stem entry 
  ]
and record_part_m_ath verbal stem entry =  
  let suff = if entry = "aas#2" then "iina" (* McDonell§158a *)
             else "aana" (* -aana *) in
  let mid_stem = match rfix stem suff  with
                 [ [ 1 :: r ] -> r | _ -> failwith "Anomaly Verbs" ] in
  (* rare (Whitney). Creates bizarre forms such as plu -> puplvaana *)
  record_part_m verbal mid_stem entry 
;
(* Thematic present system - gana is root's present class *)
value compute_thematic_active gana conj stem entry third = do 
  { compute_thematic_presenta gana conj stem entry third
  ; compute_thematic_impfta gana conj stem entry 
  ; compute_thematic_optativea gana conj stem entry 
  ; compute_thematic_imperativea gana conj stem entry 
  } 
and compute_thematic_middle gana conj stem entry third = do 
  { compute_thematic_presentm gana conj stem entry third
  ; compute_thematic_impftm gana conj stem entry 
  ; compute_thematic_optativem gana conj stem entry 
  ; compute_thematic_imperativem gana conj stem entry 
  ; record_part_m_th (vpprm gana conj) stem entry
  }
;
value compute_causativea  = compute_thematic_active cau_gana Causative
and compute_causativem    = compute_thematic_middle cau_gana Causative
and compute_desiderativea = compute_thematic_active des_gana Desiderative
and compute_desiderativem = compute_thematic_middle des_gana Desiderative
;

(*** Gana 2 (root conjugation) ***)

(* [fix2: Word.word -> string -> string -> Word.word] *)
(* set indicates connecting vowel string of se.t root *)
value fix2 stem suff set = 
  let codesf = code suff in 
  let wsfx = match codesf with 
      [ [] -> error_suffix 1
      | [ c :: _ ] -> if vowel c || c=42 (* y *) then codesf
                      else if set then [ 3 :: codesf ] (* pad with initial i *)
                      else codesf
      ] in 
  sandhi stem wsfx
;
(* correction for i, ii, u, uu roots of gana 2 *)
value correct2 weak = match weak with
    [ [ 3 ] (* i *)           -> weak (* eg ppr yat \Pan{6,4,81} *)
    | [ 3 (* i *) ::  rest ]  -> [ 42 :: weak ]
    | [ 4; 46 ] (* zii *)     -> [ 42; 1; 46 ] (* zay *)  
    | [ 4 (* ii *) ::  rest ] -> [ 42 :: [ 3 :: rest ] ] (* iy *)
    | [ 5 (* u *) ::  rest ]  -> [ 45 :: weak ]
    | [ 6 (* uu *) ::  rest ] -> [ 45 :: [ 5 :: rest ] ] (* uv *)
    | _                       -> weak 
    ] 
;
value fix2w weak suff set =
  let weakv = correct2 weak 
  and weakc = match weak with
    [ [ 4; 46 ] (* zii *) -> [ 10; 46 ] (* ze *)
    | _ -> weak 
    ] in 
  match code suff with 
    [ [ c :: _ ] -> fix2 (if vowel c then weakv else weakc) suff set
    | [] -> error_suffix 7
    ]
;
value fix2w_augment weak suff set = aug (fix2w weak suff set)
;
value fix2wi suff = (* special for root i middle *)
  match code suff with (* \Pan{6,4,77} *)
    [ [ c :: _ ] -> fix2 (if vowel c then [ 42; 3 ] else [ 3 ]) suff False
    | [] -> error_suffix 15
    ]
;
value fix2whan suff = 
  let codesf = code suff in
  let stem = match codesf with 
     [ [] -> error_suffix 2
     | [ c :: _ ] -> if vowel c then "ghn"
                     else if c=41 || c=42 || c=45  (* m y v *) then "han"
                     else "ha"
     ] in 
  sandhi (revcode stem) codesf
;
value fix2whan_augment suff =
  let codesf = code suff in
  let stem = match codesf with 
     [ [] -> error_suffix 3
     | [ c :: _ ] -> if vowel c then "aghn"
                     else if c=41 || c=42 || c=45  (* m y v *) then "ahan"
                     else "aha"
     ] in 
  sandhi (revcode stem) codesf
;
(* correction for u roots *)
value fix2s strong suff set = match strong with
  [ [ 12 (* o *) ::  rest ] -> match code suff with
      [ [ c :: _ ] -> if vowel c then fix2 strong suff set
                      else fix2 [ 13 (* au *) :: rest ] suff set
      | [] -> error_suffix 4
      ]
  | _ -> fix2 strong suff set
  ]
;
value fix2s_augment strong suff set = aug (fix2s strong suff set)
;
value fix2sbruu suff = 
  let strong = revcode "bro" in
  match code suff with
      [ [ c :: _ ] -> let suff' = if vowel c then suff else "ii" ^ suff in
                      fix2 strong suff' False
      | [] -> error_suffix 5
      ]
;
value fix2sbruu_augment suff = aug (fix2sbruu suff)
;
(* \Pan{6,1,6} reduplicated roots dropping the n of 3rd pl -anti *)
value abhyasta = fun 
  [ "jak.s" | "jaag.r" | "cakaas" -> True (* zaas has special treatment *)
  | _ -> False
  ]
;
value compute_athematic_present2a strong weak set entry third = 
  let conjugs person suff =
      (person,if entry = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
              else fix2w weak suff set) in do
  { enter1 entry (Conju (presa 2)
   [ (Singular, let l =
        [ conjugs First "mi"
        ; if entry = "as#1" then (Second, code "asi")
          else conjugs Second "si"
        ; check entry 2 third (conjugs Third "ti") 
        ] in if entry ="bruu" then [ conjugw First "mi" :: l ]
             else if entry ="stu" then [ (First, code "staviimi") :: l ]
             else l (* bruumi Whitney§632 staviimi Whitney§633 *))
   ; (Dual,
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ])
   ; (Plural, let l =
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; if entry = "zaas" then conjugs Third "ati" (* \Pan{7,1,4} *)
          else conjugw Third (if abhyasta entry then "ati" else "anti")
        ] in if entry = "m.rj" then [ conjugs Third "anti" :: l ]
             else l (* Whitney§627 *))
   ])
  }
;
value compute_athematic_present2m strong weak set entry third = 
  let conjugs person suff = 
      (person,if entry = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
              else if entry = "i" then fix2wi suff 
              else fix2w weak suff set) in
  enter1 entry (Conju (presm 2)
   [ (Singular, let l = 
        [ if entry = "as#1" then (First, code "he") else
          conjugw First "e" 
        ; conjugw Second "se"
        ; check entry 2 third (conjugw Third "te") 
        ] in if entry = "m.rj" then [ conjugs First "e" :: l ]
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ] in if entry = "m.rj" then 
                [ conjugs Second "aathe"
                ; conjugs Third  "aate"
                ] @ l
             else l (* Whitney§627 *))  
   ; (Plural, let l =
        [ conjugw First  "mahe" 
        ; if entry = "as#1" then (Second, code "dhve") else
          if entry = "aas#2" then (Second, code "aadhve") else (* -Whitney§612 *)
          conjugw Second "dhve" 
        ; if entry = "zii#1" then conjugw Third "rate" (* \Pan{7,1,6} *)
          else conjugw Third "ate" 
        ] in if entry = "m.rj" then [ conjugs Third "ate" :: l ]
             else l (* Whitney§627 *)) 
   ])
;
value compute_athematic_impft2a strong weak set entry = 
  let conjugs person suff = 
      (person,if entry = "bruu" then fix2sbruu_augment suff 
              else fix2s_augment strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan_augment suff 
              else fix2w_augment weak suff set) in
  enter1 entry (Conju (impfta 2)
   [ (Singular, let l = 
        [ conjugs First "am"
        ; if set then conjugs Second "as"
          else if entry = "as#1" then conjugs Second "iis" (* Whitney§621c *)
          else if entry = "ad#1" then conjugs Second "as"  (* Whitney§621c *)
                    else conjugs Second "s" 
        ; if set then conjugs Third "at"
          else if entry = "as#1" then conjugs Third "iit"     (* idem aasiit *)
               else if entry = "ad#1" then conjugs Third "at" (* idem aadat *)
                    else conjugs Third "t"
        ] in if set then 
        [ conjugs Second "iis"
        ; conjugs Third  "iit" 
        ] @ l else if entry = "bruu" 
                   then [ (First, code "abruvam") (* Whitney§632 *) :: l ]
                   else l)
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural, let l = 
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; if entry = "i" then conjugs Third "an" (* aayan *)
          else match entry with (* Kane§429 *)
               [ "cakaas" | "jak.s" | "jaag.r" 
            (* | "daridraa" - should concern "draa#1" TODO *)
               | "zaas" -> conjugw Third "us" 
               | _ -> conjugw Third "an" 
               ]
        ] in if entry = "m.rj" 
                  then [ conjugs Third "an" :: l ] (* Whitney§627 *)
             else if entry = "bruu" 
                  then [ (Third, code "abruuvan") :: l ] (* Whitney§632 *)
             else match weak with (* Kale§420 optional -us for roots in -aa *)
                  [ [ 2 :: s ] -> [ (Third, aug (sandhi s (code "us"))) :: l ] 
                  | _ ->  l
                  ]) 
   ])
;
value compute_athematic_impft2m strong weak set entry = 
  let conjugs person suff = 
      (person,if entry = "bruu" then fix2sbruu_augment suff 
              else fix2s_augment strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan_augment suff 
              else fix2w_augment weak suff set) in
  enter1 entry (Conju (impftm 2)
   [ (Singular, let l =
        [ if entry = "i" then conjugw First "yi" (* adhyaiyi Bucknell 128 *)
          else conjugw First "i"
        ; conjugw Second "thaas"
        ; conjugw Third  "ta"
        ] in if entry = "m.rj" then [ conjugs First "i" :: l ]
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "vahi"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ] in if entry = "m.rj" then 
                [ conjugs Second "aathaam"
                ; conjugs Third  "aataam"
                ] @ l else l (* Whitney§627 *))
    ; (Plural, let l =
        [ conjugw First  "mahi"
        ; if entry = "aas#2" then (Second, code "aadhvam") (* -Whitney§620 *) 
          else conjugw Second "dhvam"
        ; if entry = "zii#1" then conjugw Third "rata" (* \Pan{7,1,6} *) else
          if entry = "i" then conjugw Third "yata" (* Bucknell 128 *) else
          conjugw Third "ata"
        ] in if entry = "m.rj" then [ conjugs Third "ata" :: l ] else
             if entry ="duh#1" then [ conjugw Third "ra" :: l ]
             (* aduhata -> aduha-a = \Pan{7,1,41} aduha -> aduhra \Pan{7,1,8} *)
             else l (* Whitney§627 *))
   ]) 
;
value compute_athematic_optative2a weak set entry =
  let conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
                                 else fix2w weak suff set) in
  enter1 entry (Conju (opta 2)
   [ (Singular, let l =
        [ conjugw First  "yaam"
        ; conjugw Second "yaas"
        ; conjugw Third  "yaat"
        ] in if entry = "bruu" 
             then [ (Third, code "bruyaat") (* Whitney§632 *) :: l ]
             else l)
   ; (Dual,
        [ conjugw First  "yaava"
        ; conjugw Second "yaatam"
        ; conjugw Third  "yaataam"
        ])
   ; (Plural,
        [ conjugw First  "yaama"
        ; conjugw Second "yaata"
        ; conjugw Third  "yur"
        ])
   ])
;
value compute_athematic_optative2m weak set entry =
  let conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
                                 else fix2w weak suff set)
  and conjugwmrij person suff = (person, fix2 (revcode "maarj") suff set) in
  enter1 entry (Conju (optm 2)
   [ (Singular, let l =
        [ conjugw First  "iiya"
        ; conjugw Second "iithaas"
        ; conjugw Third  "iita"
        ] in if entry = "m.rj" then 
                [ conjugwmrij First  "iiya"
                ; conjugwmrij Second "iithaas"
                ; conjugwmrij Third  "iita"
                ] @ l 
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "iivahi"
        ; conjugw Second "iiyaathaam"
        ; conjugw Third  "iiyaataam"
        ] in if entry = "m.rj" then 
                [ conjugwmrij First  "iivahi"
                ; conjugwmrij Second "iiyaathaam"
                ; conjugwmrij Third  "iiyaataam"
                ] @ l 
             else l (* Whitney§627 *))
   ; (Plural, let l =
        [ conjugw First  "iimahi"
        ; conjugw Second "iidhvam"
        ; conjugw Third  "iiran" (* TODO: Kane§429 like impft2 above *)
        ] in if entry = "m.rj" then 
                [ conjugwmrij First  "iimahi"
                ; conjugwmrij Second "iidhvam"
                ; conjugwmrij Third  "iiran"
                ] @ l 
             else l (* Whitney§627 *))
   ])
;
value compute_athematic_imperative2a strong weak set entry =
  let conjugs person suff = 
      (person,if entry = "bruu" then fix2sbruu suff 
                                else fix2s strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
                                 else fix2w weak suff set) in
  enter1 entry (Conju (impera 2)
   [ (Singular, let l =
        [ conjugs First "aani"
        ; (Second, match entry with
          [ "as#1" -> code "edhi"
          | "zaas" -> code "zaadhi" 
 (* above leads to conflict between \Pan{6,4,35} (zaa+hi) and \Pan{6,4,101} 
    (zaas+dhi) [asiddhavat] => we operate in parallel zaa+dhi= zaadhi *)
          | "cakaas" -> code "cakaadhi" (* Kane§429 *)  
          | _ -> let w = if entry = "han#1" then revcode "ja" else weak in
                 match w with 
            [ [ c :: _  ] -> fix2 w suff set
              where suff = if vowel c || set then "hi" else "dhi"
            | _ -> error_empty 6
            ] (* "dhi" or "hi" after vowel *)
          ])
        ; conjugs Third "tu"
        ] in if entry = "vac" then 
                [ (Second, code "voci"); (Third, code "vocatu") ] @ l
             else if entry ="bruu" then [ conjugs Second "hi" :: l ]
                  (* braviihi Whitney§632 *)
             else if entry ="cakaas" then [ (Second, code "cakaadvi") :: l ]
                  (* Kane§429 *)  
             else l)
   ; (Dual,
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural, let l =
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; if entry = "zaas" then conjugs Third "atu" (* \Pan{7,1,4} *)
        else conjugw Third (if abhyasta entry then "atu" else "antu") 
        ] in if entry = "m.rj" then [ conjugs Third "antu" :: l ]
             else l (* Whitney§627 *))
   ])
;
value compute_athematic_imperative2m strong weak set entry =
  let conjugs person suff = 
      (person,if entry = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if entry = "han#1" then fix2whan suff 
              else fix2w weak suff set) in
  enter1 entry (Conju (imperm 2)
   [ (Singular, 
        [ conjugs First  "ai"
        ; conjugw Second "sva"
        ; conjugw Third  "taam"
        ])
   ; (Dual, let l =
        [ conjugs First  "aavahai"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ] in if entry = "m.rj" then 
                [ conjugs Second "aathaam"
                ; conjugs Third  "aataam"
                ] @ l
             else l (* Whitney§627 *))
   ; (Plural, let l =
        [ conjugs First  "aamahai"
        ; if entry = "aas#2" then (Second, code "aadhvam") (* -Whitney§617 *) 
          else conjugw Second "dhvam"
        ; if entry = "zii#1" then conjugw Third "rataam" (* \Pan{7,1,6} *)
          else conjugw Third "ataam"
        ] in if entry = "m.rj" then [ conjugs Third "ataam" :: l ]
             else l (* Whitney§627 *))
   ])
;
value compute_active_present2 sstem wstem set entry third = do
  { compute_athematic_present2a sstem wstem set entry third
  ; let weak = if entry = "as#1" then [ 48; 1 ] else wstem in
    compute_athematic_impft2a sstem weak set entry 
  ; compute_athematic_optative2a wstem set entry 
  ; compute_athematic_imperative2a sstem wstem set entry 
  ; match wstem with 
    [ [ 2 :: _ ] -> (* Ppr of roots in -aa is complex and overgenerates *)
      match entry with 
      [ "bhaa#1" | "maa#1" | "yaa#1" -> () (* no known ppra *)
      | _ -> let m_pstem = wstem and f_pstem = rev (fix2w wstem "at" set) in 
             record_part (Ppra_ 2 Primary m_pstem f_pstem entry) 
      ]
    | _ -> let m_pstem = if entry = "han#1" then revstem "ghn" 
                         else correct2 wstem in
           let f_pstem = if entry = "han#1" then revstem "ghnat" 
                         else rev (fix2w wstem "at" set) in 
           record_part (Ppra_ 2 Primary m_pstem f_pstem entry)
    ]
  ; if entry = "m.rj" then let m_pstem = revstem "maarj" in
                           let f_pstem = revstem "maarjat" in
                           record_part (Ppra_ 2 Primary m_pstem f_pstem entry)
    else ()
  }
and compute_middle_present2 sstem wstem set entry third = do
  { compute_athematic_present2m sstem wstem set entry third
  ; compute_athematic_impft2m sstem wstem set entry 
  ; compute_athematic_optative2m wstem set entry 
  ; compute_athematic_imperative2m sstem wstem set entry 
  ; match entry with
    [ "maa#1" -> () (* no pprm *)
    | "i" -> record_part_m_ath (pprm 2) [ 42; 3 ] entry (* iyaana *)
    | _ -> record_part_m_ath (pprm 2) (correct2 wstem) entry
    ]
  }
;

(*** Gana 3  ***)

value strip_ii = fun 
  [ [ 4 :: w ] -> w (* ii disappears before vowels in special roots *)
  | _ -> failwith "Wrong weak stem of special 3rd class root"
  ] 
;
value fix3w wstem iiflag dadh suff = 
  let codesf = code suff in 
  let short = if iiflag then strip_ii wstem else wstem in
  let stem = match codesf with 
     [ [] -> error_suffix 8
     | [ 5; 43 ] (* ur *) -> if iiflag then short else strong wstem (* guna *)
     | [ c :: _ ] -> if dadh then match c with (* Gonda§66 *)
            [ 32 | 33 | 35 | 48 | 49 (* t th dh s h *) -> revstem "dhad" 
               (* aspirate correction of sandhi not enough : dh+t=ddh not tt *)
            | _ -> short
            ]        else if vowel c then short else wstem
     ] in
  sandhi stem codesf
;
value fix3w_augment wstem iiflag dadh suff = aug (fix3w wstem iiflag dadh suff)
;
value compute_athematic_present3a strong weak iiflag entry third = 
  let dadh_flag = (entry="dhaa#1") in 
  let conjugs person suff = (person,fix strong suff) 
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix (revstem "jahi") suff) 
                              (* weak = jahii but optionally jahi *)
  and haa_flag = (entry="haa#1") in do
  { enter1 entry (Conju (presa 3)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check entry 3 third (conjugs Third "ti") 
        ])
   ; (Dual, let l =
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas" 
        ] in if haa_flag then l @
                [ conjughaa First  "vas"
                ; conjughaa Second "thas"
                ; conjughaa Third  "tas"
                ]
             else l)
   ; (Plural, let l =
        [ conjugw First  "mas"
        ; conjugw Second "tha" 
        ; if entry="bhas" then (Third, code "bapsati") (* Whitney§678 MW§340 *) 
          else conjugw Third  "ati" 
        ] in if haa_flag then l @ 
                [ conjughaa First  "mas"
                ; conjughaa Second "tha"
                ]    
             else l)
   ])
  ; let wstem = if iiflag then strip_ii weak else 
                if entry="bhas" then revcode "baps" (* Whitney§678 *) 
                else weak in (* 3rd pl weak stem *)
    record_part (Pprared_ Primary wstem entry) 
  }
;
value compute_athematic_present3m conj gana weak iiflag entry third = 
  let dadh_flag = (entry="dhaa#1") in
  let conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  enter1 entry (Conju (fpresm gana conj)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check entry 3 third (conjugw Third "te") 
        ])
   ; (Dual, 
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ])
   ; (Plural,
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ate"
        ])
   ])
;
value compute_athematic_impft3a strong weak iiflag entry = 
  let dadh_flag = (entry="dhaa#1") in
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix3w_augment weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix_augment (revstem "jahi") suff) 
  and haa_flag = (entry="haa#1") in 
  enter1 entry (Conju (impfta 3)
   [ (Singular, let l = 
        [ conjugs First  "am"
        ; conjugs Second "s" 
        ; conjugs Third  "t"
        ] in if haa_flag then l @
                [ conjughaa Second "s"
                ; conjughaa Third "t"
                ]
             else l)
   ; (Dual, let l = 
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ] in if haa_flag then l @
                [ conjughaa First  "va"
                ; conjughaa Second "tam"
                ; conjughaa Third  "taam"
                ]    
             else l)
   ; (Plural, let l = 
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; conjugw Third  "ur"
        ] in if haa_flag then l @
                [ conjughaa First  "ma"
                ; conjughaa Second "ta"
                ]
             else l)
   ])
;
(* common to [impft_m]  and [root_aoristm] *)
value conjugs_past_m conjug =
   [ (Singular, 
        [ conjug First  "i"
        ; conjug Second "thaas"
        ; conjug Third  "ta"
        ])
   ; (Dual,
        [ conjug First  "vahi"
        ; conjug Second "aathaam"
        ; conjug Third  "aataam"
        ])
   ; (Plural, 
        [ conjug First  "mahi"
        ; conjug Second "dhvam"
        ; conjug Third  "ata"
        ])
   ]
;
value conjug_impft_m gana conjugw = (* used by classes 3 and 9 *)
  Conju (impftm gana) (conjugs_past_m conjugw)
;
value compute_athematic_impft3m weak iiflag entry = 
  let dadh_flag = (entry="dhaa#1") in
  let conjugw person suff = (person,fix3w_augment weak iiflag dadh_flag suff) in
  enter1 entry (conjug_impft_m 3 conjugw)
;
(* Like [compute_athematic_optative2a] except for [yan#1] et [bruu] *)
value conjug_optativea gana conj conjugw =
  Conju (fopta gana conj)
   [ (Singular, 
        [ conjugw First  "yaam"
        ; conjugw Second "yaas"
        ; conjugw Third  "yaat"
        ])
   ; (Dual,
        [ conjugw First  "yaava"
        ; conjugw Second "yaatam"
        ; conjugw Third  "yaataam"
        ])
   ; (Plural,
        [ conjugw First  "yaama"
        ; conjugw Second "yaata"
        ; conjugw Third  "yur"
        ])
   ]
;
value conjug_opt_ath_a gana = conjug_optativea gana Primary
;
value compute_athematic_optative3a weak iiflag entry =
  let dadh_flag = (entry="dhaa#1") in 
  let conjugw person suff = (person,
      if entry="haa#1" then fix (revstem "jah") suff
      else fix3w weak iiflag dadh_flag suff) in
  enter1 entry (conjug_opt_ath_a 3 conjugw)
;
value conjug_opt_ath_m gana conjugw =
  Conju (optm gana)
   [ (Singular, 
        [ conjugw First  "iiya"
        ; conjugw Second "iithaas"
        ; conjugw Third  "iita"
        ])
   ; (Dual,
        [ conjugw First  "iivahi"
        ; conjugw Second "iiyaathaam"
        ; conjugw Third  "iiyaataam"
        ])
   ; (Plural,
        [ conjugw First  "iimahi"
        ; conjugw Second "iidhvam"
        ; conjugw Third  "iiran"
        ])
   ]
;
value compute_athematic_optative3m weak iiflag entry =
  let dadh_flag = (entry="dhaa#1") in 
  let conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  enter1 entry (conjug_opt_ath_m 3 conjugw)
;
value compute_athematic_imperative3a strong weak iiflag entry =
  let dadh_flag = (entry="dhaa#1") 
  and daa_flag  = (entry="daa#1") 
  and haa_flag  = (entry="haa#1") in 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix (revstem "jahi") suff) in
  enter1 entry (Conju (impera 3)
   [ (Singular, let l = 
        [ conjugs First "aani"
        ; (Second, if daa_flag then code "dehi" (* \Pan{4,4,119} *)
                   else if dadh_flag then code "dhehi" (* idem ghu \Pan{1,1,20} *)
                   else match weak with 
            [ [ c :: _  ] -> fix3w weak iiflag dadh_flag suff 
              where suff = if vowel c then (* "dhi" or "hi" after vowel *)
                              if entry = "hu" then "dhi" else "hi" 
                            else "dhi"
            | _ -> error_empty 7
            ] ) 
        ; conjugs Third "tu"
        ] in if haa_flag then l @
                [ conjughaa Second "hi" (* jahihi *)
                ; conjugs Second   "hi" (* jahaahi *) 
                ; conjughaa Third  "tu" (* jahitu *)
                ]
             else l) 
   ; (Dual, let l = 
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; if entry="bhas" then (Third, code "babdhaam") (* Whitney§678 MW§340 *) 
          else conjugw Third  "taam"
        ] in if haa_flag then l @
                [ conjughaa Second "tam"
                ; conjughaa Third  "taam"
                ]
             else l)
   ; (Plural, let l = 
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  "atu"
        ] in if haa_flag then l @ [ conjughaa Second "ta" ]
             else l)
   ])
;
value compute_imp_ath_m gana conjugs conjugw entry =
  enter1 entry (Conju (imperm gana)
   [ (Singular,
        [ conjugs First  "ai"
        ; conjugw Second "sva"
        ; conjugw Third  "taam"
        ])
   ; (Dual,
        [ conjugs First  "aavahai"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ])
   ; (Plural,
        [ conjugs First  "aamahai"
        ; conjugw Second "dhvam"
        ; conjugw Third  "ataam"
        ])
   ])
;
value compute_athematic_imperative3m strong weak iiflag entry =
  let dadh_flag = (entry="dhaa#1") in
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  compute_imp_ath_m 3 conjugs conjugw entry 
;
value compute_active_present3 sstem wstem iiflag entry third = do
  { compute_athematic_present3a sstem wstem iiflag entry third
  ; compute_athematic_impft3a sstem wstem iiflag entry 
  ; compute_athematic_optative3a wstem iiflag entry 
  ; compute_athematic_imperative3a sstem wstem iiflag entry 
  } 
and compute_middle_present3 sstem wstem iiflag entry third = do 
  { compute_athematic_present3m Primary 3 wstem iiflag entry third
  ; compute_athematic_impft3m wstem iiflag entry 
  ; compute_athematic_optative3m wstem iiflag entry 
  ; compute_athematic_imperative3m sstem wstem iiflag entry 
  ; let short = if iiflag then strip_ii wstem else wstem in 
    record_part_m_ath (pprm 3) short entry
  }
;

(*** Gana 5  ***)

value compute_athematic_present5a gana strong weak vow entry third = 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff)
        else (person,fix weak suff)
      | [] -> error_suffix 9
      ]
  and conjugw2 person suff = match weak with 
      [ [ 5 :: no_u ] -> (person,fix no_u suff)
      | _ -> failwith "5a weak ought to end in u"
      ] in do
  { enter1 entry (Conju (presa gana)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check entry gana third (conjugs Third "ti") 
        ])
   ; (Dual, let l = 
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ]   in 
        if vow then [ conjugw2 First "vas" (* optional elision of u *) :: l ]
               else l)
   ; (Plural, let l = 
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; conjugw Third  "anti"
        ] in
        if vow then [ conjugw2 First "mas" (* optional elision of u *) :: l ]
               else l)
   ])
  ; let m_pstem = if vow then weak else [ 45 (* v *) :: weak ] in
    let f_pstem = rfix m_pstem "at" in
    record_part (Ppra_ 5 Primary m_pstem f_pstem entry)
  }
;
value compute_athematic_present5m gana weak vow entry third = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> if vowel c then 
                         let w = if vow then weak else [ 45 (* v *) :: weak ] in
                         (person,fix w suff)
                      else (person,fix weak suff)
      | [] -> error_suffix 17
      ]
  and conjugw2 person suff = match weak with 
      [ [ 5 :: no_u ] -> (person,fix no_u suff)
      | _ -> failwith "5m weak ought to end in u"
      ] in 
  enter1 entry (Conju (presm gana)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check entry gana third (conjugw Third "te") 
        ])
   ; (Dual, let l = 
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ] in
        if vow then [ conjugw2 First "vahe" (* optional elision of u *) :: l ]
        else l)
   ; (Plural, let l = 
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ate"
        ] in
        if vow then [ conjugw2 First "mahe" (* optional elision of u *) :: l ]
        else l)
   ])
;
value compute_athematic_impft5a gana strong weak vow entry = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix_augment w suff)
        else (person,fix_augment weak suff)
      | [] -> error_suffix 10
      ]
  and conjugw2 person suff = match weak with 
      [ [ 5 :: no_u ] -> (person,fix_augment no_u suff)
      | _ -> failwith "5a weak ought to end in u"
      ] in
  enter1 entry (Conju (impfta gana)
   [ (Singular,  
        [ conjugs First  "am"
        ; conjugs Second "s"
        ; conjugs Third  "t"
        ]) 
   ; (Dual, let l = 
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ] in
       if vow then [ conjugw2 First "va" (* optional elision of u *) :: l ]
              else l)
   ; (Plural, let l =
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; conjugw Third  "an"
        ] in
       if vow then [ conjugw2 First "ma" (* optional elision of u *) :: l ]
       else l)
   ])
;
value compute_athematic_impft5m gana weak vow entry = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix_augment w suff)
        else (person,fix_augment weak suff)
      | [] -> error_suffix 14
      ]
  and conjugw2 person suff = match weak with 
      [ [ 5 :: no_u ] -> (person,fix_augment no_u suff)
      | _ -> failwith "5m weak ought to end in u"
      ] in
  enter1 entry (Conju (impftm gana)
   [ (Singular, 
        [ conjugw First  "i"
        ; conjugw Second "thaas"
        ; conjugw Third  "ta"
        ])
   ; (Dual, let l =
        [ conjugw First  "vahi"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ] in
       if vow then [ conjugw2 First "vahi" (* optional elision of u *) :: l ]
       else l)
   ; (Plural, let l =
        [ conjugw First  "mahi"
        ; conjugw Second "dhvam"
        ; conjugw Third  "ata"
        ] in
       if vow then [ conjugw2 First "mahi" (* optional elision of u *) :: l ]
       else l)
   ])
;
value compute_athematic_optative5a gana weak vow entry = (* gana=5 or 8 *)
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff)
        else (person,fix weak suff)
      | [] -> error_suffix 11
      ] in
  enter1 entry (conjug_opt_ath_a gana conjugw)
;
value compute_athematic_optative5m gana weak vow entry = (* gana=5 or 8 *)
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff)
        else (person,fix weak suff)
      | [] -> error_suffix 19
      ] in
  enter1 entry (conjug_opt_ath_m gana conjugw)
;
value compute_athematic_imperative5a gana strong weak vow entry = (* gana=5 or 8 *)
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> if vowel c then 
                         let w = if vow then weak else [ 45 (* v *) :: weak ] in
                         (person,fix w suff)
                      else (person,fix weak suff)
      | [] -> (person,fix weak "")
      ] in
  enter1 entry (Conju (impera gana)
   [ (Singular, 
        [ conjugs First "aani"
        ; conjugw Second (if vow then "" else "hi")
        ; conjugs Third "tu"
        ])
   ; (Dual,
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  "antu"
        ])
   ])
;
value compute_athematic_imperative5m gana strong weak vow entry = (* gana=5 or 8 *)
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
              (person,fix w suff)
        else  (person,fix weak suff)
      | [] -> (person,fix weak "")
      ] in
  compute_imp_ath_m gana conjugs conjugw entry 
;
(* Used by classes 5 and 8 *)
value compute_active_present5 gana sstem wstem vow entry third = do 
  { compute_athematic_present5a gana sstem wstem vow entry third
  ; compute_athematic_impft5a gana sstem wstem vow entry 
  ; compute_athematic_optative5a gana wstem vow entry 
  ; compute_athematic_imperative5a gana sstem wstem vow entry 
  } 
and compute_middle_present5 gana sstem wstem vow entry third = do 
  { compute_athematic_present5m gana wstem vow entry third
  ; compute_athematic_impft5m gana wstem vow entry 
  ; compute_athematic_optative5m gana wstem vow entry 
  ; compute_athematic_imperative5m gana sstem wstem vow entry 
  ; record_part_m_ath (pprm 5) wstem entry
  }
;
(* Also used by gana 8 *)
value compute_present5 gana sstem wstem vow entry third pada padam =
  match voices_of_gana gana entry with
       [ Para -> if pada then
           compute_active_present5 gana sstem wstem vow entry third
           else emit_warning ("Unexpected middle form: " ^ entry)
       | Atma -> if padam then emit_warning ("Unexpected active form: " ^ entry)
           else compute_middle_present5 gana sstem wstem vow entry third
       | Ubha ->             
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present5 gana sstem wstem vow entry thirda
          ; compute_middle_present5 gana sstem wstem vow entry thirdm
          }
       ]
;

(*** Gana 7  ***)

value compute_athematic_present7a strong weak entry third =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 entry (Conju (presa 7)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check entry 7 third (conjugs Third "ti") 
        ])
   ; (Dual,
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ])
   ; (Plural,
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; conjugw Third  "anti"
        ])
   ])
  ; let m_pstem = weak 
    and f_pstem = rfix weak "at" in
    record_part (Ppra_ 7 Primary m_pstem f_pstem entry) 
  }
;
value compute_athematic_present7m weak entry third = 
  let conjugw person suff = (person,fix weak suff) in
  enter1 entry (Conju (presm 7)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check entry 7 third (conjugw Third "te") 
        ])
   ; (Dual,
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ])
   ; (Plural,
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ate"
        ])
   ])
;
value compute_athematic_impft7a strong weak entry = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix_augment weak suff) in
  enter1 entry (Conju (impfta 7)
   [ (Singular, let l =
        [ conjugs First  "am"
        ; conjugs Second "s" 
        ; conjugs Third  "t"
        ] in match rev (fix_augment strong "s") with
             [ [ c :: r ] -> if c=32 (* t *) then 
                                [ (Second,rev [ 48 (* s *) :: r ]) :: l ]
                                (* abhinad-s -> abhinat or abhinas *)
                             else l (* horrible patch *)
             | _ -> error_empty 8
             ])
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; conjugw Third  "an"
        ])
   ])
;
value compute_athematic_impft7m weak entry = 
  let conjugw person suff = (person,fix_augment weak suff) in
  enter1 entry (Conju (impftm 7)
   [ (Singular, 
        [ conjugw First  "i"
        ; conjugw Second "thaas"
        ; conjugw Third  "ta"
        ])
   ; (Dual,
        [ conjugw First  "vahi"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ])
   ; (Plural,
        [ conjugw First  "mahi"
        ; conjugw Second "dhvam"
        ; conjugw Third  "ata"
        ])
   ])
;
value compute_athematic_optative7a weak entry =
  let glue = if entry = "hi.ms" then fun w s -> 
                List2.unstack w (code s) (* no retroflexion Whitney§183a *)
             else fix in 
  let conjugw person suff = (person,glue weak suff) in 
  enter1 entry (conjug_opt_ath_a 7 conjugw)
;
value compute_athematic_optative7m weak entry =
  let conjugw person suff = (person,fix weak suff) in
  enter1 entry (conjug_opt_ath_m 7 conjugw)
;
value compute_athematic_imperative7a strong weak entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in
  enter1 entry (Conju (impera 7)
   [ (Singular, 
        [ conjugs First "aani"
        ; (Second, match weak with 
            [ [ c :: _ ] -> fix weak suff 
              where suff = if vowel c then "hi" else "dhi"
            | _ -> error_empty 9
            ]) (* "dhi" or "hi" after vowel *)
        ; conjugs Third "tu"
        ])
   ; (Dual,
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  "antu"
        ])
   ])
;
value compute_athematic_imperative7m strong weak entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in
  compute_imp_ath_m 7 conjugs conjugw entry 
;
value compute_active_present7 sstem wstem entry third = do
  { compute_athematic_present7a sstem wstem entry third
  ; compute_athematic_impft7a sstem wstem entry 
  ; compute_athematic_optative7a wstem entry 
  ; compute_athematic_imperative7a sstem wstem entry 
  } 
and compute_middle_present7 sstem wstem entry third = do
  { compute_athematic_present7m wstem entry third
  ; compute_athematic_impft7m wstem entry 
  ; compute_athematic_optative7m wstem entry 
  ; compute_athematic_imperative7m sstem wstem entry 
  ; record_part_m_ath (pprm 7) wstem entry
  }
;
value compute_present7 sstem wstem entry third pada padam = 
  match voices_of_gana 7 entry with
  [ Para -> if pada then compute_active_present7 sstem wstem entry third
            else emit_warning ("Unexpected middle form: " ^ entry)
  | Atma -> if padam then emit_warning ("Unexpected active form: " ^ entry)
            else compute_middle_present7 sstem wstem entry third
  | Ubha -> let thirda = if pada then third else []
            and thirdm = if pada then [] else third in do
            { compute_active_present7 sstem wstem entry thirda
            ; compute_middle_present7 sstem wstem entry thirdm
            }
  ]
;

(*** Gana 8  ***)

(* Conjugation of k.r *)     (* "karo" "kuru" "kur" *)
value compute_athematic_presentk strong weak short entry third = 
  let conjugs person suff = (person,fix strong suff) 
  and conjugw person suff = (person,fix weak suff)
  and conjugwvm person suff = (person,fix short suff) (* -v -m suff *) in do
  { enter1 entry (Conju (presa 8)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check entry 8 third (conjugs Third "ti") 
        ])
   ; (Dual,
        [ conjugwvm First "vas"
        ; conjugw Second  "thas"
        ; conjugw Third   "tas"
        ])
   ; (Plural,
        [ conjugwvm First "mas"
        ; conjugw Second  "tha"
        ; conjugw Third   "anti"
        ])
   ])
  ; let f_pstem = rfix weak "at" in
    record_part (Ppra_ 8 Primary weak f_pstem entry) 
  ; record_part_m_ath (pprm 8) weak entry
  ; enter1 entry (Conju (presm 8)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; conjugw Third  "te" 
        ])
   ; (Dual, 
        [ conjugwvm First "vahe"
        ; conjugw Second  "aathe"
        ; conjugw Third   "aate"
        ])
   ; (Plural,
        [ conjugwvm First "mahe"
        ; conjugw Second  "dhve"
        ; conjugw Third   "ate"
        ])
   ])
  }
;
value compute_athematic_impftk strong weak short entry = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix_augment weak suff)
  and conjugwvm person suff = (person,fix_augment short suff) (* -v -m suff *) in do
  { enter1 entry (Conju (impfta 8)
   [ (Singular,  
        [ conjugs First  "am"
        ; conjugs Second "s"
        ; conjugs Third  "t"
        ]) 
   ; (Dual,
        [ conjugwvm First "va"
        ; conjugw Second  "tam"
        ; conjugw Third   "taam"
        ])
   ; (Plural,
        [ conjugwvm First "ma"
        ; conjugw Second  "ta"
        ; conjugw Third   "an"
        ])
   ])
  ; enter1 entry (Conju (impftm 8) (* similar to [conjugs_past_m] except for -v -m suff *)
   [ (Singular, 
        [ conjugw First  "i"
        ; conjugw Second "thaas"
        ; conjugw Third  "ta"
        ])
   ; (Dual,
        [ conjugwvm First "vahi"
        ; conjugw Second  "aathaam"
        ; conjugw Third   "aataam"
        ])
   ; (Plural, 
        [ conjugwvm First "mahi"
        ; conjugw Second  "dhvam"
        ; conjugw Third   "ata"
        ])
   ])
  }
;
value compute_athematic_optativek weak short entry =
  let conjugw person suff = (person,fix weak suff)
  and conjugs person suff = (person,fix short suff) in do
  { enter1 entry (conjug_opt_ath_a 8 conjugs) (* short since -y suffixes *)
  ; enter1 entry (conjug_opt_ath_m 8 conjugw)
  }
;
value compute_athematic_imperativek strong weak entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 entry (Conju (impera 8)
   [ (Singular, 
        [ conjugs First  "aani"
        ; conjugw Second ""
        ; conjugs Third  "tu"
        ])
   ; (Dual,
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama" (* also kurma Epics *)
        ; conjugw Second "ta"
        ; conjugw Third  "antu"
        ])
   ])
  ; compute_imp_ath_m 8 conjugs conjugw entry 
  }
;
value compute_presentk sstem wstem short entry third = do
  { compute_athematic_presentk sstem wstem short entry third
  ; compute_athematic_impftk sstem wstem short entry 
  ; compute_athematic_optativek wstem short entry 
  ; compute_athematic_imperativek sstem wstem entry 
  }
;

(*** Gana 9  ***)

value compute_athematic_present9a strong weak short entry third = 
  let conjugs person suff = (person,fix strong suff) 
  and conjugw_v person suff = (person,fix short suff) (* vowel suffix *)
  and conjugw_c person suff = (person,fix weak suff) (* consonant suffix *) in do
  { enter1 entry (Conju (presa 9)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check entry 9 third (conjugs Third "ti") 
        ])
   ; (Dual,
        [ conjugw_c First  "vas"
        ; conjugw_c Second "thas"
        ; conjugw_c Third  "tas"
        ])
   ; (Plural,
        [ conjugw_c First  "mas"
        ; conjugw_c Second "tha"
        ; conjugw_v Third  "anti"
        ])
   ])
  ; let f_pstem = rfix short "at" in
    record_part (Ppra_ 9 Primary short f_pstem entry) (* follows 3rd pl *) 
  }
;
value compute_athematic_present9m weak short entry third = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> error_suffix 16
      ] in
  enter1 entry (Conju (presm 9)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check entry 9 third (conjugw Third "te") 
        ])
   ; (Dual,
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ])
   ; (Plural,
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ate"
        ])
   ])
;
value compute_athematic_impft9a strong weak short entry = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix_augment w suff)
      | [] -> error_suffix 6
      ] in
  enter1 entry (Conju (impfta 9)
   [ (Singular,  
        [ conjugs First  "am"
        ; conjugs Second "s"
        ; conjugs Third  "t"
        ]) 
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural, 
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; conjugw Third  "an"
        ])
   ])
;
value compute_athematic_impft9m weak short entry = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix_augment w suff)
      | [] -> error_suffix 13
      ] in
  enter1 entry (conjug_impft_m 9 conjugw)
;
value compute_athematic_optative9a weak short entry =
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in (* tjs y- *)
                      (person,fix w suff)
      | [] -> error_suffix 14
      ] in
  enter1 entry (conjug_opt_ath_a 9 conjugw)
;
value compute_athematic_optative9m short entry =
  let conjugw person suff = (person,fix short suff) in (* suff starts with ii *)
  enter1 entry (conjug_opt_ath_m 9 conjugw) 
;
value compute_athematic_imperative9a strong weak short vow root entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> (person,fix weak "")
      ] 
  and conjugw2 person suff = (person,fix root suff) in
  enter1 entry (Conju (impera 9)
   [ (Singular, 
        [ conjugs First  "aani"
        ; if vow then conjugw Second "hi"
          else conjugw2  Second "aana" (* no nii suffix for consonant root *)
        ; conjugs Third "tu"
        ])
   ; (Dual,
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  "antu"
        ])
   ])
;
value compute_athematic_imperative9m strong weak short root entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> (person,fix weak "")
      ] in
  compute_imp_ath_m 9 conjugs conjugw entry 
;
value compute_active_present9 sstem wstem short vow stem entry third = do
  { compute_athematic_present9a sstem wstem short entry third
  ; compute_athematic_impft9a sstem wstem short entry 
  ; compute_athematic_optative9a wstem short entry 
  ; compute_athematic_imperative9a sstem wstem short vow stem entry 
  } 
and compute_middle_present9 sstem wstem short stem entry third = do
  { compute_athematic_present9m wstem short entry third
  ; compute_athematic_impft9m wstem short entry 
  ; compute_athematic_optative9m short entry 
  ; compute_athematic_imperative9m sstem wstem short stem entry 
  ; record_part_m_ath (pprm 9) short entry (* short and not wstem *)
  }
;
value compute_present9 sstem wstem short vow stem entry third pada padam = 
  match voices_of_gana 9 entry with
  [ Para -> if pada then 
               compute_active_present9 sstem wstem short vow stem entry third
            else emit_warning ("Unexpected middle form: " ^ entry)
  | Atma -> if padam then emit_warning ("Unexpected active form: " ^ entry)
            else compute_middle_present9 sstem wstem short stem entry third
  | Ubha -> let thirda = if pada then third else []
            and thirdm = if pada then [] else third in do
            { compute_active_present9 sstem wstem short vow stem entry thirda
            ; compute_middle_present9 sstem wstem short stem entry thirdm
            }
  ]
;

(* Benedictive/precative, formed from [conjug_optativea] with aorist stem *)
(* NB. Whitney§837 makes it an optative mode of the root aorist *)
value conjug_benedictivea conj weak entry = 
  let conjugw person suff = (person,fix weak suff) in
  enter1 entry 
  (Conju (fbenea conj)
   [ (Singular, 
        [ conjugw First  "yaasam"
        ; conjugw Second "yaas" (* ambig opt *)
        ; conjugw Third  "yaat" (* ambig opt *)
        ])
   ; (Dual,
        [ conjugw First  "yaasva"
        ; conjugw Second "yaastam"
        ; conjugw Third  "yaastaam"
        ])
   ; (Plural,
        [ conjugw First  "yaasma"
        ; conjugw Second "yaasta"
        ; conjugw Third  "yaasur"
        ])
   ])
;
value conjug_benedictivem conj sibstem entry =
  let conjug person suff = (person,fix sibstem suff) in
  enter1 entry 
  (Conju (fbenem conj)
   [ (Singular, 
        [ conjug First  "iiya"
        ; conjug Second "ii.s.thaas" 
        ; conjug Third  "ii.s.ta"  
        ])
   ; (Dual, 
        [ conjug First  "iivahi" 
        ; conjug Second "iiyaasthaam"
        ; conjug Third  "iiyaastaam" 
        ])
   ; (Plural, 
        [ conjug First  "iimahi"
        ; conjug Second "ii.dhvam"
        ; conjug Third  "iiran"
        ]) 
   ]) 
;
value compute_benedictive rstem entry = 
   (* Macdonell§150 Kale§960 Whitney§924 Henry§298 *)
  let bene_stem = let ps_stem = passive_stem entry rstem in
      match entry with (* Deshpande gram p328 *)
      [ "j~naa#1" | "daa#1" | "paa#1" | "sthaa#1" | "haa#1" -> 
           match ps_stem with 
           [ [ 4 (* ii *) :: rest ] -> [ 10 (* e *) :: rest ] (* ii -> e *)
           | _ -> failwith "Anomaly bene_stem"
           ] (* NB Deshpande: also j~naayaat *)
      | "puu#1" -> revcode "punii" (* weak gana 9 puniiyaat Vi.s.nu sahasr. *)
      | _ -> ps_stem
      ] in do
  { conjug_benedictivea Primary bene_stem entry (* productive, although rare *)
    (* middle very rare: viik.si.siiran et pratipatsiiran in Abhisamayaalafkaara
       (David Reigle) and k.r.sii.s.ta in BhP and stotras (Harry Spier) *)
  ; match entry with 
    [ "bhuu#1" -> let sibstem = revcode "bhavi.s" in 
        conjug_benedictivem Primary sibstem entry (* bhavi.sii.s.ta *)
    | "k.r#1" -> let sibstem = revcode "k.r.s" in (* k.r.sii.s.ta *)
        conjug_benedictivem Primary sibstem entry (* Kanakadhaarastotra *)
    | "iik.s" -> let sibstem = revcode "iik.si.s" in 
        conjug_benedictivem Primary sibstem entry (* viik.si.siiran *)
    | "j~naa#1" -> let sibstem = revcode "j~naas" in 
        conjug_benedictivem Primary sibstem entry (* j~naasi.s.ta Deshpande *)
    | "daa#1" -> let sibstem = revcode "daas" in 
        conjug_benedictivem Primary sibstem entry (* daasi.s.ta Deshpande *)
    | "pad#1" -> let sibstem = revcode "pats" in 
        conjug_benedictivem Primary sibstem entry (* pratipatsiiran *)
    | "m.r" -> let sibstem = revcode "m.r.s" in 
        conjug_benedictivem Primary sibstem entry (* m.r.sii.s.ta \Pan{1,3,61} *)
    | "luu#1" -> let sibstem = revcode "lavi.s" in
        conjug_benedictivem Primary sibstem entry (* lavi.sii.s.ta \Pan{3,4,116} *)
    | _ -> ()
    ]
  }
;
(*****************)
(* Future system *)
(*****************)

(* Similar to [compute_thematic_paradigm_act] *)
value compute_futurea conj stem entry = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 entry (Conju (ffutura conj)
   [ (Singular, 
        [ conjug First  "aami"
        ; conjug Second "asi"
        ; conjug Third  "ati" 
        ])
   ; (Dual,
        [ conjug First  "aavas"
        ; conjug Second "athas"
        ; conjug Third  "atas"
        ])
   ; (Plural,
        [ conjug First  "aamas"
        ; conjug Second "atha"
        ; conjug Third  "anti"
        ])
   ])
  ; record_part (Pfuta_ conj stem entry) 
  }
;
value compute_futurem conj stem entry = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 entry (Conju (ffuturm conj)
   [ (Singular, 
        [ conjug First  "e"
        ; conjug Second "ase"
        ; conjug Third  "ate"
        ])
   ; (Dual,
        [ conjug First  "aavahe"
        ; conjug Second "ethe"
        ; conjug Third  "ete"
        ])
   ; (Plural,
        [ conjug First  "aamahe"
        ; conjug Second "adhve"
        ; conjug Third  "ante"
        ])
   ])
  ; record_part_m_th pfutm stem entry
  }
;
(* Conditional - preterit of future, built from imperfect on future stem   *)
(* where non-performance of the action is implied - pluperfect conditional *)
(* used in antecedent as well as in consequent clause - Apte§216           *)
(* "si vous étiez venu, vous l'auriez vue" *)
value compute_conda conj stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in 
  enter1 entry (Conju (fconda conj) (thematic_preterit_a conjug))
;
value compute_condm conj stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (fcondm conj) (thematic_preterit_m conjug))
;
value compute_future stem entry = 
  match entry with
    [ "as#1" -> () (* uses bhuu *) 
    | "iiz#1" | "lii" | "knuu" | "baadh" -> do (* Para allowed in future *)
         { compute_futurea Primary stem entry 
         ; compute_futurem Primary stem entry  
         }
    | _ -> match voices_of entry with
       [ Para -> do (* active only *) 
         { compute_futurea Primary stem entry 
         ; match entry with (* conditional or atma on demand *)
           [ "grah" | "jiiv" | "bhuu#1" | "zaas" | "stu" | "sm.r" | "haa#1" 
                     -> compute_conda Primary stem entry
           | "khaad" -> compute_futurem Primary stem entry 
           | _ -> ()
           ]
         }
       | Atma -> (* middle only *) 
         compute_futurem Primary stem entry 
       | (* both *) _ -> do
         { compute_futurea Primary stem entry 
         ; compute_futurem Primary stem entry 
         ; match entry with (* rare conditional *)
           [ "i" | "k.r#1" | "gam" | "ji" | "j~naa#1" | "tap" | "daa#1" 
           | "nii#1" | "bandh" | "budh#1" | "m.r" | "yaj#1" | "sthaa#1" -> do
              { compute_conda Primary stem entry 
              ; compute_condm Primary stem entry 
              }
           | _ -> ()
           ]
         }
       ]
    ]
;
value compute_future_ca stem entry = do
  { compute_futurea Causative stem entry 
  ; compute_futurem Causative stem entry 
  ; match entry with (* rare conditional *)
    [ "j~naa#1" -> do
       { compute_conda Causative stem entry 
       ; compute_condm Causative stem entry 
       }
    | _ -> ()
    ]
  ; record_part_m_th pcausfm stem entry
  }
;
(* Possible intercalating vowel i for se.t and ve.t roots Whitney§935 *)
(* [intercalates] returns a set of possible intercalations.           *)
(* 3 indicates metathesis: ar becomes ra by [ar_ra] below             *)
(* 4 is specific to naz nasalisation                                  *)
(* This information should be lexicalised with a generative lexicon.  *)
value intercalates root = 
  let anit = [ 0 ]    (* no intercalation *) 
  and set  = [ 1 ]    (* intercalate i *)
  and vet  = [ 0; 1 ] (* intercalate i optionally *)
      (* NB for likh and vij 0 means intercalate i on weak stem *)
  and setl = [ 2 ]    (* intercalate ii *)
  and serb = [ 1; 2 ] (* intercalate i or ii *) in fun (* rstem *)
   [ [] -> error_empty 10
   | [ 7; 45 (* v.r *) ] -> serb (* [v.r#1] and [v.r#2] *)
   | [ 7 (* -.r *) :: _ ] -> set
   | [ 8 (* -.rr *) :: _ ] -> serb
   | [ 6; 48 (* [suu#1] *) ] -> vet
   | [ 6 (* -uu *) :: _ ] -> set (* Kale p. 186 *)
   | [ c :: r ] -> 
       if vowel c then 
          if all_consonants r then 
             match root with
             [ "k.sii" | "ji" | "nii#1" | "vaa#3" | "zii#1" | "su#2" 
             | "stu" | "sru" | "haa#1" -> vet
             | ".dii" | "nu#1" | "yu#1" | "yu#2" | "ru" | "zri" 
             | "k.su" | "k.s.nu" | "snu" (* Kale *) | "zuu"
                 -> set
             | _ -> anit 
             ] 
          else set 
       else if semivowel c then set
       else match root with
            [ "ak.s" | "a~nj" | "k.rt#1" | "k.rp" | "k.lp" | "kram" | "k.sam" 
            | "klid" | "kliz" | "gup" | "guh" | "ghu.s" | "jan" | "ta~nc" 
            | "tap" | "t.rd" | "tyaj#1" | "dah#1" | "d.rp" | "nam" | "naz" 
            | "n.rt" | "bandh" | "budh#1" | "bhaj" | "majj" | "man" | "m.rj"
            | "yam" | "ruh" | "labh" | "likh" | "vap#2" | "vas#1" | "vah#1" 
            | "vij" | "vid#1" | "v.rj" | "v.rt#1" | "vrazc" | "sad#1" | "sah#1"
            | "sidh#2" | "svap" | "han#1" | "syand" (* WR: set atma, anit para *)
                -> vet  
            | "grah" -> setl
            | "s.rj#1" -> [ 3 ] (* sra.s.taa *)
            | "k.r.s" -> [ 3 :: vet ] (* ar -> ra optionally *)
            | "bh.rjj" | "sp.rz#1" -> [ 3 :: anit ] (* idem *)
            | "ad#1" | "aap" | "krudh#1" | "kruz" | "k.sip" | "k.sud" 
            | "k.sudh#1" | "khid" | "chid#1" | "tud#1" | "tu.s" | "t.rp#1"
            | "tvi.s#1" | "diz#1" | "dih" | "du.s" | "duh#1" | "d.rz#1" 
            | "dvi.s#1" | "nah" | "nij" | "nud" | "pac" | "pad#1" | "pi.s" 
            | "pu.s#1" | "praz" | "bha~nj" | "bha.s" | "bhid#1"
            | "bhuj#1" | "bhuj#2" | "mih" | "muc#1" | "m.rz" | "yaj#1" | "yabh" 
            | "yuj#1" | "yudh#1" | "ra~nj" | "rabh" | "ram" | "raadh" | "ric"
            | "ruj#1" | "rudh#1" | "rudh#2" | "ruh#1" | "lip" | "liz" | "lih#1"
            | "lup" | "vac" | "vap#1" | "vic" | "vid#2" | "viz#1" | "vi.s#1" 
            | "vyadh" | "zak" | "zad" | "zap" | "zi.s" | "zudh" | "zu.s" 
            | "zli.s" | "sa~nj" | "sic" | "sidh#1" | "s.rp" | "skand" 
            | "sva~nj" | "svid#2" | "had" 
                -> anit 
            | _ -> set (* default all multisyllabic, gana 10, nominal verbs plus:
[ "afg" | "a~nc" | "an#2" | "arh" | "av" | "az#1" | "az#2" | "as#2" | "aas#2"
| "indh" | "inv" | "i.s#1" | "i.s#2" | "iik.s" | "iifkh" | "ii.d" | "iiz#1" 
| "uc" | "u~nch" | "umbh" | "uuh" | ".rc#1" | ".rj" | ".rdh" | "edh" | "kafk"
| "kam" | "kamp" | "ka.s" | "kaafk.s" | "ku.n.th" | "ku.n.d" | "kup" | "krand" 
| "krii.d" | "khan" | "khaad" | "gu~nj" | "gam" | "ghu.s" | "ghaat" | "ghuur.n"
| "cand" | "cit#1" | "cumb" | "chand" | "jak.s" | "jap" | "jalp" | "jinv"
| "j.rmbh" | "tak" | "tan#1" | "tan#2" | "tark" | "tvar" | "dagh" | "dabh" 
| "dham" | "dhva.ms" | "dhvan" | "nand" | "nind" | "pa.th" | "pat#1" | "pi~nj"
| "piz" | "ba.mh" | "bhand" | "bhaa.s" | "bhraaj" | "ma.mh" | "ma.n.d" | "mad#1"
| "mand#1" | "mlecch" | "yat#1" | "yaac" | "ra.mh" | "rak.s" | "raaj#1" 
| "ruc#1" | "rud#1" | "lag" | "lafg" | "lafgh" | "lap" | "lamb" | "laa~nch" 
| "la.s" | "lu.n.th" | "lok" | "loc" | "vad" | "vand" | "vam" | "vaz" | "vas#2"
| "vaa~nch" | "vaaz" | "vip" | "ven" | "vyath" | "vraj" | "vrii.d" | "za.ms" 
| "zafk" | "zas" | "zaas" | "zuc#1" | "san#1" | "skhal" | "stambh" | "spand" 
| "spardh" | "sp.rh" | "sphu.t" | "svan" | "has" | "hi.ms" ] *) 
            ]
  ] 
;
(* Whitney§631-§640 Bandharkar II p44 augment ii in present system 2nd class *)
value augment_ii = fun (*  *)
  [ "an#2" | "rud#1" | "zvas#1" | "svap" | "jak.s" -> True 
    (* and thus  "praa.n#1" too gives praa.niit *) 
  | _ -> False 
  ]
;

(* Perfect passive participle *)

value intercalate_pp root rstem = 
(* some redundancy with intercalates but really different, 
   specially since the default is anit for verbs ending with single consonant *)
  let anit = [ 0 ]    (* no intercalation *)
  and set  = [ 1 ]    (* intercalate i *)
  and vet  = [ 0; 1 ] (* intercalate i optionally *) in 
  match rstem with
  [ [ c :: r ] -> 
     if vowel c then 
        match root with
        [ "jaag.r" | "zii#1" -> set
        | _ -> anit 
        ]
     else match r with
       [ [ v :: _ ] when vowel v -> 
           match root with 
            (* TODO utiliser intercalates sauf exceptions *)
           [ "radh" | "naz#1" | "trap#1" | "d.rp" | "druh#1" | "muh" | "jap"
           | "snih#1" | "snuh#1" (* \Pan{7,2,45} *)
           | "i.s#1" | "sah#1" | "lubh" | "ru.s#1" | "ri.s" (* \Pan{7,2,48} *)
           | "uuh" | "k.subh" | "tap" | "yat#1" | "ruup" | "vas#1" | "vas#4"
           | "zap" | "zas" | "zaas" | "h.r.s" (* \Pan{7,2,...} *)
           | "zak" (* zakita \Pan{7,2,17} (Kaazikaa) *)
           | "gaah" (* gaahita *)
           | "yas" (* aayasita *)
           | "kliz" |  "puu#1" | "a~nc" (* \Pan{7,2,51,53,50} *) -> vet
           | "ghu.s" (* \Pan{7,2,23} *) | "ka.s" (* \Pan{7,2,22} *) 
           | "dh.r.s" (* \Pan{7,2,19} *) 
           | "am" | "tvar" (* \Pan{7,2,28} *) -> vet (* but only set for -tvaa *)
           | "kas" | "k.sam" | "gup" | "dyut#1" | "dham" | "nud" | "m.rj" -> vet 
             (* NB zaas vet for stem zaas but admits also zi.s only anit *)
           | "aj" | "a.t" | "at" | "an#2" | "az#2" | "aas#2" | "i.s#2"
           | "ii.d" | "iir" | "iiz#1" | "ii.s" | "iih" | "uc" | ".rc#1" | ".rj" 
           | "ej" | "edh" | "kath" | "kal" | "kaaz" | "kiil" | "kuc" | "kup"
           | "ku.s" | "kuuj" | "k.rz" | "krii.d" | "klav" | "kvath" 
           | "k.sar" | "k.sudh#1" | "k.svi.d" | "khaad" | "ga.n" | "gad" | "gal" 
           | "granth" | "gha.t" | "ghaat" | "cak" | "ca.t" | "car" | "cal"
           | "cud" | "cur" | "chal" | "jiiv" | "jval" | "ta.d" | "tam" | "tul" 
           | "t.r.s#1" | "tru.t" | "tvi.s#1" | "day" | "dal" | "dol" | "dhaav#1" 
           | "dhiir" | "dhvan" | "na.t" | "nad" | "pa.th" | "pa.n" | "pat#1"
           | "piz" | "pii.d" | "pulak" | "puuj" | "prath" | "pru.s#1" | "phal"
           | "baadh" | "bha.n" | "bhas" | "bhaa.s" | "bhaas#1" | "bhuu.s" 
           | "bhraaj" | "ma.mh" | "manth" | "mah" | "likh" | "mil" | "mi.s" 
           | "miil" | "mud#1" | "mu.s#1" | "m.rg" | "yaac" | "rac" | "ra.n"
           | "ras" | "rah" | "raaj#1" | "ruc#1" | "rud#1" | "lag" | "lap" | "lal"
           | "la.s" | "las" | "lu.th" | "lul" | "lok" | "loc" | "vad" | "val" 
           | "vas#2" | "vaaz"| "vaas#3" | "vid#1" | "vip"| "ven" | "vyath" 
           | "vraj" | "vra.n" | "vrii.d" | "zubh#1" | "zcut#1" | "zrath" 
           | "zlath" | "zlaagh" | "zvas#1" | ".s.thiiv" | "suuc"| "suud" | "sev"
           | "skhal" | "stan" | "stim" | "sthag" | "sphu.t" | "sphur" | "svad"
           | "svan" | "svar#1" | "has" | "hras" | "hraad" | "hlaad" | "hval" 
               -> set
           | "palaay" -> set (* very special item *)
           | "grah" -> set (* but will get ii *)
           | _ -> anit
           ]
       | _ -> match root with
           [ "umbh" | "muurch" | "mlecch" | "zrambh" (* vizrambhita *) 
           | "skambh" (* vi.skabdha *) | "stambh" (* stabdha stabhita *)
           | "zvas" (* samaazvasta *) -> vet  
           | "cak.s" | "jak.s" | "bh.rjj" (* ca.s.ta bh.r.s.ta *)
           | "ra~nj" | "sa~nj" | "bandh" (* rakta sakta baddha *) -> anit 
           | _ -> if aa_it root || ii_it root || u_it root || uu_it root 
                     then anit 
                  else set
           ]
       ]
  | [] -> error_empty 11
  ] 
;
value intercalate_tvaa root rstem = 
  let set  = [ 1 ] (* intercalate i *) 
  and anit = [ 0 ] (* no intercalation *)
  and vet  = [ 0; 1 ] (* intercalate i optionally *) in
  match root with
  [ "zam#2"    (* unused without preverb *)
  | "av" -> [] (* WR no absol *)
  | "ka.s" | "dh.r.s" | "am" | "tvar" | ".r.s" -> set 
  | "nud" -> anit
  | _ -> if uu_it root || u_it root then vet
         else intercalate_pp root rstem 
  ]
;
value  is_set_pp root rstem = List.mem 1 (intercalate_pp root rstem)
and   is_anit_pp root rstem = List.mem 0 (intercalate_pp root rstem)
and  is_set_tvaa root rstem = List.mem 1 (intercalate_tvaa root rstem)
and is_anit_tvaa root rstem = List.mem 0 (intercalate_tvaa root rstem)
;
type ppp_suffix = 
  [ Na of Word.word 
  | Tia of Word.word (* allowing i intercalation *)
  | Ta of Word.word (* not allowing intercalation *)
  | Va of Word.word 
  | Ka of Word.word 
  ]
;
(* The ppp constructors as postfix operators applied to a stem given as string *)
value sNa s = Na (revstem s)
and   sTa s = Ta (revstem s)
and   sTia s = Tia (revstem s)
and   sVa s = Va (revstem s)
;
(* Computes the Primary ppp stems of roots *)
value compute_ppp_stems entry rstem = 
  match entry with
      (* First participles in -na *) 
    [ "vrazc" -> [ sNa "v.rk" ] (* exception - v.rk root stem of vrazc *)
    (* Most roots starting with 2 consonants take -na \Pan{8,2,43} *)
    (* but not "k.svi.d" "zrath" *)
    | "iir" | "und" | "k.rr" | "klid" | "k.saa" | "k.sii" | "k.sud" | "k.svid"
    | "khid" | "g.rr#1" | "glai" | "chad#1" | "chid#1" | "ch.rd" | "j.rr" 
    | ".dii" | "tud#1" | "t.rd" | "t.rr" | "dagh" | "d.rr" | "dev" | "draa#1"
    | "draa#2" | "nud" | "pad#1" | "pii" | "p.rr" | "pyaa" | "bha~nj" 
    | "bhid#1" | "bhuj#1" | "majj" | "man" | "mid" | "mlaa" | "ri" | "lii" 
    | "luu#1" | "vij" | "vid#2" | "vrii" | "vlii" | "zad" | "zuu" | "z.rr" 
    | "sad#1" | "skand" | "st.rr" | "styaa" | "syand" | "svid#2" | "had"
    | "haa#2" -> 
      (* except lag which is "nipaatana" (exception) \Pan{7,2,18} *)
      let ppna w = [ Na w ] in
      match rstem with 
      [ [ 2 :: _ ] | [ 4 :: _ ] | [ 6 :: _ ] (* stems in aa ii uu *)
        -> ppna rstem 
      | [ 3 :: r ] -> ppna [ 4 :: r ]  (* piina rii.na vrii.na *)
      | [ 8 :: r ] (* .rr -> r+vow *) -> 
        let vow = 
          match entry with
          [ "p.rr" -> 6 (* uu *) 
          | _ -> 4 (* ii *) 
              (* "k.rr" | "g.rr#1" | "j.rr" | "t.rr" | "d.rr" | "st.rr" *)
          ] in
        let stem = [ 43 (* r *) :: [ vow :: r ] ] in 
        match entry with 
        [ "p.rr" -> [ Ta stem :: ppna stem ] (* alternate form puurta *)
        | "st.rr" -> [ Ta [ 7 :: r ] :: ppna stem ] (* alternate form st.rta *)
        | _ -> ppna stem
        ]
      | [ 11 :: r ] (* ai *) -> ppna [ 2 :: r ] (* glaana *)
      | [ 19 :: _ ] | [ 20 :: _ ] (* g gh *) -> ppna rstem (* daghna *)
      | [ 24 :: r ] (* j *) -> 
        let stem = match r with 
                   [ [ 26 :: s ] (* ~n *)  (* bhagna *)
                   | [ 24 :: s ] (* j *)  -> [ 19 :: s ] (* magna *)
                   | _ -> [ 19 :: r ] (* revert to guttural g *)
                   ] in
        ppna stem
      | [ 34 (* d *) :: ([ 36 (* n *) :: _ ] as r) ]  -> 
        (* d is dropped eg und skand *)
        let ppn = ppna r in
        match entry with 
        [ "und" -> [ sTa "ud" :: ppn ] (* for utta and abs -udya *)
        | _ -> ppn 
        ] 
      | [ 34 (* d *) :: r ] -> 
        (* assimilation of d to n - special sandhi Macdonnel§60 foot 1 *)
        let ppn = ppna [ 36 (* n *) :: r ] in (* en fait il faudrait d'+n->nn *)
        match entry with 
        [ "vid#2" -> [ Ta rstem :: ppn ] (* 2 forms *)
        | "nud" -> [ Ta rstem :: [ Tia rstem :: ppn ] ] (* 3 forms *)
        | _ -> ppn
        ]
      | [ 36 :: ([ 1 :: r ] as w) ] (* -an *) -> 
             [ Ta w :: ppna [ 2 :: r ] ] (* mata+maana *) 
      | [ 43 (* r *) :: r ] -> ppna rstem (* iir.na *)
      | [ 45 (* v *) :: [ 10 (* e *) :: r ] ] -> (* dev *)
             ppna [ 6 (* uu *) :: [ 42 (* y *) :: r ] ] (* dyuuna *)
      | _ -> failwith ("Unexpected ppp in -na for " ^ entry)
      ]  (* end participles in -na *)
    | "pac" -> [ sVa "pak" ] (* exception \Pan{8.2.51} *)
    | "zu.s" -> [ Ka rstem ] (* exception \Pan{8.2.52} *)
    | _ -> (* otherwise participle in -ta (Panini kta) *)
           let ppstems =
       let ppstem = match entry with 
           [ "dhaa#1" -> revcode "hi" (* double weakening hi-ta \Pan{7,4,42} *)
           | "bh.rjj" -> [ 124; 7; 40 ] (* bh.rj' - mrijification of truncate *)
           | ".rc#1"  -> revcode "arc" (* strong *)
           | ".rj"    -> revcode "arj" (* strong *)
           | "k.svi.d" -> revcode "k.sve.d"
           | "vip"    -> revcode "vep"
           | "m.rg"   -> revcode "marg" (* strong *)
           | "jak.s"  -> revcode "jagh" (* jagdha *)
           | "trai"   -> revcode "traa" (* glai given in -na section *)
           | "k.san"  -> revcode "k.sa" (* removal of final nasal *) 
           | "gam"    -> revcode "ga" (* \Pan{6,4,37} *)
           | "tan#1"  -> revcode "ta"
           | "nam"    -> revcode "na"
           | "yam"    -> revcode "ya"
           | "ram"    -> revcode "ra"
           | "van"    -> revcode "va"
           | "han#1"  -> revcode "ha" (* also "man" mata given with maana *)
           | "khan"   -> revcode "khaa" (* \Pan{6,4,42} lengthening of vowel *)
           | "jan"    -> revcode "jaa"  (* id *)
           | "san#1"  -> revcode "saa"  (* id *)
           | "am"     -> revcode "aan" (* -am -> -aan \Pan{6,4,15} Wh§955a *)
           | "kam"    -> revcode "kaan" 
           | "kram"   -> revcode "kraan"
           | "cam"    -> revcode "caan"
           | "k.sam"  -> revcode "k.saan"
           | "dam#1"  -> revcode "daan"
           | "bhram"  -> revcode "bhraan" 
           | "vam"    -> revcode "vaan"
           | "zram"   -> revcode "zraan" 
           | "zam#1" | "zam#2" -> revcode "zaan"
           | "dhvan"   -> revcode "dhvaan" (* id. for final n *) (* Wh§955a *)
           | "daa#2"   -> revcode "di" (* aa -> i \Pan{7,4,40} *)
           | "maa#1"   -> revcode "mi"
           | "zaa"     -> revcode "zi"
           | "saa#1"   -> revcode "si"
           | "sthaa#1" -> revcode "sthi"
           | "diiv#1"  -> revcode "dyuu" (* iiv -> yuu *)
           | "siiv"    -> revcode "syuu"
           | "daa#1"   -> revcode "dad" (* ad hoc \Pan{7,4,46} *)
           | "dham"    -> revcode "dhmaa"  (* \Pan{7,3,78} *)
           | "dhaav#2" -> revcode "dhau"
           | "dhv.r"   -> revcode "dhuur"
           | "puuy"    -> revcode "puu"
           | "bhi.saj#2" -> revcode "bhi.sajy" 
           | "skambh" -> revcode "skabh" (* skambh -> skabh *)
           | "stambh" -> revcode "stabh" (* stambh -> stabh *)
           | "zrath"  -> revcode "zranth"
           | "muurch" -> revcode "muur" (* muurta *)
           | "av"     -> revcode "uu" (* uuta *)
           | "i" | ".r" | "k.r#1" | "kyaa" | "khyaa" | "gu~nj" | "gh.r" 
           | "ghraa" | "ci" | "cyu" | "ji" | "daa#3" | "du" | "dru#1" | "dh.r" 
           | "dhyaa" | "dhru" | "nu#1" | "praa#1" | "bh.r" | "mi" | "m.r" 
           | "yaa#1" | "yu#1" | "yu#2" | "raa#1" | "ru" | "va~nc" | "vaa#2" 
           | "v.r#1" | "v.r#2" | "zaas" | "zri" | "zru" | "si" | "su#2"
           | "s.r" | "stu" | "snaa" | "snu" | "smi" | "sm.r" | "haa#1" | "hi#2" 
           | "hu" | "h.r#1" -> rstem 
            (* roots ending in a vowel do not take [passive_stem] in general ? *)
            (* vérifier forme passive pour racines ci-dessus *)
           | _ -> passive_stem entry rstem (* possibly duhified and mirjified *)
           ] in [ Ta ppstem :: match entry with  
                    [ ".rc#1" | ".rj" | "k.svi.d" | "ba.mh" | "ma.mh" | "manth" 
                    | "m.rg" | "yaj#1" | "vyadh" | "grah" | "vrazc" | "praz" 
                    | "zrath" | "svap" | "stambh" ->
                           [ Tia ppstem ] (* avoids *ma.mhita *) 
                    | "vaz" | "vac" | "vap" | "vap#1" | "vap#2" | "vad" 
                    | "vas#1" | "vas#4" ->
                           [ Tia rstem; Tia ppstem ]
                    | "guh" -> [ Tia (revstem "guuh") ] (* \Pan{6,4,89} *)
                    | _ -> [ Tia rstem ] (* standard Paninian way *)
                    ] 
                ] in 
           let extra_forms = 
           match entry with (* supplementary forms *)
           [ "a~nc"   -> [ sNa "ak" :: [ sTia "a~nc" ] ] (* "akna", "a~ncita" *)
           | "kuc"    -> [ sTia "ku~nc" ] (* "ku~ncita" *)
           | "grah"   -> [ sTa "g.rbh" :: [ sTia "g.rbh" ] ] (* "g.rbhiita" *)
           | "car"    -> [ sNa "ciir" ] (* irreg. na ppp "ciir.na" *)
           | "tvar"   -> [ sNa "tuur" ] (* irreg. na ppp "tuur.na" *)
           | "du"     -> [ sNa "duu" ] (* "duuna" *)
           | "lag"    -> [ sNa "lag" ] (* irreg. na ppp "lagna" \Pan{7,2,18} *)
           | "druh#1" -> [ sTa "druh" ] (* opt. duhify "druu.dha" *)
           | "dhuu#1" -> [ sTa "dhu" ]
           | "muh"    -> [ sTa "muh" ] (* opt. duhify "muu.dha" *)
           | "mlecch" -> [ sTa "mlich" ] (* "mli.s.ta" *)
           | "vaa#3"  -> [ sTa "u" ]
           | "sah#1"  -> [ sTa "soh" ] 
           | "suu#1"  -> [ sTa "su" :: [ sNa "suu" ] ] (* suta suuna *)
           | "snih#1" -> [ sTa "snih" ] (* opt. duhify "snii.dha" *)
           | "snuh#1" -> [ sTa "snuh" ] (* opt. duhify "snuu.dha" *)
           | "haa#1"  -> [ sNa "hii" :: [ sNa "haa" ] ] (* irreg. na ppp *)
           | "hrii#1" -> [ sNa "hrii" ] (* "hrii.na" *)
           | _ -> []
           ] in extra_forms @ ppstems 
    ] 
; 

(* Metathesis -arx -> -rax (x=.s.t ou jy) *)
(* similaire order/ordre meter/mètre master/maître manner/manière *) 
value ar_ra = fun  
  [ [ c :: [ 43 :: [ 1 :: r ] ] ] -> [ c :: [ 1 :: [ 43 :: r ] ] ] 
  | w -> failwith ("metathesis failure " ^ Canon.rdecode w)
  ]
;
(* Stems used for periphrastic futur, infinitive, and gerundive in -tavya *)
(* Redundancy with intercalates ought to be addressed. *)
value perstems rstem entry =
  let sstem = strong_stem entry rstem in 
  let inter = match rstem with 
      [ [ 7; 45 (* v.r *) ] -> [ 1; 2 ] (* i/ii* [v.r#1] and [v.r#2] *)
      | [ 7 (*.r *) :: _ ] -> [ 0 ]
      | _ -> match entry with
             [ "gam" | "dham" | "praz" | "vaa#3" | "za.ms" | "han#1" | "huu"
               -> [ 0 ]
             | "v.rj" -> [ 1 ]
             | "zuc#1" -> [ 0; 1 ] (* zoktum *)
             | "d.rz#1" | "sp.rz#1" -> [ 3 ] (* ar -> ra dra.s.tum *)
             | "k.r.s" | "bh.rjj" -> [ 0; 3 ] (* berk *)
             | "naz#1" -> [ 0; 1; 4 ] (* berk - (1 not in WR) *)
             | "radh" | "trap#1" | "d.rp" | "druh#1" | "muh" | "rudh#2"
             | "snih#1" | "snuh#1" (* \Pan{7,2,45} *)
             | "i.s#1" | "sah#1" | "lubh" | "ru.s#1" | "ri.s" (* \Pan{7,2,48} *)
                 -> [ 0; 1 ]
             (* TODO: also optionally all [uu-it] roots - \Pan{7,2,44} *)
             | _ -> intercalates entry rstem 
             ] 
      ] in 
  map insert_sfx inter
     where insert_sfx = fun
       [ 0 -> match entry with
              [ "majj" -> code "mafk"  (* Whitney§936a *)
              | "jan" -> code "jaa"
              | "dham" -> code "dhmaa"
              | "nij" -> code "nej" (* for gana 3 *)
              | "vah#1" -> code "voh" (* vo.dhaa \Pan{6,3,112} *)
              | "sah" -> code "soh" (* so.dhum \Pan{6,3,112} *)
              | "likh" | "vij" -> rev [ 3 :: rstem ] (* i with weak stem *)
              | "vrazc" -> code "vraz" (* ought to be truncated by int sandhi *)
              | "za.ms" -> code "zas"
              | "huu"   -> code "hvaa" 
              | _ -> rev (match rstem with 
                     [ [ c :: r ] -> match c with
                         [ 10 | 11 | 12 | 13 -> [ 2 :: r ] (* eg gai -> gaa *)
                         | _ -> sstem
                         ]
                     | [] -> error_empty 12
                     ])
              ] 
       | 1 -> let w = match entry with 
                [ "uc" | "mil" | "sphu.t" | "sphur" -> rstem (* PB for Inf ? *)
                | "guh"   -> revcode "guuh" (* \Pan{6,4,89} *) 
                | "sad#1" -> revcode "siid" 
                | "sp.rh" -> revcode "sp.rhay"
                | "haa#1" -> revcode "jah" 
                | _ -> sstem
                ] in 
              sandhi w (code "i") (* sandhi sanitizes a possible j' or h' *)
       | 2 -> sandhi sstem (code "ii") (* grah *)
       | 3 -> rev (ar_ra sstem) (* metathesis: kra.s.taa bhra.s.taa dra.s.taa *)
       | 4 -> code "na.mz" (* exception naz *)
       | _ -> failwith "perstems: weird intercalate code"
       ]
;
value compute_future_gen rstem entry =
  let sstem = strong_stem entry rstem in
  let stems = map insert_sfx (intercalates entry rstem)
     where insert_sfx = fun 
       [ 0 -> let w = match entry with
             [ "naz"      -> revcode "nafk" (* Whitney§936a *)
             | "majj"     -> revcode "mafk"  (* Whitney§936a *)
             | "d.rz#1"   -> revcode "drak" (* drak.sya *)
             | "gai"      -> revcode "gaa"
             | "jan"      -> revcode "jaa"
             | "nij"      -> revcode "nej" (* consistent with gana 3 *)
             | "bharts"   -> revcode "bhart"
             | "likh" | "vij" -> [ 3 :: rstem ] (* i with weak stem (hack) *)
             | "vas#1"    -> revcode "vat" (* vatsyati Whitney§167 \Pan{7,4,49} *)
             | "vrazc"    -> revcode "vrak" (* vrak.sya *)
             | "saa#1"    -> rstem (* saa si *)
             | _ -> sstem (* for nij gana 3 *)
             ] in sandhi w (code "sya") (* eg dah -> dhak.sya *)
       | 1 -> let w = match entry with
             [ "uc" | " mil" | "sphu.t" | "sphur" -> rstem 
             | "guh"    -> revcode "guuh" (* \Pan{6,4,89} *) 
             | "dabh"   -> revcode "dambh"  
             | "nij"    -> revcode "ni~nj" (* consistent with gana 2 *)
             | "sad#1"  -> revcode "siid" 
             | "vaa#3"  -> revcode "ve"
             | "haa#1"  -> revcode "jah" 
             | "huu"    -> revcode "hve" 
             | _ -> sstem
             ] in sandhi w (code "i.sya")
       | 2 -> sandhi sstem (code "ii.sya") (* grah *)
       | 3 -> sandhi (ar_ra sstem) (code "sya") (* metathesis k.r.s bh.rjj s.rj *)
       | _ -> failwith "Weird intercalate code"
       ] in
  iter mk_future stems 
       where mk_future stem = match Word.mirror stem with
         [ [ 1 :: st ] -> compute_future st entry
         | _ -> error_empty 13 
         ] (* Note that sandhi with sy would fail with finalize *)
;
value compute_future_10 rstem entry =
  let fsuf = revcode "i.sy" in 
  match entry with
    [ "tul" -> do (* 2 forms *)
       { compute_future (fsuf @ (revcode "tulay")) entry
       ; compute_future (fsuf @ (revcode "tolay")) entry
       }
    | _ -> let stem = strengthen_10 rstem entry in 
           let aystem = Word.mirror (sandhi stem [ 1; 42 ] (* ay *)) in
           let fstem = fsuf @ aystem in
           compute_future fstem entry
    ]
;

(******************)
(* Passive system *)
(******************)

value admits_passive = fun 
  [ (* We filter out roots with no attested passive forms *)
    "an#2" | "av" | "as#1" | "ah" | "iiz#1" | "uc" | "kan" | "kuu" 
  | "knuu" | "k.sar" | "k.si" | "kha.n.d" | "daa#2" | "dyut#1" | "dru#1" 
  | "pat#2" | "paz" | "paa#2" | "pii" | "praa#1" | "bruu" | "ruc#1" | "vas#4"
  | "vidh#1" | "vip" | "vyac" | "zam#1" | "zi~nj" | "zrambh" | "zvit" | "sap#1"
  | "siiv" | "spaz#1" | "spardh" | "h.r#2" | "hrii#1" 
  | "ma.mh" (* supplied by "mah" *) (* | "arh" | "k.lp" no ps but pfp *)
      -> False
(* But "iiz#1" "uc" "kuu" "k.sar" "dru#1" "pii" "ruc#1" "vip" "zam#1" 
       "zi~nj" "zrambh" "siiv" "spardh" "hrii#1" admit ppp. *)
  | _ -> True 
  ]
;
value admits_ppp_abs = fun
  [ "ak.s" (* vedic a.s.ta overgenerates with a.s.tan *) 
  | "ad#1" (* jak.s jagdha \Pan{2,4,36} *)
  | "bruu" (* vac *) 
  | "paz"  (* d.rz *) 
  | "as#1" | "kan" | "k.si" | "gaa#1" | "paa#2" | "praa#1" (* omit ved. praata *)
  | "bal" | "ma.mh" | "vaz" | "vyac" | "zaz" | "zam#2" | "zvit" | "sac" | "sap#1"
  | "h.r#2" (* | "spaz#1" *) -> False
  | _ -> True
  ]
;

(* Similar to [compute_thematic_middle]  *)
value compute_passive_present verbal stem entry = 
  let conjug person suff = (person,fix stem suff) in 
  enter1 entry (Conju verbal
   [ (Singular, let l =
        [ conjug First  "e"
        ; conjug Second "ase"
        ; conjug Third  "ate"
        ] in if entry = "tap" then [ conjug Third "ati" :: l ] else l
             (* Bergaigne exception tapyati *))
   ; (Dual,
        [ conjug First  "aavahe"
        ; conjug Second "ethe"
        ; conjug Third  "ete"
        ])
   ; (Plural,
        [ conjug First  "aamahe"
        ; conjug Second "adhve"
        ; conjug Third  "ante"
        ])
   ])
;
value compute_passive_imperfect verbal stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in 
  enter1 entry (Conju verbal
   [ (Singular, 
        [ conjug First  "e"
        ; conjug Second "athaas"
        ; conjug Third  "ata"
        ])
   ; (Dual,
        [ conjug First  "aavahi"
        ; conjug Second "ethaam"
        ; conjug Third  "etaam"
        ])
   ; (Plural,
        [ conjug First  "aamahi"
        ; conjug Second "adhvam"
        ; conjug Third  "anta"
        ])
   ])
;
value compute_passive_optative verbal stem entry =
  let conjug person suff = (person,fix stem suff) in 
  enter1 entry (Conju verbal
   [ (Singular, 
        [ conjug First  "eya"
        ; conjug Second "ethaas"
        ; conjug Third  "eta"
        ])
   ; (Dual,
        [ conjug First  "evahi"
        ; conjug Second "eyaathaam"
        ; conjug Third  "eyaataam"
        ])
   ; (Plural,
        [ conjug First  "emahi"
        ; conjug Second "edhvam"
        ; conjug Third  "eran"
        ])
   ])
;
value compute_passive_imperative verbal stem entry =
  let conjug person suff = (person,fix stem suff) in 
  enter1 entry (Conju verbal
   [ (Singular, 
        [ conjug First  "ai"
        ; conjug Second "asva"
        ; conjug Third  "ataam"
        ])
   ; (Dual,
        [ conjug First  "aavahai"
        ; conjug Second "ethaam"
        ; conjug Third  "etaam"
        ])
   ; (Plural,
        [ conjug First  "aamahai"
        ; conjug Second "adhvam"
        ; conjug Third  "antaam"
        ])
   ])
;
(* Same as (reversed) internal sandhi of (reversed) stem and "y" *)
value affix_y stem = 
  [ 42 (* y *) :: Int_sandhi.restore_stem stem ]
;
value compute_passive_system conj root pastem = do
  { compute_passive_present    (fpresp conj)  pastem root
  ; compute_passive_imperfect  (fimpftp conj) pastem root
  ; compute_passive_optative   (foptp conj)   pastem root
  ; compute_passive_imperative (fimperp conj) pastem root
  ; record_part_m_th (vpprp conj) pastem root 
  }
;
(* NB. For gana 4 verbs passive differs from middle mostly by accent
   but distinction necessary since different regime *)
value compute_passive conj root stem = 
  let ps_stem = affix_y stem (* "y" marks passive *) in 
  compute_passive_system conj root ps_stem 
;
value compute_passive_raw root =
  let ps_stem = passive_stem root (revstem root) in 
  compute_passive Primary root ps_stem 
;
value compute_passive_10 root ps_stem =
  match root with
  [ "tul" -> ((* no passive*))
  | _ -> compute_passive Primary root ps_stem 
  ] 
;
value compute_passive_11 root ps_stem =
  match root with
  [ "adhvara" | "asuuya" | "iras" | "ka.n.du" | "karu.na" | "tapas" 
  | "namas" -> ((* no passive *))
  | _ -> compute_passive Primary root ps_stem 
  ] 
;

(******************)
(* Perfect system *)
(******************)

(* Reduplication for perfect. [redup_perf] takes a string, and returns 
   [(s,w,o,e,b)] where [s] is the (reversed) strong stem word,
   [w] is the (reversed) weak stem word, 
   [o] is an optional lengthened stem word,
   [e] is a boolean flag (True if 2nd sg weak) 
   [b] is a boolean flag (True if optional union-vowel i) *)
(* NB b=iopt not sufficient. See Whitney§797 *)
(* Warning: baroque code ahead *)
value redup_perf root = 
  let (revw,revs,revl) = match root with
      [ "ji"     -> stems "gi"    (* palatal -> velar *)
      | "ci"     -> stems "ki"       (* idem *)
      | "cit#1"  -> stems "kit"      (* idem *)
      | "umbh"   -> stems "ubh"   (* remove penultimate nasal *)
      | "sva~nj" -> stems "svaj"     (* idem *)
      | "han#1"  -> stems "ghan"  (* velar h -> gh *)
      | "hi#2"   -> stems "ghi"      (* idem *)
      | "guh"    -> stems "guuh"  (* \Pan{6,4,89} *)
      | "diiv#1" -> stems "dev" 
      | "dham"   -> stems "dhmaa"
      | "praz" -> let w = revcode "pracch" in (w,w,w) (* Whitney§794c *)
      | "zaas" -> let w = revcode root in (w,w,w) (* redup voy a, not i *)
      | _ -> stems root (* NB: keep penultimate nasal "ta~nc" *)
      ] in
  match Word.mirror revw with (* ugly double reversal to get the stem *)
  [ [] -> error_empty 14
  | [ 3 ] (* "i" *) -> let wk = [ 4; 42 ] (* iiy \Pan{7,4,69} *) 
                       and st = [ 3; 42; 10 ] (* iye *) (* iyaya *) 
                       and lg = [ 3; 42; 11 ] (* iyai *) (* iyaaya *) in
                       (rev st, rev wk, Some (rev lg), False, True)
  | [ c1 :: r ] ->  
      if vowel c1 then let (s,w) = match c1 with
         [ 1 (* a *) -> let w = match r with
           [ [ c2 ] -> if root = "az#1" then (revw @ [ 36; 2 ]) (* aan- az1 *)
                       else ([ c2; 2 (* aa *)])
           | [ 17; _ ] | [ 26; _ ] | [ 43; 22 ] | [ 43; 49 ] 
               -> (revw @ [ 36; 2 ])
               (* aan- for [ak.s, a~nc, a~nj, arc (en fait .rc), arh] *)
           | _ ->  (revw @ [ 36; 1 ] (* an- *))
           ] in (strong w, w)
         | 3 (* i *) -> let wk = [ 4 (* ii *) :: if r = [ 47 ] (* i.s *) then r 
                                                 else [ 42 (* y *) :: r ] ]
                        and st = [ 3; 42; 10 ] (* iye *) @ r in
                        (rev st, rev wk)
         | 5 (* u *) -> let wk = [ 6 (* uu *) :: r ]
                        and redup = match root with
                            [ "vaz" -> 2 | _ -> 12 ] in
                        let st = [ 5; 45; redup ] (* uvo/uvaa *) @ r in
                        (rev st, rev wk)
         | 7 (* .r *) -> let w = match r with
                    [ [ 22 ] | [ 35 ] | [ 47 ] -> (* Whitney§788a *)
                     (revw @ [ 36; 2 ]) (* aan- for [.rc1], [.rdh], [.r.s] *)
                    | [] -> [ 43; 1 ] (* ar for .r *)
                    | _ -> revw
                    ] in (strong w, w)
                         
         | _ (* aa ii uu *) -> (revs, revw)
         ] in (s, w, None, False, False)
      else 
      let (v,p,a) = lookvoy r (* p = prosodically long, a = vriddhi augment *)
         (* lookvoy computes the vowel v, and the two booleans p and a *)
         where rec lookvoy = fun
           [ [] -> error_vowel 1
           | [ c2 ] -> if vowel c2 then (c2,False,True)
                       else error_vowel 2
           | [ c2 :: r2 ] -> 
                       if vowel c2 then 
                          let l = length (contract r2) in
                          let p = long_vowel c2 || l>1 
                          and a = c2=1 (* a *) && l=1 in 
                          (c2,p,a)
                       else lookvoy r2
           ] in (* c is reduplicating consonant candidate *)
      let c = if sibilant c1 then match r with
                 [ [] -> error_vowel 3
                 | [ c2 :: _ ] -> if vowel c2 || nasal c2 then c1
                             else if stop  c2 then c2 
                             else (* semivowel c2 *) c1 
                 ]  
              else c1 in
      let rv = (* rv is reduplicating vowel *)
        if v>6 (* .r .rr .l dipht *) then match root with
          [ "ce.s.t" | "diiv#1" | "dev" |"sev" | "mlecch" | "vye" 
              -> 3 (* i *) (* vye for vyaa *)
          | _ -> 1 (* a *) (* also bhuu elsewhere *)
          (* but Vedic k.lp etc have long aa Whitney§786a *)
          ]
        else match root with
          [ "maa#3" -> 3 (* i *) (* analogy with present *)
          | "vyath" | "vyadh" | "vyaa" | "jyaa#1" | "pyaa" | "syand" | "dyut#1"
          | "myak.s" -> 3
            (* Whitney§785 also "vyac" and ved. "tyaj#1"; "vyaa" treated other *)
          | "kan" | "mah" -> 2 (* ved lengthened redup vowel Whitney§786a *)
          | _ -> short v (* reduplicated vowel is short *)
          ]
      and rc = (* reduplicating consonant *) match c with
        [ 17 | 18 (* k kh *) -> 22 (* c *)
        | 19 | 20 | 49 (* g gh h *) -> 24 (* j *)
        | 23 | 25 | 28 | 30 | 33 | 35 | 38 | 40 -> c-1 (* xh -> x *)
        | _ -> c (* c by default *)
        ] in 
      let (affix,sampra) = match root with (* ya -> ii va -> uu *)
          [ "yaj#1" -> ([ 3 (* i *)],Some (mrijify (revcode "iij")))
          | "vac" ->   ([ 5 (* u *)],Some (revcode "uuc"))
          | "vad" ->   ([ 5 (* u *)],Some (revcode "uud"))
          | "vap" | "vap#1" | "vap#2" -> ([ 5 (* u *) ],Some (revcode "uup"))
          | "vaz" ->   ([ 5 (* u *)],Some (revcode "uuz"))
          | "vas#1" | "vas#4" -> ([ 5 (* u *)],Some (revcode "uus"))
          | "vah#1" -> ([ 5 (* u *)],Some (revcode "uuh"))
          | "vaa#3" -> ([ 5 (* u *)],Some (revcode "uuv"))
          | _ ->       ([ rv; rc ],None)
          ] 
      and vriddhi = match root with
          [ "vyadh" | "svap" | "grah" -> True 
            (* since special weak stem returned by stems *)
          | _ -> a
          ] in
      let glue = match root with
          [ "sphur" | "sphu.t" -> revaffix False affix (* no retroflexion *)
          | _ -> revaffix True affix
          ] in
      let (weak,eweak,iopt) = match sampra with (* iopt = optional i *) 
          [ Some weak -> (weak,False,True)
          | None -> if rc=c || root="bhaj" then match r with
            [ [ 1 :: w ] -> match root with
              [ "jan" -> (glue (revcode "j~n"),True,True)
              | "val" | "mah" -> (glue revw,False,False) 
              | _ -> match w with
                [ [ c' ] when consonant c' -> 
                     (revaffix True [ 10 (* e *); c ] w,True,True)
                     (* roots of form c.a.c' with c,c' consonant or .m Scharf *)
                     (* cf. \Pan{6,4,119-126} -- ZZ may lead to hiatus *)
                | _ -> (glue revw,False,False)
                ]
              ] 
            | _ -> (glue revw,False,False) 
            ]       else 
              let (short,iopt) = match root with
                [ "gam"   -> (revcode "gm",True) (* actually i forbidden *)
                | "ghas"  -> (revcode "k.s",False)
                | "han#1" -> (revcode "ghn",True)
                | "khan"  -> (revcode "khn",False)
                | _       -> (revw,False) 
                ] in (glue short,False,iopt) 
          ] 
       and strong = glue (if p then revw else revs) 
       and longifvr = if vriddhi then revl else revs in 
       let olong = if p then None else Some (glue longifvr) in
       (strong, weak, olong, eweak, iopt)
  ]
;
value compute_perfecta conj strong weak olengthened eweak iopt entry = 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 entry (Conju (fperfa conj)
   [ (Singular, let l = match olengthened with 
     [ Some lengthened -> 
        let conjugl person suff = (person,fix lengthened suff) in
        [ conjugs First "a"
        ; conjugl First "a"
        ; let conjug = if eweak then conjugw else conjugs in
          conjug Second "itha"
        ; conjugl Third "a" 
        ] 
    | None -> 
        [ conjugs First  "a"     (* ex: aap -> aapa *)
        ; conjugs Second "itha"
        ; conjugs Third  "a" 
        ] @ if entry="az#1" then 
          let optstrong = revcode "aana.mz" in
          let conjugs person suff = (person,fix optstrong suff) in
        [ conjugs First  "a"     
        ; conjugs Second "itha"
        ; conjugs Third  "a" (* actually also regular aaza Whitney§788a *)
        ] else [] (* Whitney§788a *)
    ] in if iopt then [ conjugs Second "tha" :: l ] else l)
   ; (Dual,
        [ conjugw First  "iva"
        ; conjugw Second "athur"
        ; conjugw Third  "atur"
        ])
   ; (Plural,
        [ conjugw First  "ima"
        ; conjugw Second "a"
        ; if entry="raaj#1" then (Third, code "rejur")
          else conjugw Third "ur" (* Henry: paptur véd. pat1 *)
        ])
   ]) 
  ; let pstem = if entry="raaj#1" then (revcode "rej") else weak in 
    record_part (Ppfta_ conj pstem entry)
  }
;
value compute_perfectm conj stem entry =
  let conjugw person suff = (person,fix stem suff) in do
  { enter1 entry (Conju (fperfm conj)
   [ (Singular, let l =
        [ conjugw First  "e" 
        ; conjugw Second "i.se"
        ; conjugw Third  "e" 
        ] in if entry = "guh" then
                let juguhe = code "juguhe" in (* Whitney§793i *)
                l @ [ (First,juguhe); (Third,juguhe) ]
             else l)
   ; (Dual,
        [ conjugw First  "ivahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ])
   ; (Plural,
        [ conjugw First  "imahe"
        ; conjugw Second "idhve"
        ; conjugw Third  "ire"
        ])
   ])
  ; record_part_m_ath (vppftm conj) stem entry (* -aana *)
  }
;
value compute_perfect_c strong weak olengthened eweak iopt entry =
  match voices_of entry with
  [ Para -> do
      { compute_perfecta Primary strong weak olengthened eweak iopt entry
      ; match entry with 
        [ "cit#1" -> do
           { compute_perfectm Primary weak entry
           ; compute_perfectm Primary (revcode "cikitr") entry (* WR *)
           }
        | "vac" -> compute_perfectm Primary weak entry
(* [record_part_m_ath ppftm weak entry] (* anuucaana *) *)
        | _ -> () 
        ]
      }
  | Atma -> let stem = match entry with 
                       [ "cak.s" | "ba.mh" -> strong
                       | _ -> weak
                       ] in 
            compute_perfectm Primary stem entry
  | _ -> do { compute_perfecta Primary strong weak olengthened eweak iopt entry
            ; let stem = match entry with
                         [ "kan" -> revcode "cak" (* kan -> kaa *)
                         | _ -> weak
                         ] in 
              compute_perfectm Primary stem entry
            }
  ]
;
value compute_perfecta_aa stem entry = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 entry (Conju perfa 
   [ (Singular, 
        [ conjug First  "au"
        ; conjug Second "itha"
        ; conjug Second "aatha"
        ; conjug Third  "au" 
        ])
   ; (Dual,
        [ conjug First  "iva"
        ; conjug Second "athur"
        ; conjug Third  "atur"
        ])
   ; (Plural,
        [ conjug First  "ima"
        ; conjug Second "a"
        ; conjug Third  "ur"
        ])
   ])
  ; record_part (Ppfta_ Primary stem entry)
  }
;
value compute_perfectm_aa stem entry = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 entry (Conju perfm
   [ (Singular, 
        [ conjug First  "e" 
        ; conjug Second "i.se"
        ; conjug Third  "e" 
        ])
   ; (Dual,
        [ conjug First  "ivahe"
        ; conjug Second "aathe"
        ; conjug Third  "aate"
        ])
   ; (Plural,
        [ conjug First  "imahe"
        ; conjug Second "idhve"
        ; conjug Third  "ire"
        ])
   ])
  ; record_part_m_ath ppftm stem entry (* stem-aana *)
    (* middle part rare - eg cakraa.na pecaana anuucaana zepaana *)
  }
;
value compute_perfect_aa stem entry = 
  match voices_of entry with
  [ Para -> compute_perfecta_aa stem entry
  | Atma -> compute_perfectm_aa stem entry
  | _ -> do { if entry = "traa" then () (* to avoid parasitic tatra *)
              else compute_perfecta_aa stem entry 
            ; compute_perfectm_aa stem entry (* eg tatre WR *)
            }
  ]
;
(* dissymetric in i and u - problematic *)
value fix_dup weak suff mc = (* Gonda §18.I §6 *)
  let s = code suff in match s with
  [ [ c :: _ ] -> match weak with
      [ [ 5 (* u *) :: l ] | [ 6 (* uu *) :: l ] (* eg stu *) ->
        let sf = if vowel c then [ 45 (* v *) :: s ] else s in
        sandhi [ 5 :: l ] sf
      | [ 3 (* i *) :: l ] | [ 4 (* ii *) :: l ] (* eg nii *) ->
        let sf = [ 42 (* y *) :: if vowel c then s
                                 else [ 3 (* i *) :: s ] ] in
        let isf = if mc (* multiconsonant roots eg krii *) 
                     then [ 3 (* i *) :: sf ] 
                  else sf in
        sandhi l isf
      | _ -> sandhi weak s
      ]
  | _ -> error_suffix 12
  ]
;
value multi_consonant root = match revcode root with
  [ [ v :: r ] -> vowel v && length r > 1
  | [] -> error_empty 15
  ]
;
value compute_perfecta_v strong weak entry = 
  let lengthened = lengthened weak 
  and iforb = List.mem entry (* option intercalating i forbidden Whitney§797c *)
               [ "k.r#1"; "bh.r"; "v.r#2"; "s.r"; "dru#1"; "zru"; "stu"; "sru" ]
  and mc = multi_consonant entry in
  let conjugw person suff = (person,fix_dup weak suff mc) 
  and conjugs person suff = (person,fix strong suff)
  and conjugl person suff = (person,fix lengthened suff) in do
  { enter1 entry (Conju perfa
   [ (Singular, let l =
        [ conjugs First  "a"
        ; conjugl First  "a"
        ; conjugs Second "tha"
        ; conjugl Third  "a" 
        ] in if iforb then l else [ conjugs Second "itha" :: l ])
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "athur"
        ; conjugw Third  "atur"
        ])
   ; (Plural,
        [ conjugw First  "ma"
        ; conjugw Second "a"
        ; conjugw Third  "ur"
        ])
   ])
  ; record_part (Ppfta_ Primary weak entry)
  }
;
value compute_perfectar conj stem entry = 
  let conjugs person suff = (person,fix stem suff) 
  and conjugl person suff = (person,fix (lengthened stem) suff) in do
  { enter1 entry (Conju (fperfa conj)
   [ (Singular, 
        [ conjugs First  "a"
        ; conjugl First  "a"
        ; conjugs Second "itha"
        ; conjugl Third  "a" 
        ])
   ; (Dual,
        [ conjugs First  "iva"
        ; conjugs Second "athur"
        ; conjugs Third  "atur"
        ])
   ; (Plural,
        [ conjugs First  "ima"
        ; conjugs Second "a"
        ; conjugs Third  "ur"
        ])
   ])
  ; record_part (Ppfta_ conj stem entry)
  }
;
value compute_perfect_ril stem entry = (* -.rr or multiconsonant -.r *)
  match voices_of entry with
        [ Para -> compute_perfectar Primary stem entry
        | Atma -> compute_perfectm Primary stem entry
        | _ -> do { compute_perfectar Primary stem entry
                  ; compute_perfectm Primary stem entry
                  }
        ]
;
value compute_perfectm_v weak mc entry = 
  let conjugw person suff = (person,fix_dup weak suff mc) in do 
  { enter1 entry (Conju perfm
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; if entry = "m.r" then (Third, code "mamre")
          else conjugw Third "e" 
        ])
   ; (Dual,
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ])
   ; (Plural,
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ire"
        ])
   ])
  ; record_part_m_ath ppftm weak entry (* weak-aana *)
    (* middle part rare - eg cakraa.na pecaana anuucaana zepaana *)
  }
;
value compute_perfect_bhuu root =
  let conjug person suff = (person,fix (revcode "babhuu") suff) in
  enter1 root (Conju perfa
   [ (Singular, 
        [ conjug First  "va"
        ; conjug Second "tha"
        ; conjug Second "vitha"
        ; conjug Third  "va" 
        ])
   ; (Dual,
        [ conjug First  "viva"
        ; conjug Second "vathur"
        ; conjug Third  "vatur"
        ])
   ; (Plural,
        [ conjug First  "vima"
        ; conjug Second "va"
        ; conjug Third  "vur"
        ])
   ])
;
value compute_perfect_vid root = (* perfect in the sense of present *)
  let conjugw person suff = (person,fix (revcode "vid") suff)
  and conjugs person suff = (person,fix (revcode "ved") suff) in 
  enter1 root (Conju perfa
   [ (Singular, 
        [ conjugs First  "a"
        ; conjugs Second "tha"
        ; conjugs Third  "a" 
        ])
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "thur"
        ; conjugw Third  "tur"
        ])
   ; (Plural,
        [ conjugw First  "ma"
        ; conjugw Second "a"
        ; conjugw Third  "ur"
        ])
   ])
;
value compute_perfect_ah root =
  enter1 root (Conju perfa
   [ (Singular, 
        [ (Second, code "aattha")
        ; (Third,  code "aaha") 
        ])
   ; (Dual,
        [ (Second, code "aahathur")
        ; (Third,  code "aahatur")
        ])
   ; (Plural,
        [ (Third, code "aahur")
        ])
   ])
;
value compute_perfect_vyaa root = 
  (* This code is consistent with Dhaaturuupaprapa~nca, except for
     middle 1st sg where it lists "vivyaye" rather than "vivye" *)
  let weak = revcode "vivii" (* redup de vii Whitney§801c *)
  and strong = revcode "vivye" (* \Pan{6,1,46} *)
  and long = revcode "vivyai" in
  let conjugw person suff = (person,fix_dup weak suff False)
  and conjugs person suff = (person,fix strong suff)
  and conjugl person suff = (person,fix long suff) in do
  { enter1 root (Conju perfa
   [ (Singular, 
        [ conjugl First  "a"
        ; conjugs First  "a"
        ; conjugs Second "itha" (* \Pan{7,2,66} *)
        ; conjugl Third  "a" 
        ])
   ; (Dual, 
        [ conjugw First  "va" 
        ; conjugw Second "athur"
        ; conjugw Third  "atur"
        ])
   ; (Plural,
        [ conjugw First  "ma"
        ; conjugw Second "a" 
        ; conjugw Third  "ur"
        ])
   ])
  ; record_part (Ppfta_ Primary weak root)
  ; compute_perfectm_v weak False root (* mc=False! *)
  }
;
value compute_perfect_v strong weak entry = 
  let mc = multi_consonant entry in 
  match voices_of entry with
  [ Para -> compute_perfecta_v strong weak entry
  | Atma -> compute_perfectm_v weak mc entry
  | Ubha -> do 
     { compute_perfecta_v strong weak entry
     ; compute_perfectm_v weak mc entry
     }
  ]
;
value compute_perfect entry =
(*i Bizarre pada dependency should be factored i*)
  match entry with
    [ "bhuu#1" -> do
        { compute_perfect_bhuu entry (* No middle forms Whitney§800d *)
        ; record_part (Ppfta_ Primary (revcode "babhuu") entry)
        ; record_part_m_ath ppftm (revcode "babhuuv") entry 
        }
    | "vid#1" -> do
        { compute_perfect_vid entry (* middle forms ? *)
        ; record_part (Ppfta_ Primary (revcode "vid") entry)
        }
    | "ah" -> compute_perfect_ah entry
    | "vyaa" -> compute_perfect_vyaa entry (* does not fit standard aa scheme *)
    | "zvaa" -> let (strong, weak,_,_,_) = redup_perf "zuu" in (* \Pan{6,1,30} *)
                compute_perfect_v strong weak entry (* Whitney§794b zizvaaya *)
(* Whitney§794b also jyaa pyaa vyaa hvaa; we treat vyaa above, and hvaa is huu.
   Thus pyaa is covered by pii. jyaa1 as jii gives jijyau same WR *)
    | "indh" -> compute_perfectm Primary (revcode "iidh") entry
    | "mah" -> let (strong, weak, _, _, _) = redup_perf entry in
               compute_perfectm Primary strong entry (* ZZ Atma for Para root *)
    | _ -> let (strong, weak, olong, eweak, iopt) = redup_perf entry in 
           match weak with 
           [ [ c :: rest ] -> 
             if c=2 (* aa *) || (c>9 && c<14) (* e ai o au *)
             then compute_perfect_aa rest entry (* shortened weak stem *)
             else if c>2 && c<7 (* i ii u uu *)
                  then compute_perfect_v strong weak entry
             else if c=7 (* .r *) && multi_consonant entry || c=8 (* .rr *) 
                  then compute_perfect_ril strong entry
             else if c=7 (* .r *) then compute_perfect_v strong weak entry
             else compute_perfect_c strong weak olong eweak iopt entry
           | [] -> error_empty 16
           ]
    ]
;
value compute_perfect_desida st entry =
(* [entry:string] is the root, [st] is the desiderative (reverse word) stem. *)
(* We create a fake root from [st] to reuse [redup_perf] which uses a string.*)
  let (strong, weak, olong, eweak, iopt) = redup_perf (Canon.rdecode st) in 
  compute_perfecta Desiderative strong weak olong eweak iopt entry
and compute_perfect_desidm st entry =
  let (_, weak, _, _, _) = redup_perf (Canon.rdecode st) in
  compute_perfectm Desiderative weak entry
;
(*****************************)
(* Periphrastic perfect li.t *)
(*****************************)
(* Construction of the periphrastic perfect, used for perfect of secondary 
conjugations, denominative verbs and a few roots. It builds a form in -aam
suffixed by a perfect form of the auxiliairies k.r bhuu et as \Pan{3,1,35-40} *)
value peri_perf_stem entry = 
  let stem = match entry with 
  [ "iik.s" | "ii.d" | "iir" | "iih" | "uk.s" | "uc" | "ujjh" | "uuh" | "edh" 
    (* Macdonell§140a1 Whitney§1071c Filliozat§66 edhaa.mcakre *)
  | "ind" | "indh" | "inv" | "ii.s" | "umbh" | "cakaas" -> entry
  | "aas#2"  -> "aas" (* trim *)
  | "u.s"    -> "o.s" (* guna WR *) 
  | "jaag.r" -> "jaagar" (* Macdonell§140a2 *)
  | "bh.r"   -> "bibhar" 
  | "nii#1"  -> "nay" 
  | "i"      -> "ay" (* Whitney roots *)
  | "vyaa"   -> "vye" (* Whitney roots *)
  | "huu"    -> "hve" (* Macdonell§140a3 *)
  | "hrii#1" -> "jihre" (* Whitney roots *)
  | _ -> raise Not_attested (* no known periphrastic perfect *)
  ] in revcode stem
;
value build_perpft c abstem root =
  enter1 root (Invar (c,Perpft) (fix abstem "aam"))
;

(*****************)
(* Aorist system *)
(*****************)

(* augment True for aorist, False for injunctive *)
value sigma augment stem suff = 
  let sfx = code suff in
  let ssfx = match sfx with
    [ [ 32 (* t *) :: _ ] 
    | [ 33 (* th *) :: _ ] -> match stem with
      [ [ c :: _ ] -> 
         if vowel c || nasal c || c=43 (* r *) then [ 48 (* s *) :: sfx ]
         else sfx 
      | _ -> error_empty 17
      ]
    | [ c :: _ ] -> [ 48 (* s *) :: sfx ]
    | [ ] -> []
    ] in 
  let form = sandhi stem ssfx in
  if augment then aug form else form
;
value sigma_paradigm conjug =
   [ (Singular, 
        [ conjug First  "am"
        ; conjug Second "iis"
        ; conjug Third  "iit"
        ])
   ; (Dual,
        [ conjug First  "va"
        ; conjug Second "tam"
        ; conjug Third  "taam"
        ])
   ; (Plural, 
        [ conjug First  "ma"
        ; conjug Second "ta"
        ; conjug Third  "ur"
        ])
   ]
;
value compute_ath_s_aorista long entry = 
  let conjug person suff = (person,sigma True long suff) in
  enter1 entry (Conju (aora 4) (sigma_paradigm conjug))
;
value compute_ath_s_injuncta long entry = 
  let conjug person suff = (person,sigma False long suff) in
  enter1 entry (Conju (inja 4) (sigma_paradigm conjug))
;
value compute_ath_s_aoristm stem entry = 
  let conjug person suff = (person,sigma True stem suff)
  and conjugroot person suff = (person,fix_augment stem suff) 
  and conjugdhvam person = 
      let suff = match stem with
          [ [ 1 (* a *) :: _ ] | [ 2 (* aa *) :: _ ] -> "dhvam"
          | [ 43 (* r *) :: _ ] -> ".dhvam"
          | [ c :: _ ] -> if vowel c then ".dhvam" else "dhvam"
          | _ -> error_empty 18
          ] in 
      (person,fix_augment stem suff) in 
  let conjugc = if entry = "k.r#1" (* Whitney§882a *)
                || entry = "daa#1" (* Whitney§884 *) then conjugroot 
                else match stem with  
                     [ [ 43 :: _ ] | [ 36 :: _ ] | [ 41 :: _ ] -> conjug
                       (* r             n             m  Whitney§881*)
                     | [ c :: _ ] when consonant c -> conjugroot 
                 (*[ | [ c :: _ ] when short_vowel c -> conjugroot] ? *)
                     | _ -> conjug
                     ] in 
  enter1 entry (Conju (aorm 4)
   [ (Singular, 
        [ conjug  First  "i"
        ; conjugc Second "thaas"
        ; conjugc Third  "ta"
        ])
   ; (Dual,
        [ conjug First  "vahi"
        ; conjug Second "aathaam"
        ; conjug Third  "aataam"
        ])
   ; (Plural, 
        [ conjug First  "mahi"
        ; conjugdhvam Second
        ; conjug Third "ata"
        ])
   ])
;
value compute_ath_s_injunctm stem entry = 
  let conjug person suff = (person,sigma False stem suff)
  and conjugroot person suff = (person,fix stem suff)
  and conjugdhvam person = 
      let suff = match stem with
          [ [ 1 (* a *) :: _ ] | [ 2 (* aa *) :: _ ] -> "dhvam"
          | [ 43 (* r *) :: _ ] -> ".dhvam"
          | [ c :: _ ] -> if vowel c then ".dhvam" else "dhvam"
          | _ -> error_empty 19
          ] in 
      (person,fix stem suff) in 
  let conjugc = if entry = "k.r#1" then conjugroot else conjug in
  enter1 entry (Conju (injm 4)
   [ (Singular, 
        [ conjug First "i"
        ; conjugc Second "thaas"
        ; conjugc Third "ta"
        ])
   ; (Dual,
        [ conjug First  "vahi"
        ; conjug Second "aathaam"
        ; conjug Third  "aataam"
        ])
   ; (Plural, 
        [ conjug First  "mahi"
        ; conjugdhvam Second
        ; conjug Third "ata"
        ])
   ])
;
value isigma augm stem suff long_i = 
  let sfx = code suff in
  let sfx' = match sfx with 
    [ [ 4 (* ii *) :: _ ] -> sfx 
    | _ -> let ivoy = if long_i then 4 (* ii *) else 3 (* i *) in
           (* long i for root grah - Whitney§900b *)
           Int_sandhi.int_sandhi [ 47; ivoy ] (* i.s *) sfx
    ] in
  let form = sandhi stem sfx' in
  if augm then aug form else form
;
value compute_ath_is_aorista stem entry = 
  let long_i = (entry = "grah") in
  let conjug person suff = (person,isigma True stem suff long_i) in
  enter1 entry (Conju (aora 5) (sigma_paradigm conjug))
;
value compute_ath_is_injuncta stem entry = 
  let long_i = (entry = "grah") in
  let conjug person suff = (person,isigma False stem suff long_i) in
  enter1 entry (Conju (inja 5) (sigma_paradigm conjug))
;
value isigma_m_paradigm conjug conjugdhvam =
   [ (Singular, 
        [ conjug First  "i"
        ; conjug Second "thaas"
        ; conjug Third  "ta"
        ])
   ; (Dual,
        [ conjug First  "vahi"
        ; conjug Second "aathaam"
        ; conjug Third  "aataam"
        ])
   ; (Plural, 
        [ conjug First  "mahi"
        ; conjugdhvam Second
        ; conjug Third  "ata"
        ])
   ]
;
value compute_ath_is_aoristm stem entry = 
  let long_i = (entry = "grah") in
  let conjug person suff = (person,isigma True stem suff long_i)
  and conjugdhvam person = (person,fix_augment stem suff)
      where suff = (if long_i then "ii" else "i") ^ "dhvam" in
  enter1 entry (Conju (aorm 5) (isigma_m_paradigm conjug conjugdhvam))
;
value compute_ath_is_injunctm stem entry = 
  let long_i = (entry = "grah") in
  let conjug person suff = (person,isigma False stem suff long_i)
  and conjugdhvam person = (person,fix stem suff)
      where suff = (if long_i then "ii" else "i") ^ "dhvam" in
  enter1 entry (Conju (injm 5) (isigma_m_paradigm conjug conjugdhvam))
;
value sisigma augm stem suff = 
  let sfx = code suff in
  let ssfx = match sfx with 
    [ [ 4 :: _ ] -> [ 48 (* s *) :: sfx ]
    | _ -> Int_sandhi.int_sandhi [ 47; 3; 48 ] (* si.s *) sfx
    ] in 
  let form = sandhi stem ssfx in
  if augm then aug form else form
;
value compute_ath_sis_aorista stem entry = 
  let conjug person suff = (person,sisigma True stem suff) in
  enter1 entry (Conju (aora 6) (sigma_paradigm conjug))
;
value compute_ath_sis_injuncta stem entry = 
  let conjug person suff = (person,sisigma False stem suff) in
  enter1 entry (Conju (inja 6) (sigma_paradigm conjug))
;
value sasigma augm stem suff = 
  let sfx = fix [ 48 ] (* s *) suff in
  let form = sandhi stem sfx in
  if augm then aug form else form
;
value sa_aorist_a conjug =
   [ (Singular,  
        [ conjug First  "am"
        ; conjug Second "as"
        ; conjug Third  "at" (* secondary (shorter) ending Whitney§542 *)
        ]) 
   ; (Dual,
        [ conjug First  "aava"
        ; conjug Second "atam"
        ; conjug Third  "ataam"
        ])
   ; (Plural,
        [ conjug First  "aama"
        ; conjug Second "ata"
        ; conjug Third  "an"
        ])
   ]
;
value compute_ath_sa_aorista stem entry = 
  let conjug person suff = (person,sasigma True stem suff) in
  enter1 entry (Conju (aora 7) (sa_aorist_a conjug))
;
value compute_ath_sa_injuncta stem entry = 
  let conjug person suff = (person,sasigma False stem suff) in
  enter1 entry (Conju (inja 7) (sa_aorist_a conjug))
;
value sa_aorist_m conjug =
   [ (Singular, 
        [ conjug First  "i"
        ; conjug Second "athaas"
        ; conjug Third  "ata"
        ])
   ; (Dual,
        [ conjug First  "aavahi"
        ; conjug Second "aathaam"
        ; conjug Third  "aataam"
        ])
   ; (Plural,
        [ conjug First  "aamahi"
        ; conjug Second "adhvam"
        ; conjug Third  "anta"
        ])
   ]
;
value compute_ath_sa_aoristm stem entry = 
  let conjug person suff = (person,sasigma True stem suff) in
  enter1 entry (Conju (aorm 7) (sa_aorist_m conjug))
;
value compute_ath_sa_injunctm stem entry = 
  let conjug person suff = (person,sasigma False stem suff) in
  enter1 entry (Conju (injm 7) (sa_aorist_m conjug))
;
value compute_root_aorista weak strong entry = 
  let conjugw person suff = (person,fix_augment weak suff) 
  and conjugs person suff = (person,fix_augment strong suff) in
  enter1 entry (Conju (aora 1)
   [ (Singular, if entry = "bhuu#1" then (* Whitney§830 *)
        [ (First, code "abhuuvam") (* RV abhuvam *)
        ; conjugw Second "s"
        ; conjugw Third  "t"
        ] else (* Whitney§831 *)
        [ conjugs First "am"
        ; conjugs Second "s" 
        ; conjugs Third  "t"
        ])
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural, 
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; (Third, match weak with
            [ [ 2 (* aa *) :: r ] 
                -> fix_augment r "ur"
            | [ 41; 1; 43; 17 ] (* kram *) (* Whitney§833a *)
                -> fix_augment weak "ur" (* also yam dabh n.rt mand *)
            | [ 6; 40 ] (* bhuu *) -> code "abhuuvan"
            | [ 41; 1; 19 ] (* gam *) -> code "agman"
            | _ -> fix_augment weak "an"
            ])
        ])
   ])
;
value compute_root_injuncta weak strong entry = 
  let conjugw person suff = (person,fix weak suff) 
  and conjugs person suff = (person,fix strong suff) in
  enter1 entry (Conju (inja 1)
   [ (Singular, if entry = "bhuu#1" then 
        [ (First, code "bhuuvam") 
        ; conjugw Second "s"
        ; conjugw Third  "t"
        ] else 
        [ conjugs First "am"
        ; conjugs Second "s"
        ; conjugs Third  "t"
        ])
   ; (Dual,
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural, 
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; (Third, match weak with
            [ [ 2 (* aa *) :: r ] -> fix r "ur"
            | [ 6; 40 ] (* bhuu *) -> code "bhuuvan"
            | [ 41; 1; 19 ] (* gam *) -> code "gman"
            | _ -> fix weak "an"
            ])
        ])
   ])
;
value compute_root_aoristm stem entry = (* rare *)
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (aorm 1) (conjugs_past_m conjug))
;
value compute_root_injunctm stem entry = (* rare *)
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (injm 1) (conjugs_past_m conjug))
;
value compute_root_aoristp stem entry = (* passive aorist Whitney§843 *)
  (* \Pan{3,1,60-66} suffix ci.n usage réflexif-passif agent/objet karmakart.r *)
  (* TODO use Kümmel 1996 for Vedic plural 3rd forms *)
  let conjug person suff = (person,fix_augment stem suff) in
  let conju3 = Conju aorp1 [ (Singular,[ conjug Third "i" ]) ] in
  enter1 entry conju3
;
value compute_root_injunctp stem entry = (* passive injunctive ? *)
  let conjug person suff = (person,fix stem suff) in
  let conju3 = Conju injp1 [ (Singular,[ conjug Third "i" ]) ] in
  enter1 entry conju3
;
(* identical to [compute_thematic_impfta] *)
value compute_thematic_aorista stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (aora 2) (thematic_preterit_a conjug))
;
value compute_thematic_injuncta stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (inja 2) (thematic_preterit_a conjug))
;
(* identical to [compute_thematic_impftm] *)
value compute_thematic_aoristm stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (aorm 2) (thematic_preterit_m conjug))
;
value compute_thematic_injunctm stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (injm 2) (thematic_preterit_m conjug))
;
(* identical to [compute_thematic_impfta] *) 
(* de Saussure (Memoire sur le systeme primitif des voyelles dans les langues IE)
   says: reduplicated aorists represent imperfects of a verbal class. *) 
value compute_redup_aorista stem entry =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (aora 3) (thematic_preterit_a conjug))
  (* NB Macdonnel dixit -- Gonda says "ur" for Third Plural *)
;
value compute_redup_injuncta stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (inja 3) (thematic_preterit_a conjug))
;
(* identical to [compute_thematic_impftm] *)
value compute_redup_aoristm stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (aorm 3) (thematic_preterit_m conjug))
;
value compute_redup_injunctm stem entry = 
  let conjug person suff = (person,fix stem suff) in
  enter1 entry (Conju (injm 3) (thematic_preterit_m conjug))
;
value amui = fun (* root with a amui - used in [redup_aor] *)
  [ "kath" -> True (* \Pan{7,4,93} *)
  | _ -> False
  ]
;
(* Reduplication for aorist/injunctive *)
value redup_aor weak root = 
  let mess = "Redup_aor " ^ root in 
  match rev weak with (* ugly double reversal *)
    [ [] -> error_empty 20
    | [ c1 :: r ] -> 
      if vowel c1 then match c1 with (* very rare - Whitney§862 *)
         [ 1 (* a *) -> match r with
             [ [ c2 ] -> weak @ [ c2; 1 (* a *)] (* am aorist aamamat *)
             | _ -> failwith mess
             ] 
         | 4 (* ii *) -> match r with
             [ [ 17; 47 ] (* iik.s *) -> revcode "iicik.s"
             | _ -> failwith mess
             ] 
         | 7 (* .r *) -> match r with
             [ [ 22 ] (* .rc1 *) -> revcode ".rcic"
             | _ -> failwith mess
             ] 
         | _ -> failwith mess
         ] 
      else 
      let (v,heavy) = lookvoy r 
          (* heavy syllable = long vowel, or 
                              short before two consonants (long by position) *)
         where rec lookvoy = fun
           [ [] -> failwith mess
           | [ c2 ] -> if vowel c2 then (c2,not (short_vowel c2))
                       else failwith mess
           | [ c2 :: r2 ] -> if vowel c2 then 
                                let h = if short_vowel c2 then mult r2
                                        else True in
                                (c2,h) 
                             else lookvoy r2
           ] 
      and c = if sibilant c1 then match r with
           [ [] -> failwith mess
           | [ c2 :: _ ] -> if vowel c2 then c1
                            else if nasal c2 then c1
                            else if stop c2 then c2
                            else (* semivowel c2 *) c1
           ] else c1 in
      let rv = (* rv is reduplicating vowel *)
        if v = 5 then match root with
           [ "dru#1" | "zru" | "stu" | "sru" -> 5 
           | "dyut#1" -> 3 (* also "zru" azizravat (WR) *)
           | _ -> 6 (* u -> uu *)
           ]
        else if v = 6 then 5 (* uu \R u *)
        else match root with
             [ "klid" | "tvar" | "tvi.s#1" | "zri" | "grah" | "vrazc" -> 3 
             | "j~naa#1" | "sthaa#1" | "hlaad" (* hidden heavy since stem in i *)
                 -> 3 
             | "gaah" (* heavy exception *) -> 4  
             | _ -> if heavy || amui root then 
                       if v=1 || v=2 || v=7 then 1 (* Whitney§860 *) 
                       else 3 (* short \R ii, long \R i *) (* \Pan{7,4,93} *)
                    else 4 
             ] 
      and rc = match c with (* c is reduplicating consonant *)
        [ 17 | 18 (* k kh *) -> 22 (* c *)
        | 19 | 20 | 49 (* g gh h *) -> 24 (* j *)
        | 23 | 25 | 28 | 30 | 33 | 35 | 38 | 40 -> c-1 (* xh \R x *)
        | _ -> c
        ] 
      and strengthened = match root with
        [ "ji" -> revcode "jay" 
        | _ -> match weak with
               [ [ c :: r ] -> 
                 if vowel c then match c with
                            [ 3 | 4 (* i ii *) -> [ 42 (* y *) :: weak ]
                            | 5 | 6 (* u uu *) -> [ 45 (* v *) :: weak ]
            (* or [ 45 :: [ 1 :: r ] ] (stu) 'atu.s.tavam tu.s.t'avat RV (WR) *)
                            | 7 | 8 (* .r .rr *) -> [ 43 :: [ 1 (* ar *) :: r ] ]
                            | _ -> weak (* Whitney§866-868 *)
                            ]            
                 else weak
               | _ -> error_empty 21
               ]
        ] in
      revaffix True [ rv; rc ] strengthened
    ]
;
value compute_aorist entry =
  let (weak,strong,long) = stems entry in do (* 7 formations *)
  { match entry with (* 1. root aorist - Panini sic-luk *)
    [ "k.r#1" | "kram" | "gam" | "gaa#1" | "jan" | "j~naa#1" 
    | "daa#1" | "daa#2" | "dhaa#1" | "dhaa#2" | "paa#1" | "bhuu#1" | "muc#1" 
    | "zaa" | "saa#1" | "sthaa#1" | "has" | "haa#1" -> do
      { compute_root_aorista weak strong entry 
      ; match entry with
        [ "k.r#1" | "gam" | "jan" -> compute_root_aoristm weak entry (* rare *) 
        | "sthaa#1" (* Whitney§834a. *) ->
                    compute_root_aoristm (revstem "sthi") entry (* asthita *) 
        | "dhaa#1" -> compute_root_aoristm (revstem "dhi") entry
        | _ -> ()
        ]
      ; let stem = if entry = "muc#1" then strong else match long with 
            [ [ 2 (* aa *) :: _ ] -> [ 42 (* y *) :: long ]
            | _ -> long
            ] in 
        compute_root_aoristp stem entry (* passive *)
      (* For root aorist participles, see Whitney§840 and Burrow p178 *)
      (* For optative mode Whitney§837 see benedictive/precative. *)
      }
    | "prii" -> let st = revcode "priiyaa" in compute_root_aorista st st entry 
    | "svid#2" -> let st = revcode "svidyaa" in compute_root_aorista st st entry
    | "iik.s" | "m.r" -> compute_root_aoristm weak entry
    (* Now other passive/impersonal aorist in -i *)
    | "vac" -> do (* passive aorist *)
      { compute_root_aoristp long entry 
      ; compute_root_aoristp (revcode "voc") entry 
      }
    | "p.rr" -> compute_root_aoristp (revcode "puur") entry 
    | "kaaz" |  "k.sip" | "diip" | "duh#1" | "d.rz#1" | "dvi.s#1" | "budh#1"
    | "yuj#1" | "vid#1" | "s.rj#1" 
        -> compute_root_aoristp strong entry 
    | "rabh" -> compute_root_aoristp (revcode "rambh") entry 
    | "ci" | "jaag.r" | "t.rr" | "pac" | "pad#1" | "zru" | "stu" | "hu"
        -> compute_root_aoristp long entry
           (* NB "zru" -> azraavi WR while Whitney§844a *azraayi typo *) 
    | _ -> () (* "i" -> iiyaat hard *)
    ]
  ; match entry with (* 2. thematic aorist af *)
    [ "aap" | "krudh" | "gam" | "g.rdh" | "ghas" | "das" | "dyut#1" | "muc#1" 
    | "yuj#1" | "ric" | "ruc#1" | "rudh#2" | "ruh" | "vid#2" | "v.rt#1" 
    | "zuc#1" | "zudh" | "sic" | "stan" | "huu" 
     -> do
      { compute_thematic_aorista weak entry
      ; compute_thematic_aoristm weak entry (* middle very rare *)
      }
    | "vyaa" -> let stem = revcode "vi" in do
      { compute_thematic_aorista stem entry
      ; compute_thematic_aoristm stem entry 
      }
    | "zak" | "zuu" | "zcut#1" | "zram" -> compute_thematic_aorista weak entry
    | "zru"   -> compute_thematic_aorista (revcode "zrav") entry
    | "khyaa" -> compute_thematic_aorista (revcode "khy") entry
    | "as#2"  -> compute_thematic_aorista (revcode "asth") entry
    | "pat#1" -> compute_thematic_aorista (revcode "papt") entry
    | "vac"   -> compute_thematic_aorista (revcode "voc") entry 
    | (* roots in .r or .rr take strong stem *)
      ".r" | "d.rz#1" -> compute_thematic_aorista strong entry
    | _ -> () 
    ]
  ; match entry with (* 3. reduplicated aorist caf *)
    [ "am" | ".rc#1" | "kath" | "k.r#1" | "k.r.s" | "k.lp" | "ga.n" | "gam"
    | "gaah" | "car" | "ce.s.t" | "jan" | "ji" | "tvar" | "tvi.s#1" | "dah#1"
    | "diz#1" | "dih" | "diip" | "dru#1" | "dh.r" | "naz" | "pac" | "pa.th"
    | "miil" | "muc#1" | "yaj#1" | "rak.s" | "ric" | "viz#1" | "v.r#1" 
    | "v.rt#1" | "vyadh" | "zri" | "zru" | "stu" (* | "dhaa#1" *) -> 
      let stem = redup_aor weak entry in do
      { compute_redup_aorista stem entry (* but atu.s.tavam RV (WR) *)  
      ; compute_redup_aoristm stem entry 
      }
    | "iik.s" | "kamp" | "klid" | "gup" | "cur" | "m.r" | "d.rz#1" | "dyut#1" 
    | "vrazc" | "siiv" | "sru" -> (* active only *)
      let stem = redup_aor weak entry in 
      compute_redup_aorista stem entry
    | "grah" -> do 
      { let stem = redup_aor (revcode "grah") entry in do
        { compute_redup_aorista stem entry
        ; compute_redup_aoristm stem entry 
        }
      ; let stem = redup_aor (revcode "grabh") entry in do (* ved Whitney§223g *)
        { compute_redup_aorista stem entry
        ; compute_redup_aoristm stem entry 
        }
      }
    | "daa#1" -> let stem = (revcode "diidad") (* ad hoc *) in do
        { compute_redup_aorista stem entry
        ; compute_redup_aoristm stem entry 
        }
      (* then exceptions to treatment of aa with intercalaring ii *)
    | "raadh" -> let stem = redup_aor (revcode "radh") (* riiradh *) entry in  
                 compute_redup_aorista stem entry (* Macdonnel p 126 *)
    | "haa#1" -> let stem = revcode "jiijah" in 
                 compute_redup_aorista stem entry
    | _ -> () 
    ]
  ; match entry with (* reduplicated aorist - extra forms, secondary conjs *)
    [ "naz" -> compute_redup_aorista (revcode "nez") entry
    | _ -> () 
    ]
  ; match entry with (* 4. sigma aorist sic *)
    [ "aap" | "k.r#1" | "khan" | "gup" | "chid#1" | "ji" | "tud" | "t.rr" 
    | "tyaj#1" | "dah#1" | "daa#1" | "d.rz#1" | "draa#2" | "dhaa#1" | "dhyaa"
    | "dhyai" | "dhv.r" | "nak.s" | "nii#1" | "pac" | "praz" | "prii" 
    | "budh#1" | "bhaa#1" | "bhii#1" | "muc#1" | "yaj#1" | "yuj#1" | "ram" 
    | "labh" | "v.r#2" | "vyadh" | "zru" | "sidh#1" | "s.rj#1" | "stu" 
    | "sp.rz#1" | "hu" -> do
      { let stem = match entry with
            [ "d.rz#1" | "s.rj#1" | "sp.rz#1" -> long_metathesis weak
            | "ram" -> weak 
            | _ -> long
            ] in
        compute_ath_s_aorista stem entry 
      ; match entry with (* Whitney§890 *)
            [ "khan" (* akhaan *)
            | "dah#1" (* adhaak *)
            (* | "d.rz1" adraak wrong *adaar.t below TODO use [ar_ra] *)
            | "yaj#1" (* ayaa.t *)
            (* | "s.rj1" asraak wrong *asaar.t below *)
              -> let lopa = sigma True long "" in
                 enter1 entry (Conju (aora 4) [ (Singular,[ (Third, lopa) ]) ])
            | _ -> ()
            ]
      ; if entry = "yuj#1" || entry = "chid#1" 
           then compute_ath_s_aorista strong entry else ()
        (* ayok.siit and acchetsiit besides ayauk.siit and acchaitsiit *)
      ; match entry with
        [ "gup" | "d.rz#1" | "s.rj#1" -> ()  (* active only *)
        | _ -> let stemm = match weak with
            [ [ c :: r ] -> match c with 
                [ 3 | 4 | 5 | 6 (* i ii u uu *) -> strong
                | 2 (* aa *) -> [ 3 :: r ]
                | 7 (* .r *) -> if entry = "dhv.r" then revcode "dhuur" else weak
                | _ -> weak
                ]
            | _ -> error_empty 22
            ] in compute_ath_s_aoristm stemm entry 
        ]
      }
    | "vrazc" -> let stem = revcode "vraak" in (* as for future *) 
                 compute_ath_s_aorista stem entry 
    | "spaz#1" | "smi" | "haa#2" -> compute_ath_s_aoristm weak entry (* middle only *)
    | _ -> ()
    ]
  ; match entry with (* 5. i.s aorist se.t-sic *)
    [ "ak.s" | "aj" | "aas#2" | "i.s#1" | "iik.s" | "uk.s" | "uc" | "u.s" 
    | "uuh" | ".rc#1" | "k.rt#1" | "krand" | "kram" | "k.san"  | "khan"
    | "car" | "ce.s.t" | "jap" | "jalp" | "jaag.r" | "t.rr" | "diip"
    | "pa.th" | "puu#1" | "p.rc"| "pru.s#1" | "baadh" | "budh#1" | "mad#1" 
    | "mud#1" | "muurch" | "mlecch" | "yaac" | "ruc#1" | "lu~nc" | "luu#1"
    | "vad" | "vadh" | "vid#1" | "v.r#1" | "vraj" | "z.rr" | "sidh#2" 
    | "skhal" | "stan" | "stu" | "hi.ms" -> do
      { let stem = match weak with
            [ [ 7 (* .r *) :: _ ] -> 
              if entry = "jaag.r" then strong (* jaagari.sam RF IC 2 p 88 *)
              else long (* avaariit *)
            | [ 8 (* .rr *) :: _ ] -> 
              if entry = "z.rr" then strong (* azariit *)
              else long 
            | [ c :: _ ] -> 
              if vowel c then long 
              else match entry with 
                   [ "kan" | "khan" |"car" | "mad#1" | "vad" | "skhal" -> long 
                   | _ -> strong
                   ]
            | [] -> error_empty 23
            ] in
        compute_ath_is_aorista stem entry 
      ; compute_ath_is_aoristm strong entry 
      } 
    | "ku.s" | "gup" | "vrazc" | "zcut#1" | "sphu.t" -> (* active only *)
      compute_ath_is_aorista strong entry 
    | "zuu" -> 
      compute_ath_is_aorista (revcode "zve") entry 
    | "kan" | "k.r#2"| "p.rr" -> (* active only *)
      compute_ath_is_aorista long entry 
    | "kamp" | "jan" | "zii#1" | "spand" -> (* middle only *)
      compute_ath_is_aoristm strong entry 
    | "grah" -> do 
      { let stem = revcode "grah" in do (* same as group above *)
        { compute_ath_is_aorista stem entry 
        ; compute_ath_is_aoristm stem entry 
        } 
      ; let stem = revcode "grabh" in do (* supplement (ved) -- Whitney§900b *)
        { compute_ath_is_aorista stem entry 
        ; compute_ath_is_aoristm stem entry 
        } 
      }
    | _ -> ()
    ]
  ; match entry with (* 6. si.s aorist se.t-sic *)
    [ "j~naa#1" | "dhyaa" | "dhyai" | "nam" | "paa#2" | "mnaa" | "yaa#1" | "laa" 
    | "zaa" -> do (* dhyai for dhyaa *)
      { compute_ath_sis_aorista strong entry 
      ; compute_ath_is_aoristm strong entry (* is aorist (5) used in middle *)
      }
    | _ -> ()
    ]
; match entry with (* 7. sa aorist ksa *)
      [ "kruz" | "kliz" | "guh" | "diz#1" | "dih" | "duh#1" | "lih#1" | "viz#1"
      | "v.rj" | "sp.rz#1" -> do (* \Pan{7,3,72-73} *)
      { compute_ath_sa_aorista weak entry   
      ; if entry = "kruz" || entry = "kliz" then ((* Para *)) 
        else compute_ath_sa_aoristm weak entry 
      }
    | "pac" -> do (* Kiparsky apaak.sam *)
      { compute_ath_sa_aorista long entry 
      ; compute_ath_sa_aoristm long entry 
      }
    | _ -> ()
    ]
  }
;
(* First approximation: we compute same forms as corresponding aorists. *)
(* Then restriction to attested usage *)
value compute_injunctive entry =
  let (weak,strong,long) = stems entry in do (* 7 families *)
  { match entry with (* 1. root injunct *)
    [ "gam" | "gaa#1" | "bhuu#1" -> do
      { compute_root_injuncta weak strong entry 
      ; if entry = "gam" then compute_root_injunctm weak entry (* rare *) else ()
      ; let stem = match long with
            [ [ 2 (* aa *) :: _ ] -> [ 42 (* y *) :: long ]
            | _ -> long
            ] in 
        compute_root_injunctp stem entry (* passive *)
      }
    | "k.r#1" -> compute_root_injunctm weak entry
    | _ -> () 
    ]
  ; match entry with (* 2. thematic injunct *)
    [ "gam" | "g.rdh" | "zuc#1" -> do
      { compute_thematic_injuncta weak entry
      ; compute_thematic_injunctm weak entry (* middle is very rare *)
      }
    | "zram" -> compute_thematic_injuncta weak entry (* zramat *)
    | "vac" -> compute_thematic_injuncta (revcode "voc") entry (* vocat *) 
    | "zru" -> compute_thematic_injuncta (revcode "zrav") entry (* zravat *)
    | _ -> () 
    ]
  ; match entry with (* 3. reduplicated injunct *)
    [ "k.r#1" | "gam" -> 
      let stem = redup_aor weak entry in do
      { compute_redup_injuncta stem entry
      ; compute_redup_injunctm stem entry 
      }
    | _ -> () 
    ]
  ; match entry with (* 4. sigma injunct *)
    [ "k.r#1" | "chid#1" | "tyaj#1" | "pac" | "praz" | "bhii#1" | "sidh#1" -> do
      { let stema = long in
        compute_ath_s_injuncta stema entry 
      ; if entry = "chid#1" then compute_ath_s_injuncta strong entry else ()
        (* cchetsiit besides cchaitsiit *)
      ; let stemm = match weak with
            [ [ c :: r ] -> match c with 
               [ 3 | 4 | 5 | 6 (* i ii u uu *) -> strong
               | 2 (* aa *) -> [ 3 :: r ] (* turn aa to i *)
               | _ -> weak
               ]
            | _ -> error_empty 24
            ] in
        compute_ath_s_injunctm stemm entry 
      }
    | _ -> ()
    ]
  ; match entry with (* 5. i.s injunct *)
    [ "ak.s" | "aj" | "aas#2" | "i.s#1" | "iik.s" | "uk.s" | "uc" | "u.s" 
    | "uuh" | ".rc#1" | "k.rt#1" | "krand" | "kram" | "k.san"  | "khan"  | "car" 
    | "ce.s.t" | "jalp" | "jaag.r" | "t.rr" | "diip" | "pa.th" 
    | "puu#1" | "p.rc" | "baadh" | "budh#1" | "mad#1" | "mud#1" | "muurch" 
    | "mlecch" | "yaac" | "ruc#1" | "lu~nc" | "luu#1" | "vad" | "vadh" 
    | "vid#1" | "v.r#1" | "vraj" | "z.rr" | "sidh#2" | "skhal" | "stan"
    | "stu" | "hi.ms" -> do
      { let stem = match weak with
            [ [ 7 (* .r *) :: _ ] -> 
              if entry = "jaag.r" then strong (* jaagari.sam RF IC 2 p 88 *)
              else long (* avaariit *)
            | [ 8 (* .rr *) :: _ ] -> 
              if entry = "z.rr" then strong (* azariit *)
              else long 
            | [ c :: _ ] -> 
              if vowel c then long 
              else match entry with 
                   [ "kan" | "khan" |"car" | "mad#1" | "vad" | "skhal" -> long 
                   | _ -> strong
                   ]
            | [] -> error_empty 25
            ] in
        compute_ath_is_injuncta stem entry 
      ; compute_ath_is_injunctm strong entry 
      } 
    | "gup" | "vrazc" | "zcut#1" | "sphu.t" -> (* active only *)
      compute_ath_is_injuncta strong entry 
    | "zuu" -> 
      compute_ath_is_injuncta (revcode "zve") entry 
    | "kan" | "k.r#2"| "p.rr" -> (* active only *)
      compute_ath_is_injuncta long entry 
    | "kamp" | "jan" | "zii#1" | "spand" -> (* middle only *)
      compute_ath_is_injunctm strong entry 
    | "grah" -> do 
      { let stem = revcode "grah" in do (* same as group above *)
        { compute_ath_is_injuncta stem entry 
        ; compute_ath_is_injunctm stem entry 
        } 
      ; let stem = revcode "grabh" in do (* supplement (ved) -- Whitney§900b *)
        { compute_ath_is_injuncta stem entry 
        ; compute_ath_is_injunctm stem entry 
        } 
      }
    | _ -> ()
    ]
  } (* injunctives of kinds 6. and 7. missing *)
 ;
(* Aorist of causative *)
value compute_redup_aorista_ca stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (caaora 3) (thematic_preterit_a conjug))
  (* NB Macdonnel dixit -- Gonda says "ur" for Third Plural *)
;
value compute_redup_aoristm_ca stem entry = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 entry (Conju (caaorm 3) (thematic_preterit_m conjug))
;
value compute_aor_ca cpstem entry = 
  match entry with 
  [ (* Whitney§861b *) "j~naa#1" | "daa#1" | "sthaa#1" 
    (* Henry§339: *)
  | "diip" (* adidiipat *)
  | "du.s" (* aduudu.sat *)
  | "ri.s" (* ariiri.sat *)
  | "p.r#1" (* apiiparat *)
  | "t.rr" (* atiitarat *)
  | "vah#1" (* aviivahat *) 
  | "hlaad" (* ajihladat *) 
(*| "jan"  (* wrong *ajijiinat for ajiijanat *)
  | "sp.rz#1" (* wrong *apii.spazat for apisp.rzat *) TODO *) ->
      match cpstem with (* cpstem-ayati is the ca stem *)
     [ [ 37 :: [ 2 :: w ] ] ->  (* w-aapayati *)
         let voy = if entry = "daa#1" then 1 (* a *)
                   else 3 (* i *) (* aap -> ip Whitney§861b *) in
         let istem = [ 37 :: [ voy :: w ] ] in
         let stem = redup_aor istem entry in do 
         { compute_redup_aorista_ca stem entry (* ati.s.thipat adiidapat *)
         ; compute_redup_aoristm_ca stem entry 
         }
     | [ 37 :: [ 1 :: _ ] ] -> 
         let stem = redup_aor cpstem entry in do 
         { compute_redup_aorista_ca stem entry (* ajij~napat *)
         ; compute_redup_aoristm_ca stem entry 
         }
     | [ c :: w ] -> 
         let (v,light,r) = look_rec True w
             where rec look_rec b = fun
             [ [ ] -> error_empty 26
             | [ x :: w' ] -> if vowel x then (x,b && short_vowel x,w')
                              else look_rec False w' 
             ] in
         let voy = match v with
              [ 5 (* u *) -> 6 
              | 6 (* uu *) -> 5
              | 1 | 2 -> if light then 4 (* ii *)
                     else 1 (* a *) 
              | _ -> if light then 4 (* ii *)
                     else 3 (* i *) 
              ] in
         let istem = [ c :: [ voy :: r ] ] in
         let stem = redup_aor istem entry in do 
         { compute_redup_aorista_ca stem entry (* adidiipat *) 
         ; compute_redup_aoristm_ca stem entry  
         }
     | _ -> error_empty 27
     ] 
  | _ -> () 
  ] 
;

(************************************************************************)
(* Periphrastic future, Infinitive, Passive future participle in -tavya *)
(************************************************************************)

value compute_peri_fut conj perstem entry = 
  let conjug person suff = (person,sandhi perstem (code suff)) in
  enter1 entry (Conju (conj,Perfut Active)
   [ (Singular, 
        [ conjug First  "taasmi"
        ; conjug Second "taasi"
        ; conjug Third  "taa"
        ] )
   ; (Dual,
        [ conjug First  "taasvas"
        ; conjug Second "taasthas"
        ; conjug Third  "taarau"
        ])
   ; (Plural,
        [ conjug First  "taasmas"
        ; conjug Second "taastha"
        ; conjug Third  "taaras"
        ])
   ])
;
value record_pfp_tavya conj perstem entry = 
  let pfp_stem = fix perstem "tavya" in
  record_part (Pfutp_ conj (rev pfp_stem) entry) (* rev compat entry by Pfpart *)
;
value build_infinitive c inf_stem root = do
(* By default, for Causative, we get eg bhaavayitum, and later forms such as 
   bhaavitum have to be entered as supplements; see Witney§1051c. *)
  { enter1 root (Invar (c,Infi) (fix inf_stem "tum"))
  ; enter1 root (Inftu c (fix inf_stem "tu")) (* Xtu-kaama compounds *)
(* NB. bahuv cpds in -kaama and -manas constructed with infinitives in -tu 
   See Renou HLS p72 from Patanjali; Renou grammaire §107 dagdhukaama
   also Assimil p194 eg tyaktukaama
   anu.s.thaatukaama "desirious to proceed" vaktukaama "who wants to speak"
   dra.s.tumanas "inclined to see" 
   dra.s.tuzakya "able to see" *)
  }
;
value perif conj perstem entry = do 
  { match entry with 
    [ "cint" -> () (* no future *)
    | _ -> compute_peri_fut conj perstem entry
    ]
  ; let inf_stem =  match conj with
        [ Primary -> (* Difference infinitive/tavya forms and peri-future *)
             match entry with (* should rather appear in perstems *)
             [ "g.rr#1" -> revcode "giri" (* giritum, not gariitum *) 
             | "jak.s"  -> revcode "jagh" (* jagdhum *)
             | "p.rr"   -> revcode "puuri" (* puuritum *)
             | "sva~nj" -> revcode "svaj" (* svaktum *)
             | "sa~nj"  -> revcode "saj" (* saktum *)
             | "s.rp"   -> revcode "sarpi" (* sarpitum *)
             | ".dii"   -> revcode ".dii" (* .diitum *)
             | _ -> perstem
             ] 
        | _ -> perstem 
        ] in 
    build_infinitive conj inf_stem entry (* pb saa1 setum WR -situm *)
  ; if admits_passive entry then record_pfp_tavya conj perstem entry else ()
  (* other pfps generated from [pfp_ya] et [pfp_aniiya] below *)
  }
;
(* Computes periphrastic future, infinitive and [pfp_tavya] Primary forms *)
value compute_perif rstem entry =
  let pstems = perstems rstem entry in 
  iter (fun st -> perif Primary (rev st) entry) pstems
;

(********************************************************************)
(* Passive future participle in -ya and -aniiya in all conjugations *)
(********************************************************************)

value palatal_exception root = List.mem root
  [ "aj"; "vraj" (* \Pan{7,3,60} *)
  ; "zuc#1" (* \Pan{7,3,59} zocya *) 
  ; "yaj#1"; "yaac"; "ruc#1"; ".rc#1" (* \Pan{7,3,66} *) 
  ; "tyaj#1" (* tyaajya Vaartika on \Pan{7,3,66} *)
  ; "s.rj#1"; "v.rj"; "p.rc" (* because of -kyap \Pan{3,1,110} *)
  ; "raaj#1" (* in order not to get raagya - unjustified by Panini ? *)
  ]
; 
value velarification rstem = (* \Pan{7,3,52} *) 
  match Word.mirror rstem with (* double rev *)
  [ [ c :: _ ] when velar c -> rfix rstem "ya" (* \Pan{7,3,59} *)
(* Actually the following velarification should be registered as an optional
form, since \Pan{7,3,65} says that it does not apply in the sense of necessity *)
  | _ -> let st = match rstem with (* [Int_sandhi.restore_stem] not needed *)
               [ [ 22 (* c *) :: [ 26 (* ~n *) :: r ] ] ->  
               [ 17 (* k *) :: [ 21 (* f *) :: r ] ]      (* vafkya *)
             | [ 22 (* c *) :: r ] -> [ 17 (* k *) :: r ] (* paakya vaakya *)
             | [ 24 (* j *) :: [ 24 (* j *) :: r ] ] ->
               [ 19 (* g *) :: [ 19 (* g *) :: r ] ]      (* bh.rggya *)
             | [ 24 (* j *) :: [ 26 (* ~n *) :: r ] ] ->
               [ 19 (* g *) :: [ 21 (* f *) :: r ] ]      (* safgya *)
             | [ 24 (* j *) :: r ] -> [ 19 (* g *) :: r ] (* maargya *)
             | _ -> rstem
             ] in 
         rfix st "ya"
  ]
; 
value record_pfp_ya conj ya_stem root =
  let pfp_stem = 
      if conj=Primary then 
         if palatal_exception root then rfix ya_stem "ya"
         else match root with 
              [ "hi.ms" -> revcode "hi.msya" (* no retroflex s Whitney§183a *)
              | _ -> velarification ya_stem (* .nyat *)
              ]
      else rfix ya_stem "ya" (* yat *) in
  record_part (Pfutp_ conj pfp_stem root) 
 ;
value record_pfp_aniiya conj iya_stem root =
  let pfp_stem = rfix iya_stem "aniiya" in
  record_part (Pfutp_ conj pfp_stem root) 
 ;
(* Primary conjugation pfp in -ya except for ganas 10 and 11 *)
value pfp_ya rstem entry =
  let (_, strong, long) = stems entry in
  (* NB we do not use [weak_stem] and thus rstem is not mrijified/duhified *)
  let ya_stem = match rstem with
    [ [ 1 :: _ ] -> rstem 
    | [ 2 :: r ] 
    | [ 11 (* ai *) :: r ] 
    | [ 12 (* o *) :: r ] 
    | [ 13 (* au *) :: r ] -> match entry with 
        [ "mnaa" | "zaa" | "saa#1" -> rstem (* mnaaya zaaya avasaaya *)
        | _ -> [ 10 :: r ] (* deya *)
        ]
    | [ 3 :: _ ] | [ 4 :: _ ] -> strong 
    | [ 5 (* u *) :: r ] -> match entry with 
        [ "stu" -> [ 45 :: [ 2 :: r ] ] (* u -> aav *)
        | "yu#1" -> [ 6 :: r ] (* u -> uu *)
        | "yu#2" -> raise Not_attested 
        | _ -> strong 
        ]
    | [ 6 (* uu *) :: _ ] -> match entry with 
        [ "huu" -> revcode "hav" (* havya WR (?) *)
        | _ -> strong 
        ] 
    | [ 7 (* .r *) :: _ ] -> match entry with 
        [ "dhv.r" -> strong (* dhvarya *)
        | "d.r#1" | "v.r#2" -> [ 32 :: rstem ] (* d.rtya v.rtya \Pan{3,1,109} *)
          (* others as supplementary forms with interc t in [record_pfp] below *)
        | _ -> long (* kaarya (k.rt .nyat) \Pan{3,1,124} *)
        ] 
    | [ 8 (* .rr *) :: _ ] ->  match entry with 
        [ "st.rr" -> strong (* starya *)
        | _ -> long
        ] 
      (* now consonant rules - order of patterns important *)
    | [ 22; 7 ] (* .rc *) 
    | [ 24; 7 ] (* .rj *) -> strong (* arc arj *)
    | [ 24; 7; 41 ] (* m.rj *) -> long (* maarj \Pan{7,2,114} *)
    | [ 47; 7 ] (* .r.sya autonomous *)
    | [ 32; 7; 17 ] (* k.rt *) -> raise Not_attested (* k.rtya comes from k.r1 *)
    | [ 48; 1 ] (* as1 *) -> 
            if entry = "as#1" then raise Not_attested (* use bhuu *) 
                              else rstem (* asya - may overgenerate *)   
    | [ 48; 1; 46 ] (* zas *) -> rstem 
    | [ 48; 2; 46 ] (* zaas *) -> revcode "zaa.s" (* zaa.sya + zi.sya extra *)
    | [ 33; 36; 1; 43; 19 ] (* granth *) -> revcode "grath"  
    | [ 35; 1; 45 ] (* vadh/han *) -> rstem (* vadhya *) 
    | [ 36; 1; 49 ] (* han *) -> revcode "ghaat" (* (h=h') \Pan{7,3,32+54} *)
    | [ 35; 1; 42; 45 ] (* vyadh *) -> revcode "vedh"
    | [ 46; 1; 43; 37 ] (* praz *) -> revcode "p.rcch"
    | [ 46; 1; 37 ] (* paz *) -> raise Not_attested (* pazya WR -Panini *)
    | [ 46; 1; 45 ] (* vaz *) -> rstem (* vazya (?) *)
    | [ 49; 43; 1 ] (* arh *) -> revcode "argh" (* arghya (h=h') *)
    | [ 17; 1; 46 ] (* zak *) 
    | [ 49; 1; 48 ] (* sah *) -> rstem (* zakya sahya \Pan{3,1,99} -yat *) 
    | [ 43; 1; 22 ] (* car *) -> rstem (* carya \Pan{3,1,100} -yat *) 
                                       (* but caarya obtainable as ca -.nyat *) 
    (* NB car gad mad yam also take -yat \Pan{3,1,100}: [record_extra_pfp_ya] *) 
    | [ 24; 1 ] (* aj *) -> rstem (* ajya *)
    | [ c :: [ 1 :: _ ] ] when labial c -> rstem  (* \Pan{3,1,98} -yat *) 
    | [ c :: [ 1 :: r ] ] -> [ c :: [ 2 :: r ] ] 
                      (* a lengthened if last non labial *)
                      (* above often optional, see [record_extra_pfp_ya] below *)
    | [ c :: [ 7 :: _ ] ] -> rstem (* d.rz1 v.r.s but NOT m.rj *)
    | [ c :: [ v :: _ ] ] when short_vowel v (* gunify *) -> strong
    | _ -> rstem
    ] in 
  record_pfp_ya Primary ya_stem entry 
 ;
(* Primary conjugation pfp in -ya for gana 10 *)
value pfp_ya_10 rstem entry =
  let pfp_stem = rfix rstem "ya" in
  record_part (Pfutp_ Primary pfp_stem entry) 
 ;
(* Primary conjugation pfp in -aniiya *)
value pfp_aniiya rstem entry =
  let iya_stem = 
     match entry with 
     [ "uk.s" | "cint" -> rstem (*i others ? PB [strong_stem] ? i*)
     | "yu#1" | "yu#2" -> raise Not_attested 
     | "dham" -> revcode "dhmaa"  (* \Pan{7,3,78} *)
     | "vyadh" -> revcode "vedh"
     | _ -> match Word.mirror rstem with
            [ [ 4 :: _ ] | [ 6 :: _ ] -> rstem (* ii- uu- no guna *) 
            | _ -> strong_stem entry rstem
            ]
     ] in 
  record_pfp_aniiya Primary iya_stem entry 
 ;
value record_pfp_10 entry rstem = do
  { try pfp_ya_10  rstem entry with [ Not_attested -> () ]
  ; try pfp_aniiya rstem entry with [ Not_attested -> () ]
  }
;

(**********************************)
(* Absolutive and Past Participle *)
(**********************************)

value record_part_ppp ppstem entry = do 
  { record_part (Ppp_ Primary ppstem entry)
  ; record_part (Pppa_ Primary ppstem entry) (* pp-vat (krit tavat) *)
  }
;
value record_abso_ya form entry   = enter1 entry (Invar (Primary,Absoya) form) 
  and record_abso_tvaa form entry = enter1 entry (Absotvaa Primary form)
;
(* First absolutives in -ya *)
value record_abs_ya entry rstem w = do
  (* intercalate t for light roots Kiparsky[159] Macdonell§165 *)
  { let absya = 
       if light rstem then fix w "tya" (* check test light *)
       else let rst = match entry with
            [ (* roots in -m and -n in gana 8 \Pan{6,4,37} *)
                "van" | "man" | "tan#1" (* man also in gana 4 *)
            | "gam" | "nam" | "yam" | "han#1" (* anudatta ? *)
            | "kram" | "klam" | "zam#2" | "zram" (* \Pan{6,4,15} *)
            | "daa#2" | "saa#1" | "sthaa#1" | "maa#1" (* \Pan{7,4,40} *)
            | "daa#1" (* \Pan{7,4,46} *)
            | "dhaa#1" (* \Pan{7,4,42} *)
                   -> rstem 
            | "zii#1" -> revcode "zay" (* \Pan{7,4,22} *)
            | "arh" -> revcode "argh" (* arghya (h=h') *)
            | _ -> w
            ] in match entry with
                 [ "hi.ms" -> code "hi.msya" (* no retroflex s Whitney§183 *)
                 | _ -> fix rst "ya" 
                 ] in
    record_abso_ya absya entry
  ; match entry with (* alternate forms in -ya and -tvaa *)
    [ "gam" | "tan#1" | "nam" | "man" | "van" | "han#1" ->
      (* a+nasal optional assimilation to light roots *)
        record_abso_ya (fix w "tya") entry
    | "dhaa#1" -> record_abso_tvaa (code "dhitvaa") entry 
    | "plu"    -> record_abso_ya (code "pluuya") entry
    | "b.rh#1" -> record_part_ppp (revcode "b.r.mhita") entry (* MW -WR *) 
    | "v.r#2" -> do { record_abso_tvaa (code "varitvaa") entry
                    ; record_abso_tvaa (code "variitvaa") entry 
                    }
    | "kram" -> record_abso_tvaa (code "krantvaa") entry (* \Pan{6,4,18} *)
    | "zaas" -> (* passive stem zi.s *)
        let w = revcode "zi.s" in do (* as if ipad=0 *)
        { record_part_ppp (rfix w "ta") entry 
        ; record_abso_tvaa (fix w "tvaa") entry
        ; record_abso_ya (fix w "ya") entry
        }
    | _ -> ()
    ]  
  }
;
value alternate_pp = fun
  [ "m.r.s" | "svid#2" | "dh.r.s" | "puu#1" (*i \Pan{?} i*)
    (* next roots of gu.na 1 have penultimate "u" *)
  | "kul" | "k.sud" | "guh" | "jyut" | "dyut#1" | "mud#1" | "rud#1" | "ruh#1"
  | "lul" | "zuc#1" | "zubh#1" | "zu.s" -> True
  | _ -> False
  ]
;
(* Condition for extra abs in -tvaa with guna: root starts with consonant
   and ends in any consonant but y or v and has i or u as penultimate. 
   Given by \Pan{1,2,26}. Example: sidh1 *)
value alternate_tvaa entry rstem =
  match Word.mirror rstem with (* double rev *)
  [ [ c :: _ ] -> consonant c && match rstem with
      [ [ 42 (* y *) :: _ ] | [ 45 (* v *) :: _ ] -> False
      | [ c' :: rest ] -> consonant c' && match rest with 
          [ [ 3 (* i *) :: _ ] | [ 5 (* u *) :: _ ] -> True | _ -> False ]
      | _ -> False
      ]
  | _ -> match entry with
         [ "t.r.s#1" | "m.r.s" | "k.rz" (* \Pan{1,2,25} *)
         | "puu#1" (* \Pan{1,2,22} *) -> True 
         | _ -> False
         ]
  ]
;
(* Records the (reversed) ppp stem (computed by [compute_ppp_stems])
   and builds absolutives in -tvaa and -ya ( should be separated some day). *)
value record_ppp_abs_stems entry rstem ppstems =
  let process_ppstem = fun
     [ Na w -> do 
        { record_part_ppp (rfix w "na") entry 
        ; let stem = match entry with (* roots in -d *) 
            [ "k.sud" | "chad#1" | "chid#1" | "ch.rd" | "tud#1" | "t.rd" | "nud" 
            | "pad#1" | "bhid#1" | "mid" | "vid#2" | "zad" | "sad#1" | "had" 
            | "svid#2" -> match w with 
                          [ [ 36 (* n *) :: r ] -> [ 34 (* d *) :: r ] 
                          | _ -> failwith "Anomaly Verbs"
                          ] 
            | "vrazc" -> revcode "v.rz" (* not v.rk *)
            | "und" | "skand" | "syand" -> [ 34 (* d *) :: w ]
            | _ -> w 
            ] in match entry with 
            [ "mid" -> 
                    let abs_mid st = record_abso_tvaa (fix st "itvaa") entry in
                    do { abs_mid stem; abs_mid (revcode "med") (* guna *)}
            | _  -> do { record_abso_tvaa (fix stem "tvaa") entry
                       ; record_abso_ya (fix stem "ya") entry 
                       }
            ]
        }
     | Ka w -> do 
         { record_part_ppp (rfix w "ka") entry (* zu.ska \Pan{8,2,51} *)
         ; record_abso_ya  (fix w "ya")  entry
         }
     | Va w -> do 
         { record_part_ppp  (rfix w "va")  entry 
         ; record_abso_tvaa (fix w "tvaa") entry
         ; record_abso_ya   (fix w "ya")   entry
         }
     | Ta w -> do
         { if is_anit_pp entry rstem then record_part_ppp (rfix w "ta") entry
           else ((* taken care of as Tia *))
         ; if is_anit_tvaa entry rstem then record_abso_tvaa (fix w "tvaa") entry
           else ((* taken care of as Tia *))
         ; (* abs -ya computed whether set or anit *) 
           match entry with 
           [ "av" -> record_abs_ya entry rstem (revcode "aav") (* -aavya *)
           | _    -> record_abs_ya entry rstem w
           ]
         }
     | Tia w -> let (ita,itvaa) = if entry = "grah" then ("iita","iitvaa")  
                                  else ("ita","itvaa") in do
         { if is_set_pp entry rstem then 
              match entry with
              [ "dh.r.s" | "zii#1" (* "svid#2" "k.svid" "mid" \Pan{1,2,19} *)
                 -> record_part_ppp (rfix (strong w) ita) entry
              | _ -> do
                { record_part_ppp (rfix w ita) entry
                ; if alternate_pp entry then 
                     record_part_ppp (rfix (strong w) ita) entry
                  else ()
                }
              ]
           else ()
         ; if is_set_tvaa entry rstem then do 
              { let tstem = match entry with
                    [ "m.rj" -> lengthened rstem (* maarj *)
                    | "yaj#1" | "vyadh" | "grah" | "vrazc" | "praz" | "svap"  
                    | "vaz" | "vac" | "vap" | "vap#1" | "vap#2" | "vad" 
                    | "vas#1" | "vas#4" -> w
                    | "siiv" -> revcode "sev" (* gu.na *)
                    | "stambh" -> rstem (* stabhita but stambhitvaa! *)
                    | _ -> strong w 
                    ] in
                record_abso_tvaa (fix tstem itvaa) entry
              ; if alternate_tvaa entry rstem then 
                   record_abso_tvaa (fix w "itvaa") entry
                else ()
              }
           else ()
         }
    ] in 
  iter process_ppstem ppstems
;
(* Simple version for denominatives - tentative *)
value record_ppp_abs_den ystem entry = 
 let ppstem = trunc (revstem entry) in do  
  { record_part_ppp (rfix ppstem "ita") entry 
  ; match entry with
    [ "aakar.na" -> record_abso_tvaa (fix ppstem "ya") entry (* fake abso-ya! *)
    | _ -> record_abso_tvaa (fix ystem "itvaa") entry 
    ]
  (* no general [record_abso_ya] since usually no preverb to denominatives *)
  }
;
(* Absolutive in -am - Macdonell§166 Stenzler§288 \Pan{3,4,22} .namul          *)
(* Registered both in Invar and in Absotvaa, since may be used with preverbs.  *)
(* Used specially for verbs that may be iterated - having done again and again *)
value record_abso_am root = 
  let record form = let word = code form in do 
      { record_abso_tvaa word root (* no preverb *)
      ; record_abso_ya   word root (* some preverb *)
      } in 
  match root with 
  [ "as#2"    -> record "aasam" (* may overgenerate *)
  | "ka.s"    -> record "kaa.sam" (* \Pan{3,4,34} *)
  | "kram"    -> record "kraamam"
  | "k.r#1"   -> record "kaaram" (* \Pan{3,4,26-28} *)
  | "khan"    -> record "khaanam"
  | "grah"    -> record "graaham"
  | "jiiv"    -> record "jiivam" (* \Pan{3,4,30} *)
  | "j~naa#1" -> record "j~naayam"
  | "t.r.s#1" -> record "tar.sam"
  | "daa#1"   -> record "daayam"
  | "naz#1"   -> record "naazam"
  | "paa#1"   -> record "paayam"
  | "pi.s"    -> record "pe.sam" (* \Pan{3,4,35+38} *)
  | "pu.s#1"  -> record "po.sam" (* \Pan{3,4,40} *)
  | "puur#1"  -> record "puuram" (* \Pan{3,4,31} *)
  | "praz"    -> record "p.rccham"
  | "bandh"   -> record "bandham" 
  | "bhuj#1"  -> record "bhojam"
  | "bhuu#1"  -> record "bhaavam"
  | "vad"     -> record "vaadam"
  | "v.rt#1"  -> record "vartam" (* \Pan{3,4,39} *)
  | "zru"     -> record "zraavam"
  | "sa~nj"   -> record "sa~ngam"
  | "s.r"     -> record "saaram"
  | "s.rp"    -> record "sarpam"
  | "skand"   -> record "skandam"
  | "stambh"  -> record "stambham"
  | "han"     -> record "ghaatam" (* \Pan{3,4,36+37} *)
  | "knuu"    -> record "knopam" (* from causative *)
  | _ -> ()
  ]
(* NB Bandharkar: colloquial expressions iic+V.namul suivi de forme finie de V *)
(* eg "hastagraaha.m g.r.naati" il tient par la main *)
(* Should be also definable for causative, eg knopam ca{knuu} \Pan{3,4,33} *)
;
(* absolutive of secondary conjugations *)
value record_absolutive c abs_stem_tvaa abs_stem_ya intercal entry = 
  let record_abso_ya form = enter1 entry (Invar (c,Absoya) form) 
  and record_abso_tvaa form = enter1 entry (Absotvaa c form) in do
  { let sfx = if intercal then "itvaa" else "tvaa" in
    record_abso_tvaa (fix abs_stem_tvaa sfx)
  ; record_abso_ya   (fix abs_stem_ya "ya")
  }
;
value record_pppca cpstem cstem entry =
  let ppstem = [ 1 :: [ 32 :: [ 3 :: cpstem ] ] ] (* cp-ita *) in do 
  { record_part (Ppp_ Causative ppstem entry)
  ; record_part (Pppa_ Causative ppstem entry) (* pp-vat *)
  ; let abs_stem_ya = match entry with (* Whitney§1051d *)
        [ "aap" | ".r" | ".rc#1" | ".rdh" | "kal" | "k.lp" | "kram" | "gam" 
        | "jan" | "jval" | "dh.r" | "rac" | "zam#1" | "p.rr" | "bhak.s" | "v.rj" 
            -> cstem  (* retains ay: -gamayya to distinguish from -gamya *)
        | _ -> cpstem (* eg -vaadya -vezya *)
        ] 
    and abs_stem_tvaa = cstem (* retains ay: gamayitvaa *) in
    record_absolutive Causative abs_stem_tvaa abs_stem_ya True entry 
       (* cp-ita -> cp-ayitvaa, -cp-ayya ou -cp-ya *)
  }
;
value record_pppdes stem entry =
  let ppstem = [ 1 :: [ 32 :: [ 3 :: stem ] ] ] in (* s-ita *) do
  { record_part (Ppp_ Desiderative ppstem entry)
  ; record_part (Pppa_ Desiderative ppstem entry) (* pp-vat *)
  ; let abs_stem_tvaa = [ 3 :: stem ] (* s-i *) 
    and abs_stem_ya = stem in
    record_absolutive Desiderative abs_stem_tvaa abs_stem_ya False entry 
       (* s-ita -> s-itvaa, -s-iya *)
  }
;

(******************************)
(* Intensive or frequentative *)
(******************************)

value compute_intensive_presenta strong weak iiflag entry =
(* info not used for check because of ambiguity of third sg - we want no
   error message in the conjugation engine display *)
  let conjugs person suff = (person,fix strong suff) 
  and conjugw person suff = (person,fix3w weak iiflag False suff) in do
  { enter1 entry (Conju intensa 
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugw First  "iimi"
        ; conjugs Second "si"
        ; conjugw Second "iisi"
        ; conjugs Third  "ti" 
        ; conjugw Third  "iiti" (*i PB: generates *daridreti for [draa#1] i*)
        ])
   ; (Dual, 
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ])
   ; (Plural,
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; conjugw Third  "ati"
        ])
   ])
  ; let wstem = if iiflag then match weak with 
         [ [ 4 :: w ] -> w (* ii disappears before vowels in special roots *)
         | _ -> failwith "Wrong weak stem of special intensive"
         ]         
                else weak in (* 3rd pl weak stem *)
    record_part (Pprared_ Intensive wstem entry) 
  ; if entry = "draa#1" then let ppstem = revcode "daridrita" in
                             record_part (Ppp_ Intensive ppstem entry) 
    else ((* TODO *))
  }
;
value compute_intensive_impfta strong weak iiflag entry =
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix3w_augment weak iiflag False suff) in
  enter1 entry (Conju intimpfta 
   [ (Singular, 
        [ conjugs First  "am"
        ; conjugs Second "s" 
        ; conjugw Second "iis"
        ; conjugs Third  "t"
        ; conjugw Second "iit"
        ])
   ; (Dual, 
        [ conjugw First  "va"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugw First  "ma"
        ; conjugw Second "ta"
        ; conjugw Third  "ur"
        ])
   ])
;
value compute_intensive_optativea weak iiflag entry =
  let conjugw person suff = (person,fix3w weak iiflag False suff) in
  enter1 entry (conjug_optativea int_gana Intensive conjugw)
;
value compute_intensive_imperativea strong weak iiflag entry =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag False suff) in
  enter1 entry (Conju intimpera
   [ (Singular, 
        [ conjugs First "aani"
        ; (Second, match weak with 
            [ [ c :: _  ] -> fix3w weak iiflag False suff 
              where suff = if vowel c then "hi" (* "dhi" or "hi" after vowel *)
                            else "dhi"
            | _ -> error_empty 28
            ] ) 
        ; conjugs Third  "tu"
        ; conjugs Third  "iitu"
        ])
   ; (Dual, 
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  "atu"
        ])
   ])
;
(* Reduplication for the intensive conjugation - TODO Macdonell§173 
[value redup_int entry = ...]
For the moment, the reduplicated stem is read from the lexicon. 
It is not clear whether there are enough intensive forms to warrant a paradigm
rather than a table. *)

(* Similar to [compute_active_present3] with Intensive, plus optional ii forms *)
value compute_intensivea wstem sstem entry third = 
  let iiflag = False in (*i TEMP - TODO i*)
  (* [let (sstem,wstem) = redup_int entry in] *) do 
  { compute_intensive_presenta sstem wstem iiflag entry (* no third *)
  ; compute_intensive_impfta sstem wstem iiflag entry
  ; compute_intensive_optativea wstem iiflag entry 
  ; compute_intensive_imperativea sstem wstem iiflag entry 
  ; if entry="bhuu#1" (* bobhoti *) then
       let stem = revcode "bobhav" in build_perpft Intensive stem entry
    else () (* EXPERIMENTAL *)
  }
;
(* Takes reduplicated stem from lexicon. A generative version would use 
   [redup3] and add "ya" like passive *)
value compute_intensivem = compute_thematic_middle int_gana Intensive 
and compute_intensivem2 st = 
  compute_athematic_present3m Intensive int_gana st False 
;

(******************)
(* Present system *)
(******************)
value compute_present_system entry rstem gana pada third = 
  try
   (* pada=True for active (parasmaipade), False for middle (aatmanepade) *)
   let padam = if third=[] then False else pada in (* artifact for fake below *)
   match gana with
   [ 1 | 4 | 6 | 10 (* thematic conjugation *) -> 
     let compute_thematic_present stem = 
         match voices_of_gana gana entry with
         [ Para -> (* active only *) if pada then
             compute_thematic_active gana Primary stem entry third
             else emit_warning ("Unexpected middle form: " ^ entry)
         | Atma -> (* middle only *)  
             if padam then emit_warning ("Unexpected active form: " ^ entry) 
             else compute_thematic_middle gana Primary stem entry third
         | Ubha -> 
             let thirda = if pada then third else []
             and thirdm = if pada then [] else third in do
             { compute_thematic_active gana Primary stem entry thirda
             ; compute_thematic_middle gana Primary stem entry thirdm
             }
         ] in 
     match gana with
     [ 1 -> match entry with 
            [ "kram" -> do (* 2 forms Whitney§745d *)
               { compute_thematic_present rstem 
               ; compute_thematic_present (revcode "kraam") (* lengthen *)
               }
            | "cam" -> do (* 2 forms Whitney§745d *)
               { compute_thematic_present rstem 
               ; compute_thematic_present (revcode "caam") (* lengthen *)
               }
            | "t.rr" -> do (* 2 forms *)
               { compute_thematic_present (revcode "tir") 
               ; compute_thematic_present (revcode "tar") 
               }
            | "manth" -> do (* 2 forms *)
               { compute_thematic_present rstem 
               ; compute_thematic_present (revcode "math") (* suppr nasal *)
               }
            | "a~nc" -> do (* 2 forms *)
               { compute_thematic_present rstem 
               ; compute_thematic_present (revcode "ac") (* suppr nasal *)
               }
            | "uuh" -> do (* 2 forms *)
               { compute_thematic_present rstem 
               (* [ compute_thematic_middle 1 Primary (strong rstem) entry 
                    (if pada then [] else third) ] (* ohate ved *) *)
               }
            | "huu" -> do (* 2 forms *) (* hvayati, havate *)
               { compute_thematic_present (revcode "hve") 
               ; compute_thematic_middle 1 Primary (revcode "hav") entry
                    (if pada then [] else third) (* havate *)
               }
            | _ -> let stem = match entry with 
              [ ".r"     -> revcode ".rcch"  (* \Pan{7,3,78} Whitney§747 *)
              | "gam"    -> revcode "gacch" (* \Pan{7,3,77} Whitney§747 *)
              | "yam"    -> revcode "yacch" (* \Pan{7,3,77} *)
              | "yu#2"   -> revcode "yucch" 
              | "kuc"    -> revcode "ku~nc" (* add nasal *)
              | "da.mz"  -> revcode "daz" (* suppr penult nasal \Pan{6,4,25} *)
              | "ra~nj"  -> revcode "raj" (* id *)
              | "sa~nj"  -> revcode "saj" (* id *)
              | "sva~nj" -> revcode "svaj" (* id *)
              | "daa#1"  -> revcode "dad" (* dupl Whitney§672 ved *)
                 (* \Pan{7,3,78}: yacch for prayacch in meaning of giving  *)
                 (* also "s.r"  -> "dhau" (corresponds to dhaav1)
                         "dmaa" -> "dham" (cf ppstem) *)
              | "dhaa#1"  -> revcode "dadh" (* id *) 
              | "paa#1"   -> revcode "pib" (* fake 3rd gana \Pan{7,3,78} *)
              | "ghraa"   -> revcode "jighr"     (* id \Pan{7,3,78} *)
              | "sthaa#1" -> revcode "ti.s.th" (* id \Pan{7,3,78} *)
              | "d.rh"    -> revcode "d.r.mh" (* .rh -> .r.mh *)
              | "b.rh#1"  -> revcode "b.r.mh" (* WR; Bucknell adds barhati *)
              | "iir.s" | "gaa#2" (* = gai *)
              | "daa#3" | "dyaa" | "dhyaa" | "pyaa" (* = pyai *)
              | "zu.s" | "zyaa" | "sphaa" -> [ 42 (* y *) :: rstem ] (* add y *)
              | "maa#4" -> revcode "may" (* shorten add y *)
              | "vyaa"  -> revcode "vyay"
              | "zuu"   -> revcode "zve" (* zvayati - similar to huu/hve *)
              | "knuu"  -> revcode "knuuy" 
              | "guh"   -> revcode "guuh" (* lengthen \Pan{6,4,89} *)
              | "grah"  -> revcode "g.rh.n" (* WR *)
              | "das"   -> revcode "daas" 
              | "mnaa"  -> revcode "man" (* \Pan{7,3,78} *)
              | "zad"   -> revcode "ziiy" (* \Pan{7,3,78} *)
              | "sad#1" -> revcode "siid" (* \Pan{7,3,78} *)
              | ".sad"  -> revcode ".siid" (* fictive retro-root of sad1 *)
              | "m.rj"  -> mrijify (revcode "maarj") (* vriddhi *)
              | "yaj#1" | "vraj" | "raaj#1" | "bhraaj" -> mrijify rstem
              | "kliiba" | "puula" -> (* kliibate etc *) (* denominative verbs *)
                  Phonetics.trunc_a rstem (* since thematic a added *)
              | "k.rp" -> rstem 
              | _ -> strong rstem (* default *)
              ] in compute_thematic_present stem  
            ]
     | 4 -> let weak = match entry with
            [ "bhra.mz" -> revcode "bhraz" (* suppr penult nasal *)
            | "ra~nj"   -> revcode "raj"   (* id *)
            | "i"       -> revcode "ii"
            | "jan"     -> revcode "jaa"
            | "kan"     -> revcode "kaa"
            | "klam"    -> revcode "klaam"
            | "j.rr"    -> revcode "jiir"
            | "jyaa#1"  -> revcode "jii"
            | "tam"     -> revcode "taam"
            | "dam#1"   -> revcode "daam"
            | "daa#2"   -> revcode "d"
            | "d.rz#1"  -> raise Not_attested (* replaced by paz \Pan{7,3,78} *)
            | "dhaa#2"  -> revcode "dha"
            | "bhram"   -> revcode "bhraam"
            | "mad#1"   -> revcode "maad"
            | "mid"     -> revcode "med"
            | "ri"      -> revcode "rii"
            | "vaa#3"   -> revcode "va" (* bizarre - should be ve class 1 *)
            | "vyadh"   -> revcode "vidh"
            | "zam#1"   -> revcode "zaam" 
            | "zaa"     -> revcode "z"
            | "zram"    -> revcode "zraam"
            | "saa#1"   -> revcode "s"
            | _ -> rstem
            ] in 
         let ystem = [ 42 :: weak ] (* root-y *) in
         compute_thematic_present ystem 
     | 6 -> let stem = match rstem with
            [ [ 3 :: rest ] | [ 4 :: rest ]-> [ 42 :: [ 3 :: rest ] ] 
              (* -.i -> -iy eg [k.si] [pii] *)
            | [ 5 :: rest ] | [ 6 :: rest ] -> [ 45 :: [ 5 :: rest ] ] 
              (* -.u -> -uv eg [dhru] also kuu -> kuv *)
            | [ 7 :: rest ] -> [ 42 :: [ 3 :: [ 43 :: rest ] ] ] (* mriyate *)
            | [ 8 :: rest ] -> match entry with
                   [ "p.rr" -> revcode "p.r.n" (* ugly duckling *)
                   | _ -> [ 43 :: [ 3 :: rest ] ] (* .rr/ir *)
                   ]
              (* -.rr -> -ir eg [t.rr] *)
            | _ -> match entry with
                   [ "i.s#1"  -> revcode "icch" (* \Pan{7,3,78} *)
                   | "vas#4"  -> revcode "ucch"
                   | ".rj"    -> revcode ".r~nj" 
                   | "k.rt#1" -> revcode "k.rnt" 
                   | "piz#1"  -> revcode "pi.mz"
                   | "muc#1"  -> revcode "mu~nc"
                   | "rudh#2" -> revcode "rundh"
                   | "sic"    -> revcode "si~nc"
                   | "lip"    -> revcode "limp"
                   | "lup"    -> revcode "lump"
                   | "vid#2"  -> revcode "vind"
                   | "praz"   -> revcode "p.rcch" (* ra/.r *)
                   | "vrazc"  -> revcode "v.rzc"  (* id déploiement vocalique *)
                   | "s.rj"   -> mrijify rstem
                   | _ -> rstem (* root stem *)
                   ]
            ] in compute_thematic_present stem 
     | 10 -> let process10 y_stem = do
                 { compute_thematic_present y_stem 
                 ; build_perpft Primary y_stem entry
                 ; let perstem = [ 3 :: y_stem ] (* -ayi *) in
                   perif Primary perstem entry 
                 } in 
        match entry with 
        [ "tul" -> do (* 2 forms *)
            { process10 (revcode "tulay")
            ; process10 (revcode "tolay") (* guna *)
            }
        | "gup" -> process10 (revcode "gopay") (* guna *)
        | "mid" -> process10 (revcode "minday") (* nasal *)
        | _ -> let base_stem = strengthen_10 rstem entry in 
               let ystem = rev (sandhi base_stem [ 1; 42 ] (* ay *)) in 
               process10 ystem
        ] 
     | _ -> failwith "Anomaly Verbs"
     ] (* end of thematic conjugation *) 
   | 2 -> (* athematic conjugation: 2nd class (root class) *)
     let set = augment_ii entry 
     and sstem = strong_stem entry rstem 
     and wstem = if entry="as#1" then [ 48 ] else weak_stem entry rstem in do 
     { match voices_of_gana 2 entry with
       [ Para -> (* active only *) if pada then
          compute_active_present2 sstem wstem set entry third
          else emit_warning ("Unexpected middle form: " ^ entry)
       | Atma (* middle only *) -> 
          if padam then emit_warning ("Unexpected active form: " ^ entry)
          else compute_middle_present2 sstem wstem set entry third
       | Ubha ->
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present2 sstem wstem set entry thirda
          ; compute_middle_present2 sstem wstem set entry thirdm
          }
       ]
     ; match entry with (* special cases *)
       [ "as#1" -> (* rare middle forms of as *)
         compute_athematic_present2m sstem [ 48 ] set entry (code "ste")
(*[    | "vac" -> let weak = revcode "vaz" (* douteux -WR *) in
                  compute_athematic_present2m sstem weak set entry [] ]*)
       | _ -> ()
       ]
     }
   | 3 -> let (sstem,wstem,iiflag) = redup3 entry rstem in
          match voices_of_gana 3 entry with
       [ Para -> if pada then
          compute_active_present3 sstem wstem iiflag entry third
          else emit_warning ("Unexpected middle form: " ^ entry)
       | Atma -> 
          if padam then emit_warning ("Unexpected active form: " ^ entry)
          else compute_middle_present3 sstem wstem iiflag entry third
       | Ubha ->
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present3 sstem wstem iiflag entry thirda
          ; compute_middle_present3 sstem wstem iiflag entry thirdm
          }
       ] 
   | 5 -> (* athematic conjugation: 5th class *)
     let (stem,vow) = match rstem with 
         [ [ 36; 3 ]     (* in *)  -> ([ 3 ] (* i *),True) (* Whitney§716a *)
         | [ 5; 43; 46 ] (* zru *) -> ([ 7; 46 ] (* z.r *),True) 
         | [ 40 :: [ 41 :: r ] ] -> ([ 40 :: r ],False) (* skambh stambh *)
           (* possibly other penultimate nasal lopa ? *)
         | [ c :: rest ] -> if vowel c then ([ short c :: rest ],True)
                            else (rstem,False)  
         | [] -> error_empty 29
         ] in
     let wstem = rev (sandhi stem [ 36; 5 ]) (* stem-nu *)
     and sstem = rev (sandhi stem [ 36; 12 ]) (* stem-no *) in do
     { compute_present5 5 sstem wstem vow entry third pada padam
     ; if entry = "v.r#1" then (* extra derivation *)
          let wstem = revcode "uur.nu" and sstem = revcode "uur.no" in 
          compute_present5 5 sstem wstem True entry third pada padam
       else ()
     }
   | 7 -> (* athematic conjugation: 7th class *)
     match rstem with 
     [ [ c :: rest ] when consonant c ->
       let stem = match rest with 
           [ [ hd :: tl ] -> if nasal hd then tl else rest (* hi.ms *)
           | [] -> error_empty 30
           ] 
       and nasal = homonasal c in
       let wstem = 
         if entry = "t.rh" then revcode "t.rfh"
         else [ c :: rev (sandhi stem [ nasal ]) ] (* stem-n *) 
       and sstem = 
         if entry = "t.rh" then [ c :: rev (sandhi stem [ 36; 10 (* -ne *)]) ] 
         else [ c :: rev (sandhi stem [ 36; 1 ]) ] (* stem-na *) in 
       compute_present7 sstem wstem entry third pada padam 
     | _ -> warning (entry ^ " atypic 7\n")
     ]
   | 8 -> (* k.r1 k.san tan1 man san1 *)
     match rstem with 
     [ [ 36 (* n *) :: rest ] -> 
       let wstem = rev (sandhi rest [ 36; 5 ]) (* stem-nu *)
       and sstem = rev (sandhi rest [ 36; 12 ]) (* stem-no *) in
       compute_present5 8 sstem wstem True entry third pada padam
     | [ 7; 17 ] (* k.r *) -> 
       let wstem = revcode "kuru"
       and short = revcode "kur" (* before suffix -m -y -v Macdonell§134E *)
       and sstem = revcode "karo" in
       compute_presentk sstem wstem short entry third
     | _ -> warning (entry ^ " atypic 8\n")
     ]
   | 9 -> let (stem,vow) = match entry with (* vow = vowel ending root *)
        [ "j~naa#1" -> (revcode "jaa"  ,True) (* \Pan{7,3,79} *)
        | "jyaa#1"  -> (revcode "ji"   ,True)
        | "zraa"    -> (revcode "zrii" ,True)
        | "umbh"    -> (revcode "ubh"  ,False) (* elision penul nasal *)
        | "granth"  -> (revcode "grath",False) (* id *)
        | "bandh"   -> (revcode "badh" ,False) (* id *)
        | "skambh"  -> (revcode "skabh",False) (* id *)
        | "stambh"  -> (revcode "stabh",False) (* id *)
        | "grah"    -> (revcode "g.rh" ,False) (* plus "g.rbh" added below *)
        | "k.sii"   -> (revcode "k.si" ,True)
        | _ -> match rstem with 
            [ [ c :: w ] -> (st,vowel c) 
              where st = if c=6 (* uu *) then [ 5 :: w ] (* Whitney§728a *)
                    else if c=8 (* .rr *) then [ 7 :: w ] else rstem
            | [] -> error_empty 31
            ] 
        ] in (* Macdonell§127.6 *)
     (* NB Retroflexion prevented in k.subh: k.subhnaati \Pan{8,4,39} *)
     let retn = if Int_sandhi.retron stem then 
        if entry = "k.subh" then 36 (* n *) else 31 (* .n *)  else 36 (* n *) 
     and glue = if entry = "k.subh" then List2.unstack else sandhi in
     let sstem = rev (glue stem [ 36; 2 ]) (* stem-naa *) (* naa accented *) 
     and wstem = rev (glue stem [ 36; 4 ]) (* stem-nii *) (* nii unaccented *)
     and short = [ retn :: stem ] (* stem-n *) in do
     { compute_present9 sstem wstem short vow stem entry third pada padam
     ; if entry = "grah" then (* ved alternative form "g.rbh" Vt1 \Pan{8,2,35} *)
         let stem = revcode "g.rbh" in 
         let sstem = rev (sandhi stem [ 36; 2 ]) (* stem-naa *) 
         and wstem = rev (sandhi stem [ 36; 4 ]) (* stem-nii *) 
         and short = [ 31 :: stem ] (* stem-.n *) in
         compute_present9 sstem wstem short vow stem entry [] pada padam
       else ()
     }
   | 0 -> () (* secondary conjugations - unused in this version *)
   | _ -> failwith "Illegal present class"
   ] 
  with [ Not_attested -> () ]
(* end Present system *) 
;

(******************)
(* Passive system *)
(******************)

(* Passive future participle (gerundive) in -ya and -aniiya *)
value record_pfp entry rstem = do
  { try pfp_ya rstem entry with [ Not_attested -> () ]
  ; try pfp_aniiya rstem entry with [ Not_attested -> () ]
  ; (* Supplements *)
    let record_extra_pfp_ya form = 
        record_part (Pfutp_ Primary (revcode form) entry) in
    match entry with
    [ "k.r#1" (* \Pan{3,1,120} .duk.r~n + kyap *)
    | "stu" | "bh.r" | "i" | "m.r" -> (* \Pan{3,1,109} Renou§155e *)
      (* intercalate t after roots ending in short vowel Renou§146 *)
      let pfp_tya = rfix rstem "tya" in (* k.rtya stutya bh.rtya itya m.rtya *)
      record_part (Pfutp_ Primary pfp_tya entry)
    | "ju.s" -> record_extra_pfp_ya "ju.sya" (* jo.sya \Pan{3,1,109} *) 
    | "khan" -> do 
      { record_extra_pfp_ya "khaanya" (* add to khanya \Pan{3,1,123} *)
      ; record_extra_pfp_ya "kheya"   (* further \Pan{3,1,111} *)
      } 
    | "ji" -> do
      { record_extra_pfp_ya "jitya" (* Renou§155e \Pan{3,1,117} *)
      ; record_extra_pfp_ya "jayya" (* (jeya) \Pan{6,1,81} *)
      }
    | "k.sii"  -> record_extra_pfp_ya "k.sayya" (* (k.seya) \Pan{6,1,81} *)
    | "grah"   -> record_extra_pfp_ya "g.rhya" (* \Pan{3,1,119} *)
    | "cuu.s"  -> record_extra_pfp_ya "co.sya" 
    | "ci"     -> do 
      { record_extra_pfp_ya "caayya"
        (* \Pan{3,1,131} fire only with pari- upa- sam- *)
      ; record_extra_pfp_ya "citya" (* \Pan{3,1,131} in sense of fire *)
      }
    | "vad"    -> do 
      { record_extra_pfp_ya "udya"  (* \Pan{3,1,106} for brahmodya *)
      ; record_extra_pfp_ya "vadya" (* id for brahmavadya sn *)
      }
    | "bhuu#1" -> record_extra_pfp_ya "bhaavya" (* (bhavya) \Pan{3,1,123} *)
      (* NB "bhuuya" is lexicalized as noun - \Pan{3,1,107} *)
    | "m.rj"   -> record_extra_pfp_ya "m.rjya" (* (maargya) \Pan{3,1,113} *)
    | "yuj#1"  -> record_extra_pfp_ya "yugya"  (* (yogya) \Pan{3,1,121} *)
    | "v.r#2"  -> record_extra_pfp_ya "vare.nya" (* vara.niiya (-aniiya) *)
    | "guh"    -> record_extra_pfp_ya "guhya" (* Vart \Pan{3,1,109} *) 
    | "duh#1"  -> record_extra_pfp_ya "duhya" (* idem *)
    | "za.ms"  -> record_extra_pfp_ya "zasya" (* idem *)
    | "zaas"   -> record_extra_pfp_ya "zi.sya"  (* \Pan{3,1,109} *)
      (* Following examples show that gunification is often optional. *)
      (* Some of the following forms seem actually preferable. *)
    | ".r"     -> record_extra_pfp_ya "arya"  (* (aarya) \Pan{3,1,103} (owner) *) 
    | "kup"    -> record_extra_pfp_ya "kupya" (* (kopya) \Pan{3,1,114} *) 
    | "gad"    -> record_extra_pfp_ya "gadya" (* gaadya for pv- \Pan{3,1,100} *) 
    | "mad"    -> record_extra_pfp_ya "madya" (* maadya for pv- \Pan{3,1,100} *)
    | "tyaj#1" -> record_extra_pfp_ya "tyajya" (* for sa.mtyajya (tyaajya) *) 
    | "bhid#1" -> record_extra_pfp_ya "bhidya" (* \Pan{3,1,115} for river *) 
    | "d.rz#1" -> record_extra_pfp_ya "darzya" (* WR only RV *) 
    | "yaj#1"  -> record_extra_pfp_ya "yajya"  (* devayajya \Pan{3,1,123} *) 
    | "yat"    -> record_extra_pfp_ya "yatya"  (* Vart \Pan{3,1,97} -WR *)
    | "ruc#1"  -> record_extra_pfp_ya "rucya"  (* (rocya) \Pan{3,1,114} *) 
    | "va~nc"  -> record_extra_pfp_ya "va~ncya" (* \Pan{7,3,63} for motion *) 
    | "vah#1"  -> record_extra_pfp_ya "vahya"  (* (vaahya) \Pan{3,1,102} instr *)
    | "v.r.s"  -> record_extra_pfp_ya "var.sya" (* \Pan{3,1,120} (v.r.sya) *)
    | "sa~nj"  -> record_extra_pfp_ya "sajya"  (* for prasajya (not Paninian?) *)
    (* ? takya catya hasya *)
    | _ -> ()
    ]
  }
;

(**************************)
(* Gana 11. Denominatives *)
(**************************)

(* Denominative verbs are given ga.na 11, and their stems are computed by
[den_stem_a] and [den_stem_m] below (for Para and Atma respectively).
They are derivative verbs from dative forms of substantives.
Roots kept in ga.na 10 (debatable, this is subject to change), are:
   ka.n.d kath kal kiirt kuts ga.n garh gup gha.t.t cint cur .damb 
   tandr tark tul bharts m.r.d rac rah ruup lok suud sp.rh *)
(* Also gave.s, because possible ga.na 1 and pp - should be added separately *)
(* Also lelaa, which has a strange status (marked as verb rather than root) *)
(* asu is bizarre, lexicalized under asuuya *)

(* The next two functions are used only by the Grammar interface, the forms
   memorized are computed from the lexicalized 3rd sg form *)  
(* BEWARE. the entry forms given in the next two functions must be in normalized
   form - no non-genuine anusvaara 
   This should be replaced by the recording of the 3rd sg form, like others. *)
value den_stem_a entry = (* in general transitive Whitney§1059c *)
   let rstem = revstem entry in 
   match entry with
   [ "putrakaama" | "rathakaama" (* \Pan{3,1,9} *)
   | "sukha" | "du.hkha" (* also "adhvara" "m.rga" below *)
   | "i.sudhi" | "gadgada" (* \Pan{3,1,27} *)
   | "agada" (* Kale§660 *) | "iras" (* | "pu.spa" replaced by root pu.sp *)
       -> trunc rstem (* -()yati *) (* lopa *) 
   (* | "maarg" | "mok.s" | "lak.s" | "suuc" 
    -> [ 1 :: rstem ] (* -ayati *) presently roots class 10 *)
   | "kutsaa" | "maalaa" | "mudraa" | "medhaa" 
       -> [ 1 :: trunc_aa rstem ] (* -()ayati - shortening final aa *)
   | "udazru" 
       -> [ 1 :: trunc_u rstem ] (* -()ayati - final u becomes a *)
   | "agha" | "azana#2" | "azva" | "ka.n.du" | "khela" | "jihma" | "pramada" 
   | "lohita" | "mantu" | "manda" | "valgu" | "sakhi" | "samudra#1" 
     (* to become \Pan{3,1,13} kya.s *)
   | "asu" (* lexicalized under "asuuya" *)
       -> lengthen rstem (* lengthening -aayati *) 
   | "asuuya" (* "asu" lengthened *) | "gomaya" | "vyaya" (* euphony *)
       -> trunc (trunc rstem) 
   | "artha" | "veda" | "satya" (* \Pan{3,1,25} Vt. *)
       -> [ 1 :: [ 37 :: [ 2 :: trunc rstem ] ] ] (* -aapayati - interc p *) 
   (* |  (* very rare Whitney§1059d e.g. "putra" *)
       -> [ 3 :: trunc_a rstem ] (* -()iyati *) *)
   | "adhvara" | "tavi.sa" | "putra" | "praasaada" (* treat as \Pan{3,1,10} *)
   | "udaka" | "kavi" | "dhana" | "maa.msa" | "vastra" (* desire Kale§643 *) 
       -> [ 4 :: trunc rstem ] (* -()iiyati *) (* \Pan{3,1,8} kyac *)
   | "kart.r" -> [ 4 :: [ 43 :: trunc rstem ] ] (* .r -> rii  Kale§642 *)
   | "go"     -> [ 45 :: [ 1 :: trunc rstem ] ] (* o -> av    Kale§642 *) 
   | "nau#1"  -> [ 45 :: [ 2 :: trunc rstem ] ] (* au -> aav  Kale§642 *)
   | "raajan" -> [ 4 :: trunc (trunc rstem) ]   (* nasal amui Kale§642 *)
     (* now the general case: keep the nominal stem - to cause (transitive) *)
   | "a.mza" | "afka" | "afkha" | "andha" | "aparok.sa" | "apahasta" | "amitra"
   | "aakar.na" | "aakula" | "aavila" | "i.sa" | "unmuula" | "upahasta" 
   | "ka.thora" | "kadartha" | "kar.na" | "kalafka" | "kalu.sa" | "kavala"
   | "ku.t.ta" | "kusuma" | "kha.da" | "garva" | "gocara" | "gopaa" | "carca"
   | "cuur.na" | "chala" | "chidra" | "tantra" | "tapas" | "tarafga" | "taru.na"
   | "tuhina" | "da.n.da" | "deva" | "dola" | "dravat" | "dhiira#1"
   | "nirmuula" | "nuutana" | "pa.tapa.taa" | "pallava"
   | "pavitra" | "paaza" | "pi.n.da" | "pulaka" | "puula" | "pratikuula" 
   | "prati.sedha" | "pradak.si.na" | "prasaada" | "bhi.saj" | "mantra" 
   | "malina" | "mizra" | "mukula" | "mukhara" | "mu.n.da" | "muutra" 
   | "m.rga" | "yantra" | "rasa" | "ruuk.sa" | "lagha" (* u -> a *) 
   | "var.na" | "vaasa#3" | "vizada" | "vra.na" | "zaanta" | "zithila"
   | "zyena" | ".sa.n.dha" | "sapi.n.da" | "saphala" | "sabhaaja" | "saantva" 
   | "saavadhaana" | "suutra" | "stena" (* practice \Pan{3,1,15} *)
   | "u.sas" | "namas" | "varivas" (* do \Pan{3,1,19} *)
   | "udan" (* Kale§645 *)
   | "kelaa" | "rekhaa" | "tiras" | "uras" | "payas" (* Kale§660 *)
   | "vaac" (* consonant Kale§642 *)
   | "dantura" (* possess *)
   | "k.r.s.na" (* agir comme *)
   | "viira" | "zabda" | "tira" (* MW *) | "ma~njara" | "sraja" | "manas" 
       -> rstem (* -yati *) (* standard causative meaning *)  
   | "madhu" | "v.r.sa" (* also madhvasyati v.r.siiyati *) 
   | "k.siira" | "lava.na" (* also putra *)
       -> [ 48 :: rstem ] (* -syati *) (* Kale§643 *)
   | _ -> failwith ("Unknown denominative " ^ entry)
   ] 
;
value den_stem_m entry = (* in general intransitive or reflexive Whitney§1059c *)
   let rstem = revstem entry in 
   match entry with 
   [ "artha" | "i.sa" | "kuha" | "carca" | "manas" | "mantra" | "muutra" 
   | "m.rga" | "viira" | "safgraama" | "suutra" (* also zithila below *)
       -> rstem (* (a)-yate *) 
   | "asuuya" (* "asu" lengthened *) | "vyaya" (* euphony *)
       -> trunc (trunc rstem) 
   | "tavi.sa" | "citra" (* do \Pan{3,1,19} *) | "sajja"
       -> [ 4 :: trunc_a rstem ] (* -()iiyate *)
   | "arth" -> [ 1 :: rstem ] (* arthayate for lexicon access *)
   | "apsaras" | "sumanas" (* act as, become \Pan{3,1,11-12} *) 
   | "unmanas" 
   | "uu.sman" (* emit \Pan{3,1,16} *)
       -> lengthen (trunc rstem) (* final consonant dropped *)
     (* now the general case: lengthen the nominal vowel stem *)
   | "pa.tapa.taa" | "mahii#2" | "m.r.saa" 
   | "laalaa" | "svalpazilaa" | "vimanaa" 
   | "ajira" | "kalu.sa" | "k.rpa.na" | "kliiba" | "garva" | "jala" | "jihma"
   | "taru.na" | "nika.sa" | "parok.sa" | "piiyuu.savar.sa" | "pu.spa" | "priya"
   | "bh.rza" | "maalyagu.na" | "lohita" | "zalabha" | "zithila" | "ziighra" 
   | "zyaama" | "zyena" | "safka.ta"
   | "ka.n.du" | "karu.na" | "sukha" | "du.hkha" (* feel \Pan{3,1,18} *)
(* {sukhaadi,du.hkha,t.rpta,k.rcchra,asra,aasra,aliika,pratiipa,karu.na,so.dha}
   take suffix kyaf in -aayate (ga.na) *)
   | "t.rpta" (* -MW *)
   | "abhra" | "ka.nva" | "kalaha" | "k.sepa" | "megha" | "vaira" | "zabda" 
   | "z.rfga" (* do \Pan{3,1,17} *)
   | "durdina" | "sudina" | "niihaara" (* id. vaartika *)
   | "ka.s.ta" | "k.rcchra" (* strive to \Pan{3,1,14} *)
   | "romantha" (* practice \Pan{3,1,15} *)
   | "dhuuma" | "baa.spa" | "phena" (* emit \Pan{3,1,16} *)
   | "kurafga" | "pu.skara" | "yuga" | "vi.sa" | "zizu"  | "samudra#1" 
   | "gomaya" | "sa.mdhyaa"  (* resemble *)
   | "puru.sa" (* imitate *)
   | "k.r.s.na" | "manda" | "bhuusvarga" (* to become *)
       -> lengthen rstem (* reflexive causative middle to become \Pan{3,1,13} *)
   | _ -> failwith ("Unknown denominative " ^ entry)
   ] 
;
value compute_denom stem ystem entry = do (* other than present system - rare *)
  { build_perpft Primary ystem entry 
  ; let fsuf = revcode "i.sy" in (* rare - similar to [compute_future_10] *)
    compute_future (fsuf @ ystem) entry 
  ; let perstem = [ 3 :: ystem ] (* -yi *) in  
    perif Primary perstem entry 
  ; match stem with
    [ [ 1 :: rest ] -> 
        match entry with
        [ "asuuya" -> () (* wrong asya *)
        | "m.rga" -> () (* from m.rg *)
        | _ -> do (* experimental - rare acc. to Whitney *)
               { compute_passive_11 entry rest
               ; record_pfp_10 entry rest
               }
        ]
    | _ -> () (* specially wrong for consonant stems *)
    ]
  }
;
value compute_denominative_a entry third = 
  match Word.mirror third with
      [ [ 3 :: [ 32 :: [ 1 :: ([ 42 :: s ] as ystem) ] ] ] (* -yati *) -> do
            { compute_thematic_active 11 Primary ystem entry third
            ; compute_denom s ystem entry 
            ; record_ppp_abs_den ystem entry
            }
      | _ -> failwith ("Anomalous denominative " ^ Canon.decode third)
      ]
and compute_denominative_m entry third = 
  match Word.mirror third with
      [ [ 10 :: [ 32 :: [ 1 :: ([ 42 :: s ] as ystem) ] ] ] (* -yate *) -> do
            { compute_thematic_middle 11 Primary ystem entry third
            ; compute_denom s ystem entry
            ; record_ppp_abs_den ystem entry
            }
      | _ -> failwith ("Anomalous denominative " ^ Canon.decode third)
      ]
;
(* We use the lexicalized third stem *)
value compute_denominative entry pada third = 
  match third with
  [ [] (* fake *) -> do (* pada not informative, we try both *)
     { try let stem = den_stem_a entry in 
           let ystem = [ 42 :: stem ] in do
           { compute_thematic_active 11 Primary ystem entry third
           ; compute_denom stem ystem entry
           ; record_ppp_abs_den ystem entry
           }
       with [ Failure _ -> () ]
     ; try let stem = den_stem_m entry in 
           let ystem = [ 42 :: stem ] in do
           { compute_thematic_middle 11 Primary ystem entry third
           ; compute_denom stem ystem entry
           ; record_ppp_abs_den ystem entry
           }
       with [ Failure _ -> () ]
     }
  | _ -> if pada then (* Para *) compute_denominative_a entry third  
                 else (* Atma *) compute_denominative_m entry third  
  ]
;
(***************************)
(* Main conjugation engine *)
(***************************)
(* [compute_conjugs_stems : string -> Conj_infos.vmorph -> unit]           *)
(* Called by [compute_conjugs] and [fake_compute_conjugs] below            *)
(*        and [Conjugation.secondary_conjugs]                              *)
value compute_conjugs_stems entry (vmorph,aa) = do (* main *)
  { admits_aa.val := aa (* sets the flag for phantom forms for aa- preverb *)
  ; match vmorph with
 [ Conj_infos.Prim 11 pada third -> 
      (* note: pada of denominative verbs is lexicalized *)
      compute_denominative entry pada third
 | Conj_infos.Prim 10 pada third -> 
   (* root in gana 10, pada is True for Para, False for Atma of third form *)
   let rstem = revstem entry in (* root stem reversed *)  
   try do
   { (* Present system plus perif pft and future, infinitives and pfp-tavya *)
     compute_present_system entry rstem 10 pada third 
     (* missing: imperative in -taat Whitney§570-1 (post-vedic rare) *)
     (* Future and Conditional *) 
   ; compute_future_10 rstem entry 
     (* Passive *)
   ; let ps_stem = passive_stem entry rstem in 
     compute_passive_10 entry (strong ps_stem) 
   ; record_pfp_10 entry rstem  
     (* Ppp and Absolutives *)
   ; let ystem = rfix rstem "ay" 
     and ppstem = rfix rstem "ita" in do  
     { record_part_ppp ppstem entry 
     ; record_abso_tvaa (fix ystem "itvaa") entry
     ; let ya_stem = if light_10 rstem then ystem else rstem in
       record_abso_ya (fix ya_stem "ya") entry 
     }
     (* No Perfect -- periphrastic perfect generated by process10 above *)
   }
   with [ Control.Warning s -> output_string stdout (s ^ "\n") ]
 | Conj_infos.Prim gana pada third -> 
   (* gana is root class, pada is True for Para, False for Atma of third form *)
   (* Primary conjugation *)
   let rstem = revstem entry in (* root stem reversed *)  
   try do
   { compute_present_system entry rstem gana pada third (* Present system *)
   ; (* Future and Conditional *) 
     match entry with
     [ "ifg" | "paz" | "cint" (* d.rz cit *)
     | "bruu" (* vac *)
     | "k.saa" | "cud" | "dhii#1" | "pat#2" |"praa#1" | "vidh#1" | "zlath"
        -> () (* no future *)
     | "tud#1" | "cakaas" -> () (* only periphrastic *)
     | "bharts" -> compute_future_gen rstem entry (* exception gana 10 *)
     | "umbh" -> do { compute_future_gen (revcode "ubh") entry (* 2 forms *)
                    ; compute_future_gen rstem entry
                    }
     | "saa#1" -> do { compute_future_gen (revcode "si") entry
                     ; compute_future_gen rstem entry
                     }
     | "vyadh" -> compute_future_gen (revcode "vidh") entry 
     | "zuu" -> compute_future_gen (revcode "zve") entry 
     | "knuu" -> compute_future_gen (revcode "knuuy") entry 
     | _ -> compute_future_gen rstem entry 
     ]
   ; (* Periphrastic future, Infinitive, Passive future part. in -tavya *)
     match entry with
     [ "ifg" | "paz" (* for d.rz *) | "bruu" (* for vac *) 
     | "k.saa" | "cud" | "dhii#1" | "pat#2" | "praa#1" | "vidh#1"
     | "haa#2" -> () (* no perif *)
     | "saa#1" -> do { compute_perif (revcode "si") entry 
                     ; compute_perif rstem entry
                     }
     | "vyadh"  -> compute_perif (revcode "vidh") entry 
     | "zuu"    -> compute_perif (revcode "zve") entry 
     | "knuu"   -> compute_perif (revcode "knuuy") entry 
     | "stambh" -> compute_perif (revcode "stabh") entry 
     | _ -> compute_perif rstem entry 
     ]
   ; (* Precative/Benedictive active rare, middle very rare in classical skt *)
     compute_benedictive rstem entry 
   ; (* Passive *)
     if admits_passive entry then 
        let ps_stem = passive_stem entry rstem in do
        { if entry = "arh" || entry = "k.lp" then () (* admits pfp but no ps *)
          else compute_passive Primary entry ps_stem 
          (* Passive future participle (gerundive) in -ya and -aniiya *)
        ; record_pfp entry rstem 
        }
     else ()
   ; (* Ppp computation and recording (together with absolutives) *)
     if admits_ppp_abs entry then do 
        { let ppstems = compute_ppp_stems entry rstem in 
          record_ppp_abs_stems entry rstem ppstems
        ; record_abso_am entry (* rare *)
        }
     else ()
   ; (* Perfect *) 
     match entry with
     [ "paz"  (* d.rz *) | "bruu" (* vac *) | "ma.mh" (* mah *) | "ind" 
     | "indh" | "inv" | "k.saa" | "cakaas" | "dhii#1" | "vidh#1" 
        -> () (* no perfect *)
     | "uuh" -> () (* periphrastic *)
     | _ -> compute_perfect entry
     ] (* NB perfect forms may have a passive meaning *)
   ; (* Periphrastic Perfect *) (* .namul on demand - except gana 10 above *)
     try let stem = peri_perf_stem entry in
         build_perpft Primary stem entry
     with [ Not_attested -> () ]
   ; (* Aorist *) compute_aorist entry
   ; (* Injunctive *) compute_injunctive entry
   }
   with [ Control.Warning s -> output_string stdout (s ^ "\n") ]
   (* end of Primary conjugation (including passive) *) 
 | Conj_infos.Causa third -> 
     (* Here we extract the causative stem from the third given in Dico *)
     (* rather than implementing all special cases of Whitney§1042.     *)
     (* Alternative: compute cstem instead of reading it from the lexicon.    
        Voir Panini krit ".ni" \Pan{7,3,36-43}                           *)
     let (cstem,active) = match Word.mirror third with
         [ [ 3 :: [ 32 :: [ 1 :: st ] ] ]  (* remove -ati *)
             -> (st,True)
         | [ 10 :: [ 32 :: [ 1 :: st ] ] ] (* remove -ate *)
             -> (st,False)
           (* We lose some information, but generate both active and middle *)
         | _ -> failwith ("Weird causative " ^ Canon.decode third)
         ] in
     let cpstem = match cstem with
         [ [ 42 :: [ 1 :: st ] ] (* -ay *) -> match entry with
            [ "dhvan" -> revcode "dhvaan"
            | _ -> st 
            (* doubt: ambiguity in ps when the ca stem is not lengthened       *)
            (* eg gamyate. Whitney§1052a says "causatively strengthened stem"? *)
            ]
         (* Why no ca in -aayati while such forms exist for ga.na 10 and 11 ?  *)
         | _ -> failwith ("Anomalous causative " ^ Canon.decode third)
         ] in
     let icstem = [ 3 :: cstem ] (* -ayi *) in
     let compute_causative stem = do (* both active and middle are generated *)
         { compute_causativea stem entry (if active then third else [])
         ; compute_causativem stem entry (if active then [] else third)
         } in 
     do (* active, middle, passive present; active middle future, aor *)
     { compute_causative cstem
     ; compute_passive Causative entry cpstem (* adapt [compute_passive_10]? *)
     ; let fsuf = revcode "i.sy" in
       let fustem = fsuf @ cstem in 
       compute_future_ca fustem entry 
     ; compute_aor_ca cpstem entry (* Whitney§861b Henry§339 *)
     ; (* Passive future participle in -ya *)
       match entry with
       [ "gad" | "yam" | "has" -> () (* to avoid redundancy with Primary pfp *)
       (* zi.s : justified redundancy with Primary pfp *)
       (* car :  redundancy with Primary pfp to be justified *)
       | _ -> record_pfp_ya Causative cpstem entry 
       ]
     ; (* Passive future participle in -aniiya *)
       record_pfp_aniiya Causative cpstem entry
       (* Passive past participle and absolutives *)
     ; record_pppca cpstem cstem entry
       (* Periphrastic future, Infinitive, Gerundive/pfp in -tavya *)
     ; perif Causative icstem entry 
       (* Periphrastic perfect Whitney§1045 *)
     ; build_perpft Causative cstem entry (* gamayaa.mcakaara *)
     } 
 | Conj_infos.Inten third -> (* TODO passive, perfect, future, aorist, parts *) 
     match Word.mirror third with (* active or middle are generated on demand *)
     (* paras. in -ati, -iiti, -arti (k.r2), -aati (draa1, yaj1), -etti (vid1) *)
     [ [ 3 :: [ 32 :: [ 4 :: ([ 45 :: [ 1 :: w ] ] as wk) ] ] ] (* x-aviiti *) ->
         let st = [ 12 :: w ] in
         (* x-o eg for hu johavitti -> joho -> johomi johavaani *)
         compute_intensivea wk st entry third
     | [ 3 :: [ 32 :: [ 4 :: wk ] ] ] (* other -iiti *) ->
         let st = strong wk in
         compute_intensivea wk st entry third
     | [ 3 :: [ 32 :: st ] ] (* ti *) 
     | [ 3 :: [ 27 :: st ] ] (* .ti eg veve.s.ti *) ->
         let wk = st in (* TEMP - no easy way to get weak stem from strong one *)
                        (* eg vevid from vevetti=veved+ti nenij from nenekti *)
         compute_intensivea wk st entry third 
     | [ 10 :: [ 32 :: [ 1 :: st ] ] ] -> (* -ate *) 
         compute_intensivem st entry third
     | [ 10 :: [ 32 :: st ] ] -> (* -te : nenikte *) 
         compute_intensivem2 st entry third
     | _ -> failwith ("Weird intensive " ^ Canon.decode third)
     ] 
 | Conj_infos.Desid third -> (* TODO passive, future, aorist, more parts *)
     let compute_krid st = do (* ppp pfp inf *)
         { record_pppdes st entry
         ; record_pfp_aniiya Desiderative st entry 
         ; record_pfp_ya Desiderative st entry 
(*i      ; record_des_aa Desiderative st entry (* Des k.rdantas TODO *) 
         ; record_des_u Desiderative st entry i*)
         ; perif Desiderative [ 3 :: st ] entry 
         } in
     match Word.mirror third with (* active or middle are generated on demand *)
       [ [ 3 :: [ 32 :: [ 1 :: st ] ] ] -> do 
           { compute_desiderativea st entry third
           ; compute_passive Desiderative entry st 
           ; compute_futurea Desiderative [ 42 :: st ] entry 
           ; compute_perfect_desida st entry 
           ; compute_krid st 
           }
       | [ 10 :: [ 32 :: [ 1 :: st ] ] ] -> do 
           { compute_desiderativem st entry third
           ; compute_passive Desiderative entry st 
           ; compute_futurem Desiderative [ 42 :: st ] entry 
           ; compute_perfect_desidm st entry 
           ; compute_krid st
           }
       | _ -> failwith ("Weird desiderative " ^ Canon.decode third)
       ] 
 ] 
  } (* end main do *)
;

(*********************)
(* Vedic Subjunctive *)
(*********************)

(* Various Vedic subjunctives needed for citations Whitney§562 *)
(* No attempt for full paradigms, only specific attested forms *)
(* TODO add paradigms for i a. and aas2 m. Whitney§614 *)
value compute_subjunctives () = 
  let enter_subjunctivea conj root tin =
      enter1 root (Conju (conj,Conjug Subjunctive Active) [ tin ])
  and enter_subjunctivem conj root tin =
      enter1 root (Conju (conj,Conjug Subjunctive Middle) [ tin ]) in
  let subj_sg root person form = 
    let tin = (Singular,[ (person, code form) ]) in 
    enter_subjunctivea Primary root tin
  and subj_pl root person form = 
    let tin = (Plural,[ (person, code form) ]) in 
    enter_subjunctivea Primary root tin
  and subjm_sg3 root form = 
    let tin = (Singular,[ (Third, code form) ]) in 
    enter_subjunctivem Primary root tin
  and subj_cau_sg root person form = 
    let tin = (Singular,[ (person, code form) ]) in 
    enter_subjunctivea Causative root tin
  and subj_int_sg root person form = 
    let tin = (Singular,[ (person, code form) ]) in 
    enter_subjunctivea Intensive root tin in do
  { subj_sg "zru" Third "zro.sat"
(*i [; subj_sg "tandr" Third "tandrat" not generated - PB i*)
  ; subj_sg "i.s#1" Third "icchaat"
  ; subj_sg "vac" Third "vocati" (* primary endings *) 
  ; subj_sg "vac" Third "vocat" (* secondary endings *) 
  ; subj_sg "vac" Second "vocas" (* both forms also available as inj *) 
  ; subj_sg "pat#1" Third "pataati"
  ; subj_pl "gam" Third "gman" (* for apigman *) 
  ; subj_cau_sg "jan" Second "janayaas"  
  ; subj_cau_sg "cud" Third "codayaat" (* Gaayatrii pracodayaat *)
  ; subj_int_sg "vi.s#1" Third "vevi.sati"
(*; [subj_sg] "k.r#1" First "karavaa.ni" (* became imp Whitney§578 *) *)
  ; subjm_sg3 "k.r#1" "k.r.nvate" (* aussi pr[5] md *)
  }
;

value compute_auxi_kridantas () = 
  let stems str = let st = revstem str in match st with
      [ [ 1 :: rst ] -> (rst,Word.mirror st) 
      | _ -> failwith "auxi_kridantas" 
      ] in do (* A few auxiliary action nouns are generative for cvi compounds *)
  { let (rst,st) = stems "kara.na" in 
    build_part_a_n (Primary,Action_noun) rst st "k.r#1" 
  ; let (rst,st) = stems "kaara" in (* actually, should be [Agent_noun] *)
    build_part_a_m (Primary,Action_noun) rst st "k.r#1" (* also fem in -ii? *)
  ; let (rst,st) = stems "bhaavana" in
    build_part_a_m (Primary,Action_noun) rst st "bhuu#1"
  ; let (rst,st) = stems "bhaava" in 
    build_part_a_m (Primary,Action_noun) rst st "bhuu#1"
  }
;
(* Called by [Make_roots.roots_to_conjugs] *)
value compute_conjugs root (infos : Conj_infos.root_infos) = 
  let root_entry = Canon.decode root in compute_conjugs_stems root_entry infos
;
(* Supplementary forms *)
value compute_extra_rc () = (* vedic - \Pan{7,1,38} *)
  enter1 ".rc#1" (Absotvaa Primary (code "arcya")) (* abs -ya with no preverb *)
and compute_extra_khan () = (* WR MW *)
  let root = "khan"
  and conj = Primary 
  and pstem = revcode "khaa" (* khaa substituted optionally in ps *) in 
  compute_passive conj root pstem 
and compute_extra_car () = do
  { enter1 "car" (Absotvaa Primary (code "cartvaa"))
  ; enter1 "car" (Absotvaa Primary (code "ciirtvaa"))
  ; enter1 "car" (Invar (Primary,Infi) (code "cartum")) (* epic *)
  }
and compute_extra_jnaa () =
  let entry = "j~naa#1" in (* j~napta vet \Pan{7,2,27} *)
  let cstem = revcode "j~nap" in 
  let ppstem = [ 1 :: [ 32 :: cstem ] ] (* j~napta *) in do 
  { record_part (Ppp_ Causative ppstem entry)
  ; record_part (Pppa_ Causative ppstem entry) (* pp-vat *)
  ; perif Causative cstem entry 
  }
and compute_extra_tri () = do 
      { build_infinitive Primary (revcode "tarii") "t.rr" (* id. *)
      ; build_infinitive Primary (revcode "tar") "t.rr" (* Whitney roots *)
      }
and compute_extra_dhaa () = (* Gaayatrii dhiimahi precative m. Whitney§837b *)
    enter1 "dhaa#1" (Conju benem [ (Plural,[ (First, code "dhiimahi") ]) ])
(* also "vidmahi" on yantra ? *)
and compute_extra_nind () = (* WR: RV *)
  enter1 "nand" (Conju perfa [ (Plural,[ (Third, code "ninidus") ])
                             ; (Plural,[ (First, code "nindimas") ]) ])
and compute_extra_prr () = (* paaryate as well as puuryate above *) 
    let stem = revcode "paar" in compute_passive Primary "p.rr" stem
and compute_extra_bhaas () = 
    enter1 "bhaa.s" (Invar (Primary,Infi) (code "bhaa.s.tum")) (* WR epic *)
and compute_extra_bhuj2 () = 
    enter1 "hhuj#2" (Conju (Primary,voa 7) (* epics Wh{688a} *) 
                           [ (Singular,[ (First, code "bhu~njiiyaam") ])
                           ; (Singular,[ (Second, code "bhu~njiiyaas") ])
                           ; (Singular,[ (Third, code "bhu~njiiyaat") ])
                           ])
and compute_extra_bhr () = (* Epics sa.mbhriyantu Oberlies 8.7 *) 
   enter1 "bh.r" (Conju (Primary,vmp) [ (Plural,[ (Third, code "bhriyantu") ]) ])
and compute_extra_bhram () = (* MW: Mah *)
  enter1 "bhram" (Conju perfa [ (Plural,[ (Third, code "bhremur") ]) ])
and compute_extra_muc () =  do 
  { (* ved precative `fasse que je sois libéré' *)
    enter1 "muc#1" (Conju benem [ (Singular,[ (First, code "muk.siiya") ]) ])
  ; build_infinitive Causative (revcode "moci") "muc#1"    (* Whitney§1051c *)
  }
and compute_extra_vadh () = (* no present - use "han#1" *)
  let root = "vadh"
  and rstem = revcode "vadh" in do 
  { compute_aorist root
  ; compute_future_gen rstem root 
  ; compute_passive_raw root
  (* [record_pfp root rstem] is computed by [compute_extra_participles] *)
  }
and compute_extra_zaas () = 
   let e = "zaas" in do (* epics zaasyate + Renou gram §29 *) 
     { let stem = revcode e in compute_passive Primary e stem 
     ; enter1 e (Conju (Primary,via 2) [ (Singular,[ (Second, code "azaat") ]) ])
     }
and compute_extra_zru () = 
  enter1 "zru" (* ved écoute *) 
         (Conju (impera 5) [ (Singular,[ (Second, code "zrudhi") ]) ])
and compute_extra_sanj () = (* WR Oberlies p LI but maybe prm of variant sajj *)
  let root = "sa~nj" 
  and conj = Primary
  and pastem = revcode "sajj" (* "y" replaced by j in passive *) in 
  compute_passive_system conj root pastem 
and compute_extra_skand () = do (* WR *)  
  { enter1 "skand" (Invar (Primary,Infi) (code "skanditum")) 
  ; record_abso_ya (code "skadya") "skand"
  }
and compute_extra_syand () = do (* WR *)
  { record_abso_tvaa (code "syattvaa") "syand" 
  ; record_abso_ya (code "syadya") "syand" 
  }
and compute_extra_hims () = do 
  { (* Renou gram §29 *) enter1 "hi.ms" 
    (Conju (Primary,via 7) [ (Singular,[ (Second, code "ahi.msat") ]) ])
  ; (* MW *) enter1 "hi.ms" 
    (Conju (presa 7) [ (Singular,[ (Second, code "hi.msi") ]) ])
  }
and compute_extra_huu () = do (* WR *)
   { compute_futurem Primary (revstem "hvaasy") "huu" 
   ; enter1 "huu" (Invar (Primary,Infi) (code "hvayitum")) 
   }
;
(* Extra participial forms - intensive, desiderative, no present, etc *)
value record_extra_participles () = do
  { record_part_ppp (revstem "gupta") "gup" (* gup gana 10 *)
  ; record_part_ppp (revstem "zaata") "zaa" 
  ; record_part_ppp (revstem "kaanta") "kam" 
  ; record_part_ppp (revstem "k.sita") "k.sii" 
  ; record_part_ppp (revstem "diipita") "diip" 
  ; record_part_ppp (revstem "spa.s.ta") "spaz#1"
  ; record_part (Ppra_ 1 Intensive (revstem "jaajam") (revstem "jaajamat") "jam")
  ; record_pfp "d.r#1" (revcode "d.r")
  ; record_pfp "vadh" (revcode "vadh")
  ; record_part (Pprm_ 1 Primary (revcode "gacchamaana") "gam")
  ; record_part (Pprm_ 4 Primary (revcode "kaayamaana") "kan")
  ; record_part (Ppra_ 1 Primary (revstem ".dam") (revstem ".damat") ".dam")
  }
;
(* For verbs without present forms and variants, *)
(* called by [Make_roots.roots_to_conjugs] at generation time *)
value compute_extra () = do
  { compute_perfect "ah"   (* verbs with no present system *)
  ; compute_aorist "kan" 
  ; compute_perfect "kam" 
  ; compute_perfect "ghas" 
  ; compute_perfect "ta.d" 
  ; compute_perfect "spaz#1" 
  ; compute_aorist "spaz#1" 
  ; compute_aorist "k.r#2" 
  ; compute_passive_raw "d.r#1"
  (* Now for specific extra forms *)
  ; compute_extra_rc () 
  ; compute_extra_khan ()
  ; compute_extra_car () 
  ; compute_extra_jnaa () 
  ; compute_extra_tri () 
  ; compute_extra_dhaa () 
  ; compute_extra_nind () 
  ; compute_extra_prr () 
  ; compute_extra_bhaas () 
  ; compute_extra_bhuj2 ()
  ; compute_extra_bhr ()
  ; compute_extra_bhram ()
  ; compute_extra_muc () 
  ; compute_extra_vadh ()
  ; compute_extra_zaas () 
  ; compute_extra_zru () 
  ; compute_extra_sanj ()
  ; compute_extra_skand () 
  ; compute_extra_syand ()
  ; compute_extra_hims ()
  ; compute_extra_huu ()
  ; build_infinitive Primary (revcode "rami") "ram"
  ; build_infinitive Primary (revcode "aas") "aas#2" (* Whitney§968d *)
  ; build_infinitive Causative (revcode "bhaavi") "bhuu#1" (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "dhaari") "dh.r"   (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "ze.si") "zi.s"    (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "j~naap") "j~naa#1" (* WR epics *)
    (* Infinitives in -as (kasun k.rt) \Pan{3,4,17} *)
  ; enter1 "s.rp" (Invar (Primary,Infi) (code "s.rpas")) (* vi.s.rpas *)
  ; enter1 "t.rd" (Invar (Primary,Infi) (code "t.rdas")) (* aat.rdas *)
  ; let st = revcode "si.saadhayi.s" in (* des of ca of sidh1 *)
    compute_desiderativea st "saadh" []
  ; record_extra_participles () 
  ; compute_participles ()
  ; compute_auxi_kridantas () 
  ; compute_subjunctives ()
  }
;
(* Called by [Conjugation.look_up] and [Morpho_debug.test_conj]            *)
(* Remark. For the present system only the queried [gana] is displayed,    *)
(* but all forms of other systems are displayed after it.                  *)
(* It is for the moment impossible to list forms of roots without present. *)
value fake_compute_conjugs (gana : int) (entry : string) = do
  { morpho_gen.val := False (* Do not generate phantom forms *) 
  ; let no_third = [] and pada = True in (* hacks to disable check warning *)
    let vmorph = Conj_infos.Prim gana pada no_third in do
    { compute_conjugs_stems entry (vmorph,False) (* False since no-op in fake *)
    ; match entry with (* extra forms - to be completed from [compute_extra] *)
      [ ".rc#1"  -> compute_extra_rc ()
      | "k.sii"  -> record_part_ppp (revcode "k.sita") entry
      | "khan"   -> compute_extra_khan ()
      | "gup"    -> record_part_ppp (revcode "gupta") entry 
      | "car"    -> compute_extra_car ()
      | "j~naa#1"-> compute_extra_jnaa () 
      | "t.rr"   -> compute_extra_tri () 
      | "dhaa#1" -> compute_extra_dhaa () 
      | "nind"   -> compute_extra_nind ()
      | "p.rr"   -> compute_extra_prr ()
      | "bhaa.s" -> compute_extra_bhaas ()
      | "bhuj#2" -> compute_extra_bhuj2 ()
      | "bh.r"   -> compute_extra_bhr ()
      | "bhram"  -> compute_extra_bhram ()
      | "muc#1"  -> compute_extra_muc ()
      | "vadh"   -> compute_extra_vadh ()
      | "zaa"    -> record_part_ppp (revcode "zaata") entry
      | "zaas"   -> compute_extra_zaas ()
      | "zru"    -> compute_extra_zru () 
      | "sa~nj"  -> compute_extra_sanj () 
      | "skand"  -> compute_extra_skand ()
      | "spaz#1" -> record_part_ppp (revcode "spa.s.ta") entry
      | "syand"  -> compute_extra_syand ()
      | "hi.ms"  -> compute_extra_hims ()
      | "huu"    -> compute_extra_huu ()
      | _ -> ()
      ]
    }
  }
;

(*i end; i*)
