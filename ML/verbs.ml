(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2023 Institut National de Recherche en Informatique et en Automatique *)
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
                    gana, mult, aug, trunc_a, trunc_aa, trunc_ii, trunc_u] *)
open Skt_morph; (* Infi, Absoya, Perpft *)
open Inflected; (* [Conju, Invar, Inftu, Absotvaa, roots, enter1, morpho_gen, 
                    admits_aa] *)
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
and vap1 =   Conjug (Aorist 1) Passive     (* passive of root aorist *)
and vja cl = Conjug (Injunctive cl) Active
and vjm cl = Conjug (Injunctive cl) Middle
and vjp1 =   Conjug (Injunctive 1) Passive (* passive of root injunctive *)
;
(* Finite verbal forms of roots *)
value fpresa cl conj = (conj,vpa cl)
and fpresm cl conj   = (conj,vpm cl)
and fpresp conj      = (conj,vpp)
and fimpfta cl conj  = (conj,via cl)
and fimpftm cl conj  = (conj,vim cl)
and fimpftp conj     = (conj,vip)
and fopta cl conj    = (conj,voa cl)
and foptm cl conj    = (conj,vom cl)
and foptp conj       = (conj,vop)
and fimpera cl conj  = (conj,vma cl)
and fimperm cl conj  = (conj,vmm cl)
and fimperp conj     = (conj,vmp) 
and ffutura conj     = (conj,vfa)
and ffuturm conj     = (conj,vfm)
and fconda conj      = (conj,vca)
and fcondm conj      = (conj,vcm)
and fperfa conj      = (conj,vpfa)
and fperfm conj      = (conj,vpfm)
and fbenea conj      = (conj,vbena)
and fbenem conj      = (conj,vbenm)
and faora cl conj    = (conj,vaa cl)
and faorm cl conj    = (conj,vam cl)
and finja cl conj    = (conj,vja cl)
and finjm cl conj    = (conj,vjm cl)
and faorp1 conj      = (conj,vap1)
and finjp1 conj      = (conj,vjp1)
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
and futura     = ffutura Primary
and futurm     = ffuturm Primary
and perfa      = fperfa Primary
and perfm      = fperfm Primary
and aora cl    = faora cl Primary
and aorm cl    = faorm cl Primary
and aorp1      = faorp1 Primary
and benea      = fbenea Primary
and benem      = fbenem Primary
and inja cl    = finja cl Primary
and injm cl    = finjm cl Primary
and injp1      = finjp1 Primary
;
(* Participial forms *)
value pra k = Ppra k 
and prm k   = Pprm k 
and prp     = Pprp
and pfta    = Ppfta
and pftm    = Ppftm
and futa    = Pfuta
and futm    = Pfutm
(* Also in Part: Ppp, Pppa, Ger=Pfut Passive, Inf *)
;
(* Verbal forms of roots *)
value vppra k conj = (conj,pra k)
and vpprm k conj   = (conj,prm k)
and vppfta conj    = (conj,pfta)
and vppftm conj    = (conj,pftm)
and vpfuta conj    = (conj,futa)
and vpfutm conj    = (conj,futm)
and vpprp  conj    = (conj,prp)
(* Also in Part: Ppp, Pppa, Ger=Pfut Passive, Inf *)
;
(* Verbal forms of roots *)
value ppra k = vppra k Primary
and pprm k   = vpprm k Primary
and ppfta    = vppfta Primary
and ppftm    = vppftm Primary
and pfuta    = vpfuta Primary
and pfutm    = vpfutm Primary
and pprp     = vpprp Primary
;
value primary_pprm (c,v) = 
  c=Primary && match v with [ Pprm _ -> True | _ -> False ]
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
and roman = Encode.skt_to_roma (* IAST *)
;
(* Checking consistency of computed form with witness from lexicon.      *)
(* Discrepancies are noted on a warnings log, written on stderr.         *)
(* NB currently log dumped in (D)STAT/warnings.txt by "make roots.rem".  *)
value emit_warning s =
  if morpho_gen.val then output_string stderr (s ^ "\n") else ((* cgi *))
;
value report root gana listed computed =
  let s1 = Canon.decode computed
  and s2 = Canon.decode listed in
  let message = root ^ " [" ^ string_of_int gana ^ "] wrong 3rd pr "
                      ^ s1 ^ " for " ^ s2 in
  emit_warning message 
;
(* third is attested from Dico, form is generated by morphology *)
value check root gana third ((_,form) as res) = do
  { if third=[] (* no checking *) || third=form then () 
    else match root with 
         [ "a~nc" | "kalu.s" | "kram" | "grah" | "cam" | "tul" | "t.rr"
         | "manth" | "v.r#1" | "huu" | "putr" 
           (* these roots have multiple ga.nas, i.e. different entries in DP *)
             -> () (* 2 forms - avoids double warning *)
         | _ -> report root gana third form
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
   We enter in the roots lexicon an root:
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
  | [ c :: [ n :: [ v :: rest ] ] ] when short_vowel v && nasal n -> 
    [ c :: [ n :: final_vriddhi v rest ] ] (* NEW 23-04-23 abhaantsiit *)
  | s -> s
  ]
;
value strengthen_10 rstem = fun
  [ "m.r.d" | "sp.rh" -> rstem (* exceptions with weak stem *)
(*| "k.sal" -> lengthened rstem (* v.rddhi *) - irrelevant for k.sal as ca *)
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
value strong_stem root rstem = (* rstem = revstem root *)
  match root with 
    [ "am" -> revcode "amii" (* amiiti *)
    | "dah#1" | "dih" | "duh#1" | "druh#1" | "muh" | "snih#1" | "snuh#1"
               -> duhify (strong rstem)
    | "nah"    -> nahify (strong rstem)
    | "m.rj"   -> mrijify (revcode "maarj") (* maar.s.ti [long_metathesis] *)
    | "yaj#1" | "vraj" | "raaj#1" | "bhraaj" | "s.rj#1" 
               -> mrijify (strong rstem)
    | "bh.rjj" -> mrijify (strong (truncate rstem)) (* bh.rsj \Pan{8,2,29} *)
    | "nij"    -> revcode "ni~nj" (* nasalisation for gana 2 *) 
    | "zrath"  -> revcode "zranth"
    | "rabh"   -> revcode "rambh"
    | "dham"   -> revcode "dhmaa" 
    | _ -> strong rstem
    ]
;
value weak_stem root rstem = (* rstem = revstem root *)
  match root with 
    [ "dah#1" | "dih" | "duh#1" | "druh#1" | "muh" | "snih#1" | "snuh#1"
               -> duhify rstem
    | "nah"    -> nahify rstem
    | "m.rj" | "yaj#1" | "vraj" | "raaj#1" | "bhraaj" | "s.rj#1" 
               -> mrijify rstem
    | "bh.rjj" -> mrijify (truncate rstem)
    | "nij"    -> revcode "ni~nj" (* nasalisation *)
    | "vaz"    -> revcode "uz" (* but not vac ! *)
    | "myak.s" -> revcode "mik.s" 
(*  | "dhmaa"  -> revcode "dham" - incorrect for perfect *)
(*  | "grah"   -> revcode "g.rh"
    | "grabh"  -> revcode "g.rbh" -- implicit from stems *)
(*  | "sad#1"  -> revcode "siid" - incorrect for perfect ! *)
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
     | "grabh"  -> sampra "g.rbh" (* archaic variant of grah *) 
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
value passive_stem root rstem = (* Panini yak (k : no guna, samprasaara.na) *)
  let weak = match root with 
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
    | "vah#1" | "vak.s" (* idem - specific code for va-x roots *)
              -> match rstem with 
                 [ [ 48 :: _ ] -> [ 47 ; 5 (* u *) ] (* vas  \R u.s *)
                 | [ c :: _ ] -> [ c ; 5 (* u *) ]   (* va-x \R u-x *)
                 | [] -> failwith "Anomalous passive_stem"
                 ]
    | "vaa#3" -> revcode "uu" (* \Pan{6,1,15} *) 
    | "zaas"  -> revcode "zi.s" (* ambiguous zi.s.ta, zi.syate *)
    | "zii#1" -> revcode "zay" (* \Pan{7,4,22} *) 
    | "dham"  -> revcode "dhmaa" (* \Pan{7,3,78} *) 
    | ".s.thiiv" -> revcode ".s.thiv" 
    | "indh" | "und" | "umbh" | "gumph" | "granth" | "da.mz" | "dhva.ms"  
    | "bandh" | "bhra.mz" | "za.ms" | "zrambh" | "skambh" | "skand" | "sra.ms" 
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
            | _ -> match root with
                   [ "j~naa#1" | "traa" | "dhmaa" | "bhaa#1" | "mnaa" 
                   | "laa" | "zaa" | "haa#2" -> weak
                   | _ -> [ 4 (* ii *) :: rst ]
                   ]
            ]
        | 3 (* i *) -> [ 4 (* ii *) :: rst ]
        | 5 (* u *) -> match root with
            [ "k.su" | "k.s.nu" | "plu" | "sru" -> weak
            | _ -> [ 6 (* uu *) :: rst ]
            ]
        | 7 (* .r *) -> match rst with
            [ [ _ ] -> [ 3 :: [ 43 :: rst ] ] (* ri- *)
            | _ (* 0 or 2 consonants *) -> [ 43 :: [ 1 :: rst ] ] (* ar- *)
            ]
        | 8 (* .rr *) -> match rst with
            [ [ d :: _ ] -> 
              if labial d then [ 43 :: [ 6 :: rst ] ] (* puuryate \Pan{7,1,102}*)
                          else [ 43 :: [ 4 :: rst ] ] (* kiiryate ziiryate *)
            | _ -> error_empty 4
            ] 
        | _ -> if c>9 && c<14 (* e ai o au *) then match root with
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
value redup3 root rstem = 
  match Word.mirror rstem with 
    [ [] -> failwith "Empty root"
    | [ 7 (* .r *) ] -> (* Whitney§643d *) (revstem "iyar",revstem "iy.r",False)
    | [ c1 :: r ] -> if vowel c1 then failwith "Attempt reduplicating vowel root"
                     else 
      let v = lookvoy r
         where rec lookvoy = fun
           [ [] -> failwith "Attempt to reduplicate root with no vowel"
           | [ c2 :: r2 ] -> if vowel c2 then c2 else lookvoy r2
           ] 
      and iflag = match root with (* special flag for some aa roots *)
           [ "gaa#1" | "ghraa" | "maa#1" | "zaa" | "haa#2" -> True
           | _ -> False 
           ] 
      and iflag2 = match root with (* special flag for some other roots *)
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
        if root="v.rt#1" then 1 (* a *) else
        if rivarna v || iflag || iflag2 then 3 (* i *)
        else if root="nij" then 10 (* e *) (* Whitney says intensive! *)
        else short v (* reduplicated vowel is short *)
      and rc = match c with (* rc is reduplicating consonant *)
        [ 17 | 18 (* k kh *) -> 22 (* c *)
        | 19 | 20 | 49 (* g gh h *) -> 24 (* j *) 
        | 149 | 249 (* h' h2 *) -> failwith "Weird root of class 3"
        | 23 | 25 | 28 | 30 | 33 | 35 | 38 | 40 -> c-1 (* aspiration loss *)
        | _ -> c
        ] 
      and iiflag = iflag || root = "haa#1" in 
      let (strong,weak) = 
           if iiflag then match rstem with
              [ [ 2 :: rest ] -> (rstem,[ 4 :: rest ]) (* aa \R ii *)
              | _ -> failwith "Anomaly Verbs"
              ]
           else let wstem = match root with
                [ "daa#1" | "dhaa#1"  -> match rstem with
                   [ [ 2 :: rest ] -> rest (* drop final aa *)
                   | _ -> failwith "Anomaly Verbs"
                   ]
                | "p.rr" -> revstem "puur"(* \Pan{7,1,102} [labial].rr -> ur *)
                | _ -> rstem 
                ] in 
      (strong rstem,wstem)
      and glue = match root with
          [ "s.r" -> revaffix False [ rv; rc ] (* no retroflexion: sisarti *)
          | _ -> revaffix True [ rv; rc ] 
          ] in (glue strong,glue weak,iiflag) 
    ]
;

(* Dhatupatha it markers (from AK's listing) Renou: exposant d'une racine *)
(* NB Use of these markers should progressively replace lists of exceptions *)
value aa_it = fun
  [ (* "muurch" | WRONG ? *) 
    "phal" | "zvit#1" | "svid#2" | "tvar" | "dh.r.s" -> True
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
| "dabh" | "dham" | "dhmaa" | "dhva.ms" | "dhvan" | "pa.th" | "pat#1" | "piz" 
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
and u_it = fun (* \Pan{7,2,56} -i- optional before -tvaa vet *)
  [ "sidh#2" | "a~nc" | "va~nc" | "zrambh" | "stubh" | "kam" | "cam" | "jam"
  | "kram" | ".s.thiiv" | "dhaav#1" | "gras" | "mi.s" | "p.r.s" | "v.r.s" 
  | "gh.r.s" | "zas" | "za.ms" | "sra.ms" | "dhva.ms" | "v.rt" | "v.rdh#1" 
  | "bhram" | "ram" | "m.rdh" | "khan" | "zaas" | "diiv#1" | "siiv" | "sidh#1"
  | "zam#1" | "tam" | "dam#1" | "zram" | "as#2" | "yas" | "jas" | "das" 
  | "bhra.mz" | ".rdh" | "g.rdh" | "dambh" | "i.s#1" | "t.rd" | "tan#1"
  | "k.san" -> True
  | _ -> False
  ]
and uu_it = fun (* licences -i- in perstems \Pan{7,2,44} *)
  [ "trap" | "k.sam" | "gaah" | "ak.s" | "tak.s" | "tvak.s" | "syand" | "k.rp" 
  | "guh" | "m.rj" | "klid" | "az#1" | "vrazc" | "b.rh#2" | "v.rh" | "a~nj"
  | "kli.s" | "ta~nc" -> True 
  | _ -> False
  ]
and o_it = fun (* these roots have ppp in -na \Pan{8,2,45} - unused here *)
  [ "zuu" | "haa#1" | "haa#2" | "vij" | "vrazc" | "bhuj#1" | "bha~nj" | "lag" 
 (* | "iir" | "und" | "k.rr" | "klid" | "k.sii" | "k.sud" | "k.svid" | "khid"
    | "g.rr#1" | "glaa" | "chad#1" | "chid#1" | "ch.rd" | "j.rr" | ".dii"
    | "tud#1" | "t.rd" | "t.rr" | "dagh" | "d.rr" | "dev" | "draa#1" | "draa#2"
    | "nud" | "pad#1" | "pii" | "p.rr" | "pyaa" | "bhid#1" | "majj" | "man"
    | "mid" | "mlaa" | "ri" | "lii" | "luu#1" | "vid#2" | "vlii" | "zad" | "z.rr"
    | "sad#1" | "skand" | "st.rr" | "styaa" | "syand" | "svid#2" | "had" *)
 (* should include "suu#2" suuna, "vrii" vrii.na and "k.saa" k.saa.na *)
      -> True 
  | _ -> False
  ]
and no_guna = fun (* ku.taadi Kale§463 *)
  [ "kuc" | "ku.t" | "gur" | "ghu.t" | "cu.t" | "cu.d" | "chur" | "ju.t" 
  | "tu.t" | "tu.d" | "tru.t" | "dham" | "dhmaa" | "dhru" | "nuu" | "pu.t"
  | "pu.d" | "lu.t" | "lu.th" | "lu.d" | "vra.d" | "sphu.t" | "sphur" -> True
  |  _ -> False
  ]
;
(******************)
(* Present system *)
(******************)

(* In all such functions, [(stem : word)] is the code of the reversed stem. *)
(* Exemple pour cyu: stem=strong=guna=cyo et cyo+ati=cyavati par [int_sandhi] *)
value compute_thematic_presenta cl conj stem root third = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 root (Conju (fpresa cl conj)
   [ (Singular, 
        [ conjug First  "aami"
        ; conjug Second "asi"
        ; check root cl third (conjug Third "ati") 
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
  ; let m_stem = match root with (* Whitney§450 *)
        [ "b.rh#1" -> revcode "b.rh" (* not b.r.mh *)
        | _ -> stem 
        ] in
    let f_stem = match root with (* Whitney§450f *)
        [ "j.rr" | "p.r.s" | "b.rh#1" (* | "mah" *) | "v.rh" -> rfix m_stem "at" 
        | _ -> rfix m_stem "ant" 
        ] in 
    if cl=4 && root="daa#2" || root="mah" then () (* to avoid dyat mahat *)
    else record_part (Ppra_ cl conj m_stem f_stem root)
  }
;
value compute_thematic_presentm cl conj stem root third = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (fpresm cl conj)
   [ (Singular, 
        [ conjug First  "e"
        ; conjug Second "ase"
        ; check root cl third (conjug Third "ate")
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
value compute_thematic_impfta cl conj stem root =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (fimpfta cl conj) (thematic_preterit_a conjug))
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
value compute_thematic_impftm cl conj stem root =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (fimpftm cl conj) (thematic_preterit_m conjug))
;
value compute_thematic_optativea cl conj stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (fopta cl conj)
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
value compute_thematic_optativem cl conj stem root =
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (foptm cl conj)
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
value compute_thematic_imperativea cl conj stem root =
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (fimpera cl conj)
   [ (Singular, 
        [ conjug First  "aani" (* gacchaani let me  go *)
        ; conjug Second "a"    (* gaccha    go         *)
        ; conjug Third  "atu"  (* gacchatu  let him go *)
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
value compute_thematic_imperativem cl conj stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (fimperm cl conj)
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
value record_part_m (conj,part_kind) stem root = match part_kind with 
  [ Pprm k -> record_part (Pprm_ k conj stem root)
  | Pprp   -> record_part (Pprp_ conj stem root)
  | Ppfta  -> record_part (Ppfta_ conj stem root)
  | Ppftm  -> record_part (Ppftm_ conj stem root)
  | Pfutm  -> record_part (Pfutm_ conj stem root)
  | _ -> failwith "Unexpected participle"
  ]
;
(* First an abbreviated pprm in -aana rather than -maana *)
value record_pprm_aana verbal stem root = 
  (* [trunc_a] needed because possible retroflexion in aa.na *)
  let mid_stem = trunc_a (rfix stem "aana") in 
  record_part_m verbal mid_stem root 
;
value record_part_m_th verbal stem root =  
  match root with
  [ "cint" when primary_pprm verbal -> 
         let mid_stem = revcode "cintayaan" in
         record_part_m verbal mid_stem root (* irregular *)
  | "nam" | "pac" | "majj" | "sva~nj" | "huu" (* ga.na 1 -aana Whitney§741 *)
  | "muc#1" | "sp.rz#1" (* ga.na 6 -aana Whitney§752.5e *)
    when primary_pprm verbal -> 
         record_pprm_aana verbal stem root
  | "zak" when verbal = (Desiderative,Pprm des_gana) -> 
         record_pprm_aana verbal stem root (* zik.saa.na Whitney§741 *)
  | _ -> let mid_stem = trunc_a (rfix stem "amaana") (* -maana *) in
         (* [trunc_a] needed because possible retroflexion in amaa.na *)
         record_part_m verbal mid_stem root 
  ]
and record_part_m_ath verbal stem root =  
  let suff = if root = "aas#2" then "iina" (* McDonell§158a *)
             else "aana" (* -aana *) in
  let mid_stem = match rfix stem suff  with
                 [ [ 1 :: r ] -> r | _ -> failwith "Anomaly Verbs" ] in
  (* rare (Whitney). Creates bizarre forms such as plu -> puplvaana *)
  record_part_m verbal mid_stem root 
;
(* Thematic present system - gana is root's present class *)
value compute_thematic_active gana conj stem root third = do 
  { compute_thematic_presenta gana conj stem root third
  ; compute_thematic_impfta gana conj stem root 
  ; compute_thematic_optativea gana conj stem root 
  ; compute_thematic_imperativea gana conj stem root 
  } 
and compute_thematic_middle gana conj stem root third = do 
  { compute_thematic_presentm gana conj stem root third
  ; compute_thematic_impftm gana conj stem root 
  ; compute_thematic_optativem gana conj stem root 
  ; compute_thematic_imperativem gana conj stem root 
  ; record_part_m_th (vpprm gana conj) stem root
  }
;
value compute_causativea  = compute_thematic_active cau_gana Causative
and compute_causativem    = compute_thematic_middle cau_gana Causative
and compute_desiderativea = compute_thematic_active des_gana Desiderative
and compute_desiderativem = compute_thematic_middle des_gana Desiderative
;

(*** Gana 2 (root conjugation) ***)

(* [fix2: Word.word -> string -> string -> bool -> Word.word] *)
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
value fix2wi suff = (* special for root i Atma ii -> iiy ai -> aiy *)
  match code suff with (* \Pan{6,4,77} MacDonell§134.3d *)
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
value compute_athematic_present2a strong weak set root third = 
  let conjugs person suff =
      (person,if root = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
              else fix2w weak suff set) in do
  { enter1 root (Conju (presa 2)
   [ (Singular, let l =
        [ conjugs First "mi"
        ; if root = "as#1" then (Second, code "asi")
          else conjugs Second "si"
        ; check root 2 third (conjugs Third "ti") 
        ] in if root ="bruu" then [ conjugw First "mi" :: l ]
             else if root ="stu" then [ (First, code "staviimi") :: l ]
             else l (* bruumi Whitney§632 staviimi Whitney§633 *))
   ; (Dual, if root = "vac" then [] else 
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ])
   ; (Plural, let l = 
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; if root = "zaas" then conjugs Third "ati" (* \Pan{7,1,4} *)
          else conjugw Third (if abhyasta root then "ati" else "anti")
        ] in if root = "m.rj" then [ conjugs Third "anti" :: l ]
             else if root = "vac" then [] 
             else l (* Whitney§627 *))
   ])
  }
;
value compute_athematic_present2m strong weak set root third = 
  let conjugs person suff = 
      (person,if root = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
              else if root = "i" then fix2wi suff (* Gonda§64.III *)
              else fix2w weak suff set) in
  enter1 root (Conju (presm 2)
   [ (Singular, let l = 
        [ if root = "as#1" then (First, code "he") else
          conjugw First "e" 
        ; conjugw Second (if root = "ii.d" then "ise" else "se") (* MW *)
        ; check root 2 third (conjugw Third "te") 
        ] in if root = "m.rj" then [ conjugs First "e" :: l ]
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ] in if root = "m.rj" then 
                [ conjugs Second "aathe"
                ; conjugs Third  "aate"
                ] @ l
             else l (* Whitney§627 *))  
   ; (Plural, let l =
        [ conjugw First  "mahe" 
        ; if root = "as#1" then (Second, code "dhve") else
          if root = "aas#2" then (Second, code "aadhve") else (* -Whitney§612 *)
          conjugw Second "dhve" 
        ; if root = "zii#1" then conjugw Third "rate" (* \Pan{7,1,6} *)
          else conjugw Third "ate" 
        ] in if root = "m.rj" then [ conjugs Third "ate" :: l ]
             else l (* Whitney§627 *)) 
   ])
;
value compute_athematic_impft2a strong weak set root = 
  let conjugs person suff = 
      (person,if root = "bruu" then fix2sbruu_augment suff 
              else fix2s_augment strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan_augment suff 
              else fix2w_augment weak suff set) in
  enter1 root (Conju (impfta 2)
   [ (Singular, let l = 
        [ conjugs First "am"
        ; if set then conjugs Second "as"
          else if root = "as#1" then conjugs Second "iis" (* Whitney§621c *)
          else if root = "ad#1" then conjugs Second "as"  (* Whitney§621c *)
          else conjugs Second "s" (* PB for "i": "ais" clashes with i. pl. *)
        ; if set then conjugs Third "at"
          else if root = "as#1" then conjugs Third "iit" (* idem aasiit *)
          else if root = "ad#1" then conjugs Third "at"  (* idem aadat *)
          else conjugs Third "t"
        ] in if set then [ conjugs Second "iis"; conjugs Third  "iit" ] @ l 
             else if root = "bruu" 
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
        ; if root = "i" then conjugs Third "an" (* aayan *)
          else match root with (* Kane§429 *)
               [ "cakaas" | "jak.s" | "jaag.r" 
            (* | "daridraa" - should concern "draa#1" TODO *)
               | "zaas" -> conjugw Third "us" 
               | _ -> conjugw Third "an" 
               ]
        ] in if root = "m.rj" 
                  then [ conjugs Third "an" :: l ] (* Whitney§627 *)
             else if root = "bruu" 
                  then [ (Third, code "abruuvan") :: l ] (* Whitney§632 *)
             else match weak with (* Kale§420 optional -us for roots in -aa *)
                  [ [ 2 :: s ] -> [ (Third, aug (sandhi s (code "us"))) :: l ] 
                  | _ ->  l
                  ]) 
   ])
;
value compute_athematic_impft2m strong weak set root = 
  let conjugs person suff = 
      (person,if root = "bruu" then fix2sbruu_augment suff 
              else fix2s_augment strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan_augment suff 
              else fix2w_augment weak suff set) in
  enter1 root (Conju (impftm 2)
   [ (Singular, let l =
        [ if root = "i" then conjugw First "yi" (* adhyaiyi Bucknell 128 *)
          else conjugw First "i"
        ; conjugw Second "thaas"
        ; conjugw Third  "ta"
        ] in if root = "m.rj" then [ conjugs First "i" :: l ]
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "vahi"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ] in if root = "m.rj" then 
                [ conjugs Second "aathaam"
                ; conjugs Third  "aataam"
                ] @ l else l (* Whitney§627 *))
    ; (Plural, let l =
        [ conjugw First  "mahi"
        ; if root = "aas#2" then (Second, code "aadhvam") (* -Whitney§620 *) 
          else conjugw Second "dhvam"
        ; if root = "zii#1" then conjugw Third "rata" (* \Pan{7,1,6} *) else
          if root = "i" then conjugw Third "yata" (* Bucknell 128 *) else 
          conjugw Third "ata"
        ] in if root = "m.rj" then [ conjugs Third "ata" :: l ] else
             if root ="duh#1" then [ conjugw Third "ra" :: l ]
             (* aduhata -> aduha-a = \Pan{7,1,41} aduha -> aduhra \Pan{7,1,8} *)
             else l (* Whitney§627 *))
   ]) 
;
value compute_athematic_optative2a weak set root =
  let conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
                                 else fix2w weak suff set) in
  enter1 root (Conju (opta 2)
   [ (Singular, let l =
        [ conjugw First  "yaam"
        ; conjugw Second "yaas"
        ; conjugw Third  "yaat"
        ] in if root = "bruu" 
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
value compute_athematic_optative2m weak set root =
  let conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
              else if root = "i" then fix2wi suff (* adhiiyiita *)
              else fix2w weak suff set)
  and conjugwmrij person suff = (person, fix2 (revcode "maarj") suff set) in
  enter1 root (Conju (optm 2)
   [ (Singular, let l = (* ii below replaced by iyii for root i ? *)
        [ conjugw First  "iiya"
        ; conjugw Second "iithaas"
        ; conjugw Third  "iita"
        ] in if root = "m.rj" then 
                [ conjugwmrij First  "iiya"
                ; conjugwmrij Second "iithaas"
                ; conjugwmrij Third  "iita"
                ] @ l 
             else l (* Whitney§627 *))
   ; (Dual, let l =
        [ conjugw First  "iivahi"
        ; conjugw Second "iiyaathaam"
        ; conjugw Third  "iiyaataam"
        ] in if root = "m.rj" then 
                [ conjugwmrij First  "iivahi"
                ; conjugwmrij Second "iiyaathaam"
                ; conjugwmrij Third  "iiyaataam"
                ] @ l 
             else l (* Whitney§627 *))
   ; (Plural, let l =
        [ conjugw First  "iimahi"
        ; conjugw Second "iidhvam"
        ; conjugw Third  "iiran" (* special dropping n TODO see Kane§429 *)
        ] in if root = "m.rj" then 
                [ conjugwmrij First  "iimahi"
                ; conjugwmrij Second "iidhvam"
                ; conjugwmrij Third  "iiran"
                ] @ l 
             else l (* Whitney§627 *))
   ])
;
value compute_athematic_imperative2a strong weak set root =
  let conjugs person suff = 
      (person,if root = "bruu" then fix2sbruu suff 
                                else fix2s strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
                                 else fix2w weak suff set) in
  enter1 root (Conju (impera 2)
   [ (Singular, let l =
        [ conjugs First "aani"
        ; (Second, match root with
          [ "as#1" -> code "edhi"
          | "zaas" -> code "zaadhi" 
 (* above leads to conflict between \Pan{6,4,35} (zaa+hi) and \Pan{6,4,101} 
    (zaas+dhi) [asiddhavat] => we operate in parallel zaa+dhi= zaadhi *)
          | "cakaas" -> code "cakaadhi" (* Kane§429 *)  
          | _ -> let w = if root = "han#1" then revcode "ja" else weak in
                 match w with 
            [ [ c :: _  ] -> fix2 w suff set
              where suff = if vowel c || set then "hi" else "dhi"
            | _ -> error_empty 6
            ] (* "dhi" or "hi" after vowel *)
          ])
        ; conjugs Third "tu"
        ] in if root = "vac" then 
                [ (Second, code "voci"); (Third, code "vocatu") ] @ l
             else if root ="bruu" then [ conjugs Second "hi" :: l ]
                  (* braviihi Whitney§632 *)
             else if root ="cakaas" then [ (Second, code "cakaadvi") :: l ]
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
        ; if root = "zaas" then conjugs Third "atu" (* \Pan{7,1,4} *)
        else conjugw Third (if abhyasta root then "atu" else "antu") 
        ] in if root = "m.rj" then [ conjugs Third "antu" :: l ]
             else l (* Whitney§627 *))
   ])
;
value compute_athematic_imperative2m strong weak set root =
  let conjugs person suff = 
      (person,if root = "bruu" then fix2sbruu suff 
              else fix2s strong suff set)
  and conjugw person suff =
      (person,if root = "han#1" then fix2whan suff 
              else fix2w weak suff set) in
  let conjugf = if root = "suu#1" then conjugw (* Bandharkar II p 37 *)
                else if root = "dvi.s#1" then conjugw (* DRP I p 664 *)
                  (* consistent with KU{2,3,19} vidvi.saavahai but not Wh§617b *)
                else conjugs in
  enter1 root (Conju (imperm 2)
   [ (Singular, 
        [ conjugf First  "ai"
        ; conjugw Second "sva"
        ; conjugw Third  "taam"
        ])
   ; (Dual, let l =
        [ conjugf First  "aavahai"
        ; conjugw Second "aathaam"
        ; conjugw Third  "aataam"
        ] in if root = "m.rj" then 
                [ conjugs Second "aathaam"
                ; conjugs Third  "aataam"
                ] @ l
             else l (* Whitney§627 *))
   ; (Plural, let l =
        [ conjugf First  "aamahai"
        ; if root = "aas#2" then (Second, code "aadhvam") (* -Whitney§617 *) 
          else conjugw Second "dhvam"
        ; if root = "zii#1" then conjugw Third "rataam" (* \Pan{7,1,6} *)
          else conjugw Third "ataam"
        ] in if root = "m.rj" then [ conjugs Third "ataam" :: l ]
             else l (* Whitney§627 *))
   ])
;
value compute_active_present2 sstem wstem set root third = do
  { compute_athematic_present2a sstem wstem set root third
  ; let weak = if root = "as#1" then [ 48; 1 ] else wstem in
    compute_athematic_impft2a sstem weak set root 
  ; compute_athematic_optative2a wstem set root 
  ; compute_athematic_imperative2a sstem wstem set root 
  ; match wstem with 
    [ [ 2 :: _ ] -> (* Ppr of roots in -aa is complex and overgenerates *)
      match root with 
      [ "bhaa#1" | "maa#1" | "yaa#1" -> () (* no known ppra *)
      | _ -> let m_pstem = wstem and f_pstem = rev (fix2w wstem "at" set) in 
             record_part (Ppra_ 2 Primary m_pstem f_pstem root) 
      ]
    | _ -> let m_pstem = if root = "han#1" then revstem "ghn" 
                         else correct2 wstem in
           let f_pstem = if root = "han#1" then revstem "ghnat" 
                         else rev (fix2w wstem "at" set) in 
           record_part (Ppra_ 2 Primary m_pstem f_pstem root)
    ]
  ; if root = "m.rj" then let m_pstem = revstem "maarj" in
                          let f_pstem = revstem "maarjat" in
                          record_part (Ppra_ 2 Primary m_pstem f_pstem root)
    else ()
  }
and compute_middle_present2 sstem wstem set root third = do
  { compute_athematic_present2m sstem wstem set root third
  ; compute_athematic_impft2m sstem wstem set root 
  ; compute_athematic_optative2m wstem set root 
  ; compute_athematic_imperative2m sstem wstem set root 
  ; match root with
    [ "iiz#1" | "maa#1" -> () (* no pprm *)
    | "i" -> record_part_m_ath (pprm 2) [ 42; 3 ] root (* iyaana *)
    | _ -> record_part_m_ath (pprm 2) (correct2 wstem) root
    ]
  }
;

(*** Gana 3  ***)

value fix3w wstem iiflag dadh suff = 
  let codesf = code suff in 
  let short = if iiflag then trunc_ii wstem else wstem in
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
value compute_athematic_present3a strong weak iiflag root third = 
  let dadh_flag = (root="dhaa#1") in 
  let conjugs person suff = (person,fix strong suff) (* gu.na *)
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix (revstem "jahi") suff) 
                              (* weak = jahii but optionally jahi *)
  and haa_flag = (root="haa#1") in do
  { enter1 root (Conju (presa 3)
   [ (Singular, 
        [ conjugs First  "mi" (* Panini mip, where p indicates guna *)
        ; conjugs Second "si" (* sip *)
        ; check root 3 third (conjugs Third "ti") (* tip *)
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
        ; if root="bhas" then (Third, code "bapsati") (* Whitney§678 MW§340 *) 
          else conjugw Third  "ati" 
        ] in if haa_flag then l @ 
                [ conjughaa First  "mas"
                ; conjughaa Second "tha"
                ]    
             else l)
   ])
  ; let wstem = if iiflag then trunc_ii weak else 
                if root="bhas" then revcode "baps" (* Whitney§678 *) 
                else weak in (* 3rd pl weak stem *)
    record_part (Pprared_ Primary wstem root) 
  }
;
value compute_athematic_present3m conj gana weak iiflag root third = 
  let dadh_flag = (root="dhaa#1") in
  let conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  enter1 root (Conju (fpresm gana conj)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check root 3 third (conjugw Third "te") 
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
value compute_athematic_impft3a strong weak iiflag root = 
  let dadh_flag = (root="dhaa#1") in
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix3w_augment weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix_augment (revstem "jahi") suff) 
  and haa_flag = (root="haa#1") in 
  enter1 root (Conju (impfta 3)
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
(* common to [impft_m] and [root_aoristm] *)
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
value compute_athematic_impft3m weak iiflag root = 
  let dadh_flag = (root="dhaa#1") in
  let conjugw person suff = (person,fix3w_augment weak iiflag dadh_flag suff) in
  enter1 root (conjug_impft_m 3 conjugw)
;
(* Like [compute_athematic_optative2a] except for [yan#1] and [bruu] *)
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
value compute_athematic_optative3a weak iiflag root =
  let dadh_flag = (root="dhaa#1") in 
  let conjugw person suff = (person,
      if root="haa#1" then fix (revstem "jah") suff
      else fix3w weak iiflag dadh_flag suff) in
  enter1 root (conjug_opt_ath_a 3 conjugw)
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
value compute_athematic_optative3m weak iiflag root =
  let dadh_flag = (root="dhaa#1") in 
  let conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  enter1 root (conjug_opt_ath_m 3 conjugw)
;
value compute_athematic_imperative3a strong weak iiflag root =
  let dadh_flag = (root="dhaa#1") 
  and daa_flag  = (root="daa#1") 
  and haa_flag  = (root="haa#1") in 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff)
  and conjughaa person suff = (person,fix (revstem "jahi") suff) in
  enter1 root (Conju (impera 3)
   [ (Singular, let l = 
        [ conjugs First "aani"
        ; (Second, if daa_flag then code "dehi" (* \Pan{4,4,119} *)
                   else if dadh_flag then code "dhehi" (* idem ghu \Pan{1,1,20} *)
                   else match weak with 
            [ [ c :: _  ] -> fix3w weak iiflag dadh_flag suff 
              where suff = if vowel c then (* "dhi" or "hi" after vowel *)
                              if root = "hu" then "dhi" else "hi" 
                              (* "hu" only exception \Pan{6,4,101} Müller p153 *)
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
        ; if root="bhas" then (Third, code "babdhaam") (* Whitney§678 MW§340 *) 
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
value compute_imp_ath_m gana conjugs conjugw root =
  enter1 root (Conju (imperm gana)
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
value compute_athematic_imperative3m strong weak iiflag root =
  let dadh_flag = (root="dhaa#1") in
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag dadh_flag suff) in
  compute_imp_ath_m 3 conjugs conjugw root 
;
value compute_active_present3 sstem wstem iiflag root third = do
  { compute_athematic_present3a sstem wstem iiflag root third
  ; compute_athematic_impft3a sstem wstem iiflag root 
  ; compute_athematic_optative3a wstem iiflag root 
  ; compute_athematic_imperative3a sstem wstem iiflag root 
  } 
and compute_middle_present3 sstem wstem iiflag root third = do 
  { compute_athematic_present3m Primary 3 wstem iiflag root third
  ; compute_athematic_impft3m wstem iiflag root 
  ; compute_athematic_optative3m wstem iiflag root 
  ; compute_athematic_imperative3m sstem wstem iiflag root 
  ; let short = if iiflag then trunc_ii wstem else wstem in 
    record_part_m_ath (pprm 3) short root
  }
;

(*** Gana 5  ***)

value compute_athematic_present5a gana strong weak vow root third = 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff) (* \Pan{6,4,77} *) 
        else (person,fix weak suff)
      | [] -> error_suffix 9
      ]
  and conjugw2 person suff = match weak with 
      [ [ 5 :: no_u ] -> (person,fix no_u suff)
      | _ -> failwith "5a weak ought to end in u"
      ] in do
  { enter1 root (Conju (presa gana)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check root gana third (conjugs Third "ti") 
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
    record_part (Ppra_ 5 Primary m_pstem f_pstem root)
  }
;
value compute_athematic_present5m gana weak vow root third = 
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
  enter1 root (Conju (presm gana)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check root gana third (conjugw Third "te") 
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
value compute_athematic_impft5a gana strong weak vow root = 
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
  enter1 root (Conju (impfta gana)
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
value compute_athematic_impft5m gana weak vow root = 
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
  enter1 root (Conju (impftm gana)
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
value compute_athematic_optative5a gana weak vow root = (* gana=5 or 8 *)
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff)
        else (person,fix weak suff)
      | [] -> error_suffix 11
      ] in
  enter1 root (conjug_opt_ath_a gana conjugw)
;
value compute_athematic_optative5m gana weak vow root = (* gana=5 or 8 *)
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
           (person,fix w suff)
        else (person,fix weak suff)
      | [] -> error_suffix 19
      ] in
  enter1 root (conjug_opt_ath_m gana conjugw)
;
value compute_athematic_imperative5a gana strong weak vow root = (* gana=5 or 8 *)
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> if vowel c then 
                         let w = if vow then weak else [ 45 (* v *) :: weak ] in
                         (person,fix w suff)
                      else (person,fix weak suff)
      | [] -> (person,fix weak "")
      ] in
  enter1 root (Conju (impera gana)
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
value compute_athematic_imperative5m gana strong weak vow root = (* gana=5 or 8 *)
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> 
        if vowel c then 
           let w = if vow then weak else [ 45 (* v *) :: weak ] in
              (person,fix w suff)
        else  (person,fix weak suff)
      | [] -> (person,fix weak "")
      ] in
  compute_imp_ath_m gana conjugs conjugw root 
;
(* Used by classes 5 and 8 *)
value compute_active_present5 gana sstem wstem vow root third = do 
  { compute_athematic_present5a gana sstem wstem vow root third
  ; compute_athematic_impft5a gana sstem wstem vow root 
  ; compute_athematic_optative5a gana wstem vow root 
  ; compute_athematic_imperative5a gana sstem wstem vow root 
  } 
and compute_middle_present5 gana sstem wstem vow root third = do 
  { compute_athematic_present5m gana wstem vow root third
  ; compute_athematic_impft5m gana wstem vow root 
  ; compute_athematic_optative5m gana wstem vow root 
  ; compute_athematic_imperative5m gana sstem wstem vow root 
  ; record_part_m_ath (pprm 5) wstem root
  }
;
(* Also used by gana 8 *)
value compute_present5 gana sstem wstem vow root third pada padam =
  match voices_of_gana gana root with
       [ Para -> if pada then
           compute_active_present5 gana sstem wstem vow root third
           else emit_warning ("Unexpected middle form: " ^ root)
       | Atma -> if padam then emit_warning ("Unexpected active form: " ^ root)
           else compute_middle_present5 gana sstem wstem vow root third
       | Ubha ->             
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present5 gana sstem wstem vow root thirda
          ; compute_middle_present5 gana sstem wstem vow root thirdm
          }
       ]
;

(*** Gana 7  ***)

value compute_athematic_present7a strong weak root third =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 root (Conju (presa 7)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check root 7 third (conjugs Third "ti") 
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
    record_part (Ppra_ 7 Primary m_pstem f_pstem root) 
  }
;
value compute_athematic_present7m weak root third = 
  let conjugw person suff = (person,fix weak suff) in
  enter1 root (Conju (presm 7)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check root 7 third (conjugw Third "te") 
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
value compute_athematic_impft7a strong weak root = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix_augment weak suff) in
  enter1 root (Conju (impfta 7)
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
value compute_athematic_impft7m weak root = 
  let conjugw person suff = (person,fix_augment weak suff) in
  enter1 root (Conju (impftm 7)
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
value compute_athematic_optative7a weak root =
  let glue = if root = "hi.ms" then fun w s -> 
                List2.unstack w (code s) (* no retroflexion Whitney§183a *)
             else fix in 
  let conjugw person suff = (person,glue weak suff) in 
  enter1 root (conjug_opt_ath_a 7 conjugw)
;
value compute_athematic_optative7m weak root =
  let conjugw person suff = (person,fix weak suff) in
  enter1 root (conjug_opt_ath_m 7 conjugw)
;
value compute_athematic_imperative7a strong weak root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in
  enter1 root (Conju (impera 7)
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
value compute_athematic_imperative7m strong weak root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in
  compute_imp_ath_m 7 conjugs conjugw root 
;
value compute_active_present7 sstem wstem root third = do
  { compute_athematic_present7a sstem wstem root third
  ; compute_athematic_impft7a sstem wstem root 
  ; compute_athematic_optative7a wstem root 
  ; compute_athematic_imperative7a sstem wstem root 
  } 
and compute_middle_present7 sstem wstem root third = do
  { compute_athematic_present7m wstem root third
  ; compute_athematic_impft7m wstem root 
  ; compute_athematic_optative7m wstem root 
  ; compute_athematic_imperative7m sstem wstem root 
  ; record_part_m_ath (pprm 7) wstem root
  }
;
value compute_present7 sstem wstem root third pada padam = 
  match voices_of_gana 7 root with
  [ Para -> if pada then compute_active_present7 sstem wstem root third
            else emit_warning ("Unexpected middle form: " ^ root)
  | Atma -> if padam then emit_warning ("Unexpected active form: " ^ root)
            else compute_middle_present7 sstem wstem root third
  | Ubha -> let thirda = if pada then third else []
            and thirdm = if pada then [] else third in do
            { compute_active_present7 sstem wstem root thirda
            ; compute_middle_present7 sstem wstem root thirdm
            }
  ]
;

(*** Gana 8  ***)

(* Conjugation of k.r *)     (* "karo" "kuru" "kur" *)
value compute_athematic_presentk strong weak short root third = 
  let conjugs person suff = (person,fix strong suff) 
  and conjugw person suff = (person,fix weak suff)
  and conjugwvm person suff = (person,fix short suff) (* -v -m suff *) in do
  { enter1 root (Conju (presa 8)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check root 8 third (conjugs Third "ti") 
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
    record_part (Ppra_ 8 Primary weak f_pstem root) 
  ; record_part_m_ath (pprm 8) weak root
  ; enter1 root (Conju (presm 8)
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
value compute_athematic_impftk strong weak short root = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix_augment weak suff)
  and conjugwvm person suff = (person,fix_augment short suff) (* -v -m suff *) in do
  { enter1 root (Conju (impfta 8)
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
  ; enter1 root (Conju (impftm 8) (* similar to [conjugs_past_m] except for -v -m suff *)
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
value compute_athematic_optativek weak short root =
  let conjugw person suff = (person,fix weak suff)
  and conjugs person suff = (person,fix short suff) in do
  { enter1 root (conjug_opt_ath_a 8 conjugs) (* short since -y suffixes *)
  ; enter1 root (conjug_opt_ath_m 8 conjugw)
  }
;
value compute_athematic_imperativek strong weak root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 root (Conju (impera 8)
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
  ; compute_imp_ath_m 8 conjugs conjugw root 
  }
;
value compute_presentk sstem wstem short root third = do
  { compute_athematic_presentk sstem wstem short root third
  ; compute_athematic_impftk sstem wstem short root 
  ; compute_athematic_optativek wstem short root 
  ; compute_athematic_imperativek sstem wstem root 
  }
;

(*** Gana 9  ***)

value compute_athematic_present9a strong weak short root third = 
  let conjugs person suff = (person,fix strong suff) 
  and conjugw_v person suff = (person,fix short suff) (* vowel suffix *)
  and conjugw_c person suff = (person,fix weak suff) (* consonant suffix *) in do
  { enter1 root (Conju (presa 9)
   [ (Singular, 
        [ conjugs First  "mi"
        ; conjugs Second "si"
        ; check root 9 third (conjugs Third "ti") 
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
    record_part (Ppra_ 9 Primary short f_pstem root) (* follows 3rd pl *) 
  }
;
value compute_athematic_present9m weak short root third = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> error_suffix 16
      ] in
  enter1 root (Conju (presm 9)
   [ (Singular, 
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; check root 9 third (conjugw Third "te") 
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
value compute_athematic_impft9a strong weak short root = 
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix_augment w suff)
      | [] -> error_suffix 6
      ] in
  enter1 root (Conju (impfta 9)
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
value compute_athematic_impft9m weak short root = 
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix_augment w suff)
      | [] -> error_suffix 13
      ] in
  enter1 root (conjug_impft_m 9 conjugw)
;
value compute_athematic_optative9a weak short root =
  let conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in (* tjs y- *)
                      (person,fix w suff)
      | [] -> error_suffix 14
      ] in
  enter1 root (conjug_opt_ath_a 9 conjugw)
;
value compute_athematic_optative9m short root =
  let conjugw person suff = (person,fix short suff) in (* suff starts with ii *)
  enter1 root (conjug_opt_ath_m 9 conjugw) 
;
value compute_athematic_imperative9a strong weak short vow stem root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> (person,fix weak "")
      ] 
  and conjugw2 person suff = (person,fix stem suff) in
  enter1 root (Conju (impera 9)
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
value compute_athematic_imperative9m strong weak short root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = match code suff with
      [ [ c :: _ ] -> let w = if vowel c then short else weak in
                      (person,fix w suff)
      | [] -> (person,fix weak "")
      ] in
  compute_imp_ath_m 9 conjugs conjugw root 
;
value compute_active_present9 sstem wstem short vow stem root third = do
  { compute_athematic_present9a sstem wstem short root third
  ; compute_athematic_impft9a sstem wstem short root 
  ; compute_athematic_optative9a wstem short root 
  ; compute_athematic_imperative9a sstem wstem short vow stem root 
  } 
and compute_middle_present9 sstem wstem short root third = do
  { compute_athematic_present9m wstem short root third
  ; compute_athematic_impft9m wstem short root 
  ; compute_athematic_optative9m short root 
  ; compute_athematic_imperative9m sstem wstem short root 
  ; record_part_m_ath (pprm 9) short root (* short and not wstem *)
  }
;
value compute_present9 sstem wstem short vow stem root third pada padam = 
  match voices_of_gana 9 root with
  [ Para -> if pada then 
               compute_active_present9 sstem wstem short vow stem root third
            else emit_warning ("Unexpected middle form: " ^ root)
  | Atma -> if padam then emit_warning ("Unexpected active form: " ^ root)
            else compute_middle_present9 sstem wstem short root third
  | Ubha -> let thirda = if pada then third else []
            and thirdm = if pada then [] else third in do
            { compute_active_present9 sstem wstem short vow stem root thirda
            ; compute_middle_present9 sstem wstem short root thirdm
            }
  ]
;

(* Benedictive/precative, formed from [conjug_optativea] with aorist stem *)
(* NB. Whitney§837 makes it an optative mode of the root aorist *)
value conjug_benedictivea conj weak root = 
  let conjugw person suff = (person,fix weak suff) in
  enter1 root 
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
value conjug_benedictivem conj sibstem root =
  let conjug person suff = (person,fix sibstem suff) in
  enter1 root 
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
value compute_benedictive rstem root = 
   (* Macdonell§150 Kale§960 Whitney§924 Henry§298 *)
  let bene_stem = let ps_stem = passive_stem root rstem in
      match root with (* Deshpande gram p328 *)
      [ "daa#1" | "paa#1" | "sthaa#1" | "haa#1" -> (* not "j~naa#1" *) 
           match ps_stem with 
           [ [ 4 (* ii *) :: rest ] -> [ 10 (* e *) :: rest ] (* ii -> e *)
           | _ -> failwith "Anomaly bene_stem"
           ] (* NB Deshpande: also j~naayaat *)
      | "puu#1" -> revcode "punii" (* weak gana 9 puniiyaat Vi.s.nu sahasr. *)
      | "paa#2" -> revcode "paa" (* paayaat *)
      | _ -> ps_stem
      ] in do
  { match root with 
    [ "gur" | "pyaa" (* pii *) -> ()
    | _ -> conjug_benedictivea Primary bene_stem root (* productive, but rare *)
    ]
    (* middle very rare: viik.si.siiran et pratipatsiiran in Abhisamayaalafkaara
       (David Reigle) and k.r.sii.s.ta in BhP and stotras (Harry Spier) *)
  ; match root with 
    [ "bhuu#1" -> let sibstem = revcode "bhavi.s" in 
        conjug_benedictivem Primary sibstem root (* bhavi.sii.s.ta *)
    | "k.r#1" -> let sibstem = revcode "k.r.s" in (* k.r.sii.s.ta *)
        conjug_benedictivem Primary sibstem root (* Kanakadhaarastotra *)
    | "iik.s" -> let sibstem = revcode "iik.si.s" in 
        conjug_benedictivem Primary sibstem root (* viik.si.siiran *)
    | "j~naa#1" -> let sibstem = revcode "j~naas" in 
        conjug_benedictivem Primary sibstem root (* j~naasi.s.ta Deshpande *)
    | "daa#1" -> let sibstem = revcode "daas" in 
        conjug_benedictivem Primary sibstem root (* daasi.s.ta Deshpande *)
    | "pad#1" -> let sibstem = revcode "pats" in 
        conjug_benedictivem Primary sibstem root (* pratipatsiiran *)
    | "m.r" -> let sibstem = revcode "m.r.s" in 
        conjug_benedictivem Primary sibstem root (* m.r.sii.s.ta \Pan{1,3,61} *)
    | "luu#1" -> let sibstem = revcode "lavi.s" in
        conjug_benedictivem Primary sibstem root (* lavi.sii.s.ta \Pan{3,4,116} *)
    | "suu#1" -> let sibstem = revcode "savi.s" in
        conjug_benedictivem Primary sibstem root (* \Pan{3,4,116} BhG{3,10} *)
    | "gur" -> let sibstem = revcode "guri.s" in
        conjug_benedictivem Primary sibstem root (* Kale *)
    | _ -> ()
    ]
  }
;
(*****************)
(* Future system *)
(*****************)

(* Similar to [compute_thematic_paradigm_act] *)
value compute_futurea conj stem root = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 root (Conju (ffutura conj)
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
  ; record_part (Pfuta_ conj stem root) 
  }
;
value compute_futurem conj stem root = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 root (Conju (ffuturm conj)
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
  ; record_part_m_th pfutm stem root
  }
;
(* NB Passive forms of future, conditional, aorist and benedictive may be formed
   for certain roots according to Bandharkar II p 103 -- not implemented here *)

(* Conditional - preterit of future, built from imperfect on future stem   *)
(* where non-performance of the action is implied - pluperfect conditional *)
(* Speaks of things that might have, but have not happened \Pan{3,3,139}   *)
(* Used in antecedent as well as in consequent clause - Apte§216           *)
(* "si vous étiez venu, vous l'auriez vue" *)
value compute_conda conj stem root = 
  let conjug person suff = (person,fix_augment stem suff) in 
  enter1 root (Conju (fconda conj) (thematic_preterit_a conjug))
;
value compute_condm conj stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (fcondm conj) (thematic_preterit_m conjug))
;
value compute_future stem root = 
  match root with
    [ "iiz#1" | "lii" | "knuu" | "baadh" -> do (* Para allowed in future *)
         { compute_futurea Primary stem root 
         ; compute_futurem Primary stem root  
         }
    | _ -> match voices_of root with
       [ Para -> do (* active only *) 
         { compute_futurea Primary stem root 
         ; match root with (* conditional or Atma on demand *)
           [ "ku.t" (* Kale *)| "jiiv" | "tyaj#1" | "d.rz#1" | "pat#1" | "bha.n"
           | "bhii#1" |  "bhuu#1" | "zaas" | "stu" | "sm.r" 
                     -> compute_conda Primary stem root
           | "khaad" | "gad" | "vac" (* BhG pravak.sye *) 
                     -> compute_futurem Primary stem root 
           | "haa#1" -> do { compute_futurem Primary stem root (* manquer *)
                           ; compute_conda Primary stem root
                           }
           | _ -> ()
           ]
         }
       | Atma -> do (* middle only *) 
         { compute_futurem Primary stem root
         ; match root with (* rare conditional *)
           [ "gur" (* Kale *) -> compute_condm Primary stem root 
           | _ -> ()
           ]
         }
       | Ubha -> (* both *) do
         { compute_futurea Primary stem root 
         ; compute_futurem Primary stem root 
         ; match root with (* rare conditional *)
           [ "i" | "k.r#1" | "gam" | "grah" | "ji" | "j~naa#1" | "tap"
           | "daa#1" | "nii#1" | "bandh" | "budh#1" | "bhid#1" | "bh.r" | "m.r"
           | "yaj#1" | "vad" | "sthaa#1"| "hu" -> do
              { compute_conda Primary stem root 
              ; compute_condm Primary stem root 
              }
           | _ -> ()
           ]
         }
       ]
    ]
;
value compute_future_ca stem root = do
  { compute_futurea Causative stem root 
  ; compute_futurem Causative stem root 
  ; match root with (* rare conditional *)
    [ "j~naa#1" -> do
       { compute_conda Causative stem root 
       ; compute_condm Causative stem root 
       }
    | _ -> ()
    ]
  ; record_part_m_th pcausfm stem root
  }
;
(* Possible intercalating vowel i for se.t and ve.t roots Whitney§935 *)
(* [intercalates] returns a set of possible intercalations.           *)
(* 3 indicates metathesis: ar becomes ra by [ar_ra] below             *)
(* 4 is specific to naz1 nasalisation                                  *)
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
       else if semivowel c then set (* mil WR but not Kale *)
       else match root with
            [ "k.rt#1" | "c.rt" | "ch.rd" | "t.rd" | "n.rt" (* \Pan{7,2,57} *)
            | "ak.s" | "a~nj" | "k.rp" | "k.lp" | "kram" | "k.sam" 
            | "klid" | "kliz" | "gup" | "guh" | "ghu.s" | "jan" | "ta~nc" 
            | "tap" | "tyaj#1" | "dah#1" | "d.rp" | "nam" | "naz#1" 
            | "bandh" | "budh#1" | "bhaj" | "majj" | "man" | "m.rj"
            | "yam" | "ruh" | "labh" | "likh" | "vap#2" | "vas#1" | "vah#1" 
            | "vij" | "vid#1" | "v.rj" | "v.rt#1" | "v.rdh#1" | "vrazc" | "z.rdh"
            | "sad#1" | "sah#1" | "sidh#2" | "svap" | "han#1" | "syand" 
            (* Pan{7,2,59} set A anit P "v.rt#1" "v.rdh#1" "z.rdh" "syand" 
               for these 4 roots, we generate both forms in Atma and in Para *)
                -> vet 
            | "grah" -> setl (* \Pan{7,2,37} g.rhiita g.rhiitvaa *)
            | "s.rj#1" -> [ 3 ] (* sra.s.taa *)
            | "k.r.s" -> [ 3 :: vet ] (* ar -> ra optionally *)
            | "bh.rjj" | "sp.rz#1" -> [ 3 :: anit ] (* idem *)
            | "ad#1" | "aap" | "krudh#1" | "kruz" | "k.sip" | "k.sud" 
            | "k.sudh#1" | "khid" | "ghas" | "chid#1" | "tud#1" | "tu.s" 
            | "t.rp#1" | "tvi.s#1" | "diz#1" | "dih" | "du.s" | "duh#1" 
            | "d.rz#1" | "dvi.s#1" | "nah" | "nij" | "nud" | "pac" | "pad#1" 
            | "pi.s" | "pu.s#1" | "praz" | "bha~nj" | "bha.s" | "bhid#1"
            | "bhuj#1" | "bhuj#2" | "mih" | "muc#1" | "m.rz" | "yaj#1" | "yabh" 
            | "yuj#1" | "yudh#1" | "ra~nj" | "rabh" | "ram" | "raadh" | "ric"
            | "ruj#1" | "rudh#1" | "rudh#2" | "ruh#1" | "lip" | "liz" | "lih#1"
            | "lup" | "vac" | "vap#1" | "vic" | "vid#2" | "viz#1" | "vi.s#1" 
            | "vyadh" | "zak" | "zad" | "zap" | "zi.s" | "zudh" | "zu.s#1" 
            | "zli.s" | "sa~nj" | "sic" | "sidh#1" | "s.rp" | "skand" 
            | "sva~nj" | "svid#2" | "had" 
                -> anit 
            | _ -> set (* default all multisyllabic, gana 10, nominal verbs plus:
[ "afg" | "a~nc" | "an#2" | "arh" | "av" | "az#1" | "az#2" | "as#2" | "aas#2"
| "indh" | "inv" | "i.s#1" | "i.s#2" | "iik.s" | "iifkh" | "ii.d" | "iiz#1" 
| "uc" | "u~nch" | "umbh" | "uuh" | ".rc#1" | ".rj" | ".rdh" | "edh" | "kafk"
| "kam" | "kamp" | "ka.s" | "kaafk.s" | "ku.n.th" | "ku.n.d" | "kup" | "krand" 
| "krii.d" | "khan" | "khaad" | "gu~nj" | "gam" (* \Pan{7,2,37} *) | "ghu.s" 
| "ghaat" | "ghuur.n"
| "cand" | "cit#1" | "cumb" | "chand" | "jak.s" | "jap" | "jalp" | "jinv"
| "j.rmbh" | "tak" | "tan#1" | "tan#2" | "tark" | "tvar" | "dagh" | "dabh" 
| "dham" | "dhva.ms" | "dhvan" | "nand" | "nind" | "pa.th" | "pat#1" | "pi~nj"
| "piz" | "ba.mh" | "bhand" | "bhaa.s" | "bhraaj" | "ma.mh" | "ma.n.d" | "mad#1"
| "mand#1" | "mlecch" | "yat#1" | "yas" | "yaac" | "ra.mh" | "rak.s" | "raaj#1" 
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
  [ "an#2" (* and thus "praa.n1" too gives praa.niit *) 
  | "rud#1" | "zvas#1" | "svap" | "jak.s" -> True 
  | _ -> False 
  ]
;

(* Perfect passive participle *)

value intercalate_pp root rstem = 
(* some redundancy with intercalates but really different, 
   specially since the default is anit for verbs ending with single consonant *)
(* Pan{7,2,28ff} give vet for -kta as special interpolation *)
  let anit = [ 0 ]    (* no intercalation *)
  and set  = [ 1 ]    (* intercalate i *)
  and vet  = [ 0; 1 ] (* intercalate i optionally *) in 
  match rstem with
  [ [ c :: r ] -> 
     if vowel c then 
        match root with
        [ "jaag.r" | "zii#1" -> set
        | "dhmaa" -> vet
        | _ -> anit 
        ]
  (* else if semivowel c then set (* consistent with intercalates *) TO CHECK *)
     else match r with
       [ [ v :: _ ] when vowel v -> 
           match root with 
            (* TODO utiliser intercalates sauf exceptions *)
           [ "radh" | "naz#1" | "trap" | "d.rp" | "druh#1" | "muh" | "jap"
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
                           (* also "ru.s#1" et "sa.m-ghu.h" "aa-svan" *)
           | "kas" | "k.sam" | "gup" | "dyut#1" | "dham" | "nud" 
           | "m.rj" -> vet 
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
           | "baadh" | "bha.n" | "bhas" | "bhaam" | "bhaa.s" | "bhaas#1" 
           | "bhuu.s" | "bhraaj" | "ma.mh" | "manth" | "mah" | "likh" | "mil" 
           | "mi.s" | "miil" | "mud#1" | "mu.s#1" | "m.rg" | "yaac" | "rac" 
           | "ra.t" | "ra.n" | "ras" | "rah" | "raaj#1" | "ruc#1" | "rud#1" 
           | "lag" | "lap" | "lal" | "la.s" | "las" | "lu.th" | "lul" | "lok" 
           | "loc" | "vad" | "van" | "val" | "vaz" | "vas#2" | "vaaz"| "vaas#3"
           | "vid#1" | "vip"| "ven" | "vyath" | "vraj" | "vra.n" | "vrii.d" 
           | "zubh#1" | "zcut#1" | "zrath" | "zlath" | "zlaagh" | "zvas#1" 
           | "suuc" | "suud" | "sev" | "skhal" | "stan" | "stim" | "sthag"
           | "sphu.t" | "sphur" | "svad" | "svan" | "svar#1" | "has" | "hras" 
           | "hraad" | "hlaad" | "hval" | ".dhauk" 
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
  | "ka.s" | "k.rt#1" | "dh.r.s" | "am" | "tvar" | ".r.s" | "v.rj" -> set 
  | "nud" -> anit
  | "k.lp" -> vet (* Bucknell *)
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
value compute_ppp_stems root rstem = 
  match root with
      (* First participles in -na *) 
    [ "vrazc" -> [ sNa "v.rk" ] (* exception - v.rk root stem of vrazc *)
    (* Most roots starting with 2 consonants take -na \Pan{8,2,43} *)
    (* but not "k.svi.d" "zrath" *)
    (* "ad1" could give "anna", declared unaadi at present *)
    | "iir" | "und" | "k.rr" | "klid" | "k.saa" | "k.sii" | "k.sud" | "k.svid"
    | "khid" | "g.rr#1" | "glaa" | "chad#1" | "chid#1" | "ch.rd" | "j.rr" 
    | ".dii" | "tud#1" | "t.rd" | "t.rr" | "dagh" | "d.rr" | "dev" | "draa#1"
    | "draa#2" | "nud" | "pad#1" | "pii" | "p.rr" | "pyaa" | "bha~nj" 
    | "bhid#1" | "bhuj#1" | "majj" | "mid" | "mlaa" | "ri" | "ruj#1"
    | "lii" | "luu#1" | "vij" | "vid#2" | "vrii" | "vlii" | "zad" | "zuu" 
    | "z.rr" | "sad#1" | "skand" | "st.rr" | "styaa" | "syand" | "svid#2" | "had"
    | "haa#2" -> 
      (* except lag which is "nipaatana" (exception) \Pan{7,2,18} *)
      let ppna w = [ Na w ] in
      match rstem with 
      [ [ 2 :: _ ] | [ 4 :: _ ] | [ 6 :: _ ] (* stems in aa ii uu *)
        -> let ppn = ppna rstem in
           match root with [ "pyaa" -> [ Ta rstem :: ppn ] (* 2 forms *)
                           | _ -> ppn
                           ]
      | [ 3 :: r ] -> ppna [ 4 :: r ]  (* piina rii.na vrii.na *)
      | [ 8 :: r ] (* .rr -> r+vow *) -> 
        let vow = 
          match root with
          [ "p.rr" -> 6 (* uu *) (* \Pan{7,1,102} [labial].rr -> ur *) 
          | _ -> 4 (* ii *) 
              (* "k.rr" | "g.rr#1" | "j.rr" | "t.rr" | "d.rr" | "st.rr" *)
          ] in
        let stem = [ 43 (* r *) :: [ vow :: r ] ] in 
        match root with 
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
        match root with 
        [ "und" -> [ sTa "ud" :: ppn ] (* for utta and abs -udya *)
        | _ -> ppn 
        ] 
      | [ 34 (* d *) :: r ] -> 
        (* assimilation of d to n - special sandhi Macdonnel§60 foot 1 *)
        let ppn = ppna [ 36 (* n *) :: r ] in (* en fait il faudrait d'+n->nn *)
        match root with 
        [ "vid#2" -> [ Ta rstem :: ppn ] (* 2 forms *)
        | "nud" -> [ Ta rstem :: [ Tia rstem :: ppn ] ] (* 3 forms *)
        | _ -> ppn (* eg ad1 -> anna *)
        ]
 (*     | [ 36 :: ([ 1 :: r ] as w) ] (* -an *) -> 
             [ Ta w :: ppna [ 2 :: r ] ] (* mata+maana *) *)
      | [ 43 (* r *) :: r ] -> ppna rstem (* iir.na *)
      | [ 45 (* v *) :: [ 10 (* e *) :: r ] ] -> (* dev *)
             ppna [ 6 (* uu *) :: [ 42 (* y *) :: r ] ] (* dyuuna *)
      | _ -> failwith ("Unexpected ppp in -na for " ^ root)
      ]  (* end participles in -na *)
    | "pac" -> [ sVa "pak" ] (* exception \Pan{8.2.51} *)
    | "zu.s#1" -> [ Ka rstem ] (* exception \Pan{8.2.52} *)
    | _ -> (* otherwise participle in -ta (Panini kta) *)
           let ppstems =
       let ppstem = match root with 
           [ "dhaa#1" -> revcode "hi" (* double weakening hi-ta \Pan{7,4,42} *)
           | "bh.rjj" -> [ 124; 7; 40 ] (* bh.rj' - mrijification of truncate *)
           | ".rc#1"  -> revcode "arc" (* strong *)
           | ".rj"    -> revcode "arj" (* strong *)
           | "k.svi.d" -> revcode "k.sve.d"
           | "vip"    -> revcode "vep"
           | "m.rg"   -> revcode "marg" (* strong *)
           | "jak.s"  -> revcode "jagh" (* jagdha \Pan{2,4,36} *)
           | "k.san"  -> revcode "k.sa" (* removal of final nasal *) 
           | "gam"    -> revcode "ga" (* \Pan{6,4,37} *)
           | "tan#1"  -> revcode "ta"
           | "nam"    -> revcode "na"
           | "yam"    -> revcode "ya"
           | "ram"    -> revcode "ra"
           | "van"    -> revcode "va"
           | "han#1"  -> revcode "ha" 
           | "man"    -> revcode "ma"
           | "khan"   -> revcode "khaa" (* \Pan{6,4,42} lengthening of vowel *)
           | "jan"    -> revcode "jaa"  (* id *)
           | "san#1"  -> revcode "saa"  (* id *)
           | "am"     -> revcode "aan" (* -am -> -aan \Pan{6,4,15} Wh§955a *)
           | "kam"    -> revcode "kaan" 
           | "kram"   -> revcode "kraan"
           | "klam"   -> revcode "klaan"
           | "cam"    -> revcode "caan"
           | "k.sam"  -> revcode "k.saan"
           | "dam#1"  -> revcode "daan"
           | "bhram"  -> revcode "bhraan" 
           | "vam"    -> revcode "vaan"
           | "zram"   -> revcode "zraan" 
           | "zam#1" | "zam#2" -> revcode "zaan" (* a -> aa \Pan{6,4,15} *)
           | "dhvan"   -> revcode "dhvaan" (* id. for final n *) (* Wh§955a *)
           | "daa#2"   -> revcode "di" (* aa -> i \Pan{7,4,40} *)
           | "maa#1"   -> revcode "mi"
           | "zaa"     -> revcode "zi"
           | "saa#1"   -> revcode "si"
           | "sthaa#1" -> revcode "sthi"
           | "diiv#1"  -> revcode "dyuu" (* iiv -> yuu *)
           | "siiv"    -> revcode "syuu"
           | ".s.thiiv" -> revcode ".s.thyuu"
           | "daa#1"   -> revcode "dad" (* ad hoc \Pan{7,4,46} *)
           | "dham"    -> revcode "dhmaa"  (* \Pan{7,3,78} *)
           | "dhaav#2" -> revcode "dhau"
           | "dhv.r"   -> revcode "dhuur"
           | "puuy"    -> revcode "puu"
           | "skambh" -> revcode "skabh" (* skambh -> skabh *)
           | "stambh" -> revcode "stabh" (* stambh -> stabh *)
           | "zrath"  -> revcode "zranth"
           | "muurch" -> revcode "muur" (* muurta *)
           | "av"     -> revcode "uu" (* uuta *)
           | "i" | ".r" | "k.r#1" | "kyaa" | "khyaa" | "gu~nj" | "gh.r" | "ghraa"
           | "ci" | "cyu" | "ji" | "traa" | "daa#3" | "du" | "dru#1" | "dh.r" 
           | "dhyaa" | "dhru" | "nu#1" | "praa#1" | "bh.r" | "mi" | "m.r" 
           | "yaa#1" | "yu#1" | "yu#2" | "raa#1" | "ru" | "va~nc" | "vaa#2" 
           | "v.r#1" | "v.r#2" | "zaas" | "zri" | "zru" | "si" | "su#2"
           | "s.r" | "stu" | "snaa" | "snu" | "smi" | "sm.r" | "haa#1" | "hi#2" 
           | "hu" | "h.r#1" -> rstem 
            (* many roots ending in a vowel do not take [passive_stem] ? *)
            (* vérifier forme passive pour racines ci-dessus *)
           | _ -> passive_stem root rstem (* possibly duhified and mirjified *)
           ] in [ Ta ppstem :: match root with  
                  (* pp in -ita built on rstem (default) or on ppstem or both *)
                    [ ".rc#1" | ".rj" | "k.svi.d" | "granth" | "grah" | "praz"
                    | "ba.mh" | "ma.mh" | "manth" | "m.rg" | "yaj#1" | "vyadh"
                    | "vrazc" | "vaz" | "vas#1" | "vas#4" | "zrath" 
                    | "stambh"| "svap" ->
                           [ Tia ppstem ] 
                    | "vap" | "vap#1" | "vap#2" | "vad" ->
                           [ Tia rstem; Tia ppstem ]
                    | "dhmaa" -> [ Tia (revstem "dham") ]
                    | "guh" -> [ Tia (revstem "guuh") ] (* \Pan{6,4,89} *)
                    | _ -> [ Tia rstem ] (* standard Paninian way *)
                    ] 
                ] in 
           let extra_forms = 
           match root with (* supplementary forms *)
           [ "a~nc"   -> [ sNa "ak" :: [ sTia "a~nc" ] ] (* "akna", "a~ncita" *)
           | "kuc"    -> [ sTia "ku~nc" ] (* "ku~ncita" *)
           | "grah"   -> [ sTa "g.rbh" :: [ sTia "g.rbh" ] ] (* "g.rbhiita" *)
           | "car"    -> [ sNa "ciir" ] (* irreg. na ppp "ciir.na" *)
           | "gur"    -> [ sNa "guur" ] (* Kale na ppp "guur.na" *)
           | "tvar"   -> [ sNa "tuur" ] (* irr. na ppp "tuur.na" \Pan{6,4,20} *)
           | "jvar"   -> [ sNa "juur" ] (* idem na ppp "juur.na" \Pan{6,4,20} *)
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
  | [ c :: [ 43 :: [ 2 :: r ] ] ] -> [ c :: [ 2 :: [ 43 :: r ] ] ] 
  | w -> failwith ("Metathesis " ^ Canon.rdecode w)
  ]
;
(* Stems used for periphrastic futur, infinitive, and gerundive in -tavya *)
(* Redundancy and discrepancies with intercalates ought to be addressed.  *)
value perstems rstem root =
  let sstem = strong_stem root rstem in 
  let inter = match rstem with 
      [ [ 7; 45 (* v.r *) ] -> [ 1; 2 ] (* i/ii* [v.r#1] and [v.r#2] *)
      | [ 7 (*.r *) :: _ ] -> [ 0 ]
      | _ -> match root with
             [ "gam" | "dham" | "praz" | "vaa#3" | "za.ms" | "han#1" | "huu"
               -> [ 0 ]
             | "k.rt#1" | "jan" | "v.rj" | "v.rt#1" | "v.rdh#1" | "z.rdh"
               -> [ 1 ] 
             (* not "syand" WR syanttaa *)
             | "zuc#1" -> [ 0; 1 ] (* zoktum *)
             | "d.rz#1" | "sp.rz#1" -> [ 3 ] (* ar -> ra dra.s.tum *)
             | "k.r.s" | "bh.rjj" -> [ 0; 3 ] (* berk *)
             | "naz#1" -> [ 0; 1; 4 ] (* berk - (1 not in WR) *)
             | "radh" | "trap" | "d.rp" | "druh#1" | "muh" | "rudh#2"
             | "snih#1" | "snuh#1" (* \Pan{7,2,45} *)
             | "i.s#1" | "sah#1" | "lubh" | "ru.s#1" | "ri.s" (* \Pan{7,2,48} *)
                 -> [ 0; 1 ]
             (* perhaps also optionally all [uu_it] roots ? \Pan{7,2,44} *)
             | _ -> intercalates root rstem 

             ] 
      ] in 
  map insert_sfx inter
     where insert_sfx = fun
       [ 0 -> match root with
              [ "majj"  -> code "mafk"  (* Whitney§936a *)
              | "jan"   -> code "jaa"
              | "dham"  -> code "dhmaa"
              | "nij"   -> code "nej" (* for gana 3 *)
              | "vah#1" -> code "voh" (* vo.dhaa \Pan{6,3,112} *)
              | "sah#1" -> code "soh" (* so.dhum \Pan{6,3,112} *)
              | "likh" | "vij" -> rev [ 3 :: rstem ] (* i with weak stem *)
              | "vrazc" -> code "vraz" (* ought to be truncated by int sandhi *)
              | "za.ms" -> code "zas"
              | "huu"   -> code "hvaa" 
              | "dhru"  -> code "dhru" (* no guna *)
              | _ -> rev (match rstem with 
                     [ [ c :: r ] -> match c with
                         [ 10 | 11 | 12 | 13 -> [ 2 :: r ] (* eg gai -> gaa *)
                         | _ -> sstem
                         ]
                     | [] -> error_empty 12
                     ])
              ] 
       | 1 -> let w = match root with 
                [ r when no_guna r -> rstem
                | "uc" | "mil" (* WR MW but Kale mel *) -> rstem (* cf futur *)
                | "guh"   -> revcode "guuh" (* \Pan{6,4,89} *) 
                | "dabh"   -> revcode "dambh" (* WR *)
                | "sad#1" -> revcode "siid" 
                | "sp.rh" -> revcode "sp.rhay"
                | "haa#1" -> revcode "jah" 
                | _ -> sstem
                ] in 
              sandhi w (code "i") (* sandhi sanitizes a possible j' or h' *)
       | 2 -> sandhi sstem (code "ii") (* grah *)
       | 3 -> rev (ar_ra sstem) (* metathesis: kra.s.taa bhra.s.taa dra.s.taa *)
       | 4 -> code "na.mz" (* exception naz1 *)
       | _ -> failwith "perstems: unexpected intercalate code"
       ]
;
value compute_future_gen rstem root =
  let sstem = strong_stem root rstem in
  let stems = map insert_sfx (intercalates root rstem)
     where insert_sfx = fun 
       [ 0 -> let w = match root with
             [ "naz#1"    -> revcode "nafk" (* Whitney§936a *)
             | "majj"     -> revcode "mafk" (* Whitney§936a *)
             | "d.rz#1"   -> revcode "drak" (* drak.sya *)
             | "gai"      -> revcode "gaa"
             | "jan"      -> revcode "jaa"
             | "nij"      -> revcode "nej" (* consistent with gana 3 *)
             | "ghas"     -> revcode "ghat"
             | "bharts"   -> revcode "bhart"
             | "likh" | "vij" -> [ 3 :: rstem ] (* i with weak stem (hack) *)
             | "vas#1"    -> revcode "vat" (* vatsyati Whitney§167 Pan{7,4,49} *)
             | "vrazc"    -> revcode "vrak" (* vrak.sya *)
             | "saa#1"    -> rstem (* saa si *)
             | "dhru"     -> rstem (* no guna *)
             | _ -> sstem (* for nij gana 3 *)
             ] in sandhi w (code "sya") (* eg dah -> dhak.sya *)
       | 1 -> let w = match root with
             [ r when no_guna r -> rstem
             | "uc" | "mil" (* WR MW but Kale mel *) -> rstem 
             | "guh"    -> revcode "guuh" (* \Pan{6,4,89} *) 
             | "dabh"   -> revcode "dambh" (* WR *)
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
         [ [ 1 :: st ] -> compute_future st root
         | _ -> error_empty 13 
         ] (* Note that sandhi with sy would fail with finalize *)
;
value compute_future_10 rstem root =
  let fsuf = revcode "i.sy" in 
  match root with
    [ "tul" -> do (* 2 forms *)
       { compute_future (fsuf @ (revcode "tulay")) root
       ; compute_future (fsuf @ (revcode "tolay")) root
       }
    | _ -> let stem = strengthen_10 rstem root in 
           let aystem = Word.mirror (sandhi stem [ 1; 42 ] (* ay *)) in
           let fstem = fsuf @ aystem in
           compute_future fstem root
    ]
;

(******************)
(* Passive system *)
(******************)

value admits_passive = fun 
  [ (* We filter out roots with no attested passive forms *)
    "an#2" | "av" | "as#1" | "ah" | "iiz#1" | "uc" | "kan" | "kam" | "kuu"
  | "k.r#2" | "knuu" | "k.sar" | "k.si" | "kha.n.d" | "glaa" | "ghas"| "chur"
  | "ta.d" | "daa#2" | "dyut#1" | "dru#1" | "pat#2" | "paz" | "paa#2" 
  | "pii" | "pyaa" | "praa#1" | "bruu" | "ruc#1" | "vas#4" | "vidh#1" | "vip"
  | "vyac" | "zam#1" | "zi~nj" | "z.rdh" | "zrambh" | "zvit#1"
  | "sap#1" | "siiv" | "spaz#1" | "spardh" | "h.r#2" | "hrii#1" 
  | "ma.mh" (* supplied by "mah" *) (* | "arh" | "k.lp" no ps but pfp *)
      -> False
(* But "iiz#1" "uc" "kuu" "k.sar" "glaa" "dru#1" "pii" "ruc#1" "vip" "zam#1" 
       "zi~nj" "zrambh" "siiv" "spardh" "hrii#1" admit ppp. *)
  | _ -> True 
  ]
;
value admits_ppp_abs = fun
  [ "ak.s" (* vedic a.s.ta overgenerates with a.s.tan *) 
  | "ad#1" (* but anna could be ppp ? *)
  (* jak.s jagdha \Pan{2,4,36} *)
  | "ah" | "bruu" (* vac *) 
  | "paz"  (* d.rz *) 
  | "as#1" | "kan" | "k.r#2" | "k.si" | "gaa#1" | "ghas" | "chur" | "ta.d" 
  |" paa#2" | "praa#1" (* omit ved. praata *) | "bal" | "ma.mh" | "va~nc" 
  | "vadh" (* han *) | "vip" | "vyac" | "zaz" | "zam#2" | "z.rdh" 
  | "zvit#1" | "sac" | "sap#1" | "h.r#2" -> False
  | _ -> True
  ]
;

(* Similar to [compute_thematic_middle]  *)
value compute_passive_present verbal stem root = 
  let conjug person suff = (person,fix stem suff) in 
  enter1 root (Conju verbal
   [ (Singular, let l =
        [ conjug First  "e"
        ; conjug Second "ase"
        ; conjug Third  "ate"
        ] in if root = "tap" then [ conjug Third "ati" :: l ] else l
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
value compute_passive_imperfect verbal stem root = 
  let conjug person suff = (person,fix_augment stem suff) in 
  enter1 root (Conju verbal
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
value compute_passive_optative verbal stem root =
  let conjug person suff = (person,fix stem suff) in 
  enter1 root (Conju verbal
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
value compute_passive_imperative verbal stem root =
  let conjug person suff = (person,fix stem suff) in 
  enter1 root (Conju verbal
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
value compute_passive_raw root = (* "d.r#1" *)
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
      | "dham"   -> stems "dhmaa"
      | ".s.thiiv" -> stems ".s.thiv"
      | "praz" -> let w = revcode "pracch" in (w,w,w) (* Whitney§794c *)
      | "zaas" -> let w = revcode root in (w,w,w) (* redup voy a, not i *)
      | _ -> stems root (* NB: keep penultimate nasal "ta~nc" *)
      ] in
  match Word.mirror revw with (* ugly double reversal to get the stem *)
  (* then we do not use revs and revl but recompute them with strong *)
  [ [] -> error_empty 14
  | [ 3 ] (* "i" *) -> let wk = [ 4; 42 ] (* iiy \Pan{7,4,69} *) 
                       and st = [ 3; 42; 10 ] (* iye *) (* iyaya *) 
                       and lg = [ 3; 42; 11 ] (* iyai *) (* iyaaya *) in
                       (rev st, rev wk, Some (rev lg), False, True)
  | [ c1 :: r ] ->  
      if vowel c1 then let (s,w) = match c1 with (* initial vowel *)
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
         ] in let iopt = match root with (* form without i allowed Kale §508 *)
                  [ "ak.s" -> True (* more to add *)
                  | _ -> False (* default mandatory intercalate i *)
                  ] in
              (s, w, None, False, iopt)
      else (* root starts with consonant c1 *)
      let rec lookvoy = fun (* search for leftmost vowel *)
           [ [] -> error_vowel 1 (* no vowel *)
           | [ c2 ] -> if vowel c2 then (c2,False,True)
                       else error_vowel 2 (* no vowel *)
           | [ c2 :: r2 ] -> 
                       if vowel c2 then 
                          let l = length (contract r2) in
                          let p = long_vowel c2 || l>1 
                          and a = c2=1 (* a *) && l=1 in 
                          (c2,p,a)
                       else lookvoy r2
           ] in
      let (v,p,a) = lookvoy r in (* p = prosodically long, a = vriddhi augment *)
         (* lookvoy computes the vowel v, and the two booleans p and a *)
      let c = if sibilant c1 then match r with
                 [ [] -> error_vowel 3
                 | [ c2 :: _ ] -> if vowel c2 || nasal c2 then c1
                             else if stop  c2 then c2 
                             else (* semivowel c2 *) c1 
                 ]  
              else c1 in
      let rv = (* rv is reduplicating vowel *)
        if v>6 (* .r .rr .l dipht *) then match root with
          [ "ce.s.t" | "dev" |"sev" | "mlecch" | "vye" 
              -> 3 (* i *) (* vye for vyaa *)
          | "g.r" -> 2 (* Vedic - also k.lp etc have long aa Whitney§786a *)
          | ".dhauk" -> 5 (* .du.dhauke \Pan{7,4,59} *)
          | _ -> 1 (* a *) (* also bhuu elsewhere *)
          ]
        else match root with
          [ "maa#3" -> 3 (* i *) (* analogy with present *)
          | "vyath" | "vyadh" | "vyaa" | "jyaa#1" | "pyaa" | "syand" | "dyut#1"
          | "myak.s" -> 3
            (* Whitney§785 also "vyac" and ved. "tyaj#1"; "vyaa" treated other *)
          | "kan" | "mah" -> 2 (* ved lengthened redup vowel Whitney§786a *)
          | "pii" -> 4 (* piipaaya *)
          | _ -> short v (* reduplicated vowel is short *)
          ]
      and rc = (* reduplicating consonant *) match c with
        [ 17 | 18 (* k kh *) -> 22 (* c *)
        | 19 | 20 | 49 (* g gh h *) -> 24 (* j *)
        | 28 (* .th *) when root=".s.thiiv" -> 32 (* t *) (* preferably *)
        | 23 | 25 | 28 | 30 | 33 | 35 | 38 | 40 -> c-1 (* xh -> x *)
        | _ -> c (* c by default *)
        ] in 
      (* TODO: share code of sampra with [passive_stem] PB: iij vs ij *)
      let (affix,sampra) = match root with (* ya -> ii va -> uu *)
          [ "yaj#1" -> ([ 3 (* i *)],Some (mrijify (revcode "iij")))
          | "vac" ->   ([ 5 (* u *)],Some (revcode "uuc"))
          | "vad" ->   ([ 5 (* u *)],Some (revcode "uud"))
          | "vap" | "vap#1" | "vap#2" -> ([ 5 (* u *) ],Some (revcode "uup"))
          | "vaz" ->   ([ 5 (* u *)],Some (revcode "uuz"))
          | "vas#1" | "vas#4" -> ([ 5 (* u *)],Some (revcode "uu.s"))
          | "vah#1" -> ([ 5 (* u *)],Some (revcode "uuh"))
          | "vaa#3" -> ([ 5 (* u *)],Some (revcode "uuv"))
          | _ ->       ([ rv; rc ],None)
          ] 
      and vriddhi = match root with
          [ "vyadh" | "svap" | "grah" | "grabh" (* fictive *) -> True 
            (* since special weak stem returned by stems *)
          | _ -> a
          ] in
      let glue = match root with
          [ "sphur" | "sphu.t" -> revaffix False affix (* no retroflexion *)
          | _ -> revaffix True affix
          ] in
      let (weak,eweak,iopt) = match sampra with (* iopt = optional i *) 
          [ Some weak -> (weak,False,True)
          | None -> let trap = fun (* MW Gram §375g *)
              [ "trap" | "phal" | "bhaj" | "zrath" -> True | _ -> False ] in
                    if rc=c || trap root then match r with
            [ [ 1 :: w ] -> match root with
              [ "jan" -> (glue (revcode "j~n"),True,True)
              | "val" | "mah" -> (glue revw,False,False) 
              | _ -> match w with
                [ [ c' ] when consonant c' -> 
                     (revaffix True [ 10 (* e *); c ] w,True,True)
                     (* Scharf: roots of form c.a.c' with c,c' consonant or .m *)
                     (* cf. \Pan{6,4,119-126} -- ZZ may lead to hiatus *)
                | [ 24; 24 ] (* majj *) -> (glue revw,False,True)
                | _ -> (glue revw,False,False)
                ]
              ] 
            | [ 43; 1; x ] -> ([ x; 10; 43; c ],True,True) (* "trap" | "zrath" *)
            | _ -> (glue revw,False,False) 
            ]       else 
              let (short,iopt) = match root with
                [ "gam"   -> (revcode "gm",True) (* actually i forbidden TODO *)
                | "ghas"  -> (revcode "k.s",False)
                | "han#1" -> (revcode "ghn",True)
                | "khan"  -> (revcode "khn",False)
                | _       -> let iopt = match root with
                             [ "ku.s" | "kliz" | "k.sam" (* more to add *)
                                 -> True (* Kale§508 *)
                             | _ -> False
                             ] in (revw,iopt) 
                ] in (glue short,False,iopt) 
          ] 
       and strong = glue (if p then match root with 
                             [ "jaag.r" -> revs (* verrue but Deshpande OK *)
                             | _        -> revw
                             ]
                          else revs) 
       and longifvr = if vriddhi then revl else revs in 
       let olong = if p then None else Some (glue longifvr) in
       (strong, weak, olong, eweak, iopt)
  ]
;
value compute_perfecta conj strong weak olengthened eweak iopt root = 
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix weak suff) in do
  { enter1 root (Conju (fperfa conj)
   [ (Singular, let l = match olengthened with 
     [ Some lengthened -> 
        let conjugl person suff = (person,fix lengthened suff) in
        [ conjugs First "a"
        ; conjugl First "a" 
        ; let conjug = if eweak then conjugw else conjugs in
          conjug Second "itha" (* naz: nezitha not neniza as in Kale§508 *)
        ; conjugl Third "a" 
        ] 
    | None -> 
        [ conjugs First  "a"     (* ex: aap -> aapa *)
        ; conjugs Second "itha" 
        ; conjugs Third  "a" 
        ] @ if root="az#1" then (* Para in Vedic only *)
          let optstrong = revcode "aana.mz" in
          let conjugs person suff = (person,fix optstrong suff) in
        [ conjugs First  "a"     
        ; conjugs Second "itha"
        ; conjugs Third  "a" (* actually also regular aaza Whitney§788a *)
        ] else [] (* Whitney§788a *)
    ] in if iopt then (* add forms without intercalating i *)
        let conjug = (* Whitney§801g nana.m.s.tha mafktha *)
            if root="naz#1" then fun p s -> (p,fix (revcode "nana.mz") s)
            else if root="majj" then fun p s -> (p,fix (revcode "ma.mj") s)
            else conjugs in
        [ conjug Second "tha" :: l ] 
            else if no_guna root then (* Kale ku.taadi *)
        [ conjugw Second "itha" :: l ] else l)
   ; (Dual, let l =
        [ conjugw First  "iva"
        ; conjugw Second "athur"
        ; conjugw Third  "atur"
        ] in l) (* [if iopt then [ conjugw First  "va" :: l ] else l] NO *)
   ; (Plural, let l =
        [ conjugw First  "ima"
        ; conjugw Second "a"
        ; if root="raaj#1" then (Third, code "rejur")
          else conjugw Third "ur" (* Henry: paptur véd. pat1 Varenne§39 *)
        ] in l) (* [if iopt then [ conjugw First  "ma" :: l ] else l] NO *)
   ]) 
  ; let pstem = if root="raaj#1" then (revcode "rej") else weak in
    if root=".s.thiiv" then ((* unattested *)) else 
    record_part (Ppfta_ conj pstem root)
  }
;
value compute_perfectm conj stem root =
  let iopt = fun (* Kale§508 ve.t *) 
      [ "trap" | "k.sam" | "az#1" -> True | _ -> False ] in
  let conjugw person suff = (person,fix stem suff) in do
  { enter1 root (Conju (fperfm conj)
   [ (Singular, let l =
        [ conjugw First  "e" 
        ; conjugw Second "i.se"
        ; conjugw Third  "e" 
        ] in if root = "guh" then
                let juguhe = code "juguhe" in (* Whitney§793i *)
                l @ [ (First,juguhe); (Third,juguhe) ]
             else if iopt root then l @ [ conjugw Second "se" ] else l)
   ; (Dual, let l =
        [ conjugw First  "ivahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ] in if iopt root then l @ [ conjugw First "vahe" ] else l)
   ; (Plural, let l =
        [ conjugw First  "imahe"
        ; conjugw Second "idhve"
        ; conjugw Third  "ire"
        ] in if iopt root then 
                l @ [ conjugw First "mahe"; conjugw Second "dhve" ] 
                    (* but not [ conjugw Third  "re" ] ? *)
             else l)
   ])
  ; record_part_m_ath (vppftm conj) stem root (* -aana *)
  }
;
value compute_perfect_c strong weak olengthened eweak iopt root =
  match voices_of root with
  [ Para -> do
      { compute_perfecta Primary strong weak olengthened eweak iopt root
      ; match root with 
        [ "cit#1" -> do
           { compute_perfectm Primary weak root
           ; compute_perfectm Primary (revcode "cikitr") root (* WR *)
           }
        | "vac" -> compute_perfectm Primary weak root
        | _ -> () 
        ]
      }
  | Atma -> let stem = match root with 
                       [ "cak.s" | "ba.mh" -> strong
                       | _ -> weak
                       ] in 
            compute_perfectm Primary stem root
  | _ -> do { compute_perfecta Primary strong weak olengthened eweak iopt root
            ; let stem = match root with
                         [ "kan" -> revcode "cak" (* kan -> kaa *)
                         | _ -> weak
                         ] in 
              compute_perfectm Primary stem root
            ; if root = "grah" then (* archaic variant grabh *) 
                 let (s, w, o, e, i) = redup_perf "grabh" in 
                 do { compute_perfecta Primary s w o e i root
                    ; compute_perfectm Primary w root
                    }
              else ()
    
            }
  ]
;
value compute_perfecta_aa stem root = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 root (Conju perfa 
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
  ; record_part (Ppfta_ Primary stem root)
  }
;
value compute_perfectm_aa stem root = 
  let conjug person suff = (person,fix stem suff) in do
  { enter1 root (Conju perfm
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
  ; record_part_m_ath ppftm stem root (* stem-aana *)
    (* middle part rare - eg cakraa.na pecaana anuucaana zepaana *)
  }
;
value compute_perfect_aa stem root = 
  match voices_of root with
  [ Para -> compute_perfecta_aa stem root
  | Atma -> compute_perfectm_aa stem root
  | _ -> do { compute_perfecta_aa stem root 
            ; compute_perfectm_aa stem root 
            }
  ]
;
(* dissymetric in i and u - problematic *)
value fix_dup weak suff mc = (* Gonda §18.I §6 *)
  let s = code suff in match s with
  [ [ c :: _ ] -> match weak with
      [ [ 5 (* u *) :: l ] | [ 6 (* uu *) :: l ] (* eg stu *) ->
        let sf = if vowel c then [ 45 (* v *) :: s ] else s in
        sandhi [ 5 (* u *) :: l ] sf
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
(* Different from Phonetics.mult, assumes root starts with vowel *)
value multi_consonant root = match revstem root with
  [ [ v :: r ] -> vowel v && length r > 1
  | [] -> error_empty 15
  ]
;
(* Roots not admitting intercalating i Whitney§797c MacDonell§136a *)
(* In later language, often i was added before consonantal suffixes, except: *)
value no_inter_i root = 
  List.mem root [ "k.r#1"; "bh.r"; "v.r#2"; "s.r"; "dru#1"; "zru"; "stu"; "sru" ]
(* Beware: "k.r#1" with sam-s- Kale§503 see sandhi rule ("am","cak","a~ncask")
   Atma 2p pl sa~ncaskaridhve but not optional sa~ncaskari.dhve *)
(* also Kale§504 says "stu" uses i *)
;
value compute_perfecta_v strong weak root = 
(* PB Kale§504 ru gives ruruviva/ruruvima rather than ruraviva/ruravima only *)
  let lengthened = lengthened weak 
  and iforb = no_inter_i root
  and mc = multi_consonant root in
  let conjugw person suff = (person,fix_dup weak suff mc) 
  and conjugs person suff = (person,fix strong suff)
  and conjugl person suff = (person,fix lengthened suff) in do
  { enter1 root (Conju perfa
   [ (Singular, let l =
        [ conjugs First  "a"
        ; conjugl First  "a"
        ; if root = "v.r#2" then conjugs Second "itha" (* Kale§504 *)
          else conjugs Second "tha"
        ; conjugl Third  "a" 
        ] in if iforb then l else [ conjugs Second "itha" :: l ])
   ; (Dual, let l =
        [ conjugw First  "va" 
        ; conjugw Second "athur"
        ; conjugw Third  "atur"
        ] in if iforb then l else [ conjugs First "iva" :: l ])
   ; (Plural, let l =
        [ conjugw First  "ma" 
        ; conjugw Second "a"
        ; conjugw Third  "ur"
        ] in if iforb then l else [ conjugs First "ima" :: l ])
   ])
  ; record_part (Ppfta_ Primary weak root)
  }
;
value compute_perfectar conj stem root = 
(* NB Kale§504 z.rr authorizes also forms without intercalate i zazru.h *)
  let conjugs person suff = (person,fix stem suff) 
  and conjugl person suff = (person,fix (lengthened stem) suff) in do
  { enter1 root (Conju (fperfa conj)
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
  ; record_part (Ppfta_ conj stem root)
  }
;
value compute_perfect_ril stem root = (* -.rr or multiconsonant -.r *)
  match voices_of root with
  [ Para -> compute_perfectar Primary stem root
  | Atma -> compute_perfectm Primary stem root
  | _ -> do { compute_perfectar Primary stem root
            ; compute_perfectm Primary stem root
            }
  ]
;
value compute_perfectm_v weak mc root = 
  let iforb = no_inter_i root
  and conjugw person suff = (person,fix_dup weak suff mc) in do 
  { enter1 root (Conju perfm
   [ (Singular, let l =
        [ conjugw First  "e" 
        ; conjugw Second "se"
        ; if root = "m.r" then (Third, code "mamre")
          else conjugw Third "e" 
        ] in if iforb then l else [ conjugw Second "ise" :: l ])
   ; (Dual, let l =
        [ conjugw First  "vahe"
        ; conjugw Second "aathe"
        ; conjugw Third  "aate"
        ] in if iforb then l else [ conjugw First "ivahe" :: l ])
   ; (Plural, let l =
        [ conjugw First  "mahe"
        ; conjugw Second "dhve"
        ; conjugw Third  "ire"
        ] in if iforb then l else 
        [ conjugw First "imahe" :: [conjugw Second "idhve" :: l ]])
   ])
  ; record_part_m_ath ppftm weak root (* weak-aana *)
    (* middle part rare - eg cakraa.na pecaana anuucaana zepaana *)
  }
;
value compute_perfect_bhuu root = (* \Pan{7,4,73} *)
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
value compute_perfect_v strong weak root = 
  let mc = multi_consonant root in 
  match voices_of root with
  [ Para -> compute_perfecta_v strong weak root
  | Atma -> match root with (* pada anomaly *)
            [ "pii" -> compute_perfecta_v strong weak root (* piipaaya *)
            | _ -> compute_perfectm_v weak mc root
            ]
  | Ubha -> do 
     { compute_perfecta_v strong weak root
     ; compute_perfectm_v weak mc root
     }
  ]
;
value compute_perfect root =
(*i Bizarre pada dependency should be factored i*)
  match root with
    [ "bhuu#1" -> do
        { compute_perfect_bhuu root (* No middle forms Whitney§800d *)
        ; record_part (Ppfta_ Primary (revcode "babhuu") root)
        ; record_part_m_ath ppftm (revcode "babhuuv") root 
        }
    | "vid#1" -> do
        { compute_perfect_vid root (* middle forms ? *)
        ; record_part (Ppfta_ Primary (revcode "vid") root)
        }
    | "ah" -> compute_perfect_ah root
    | "vyaa" -> compute_perfect_vyaa root (* does not fit standard aa scheme *)
    | "zuu" | "zvaa" (* zuu in lexicon *) -> do 
        { let (strong, weak,_,_,_) = redup_perf "zuu" in 
          (* we allow vocalic deployment even though not \Pan{6,1,17} *)
          compute_perfect_v strong weak root (* zuzaava *)
        ; let (strong, weak,_,_,_) = redup_perf "zvi" in (* \Pan{6,1,30} *)
          compute_perfect_v strong weak root (* Whitney§794b zizvaaya *)
        }
(* Whitney§794b also jyaa pyaa vyaa hvaa; we treat vyaa above, and hvaa is huu.
   Thus pyaa is covered by pii. jyaa1 as jii gives jijyau same WR *)
    | "indh" -> compute_perfectm Primary (revcode "iidh") root
    | "mah" -> let (strong, weak, _, _, _) = redup_perf root in
               compute_perfectm Primary strong root (* ZZ Atma for Para root *)
    | "diiv#1" -> let (strong, weak, olong, eweak, iopt) = redup_perf "div" in 
                  compute_perfect_c strong weak olong eweak iopt root
    | _ -> let (strong, weak, olong, eweak, iopt) = redup_perf root in 
           match weak with 
           [ [ c :: rest ] -> 
             if c=2 (* aa *) || (c>9 && c<14) (* e ai o au *)
             then compute_perfect_aa rest root (* shortened weak stem *)
             else if c>2 && c<7 (* i ii u uu *)
                  then compute_perfect_v strong weak root
             else if c=7 (* .r *) && multi_consonant root || c=8 (* .rr *) 
                  (* PB Kale§504 v.r2 as v.rr uses this but contrary to WR *)
                  then compute_perfect_ril strong root
             else if c=7 (* .r *) then compute_perfect_v strong weak root
             else compute_perfect_c strong weak olong eweak iopt root
           | [] -> error_empty 16
           ]
    ]
;
value compute_perfect_desida st root =
(* [root:string] is the root, [st] is the desiderative (reverse word) stem. *)
(* We create a fake root from [st] to reuse [redup_perf] which uses a string.*)
(* This gives often weird results eg ghas - should be seriously revised *) 
  let (strong, weak, olong, eweak, iopt) = redup_perf (Canon.rdecode st) in 
  compute_perfecta Desiderative strong weak olong eweak iopt root
and compute_perfect_desidm st root =
  let (_, weak, _, _, _) = redup_perf (Canon.rdecode st) in
  compute_perfectm Desiderative weak root
;
(*****************************)
(* Periphrastic perfect li.t *)
(*****************************)
(* Construction of the periphrastic perfect, used for perfect of secondary 
conjugations, denominative verbs and a few roots. It builds a form in -aam
suffixed by a perfect form of the auxiliairies k.r bhuu and as \Pan{3,1,35-40} *)
value peri_perf_stem root = 
  let stem = match root with 
  [ "iik.s" | "ii.d" | "iir" | "iih" | "uk.s" | "uc" | "ujjh" | "uuh" | "edh" 
    (* Macdonell§140a1 Whitney§1071c Filliozat§66 edhaa.mcakre *)
  | "ind" | "indh" | "inv" | "ii.s" | "umbh" | "cakaas" -> root
  | "aas#2"  -> "aas" (* trim homo *)
  | "iiz#1"  -> "iiz" (* id MWG§385 *) 
  | "u.s"    -> "o.s" (* guna WR *) 
  | "jaag.r" -> "jaagar" (* Macdonell§140a2 *)
  | "bhii#1" -> "bibhay" (* Henry§242 *)
  | "bh.r"   -> "bibhar" (* Henry§242 *)
  | "nii#1"  -> "nay" 
  | "i"      -> "ay" (* Whitney roots *)
  | ".r"     -> "ar" (* id *)
  | "vid#1"  -> "vid" (* Henry§242 *)
  | "vyaa"   -> "vye" (* Whitney roots *)
  | "hu"     -> "juhav" (* Henry§242 *)
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
value compute_ath_s_aorista long root = 
  let conjug person suff = (person,sigma True long suff) in
  enter1 root (Conju (aora 4) (sigma_paradigm conjug))
;
value compute_ath_s_injuncta long root = 
  let conjug person suff = (person,sigma False long suff) in
  enter1 root (Conju (inja 4) (sigma_paradigm conjug))
;
value compute_ath_s_aoristm stem root = 
  let conjug person suff = (person,sigma True stem suff)
  and conjugroot person suff = (person,fix_augment stem suff) 
  and conjugdhvam person = (* PB: daa1 dhaa1 stem end in i thus ".dhvam" *)
      let suff = match stem with
          [ [ 1 (* a *) :: _ ] | [ 2 (* aa *) :: _ ] -> "dhvam"
          | [ 43 (* r *) :: _ ] -> ".dhvam"
          | [ c :: _ ] -> if vowel c then ".dhvam" else "dhvam"
          | _ -> error_empty 18
          ] in 
      (person,fix_augment stem suff) in 
  let conjugc = if root = "k.r#1" (* Whitney§882a *)
                || root = "daa#1" (* Whitney§884 *) then conjugroot 
                else match stem with  
                     [ [ 43 :: _ ] | [ 36 :: _ ] | [ 41 :: _ ] -> conjug
                       (* r             n             m  Whitney§881*)
                     | [ c :: _ ] when consonant c -> conjugroot 
                 (*[ | [ c :: _ ] when short_vowel c -> conjugroot] ? *)
                     | _ -> conjug
                     ] in 
  enter1 root (Conju (aorm 4)
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
value compute_ath_s_injunctm stem root = 
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
  let conjugc = if root = "k.r#1" (* Whitney§882a *)
                || root = "daa#1" (* Whitney§884 *) then conjugroot 
                else match stem with  
                     [ [ 43 :: _ ] | [ 36 :: _ ] | [ 41 :: _ ] -> conjug
                       (* r             n             m  Whitney§881*)
                     | [ c :: _ ] when consonant c -> conjugroot 
                 (*[ | [ c :: _ ] when short_vowel c -> conjugroot] ? *)
                     | _ -> conjug
                     ] in 
  enter1 root (Conju (injm 4)
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
value compute_ath_is_aorista stem root = 
  let long_i = (root = "grah") in
  let conjug person suff = (person,isigma True stem suff long_i) in
  enter1 root (Conju (aora 5) (sigma_paradigm conjug))
;
value compute_ath_is_injuncta stem root = 
  let long_i = (root = "grah") in
  let conjug person suff = (person,isigma False stem suff long_i) in
  enter1 root (Conju (inja 5) (sigma_paradigm conjug))
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
value sigma_j stem suff = (* horrible verrue pour "j~naa#1" selon Deshpande *)
   aug (sandhi [ 48 :: stem ] (code suff)) 
;
value compute_ath_is_aoristm stem root = 
  let long_i = (root = "grah") in
  let conjug person suff = (person, 
              if root = "j~naa#1" then sigma_j stem suff (* verrue *)
              else isigma True stem suff long_i)
  and conjugdhvam person = (person,fix_augment stem suff)
      where suff = (if long_i then "ii" else "i") ^ "dhvam" in
  enter1 root (Conju (aorm 5) (isigma_m_paradigm conjug conjugdhvam))
;
value compute_ath_is_injunctm stem root = 
  let long_i = (root = "grah") in
  let conjug person suff = (person,isigma False stem suff long_i)
  and conjugdhvam person = (person,fix stem suff)
      where suff = (if long_i then "ii" else "i") ^ "dhvam" in
  enter1 root (Conju (injm 5) (isigma_m_paradigm conjug conjugdhvam))
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
value compute_ath_sis_aorista stem root = 
  let conjug person suff = (person,sisigma True stem suff) in
  enter1 root (Conju (aora 6) (sigma_paradigm conjug))
;
value compute_ath_sis_injuncta stem root = 
  let conjug person suff = (person,sisigma False stem suff) in
  enter1 root (Conju (inja 6) (sigma_paradigm conjug))
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
value compute_ath_sa_aorista stem root = 
  let conjug person suff = (person,sasigma True stem suff) in
  enter1 root (Conju (aora 7) (sa_aorist_a conjug))
;
value compute_ath_sa_injuncta stem root = 
  let conjug person suff = (person,sasigma False stem suff) in
  enter1 root (Conju (inja 7) (sa_aorist_a conjug))
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
value compute_ath_sa_aoristm stem root = 
  let conjug person suff = (person,sasigma True stem suff) in
  enter1 root (Conju (aorm 7) (sa_aorist_m conjug))
;
value compute_ath_sa_injunctm stem root = 
  let conjug person suff = (person,sasigma False stem suff) in
  enter1 root (Conju (injm 7) (sa_aorist_m conjug))
;
value compute_root_aorista weak strong root = 
  let conjugw person suff = (person,fix_augment weak suff) 
  and conjugs person suff = (person,fix_augment strong suff) in
  enter1 root (Conju (aora 1)
   [ (Singular, if root = "bhuu#1" then (* Whitney§830 *)
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
value compute_root_injuncta weak strong root = 
  let conjugw person suff = (person,fix weak suff) 
  and conjugs person suff = (person,fix strong suff) in
  enter1 root (Conju (inja 1)
   [ (Singular, if root = "bhuu#1" then 
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
value compute_root_aoristm stem root = (* rare *)
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (aorm 1) (conjugs_past_m conjug))
;
value compute_root_injunctm stem root = (* rare *)
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (injm 1) (conjugs_past_m conjug))
;
value compute_root_aoristp stem root = (* passive aorist Whitney§843 *)
  (* \Pan{3,1,60-66} suffix ci.n usage réflexif-passif agent/objet karmakart.r *)
  (* TODO use Kümmel 1996 for Vedic plural 3rd forms *)
  let conjug person suff = (person,fix_augment stem suff) in
  let conju3 = Conju aorp1 [ (Singular,[ conjug Third "i" ]) ] in
  enter1 root conju3
;
value compute_root_injunctp stem root = (* passive injunctive ? *)
  let conjug person suff = (person,fix stem suff) in
  let conju3 = Conju injp1 [ (Singular,[ conjug Third "i" ]) ] in
  enter1 root conju3
;
(* identical to [compute_thematic_impfta] *)
value compute_thematic_aorista stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (aora 2) (thematic_preterit_a conjug))
;
value compute_thematic_injuncta stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (inja 2) (thematic_preterit_a conjug))
;
(* identical to [compute_thematic_impftm] *)
value compute_thematic_aoristm stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (aorm 2) (thematic_preterit_m conjug))
;
value compute_thematic_injunctm stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (injm 2) (thematic_preterit_m conjug))
;
(* identical to [compute_thematic_impfta] *) 
(* de Saussure (Memoire sur le systeme primitif des voyelles dans les langues IE)
   says: reduplicated aorists represent imperfects of a verbal class. *) 
value compute_redup_aorista stem root =  
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (aora 3) (thematic_preterit_a conjug))
  (* NB Macdonnel dixit -- Gonda says "ur" for Third Plural *)
;
value compute_redup_injuncta stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (inja 3) (thematic_preterit_a conjug))
;
(* identical to [compute_thematic_impftm] *)
value compute_redup_aoristm stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (aorm 3) (thematic_preterit_m conjug))
;
value compute_redup_injunctm stem root = 
  let conjug person suff = (person,fix stem suff) in
  enter1 root (Conju (injm 3) (thematic_preterit_m conjug))
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
value compute_aorist root =
  let (weak,strong,long) = stems root in do (* 7 formations *)
  { match root with (* 1. root aorist - Panini sic-luk *)
    [ "k.r#1" | "kram" | "gam" | "gaa#1" | "ci" | "chid#1" | "jan" | "j~naa#1" 
    | "tan#1" | "daa#1" | "daa#2" | "dhaa#1" | "dhaa#2" | "paa#1" | "bhid#1" 
    | "bhuu#1" | "muc#1" | "v.r#1" | "zaa" | "saa#1" | "sthaa#1" | "svap" 
    | "has" | "haa#1" -> do
      { compute_root_aorista weak strong root 
      ; match root with (* Atma rare *) 
        [ "k.r#1" | "gam" | "jan" | "tan#1" | "v.r#1" -> 
                    compute_root_aoristm weak root 
        | "sthaa#1" (* Whitney§834a. *) ->
                    compute_root_aoristm (revstem "sthi") root (* asthita *) 
        | "dhaa#1" -> compute_root_aoristm (revstem "dhi") root
        | _ -> ()
        ]
      ; let stem = match root with
            [ "kram" | "gam" | "jan" -> weak (* ajani but Vedic ajaani *)
            | "muc#1" -> strong 
            | "ci" -> revstem "ce.s" (* Deshpande irregular *)
            | "tan#1" -> revstem "taay" (* WR *)
            |  _ -> match long with 
                    [ [ 2 (* aa *) :: _ ] -> [ 42 (* y *) :: long ]
                    | _ -> long
                    ] 
            ] in
        compute_root_aoristp stem root (* passive *)
      (* For root aorist participles, see Whitney§840 and Burrow p178 *)
      (* For optative mode Whitney§837 see benedictive/precative.     *)
      }
    | "prii" -> let st = revcode "priiyaa" in compute_root_aorista st st root 
    | "svid#2" -> let st = revcode "svidyaa" in compute_root_aorista st st root
    | "iik.s" | "m.r" | "v.r#2" -> compute_root_aoristm weak root
    (* Now other passive/impersonal aorists in -i *)
    | "vac" -> do (* passive aorist *)
      { compute_root_aoristp long root 
      ; compute_root_aoristp (revcode "voc") root 
      }
    | "p.rr" -> compute_root_aoristp (revcode "puur") root 
    | "kaaz" | "k.rt#1" | "k.sip" | "gur" | "tru.t" | "diip" | "duh#1" | "d.rz#1"
    | "dvi.s#1" | "budh#1" | "yuj#1" | "rabh" | "vid#1" | "s.rj#1" 
        -> compute_root_aoristp strong root 
    | "zam#1" -> do { compute_root_aoristp weak root (* since udatta *)
                    ; compute_root_aoristp long root (* WR *)
                    }
    | "jaag.r" | "t.rr" | "pac" | "pad#1" | "z.rr" | "zru" | "stu" | "hu"
        -> compute_root_aoristp long root
           (* NB "zru" -> azraavi WR while Whitney§844a *azraayi typo *) 
    (* | "i" -> iiyaat hard *)
    | _ -> () 
    ]
  ; match root with (* 2. thematic aorist af *)
    [ "aap" | "k.rt#1" | "krudh" | "gam" | "g.rdh" | "ghas" | "ghu.s"  
    | "chid#1" | "das" | "dyut#1"  | "bhid#1" | "mad#1" | "muc#1" | "yuj#1" 
    | "ric" | "ruc#1" | "rudh#2" | "ruh" | "vid#2" | "v.rt#1" | "v.rdh#1"
    | "zuc#1" | "zudh" | "sic" | "stan" | "huu" 
     -> do
      { compute_thematic_aorista weak root
      ; compute_thematic_aoristm weak root (* middle very rare *)
      }
    | "vac" -> let stem = revcode "voc" in do
      { compute_thematic_aorista stem root
      ; compute_thematic_aoristm stem root 
      }
    | "vyaa" -> let stem = revcode "vi" in do
      { compute_thematic_aorista stem root
      ; compute_thematic_aoristm stem root 
      }
    | "zak" | "zam#1" | "zuu" | "z.rdh" | "zcut#1" | "zram" 
        -> compute_thematic_aorista weak root
    | "zru"   -> compute_thematic_aorista (revcode "zrav") root
    | "khyaa" -> compute_thematic_aorista (revcode "khy") root
    | "as#2"  -> compute_thematic_aorista (revcode "asth") root
    | "zaas"  -> compute_thematic_aorista (revcode "zi.s") root
    | "pat#1" -> compute_thematic_aorista (revcode "papt") root
    | (* roots in .r or .rr take strong stem *)
      ".r" | "d.rz#1" -> compute_thematic_aorista strong root
    | _ -> () 
    ]
  ; match root with (* 3. reduplicated aorist caf *)
    [ "am" | ".rc#1" | "kath" | "k.r#1" | "k.r.s" | "k.lp" | "ga.n" | "gam"
    | "gaah" | "ghu.s" | "car" | "ce.s.t" | "jan" | "ji" | "tvar" | "tvi.s#1"
    | "dah#1" | "diz#1" | "dih" | "diip" | "dru#1" | "dh.r" | "naz#1" | "pac" 
    | "pa.th" | "miil" | "muc#1" | "yaj#1" | "rak.s" | "ric" | "viz#1" | "v.r#1" 
    | "v.rt#1" | "v.rdh#1" | "vyadh" | "zri" | "zru" | "stu" | "svap" | "bh.r"
      (* | "dhaa#1" *) -> 
      let stem = redup_aor weak root in do
      { compute_redup_aorista stem root (* but atu.s.tavam RV (WR) *)  
      ; compute_redup_aoristm stem root 
      }
    | "iik.s" | "kamp" | "klid" | "gup" | "cur" | "m.r" | "d.rz#1" | "dyut#1" 
    | "vrazc" | "zaas" | "z.rdh"| "siiv" | "sru" -> (* active only *)
      let stem = redup_aor weak root in 
      compute_redup_aorista stem root
    | "grah" -> do 
      { let stem = redup_aor (revcode "grah") root in do
        { compute_redup_aorista stem root
        ; compute_redup_aoristm stem root 
        }
      ; let stem = redup_aor (revcode "grabh") root in do (* ved Whitney§223g *)
        { compute_redup_aorista stem root
        ; compute_redup_aoristm stem root 
        }
      }
(*i | "daa#1" -> let stem = (revcode "diidad") (* ad hoc *) in do
        { compute_redup_aorista stem root
        ; compute_redup_aoristm stem root 
        } suppressed Jan 2021 for lack of justification i*)
      (* then exceptions to treatment of aa with intercalaring ii *)
    | "raadh" -> let stem = redup_aor (revcode "radh") (* riiradh *) root in  
                 compute_redup_aorista stem root (* Macdonnel p 126 *)
    | "haa#1" -> let stem = revcode "jiijah" in 
                 compute_redup_aorista stem root
    | _ -> () 
    ]
  ; match root with (* reduplicated aorist - extra forms, secondary conjs *)
    [ "naz#1" -> compute_redup_aorista (revcode "nez") root
    | _ -> () 
    ]
  ; match root with (* 4. sigma aorist sic *)
    [ "aap" | "k.r#1" | "khan" | "gup" | "gh.r" | "ci" | "chid#1" | "ji"
    | "tud#1" | "t.rr" | "tyaj#1" | "dah#1" | "daa#1" | "d.rz#1" | "draa#2" 
    | "dhaa#1" | "dhyaa" | "dhyai" | "dhv.r" | "nak.s" | "nii#1" | "pac" | "bandh"
    | "bhid#1" | "m.r" (* Deshpande: am.rta [1] am.r.saataam [4] am.r.sata [4] *)
    | "yaj#1" | "yuj#1" | "ram" | "rudh#2" | "labh" | "v.r#2" | "vyadh" | "zru"
    | "sidh#1" | "s.rj#1" | "stu" | "sp.rz#1" | "svap" | "haa#1" | "hu" 
     -> do
      { let stem = match root with
            [ "d.rz#1" | "s.rj#1" | "sp.rz#1" -> long_metathesis weak
            | ".r.s" | "ram" -> weak 
            | _ -> long (* bandh -> abhaantsiit Whitney§891 *) 
            ] in
        compute_ath_s_aorista stem root 
      ; match root with (* Whitney§890 *)
            [ "khan" (* akhaan *) 
            | "dah#1" (* adhaak *)
            (*i | "d.rz1" adraak wrong *adaar.t below TODO use [ar_ra] i*)
            | "yaj#1" (* ayaa.t *)
            (* | "s.rj1" asraak wrong *asaar.t below *)
              -> let lopa = sigma True long "" in
                 enter1 root (Conju (aora 4) [ (Singular,[ (Third, lopa) ]) ])
            | _ -> ()
            ]
      ; if root = "yuj#1" || root = "chid#1" || root = "bhid#1" 
           then compute_ath_s_aorista strong root else ()
        (* ayok.siit and acchetsiit besides ayauk.siit and acchaitsiit *)
      ; match root with
        [ "gup" | "gh.r" | "t.rr" | "d.rz#1" | "s.rj#1"
            -> ()  (* active only *)
        | _ -> let stemm = match weak with
            [ [ c :: r ] -> match c with 
                [ 3 | 4 | 5 | 6 (* i ii u uu *) -> strong
                | 2 (* aa *) -> [ 3 :: r ] (* turn aa to i Kale§535 *)
                    (* but Whitney§884 says only daa1 dhaa1 and sthaa1 *)
                | 7 (* .r *) -> if root = "dhv.r" then revcode "dhuur" else weak
                | _ -> weak
                ]
            | _ -> error_empty 22
            ] in compute_ath_s_aoristm stemm root 
        ]
      }
    | "k.r.s" -> do (* optional metathesis *)
       { let stem = long in compute_ath_s_aorista stem root (* akaark.siit *)
       ; let stem = ar_ra long in 
         compute_ath_s_aorista stem root         (* metathesis akraak.siit *)
       }
    | "vrazc" -> let stem = revcode "vraak" in (* as for future *) 
                 compute_ath_s_aorista stem root 
    | "gaah" | "spaz#1" | "smi" | "haa#2" -> 
                 compute_ath_s_aoristm weak root (* middle only *)
    | _ -> ()
    ]
  ; match root with (* 5. i.s aorist se.t-sic *)
    [ "ak.s" | "aj" | "aas#2" | "i.s#1" | "iik.s" | "uk.s" | "uc" | "u.s" 
    | "uuh" | ".rc#1" | ".r.s" | "k.rt#1" | "krand" | "kram" | "kliz" | "ku.t"
    | "k.san" | "ghu.s" | "car" | "ce.s.t" | "jap" | "jalp" | "jaag.r" | "tan#1"
    | "t.rr" | "tru.t" | "diip" | "pu.t" | "puu#1" | "p.rc"| "pru.s#1" | "baadh" 
    | "budh#1" | "mad#1" | "mud#1" | "muurch" | "mlecch" | "yaac" | "rak.s" 
    | "ruc#1" | "lu~nc" | "lu.th" | "luu#1" | "vad" | "vadh" | "vaz" | "vid#1"
    | "v.r#1" | "v.rdh#1" | "vyath" | "vraj" | "z.rr" | "sidh#2" | "skhal"
    | "stan" | "stu" | "sphur" | "hi.ms" -> do
      { let stem = match weak with
            [ [ 7 (* .r *) :: _ ] -> (* complex Paninian see Müller Gram xii *)
              if root = "jaag.r" then strong (* jaagari.sam RF IC 2 p 88 *)
              else long (* avaariit *)
            | [ 8 (* .rr *) :: _ ] -> 
              if root = "z.rr" then strong (* azariit *)
              else long 
            | [ c :: _ ] -> 
              if vowel c then long 
              else match root with 
                   [ "ku.t" | "gur" | "tru.t" | "pu.t" | "lu.th"| "sphur"
                       -> weak (* Kale no guna *)
                   | "kan" | "khan" |"car" | "mad#1" | "vad" | "vraj" | "skhal" 
                       -> long 
                   | _ -> strong
                   ]
            | [] -> error_empty 23
            ] in
        compute_ath_is_aorista stem root 
       ; match root with (* weird logic *)      
        [ ".r.s" | "ku.t" | "t.rr" | "tru.t" | "pu.t" | "sphur" 
            -> ()  (* active only *)
        | _ -> compute_ath_is_aoristm strong root 
        ]
      } 
    | "khan" | "pa.th" -> do (* Deshpande *)
      { compute_ath_is_aorista strong root (* apa.thiit *)
      ; compute_ath_is_aorista long root   (* apaa.thiit *)
      ; compute_ath_is_aoristm strong root 
      } 
    | "ku.s" | "gup" | "vrazc" | "zcut#1" | "sphu.t" -> (* active only *)
      compute_ath_is_aorista strong root 
    | "gur" -> (* Kale no guna only Atma *)
      compute_ath_is_aoristm weak root 
    | "zuu" -> 
      compute_ath_is_aorista (revcode "zve") root 
    | "kan" | "k.r#2"| "p.rr" | "zaz" -> (* active only *)
      compute_ath_is_aorista long root 
    | "kamp" | "gaah" | "jan" | "v.rt#1" | "zii#1" | "spand" -> (* middle only *)
      compute_ath_is_aoristm strong root 
    | "grah" -> do 
      { let stem = revcode "grah" in do (* same as group above *)
        { compute_ath_is_aorista stem root 
        ; compute_ath_is_aoristm stem root 
        } 
      ; let stem = revcode "grabh" in do (* supplement (ved) -- Whitney§900b *)
        { compute_ath_is_aorista stem root 
        ; compute_ath_is_aoristm stem root 
        } 
      }
    | _ -> ()
    ]
  ; match root with (* 6. si.s aorist se.t-sic *)
    [ "j~naa#1" | "dhyaa" | "dhyai" | "nam" | "zaa" -> do (* dhyai for dhyaa *)
      { compute_ath_sis_aorista strong root 
      ; compute_ath_is_aoristm strong root (* is aorist (5) used in middle *)
      }
    | "dham" | "dhmaa" | "paa#2" | "bhaa#1" | "mnaa" | "yaa#1" | "laa" (* Para *)
      -> compute_ath_sis_aorista strong root 
    | _ -> ()
    ]
; match root with (* 7. sa aorist ksa *)
      [ "k.r.s" | "kruz" | "kliz" | "guh" | "diz#1" | "dih" | "duh#1" | "dvi.s#1"
      | "lih#1" | "viz#1" | "v.rj" | "sp.rz#1" -> do (* \Pan{7,3,72-73} *)
      { compute_ath_sa_aorista weak root   
      ; if root = "kruz" || root = "kliz" then ((* Para *)) 
        else compute_ath_sa_aoristm weak root 
      }
    | "pac" -> do (* Kiparsky apaak.sam *)
      { compute_ath_sa_aorista long root 
      ; compute_ath_sa_aoristm long root 
      }
    | _ -> ()
    ]
  }
;
(* First approximation: we compute same forms as corresponding aorists. *)
(* Then restriction to attested usage *)
value compute_injunctive root =
  let (weak,strong,long) = stems root in do (* 7 families *)
  { match root with (* 1. root injunct *)
    [ "gam" | "gaa#1" | "bhid#1" | "bhuu#1" | "svap" -> do
      { compute_root_injuncta weak strong root 
      ; if root = "gam" then compute_root_injunctm weak root (* rare *) else ()
      ; let stem = match long with
            [ [ 2 (* aa *) :: _ ] -> [ 42 (* y *) :: long ]
            | _ -> long
            ] in 
        compute_root_injunctp stem root (* passive *)
      }
    | "k.r#1" -> compute_root_injunctm weak root
    | _ -> () 
    ]
  ; match root with (* 2. thematic injunct *)
    [ "k.rt#1" | "gam" | "g.rdh" | "ghas" | "zuc#1" -> do
      { compute_thematic_injuncta weak root
      ; compute_thematic_injunctm weak root (* middle is very rare *)
      }
    | "zram" -> compute_thematic_injuncta weak root (* zramat *)
    | "vac" -> let weak = revcode "voc" in do
               { compute_thematic_injuncta weak root (* vocat *) 
               ; compute_thematic_injunctm weak root (* vocanta *) 
               }
    | "zru" -> compute_thematic_injuncta (revcode "zrav") root (* zravat *)
    | _ -> () 
    ]
  ; match root with (* 3. reduplicated injunct *)
    [ "k.r#1" | "gam" -> 
      let stem = redup_aor weak root in do
      { compute_redup_injuncta stem root
      ; compute_redup_injunctm stem root 
      }
    | _ -> () 
    ]
  ; match root with (* 4. sigma injunct *)
    [ "k.r#1" | "chid#1" | "tyaj#1" | "daa#1" | "dhaa#1" | "pac" | "praz" 
    | "bhii#1" | "labh" | "sidh#1" | "svap" -> do
      { let stema = long in
        compute_ath_s_injuncta stema root 
      ; if root = "chid#1" then compute_ath_s_injuncta strong root else ()
        (* cchetsiit besides cchaitsiit *)
      ; let stemm = match weak with
            [ [ c :: r ] -> match c with 
               [ 3 | 4 | 5 | 6 (* i ii u uu *) -> strong
               | 2 (* aa *) -> [ 3 :: r ] (* turn aa to i *)
               | _ -> weak
               ]
            | _ -> error_empty 24
            ] in
        compute_ath_s_injunctm stemm root 
      }
    | _ -> ()
    ]
  ; match root with (* 5. i.s injunct *)
    [ "ak.s" | "aj" | "aas#2" | "i.s#1" | "iik.s" | "uk.s" | "uc" | "u.s" 
    | "uuh" | ".rc#1" | "k.rt#1" | "krand" | "kram" | "k.san"  | "khan"  | "car" 
    | "ce.s.t" | "jalp" | "jaag.r" | "t.rr" | "diip" | "pa.th" 
    | "puu#1" | "p.rc" | "baadh" | "budh#1" | "mad#1" | "mud#1" | "muurch" 
    | "mlecch" | "yaac" | "raadh" | "ruc#1" | "lu~nc" | "luu#1" | "vad" | "vadh"     | "vaz" | "vid#1" | "v.r#1" | "vyath" | "vraj" | "z.rr" | "sidh#2" 
    | "skhal" | "stan" | "stu" | "hi.ms" -> do
      { let stem = match weak with
            [ [ 7 (* .r *) :: _ ] -> 
              if root = "jaag.r" then strong (* jaagari.sam RF IC 2 p 88 *)
              else long (* avaariit *)
            | [ 8 (* .rr *) :: _ ] -> 
              if root = "z.rr" then strong (* azariit *)
              else long 
            | [ c :: _ ] -> 
              if vowel c then long 
              else match root with 
                   [ "kan" | "khan" |"car" | "mad#1" | "vad" | "skhal" -> long 
                   | _ -> strong
                   ]
            | [] -> error_empty 25
            ] in
        compute_ath_is_injuncta stem root 
      ; match root with 
        [ "t.rr" -> () (* active only *)
        | _ -> compute_ath_is_injunctm strong root 
        ]
      } 
    | "gup" | "vrazc" | "zcut#1" | "sphu.t" -> (* active only *)
      compute_ath_is_injuncta strong root 
    | "zuu" -> 
      compute_ath_is_injuncta (revcode "zve") root 
    | "kan" | "k.r#2"| "p.rr" -> (* active only *)
      compute_ath_is_injuncta long root 
    | "kamp" | "jan" | "zii#1" | "spand" -> (* middle only *)
      compute_ath_is_injunctm strong root 
    | "grah" -> do 
      { let stem = revcode "grah" in do (* same as group above *)
        { compute_ath_is_injuncta stem root 
        ; compute_ath_is_injunctm stem root 
        } 
      ; let stem = revcode "grabh" in do (* supplement (ved) -- Whitney§900b *)
        { compute_ath_is_injuncta stem root 
        ; compute_ath_is_injunctm stem root 
        } 
      }
    | _ -> ()
    ]
  } (* injunctives of kinds 6. and 7. missing *)
 ;
(* Aorist of causative *)
value compute_redup_aorista_ca stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (caaora 3) (thematic_preterit_a conjug))
  (* NB Macdonnel dixit -- Gonda says "ur" for Third Plural *)
;
value compute_redup_aoristm_ca stem root = 
  let conjug person suff = (person,fix_augment stem suff) in
  enter1 root (Conju (caaorm 3) (thematic_preterit_m conjug))
;
value compute_aor_ca cpstem root = 
  match root with 
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
         let voy = if root = "daa#1" then 1 (* a *)
                   else 3 (* i *) (* aap -> ip Whitney§861b *) in
         let istem = [ 37 :: [ voy :: w ] ] in
         let stem = redup_aor istem root in do 
         { compute_redup_aorista_ca stem root (* ati.s.thipat adiidapat *)
         ; compute_redup_aoristm_ca stem root 
         }
     | [ 37 :: [ 1 :: _ ] ] -> 
         let stem = redup_aor cpstem root in do 
         { compute_redup_aorista_ca stem root (* ajij~napat *)
         ; compute_redup_aoristm_ca stem root 
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
         let stem = redup_aor istem root in do 
         { compute_redup_aorista_ca stem root (* adidiipat *) 
         ; compute_redup_aoristm_ca stem root  
         }
     | _ -> error_empty 27
     ] 
  | _ -> () 
  ] 
;

(************************************************************************)
(* Periphrastic future, Infinitive, Passive future participle in -tavya *)
(************************************************************************)

value compute_peri_fut conj perstem root = 
  let conjug person suff = (person,sandhi perstem (code suff)) in
  enter1 root (Conju (conj,Perfut Active)
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
value record_pfp_tavya conj perstem root = 
  let pfp_stem = fix perstem "tavya" in
  record_part (Pfutp_ conj (rev pfp_stem) root) (* rev compat root by Pfpart *)
;
value build_infinitive c inf_stem root = do
(* By default, for Causative, we get eg bhaavayitum, and later forms such as 
   bhaavitum have to be entered as supplements; see Witney§1051c. *)
  { enter1 root (Invar (c,Infi) (fix inf_stem "tum"))
  ; enter1 root (Inftu c (fix inf_stem "tu")) (* Xtu-kaama compounds *)
(* NB. bahuvrihi cpds in -kaama and -manas constructed with infinitives in -tu 
   See Renou HLS p72 from Pata~njali; Renou grammaire §107 dagdhukaama
   Vt to Pan{6,1,144} bhoktukaama.h 
   also Assimil p194 eg tyaktukaama and Kale§917 noted "tufkaamamanasorapi" 
   anu.s.thaatukaama "desirious to proceed" vaktukaama "who wants to speak"
   pratiyaatukaamam "désireux de retourner" Rag{5,18} 
   pra.s.tumanas Kum{5,40} "désireux de poser une question" Kale§917
   dra.s.tumanas "inclined to see" 
   dra.s.tuzakya "able to see" - possible extension in -zakya compoudns *)
  }
;
value perif conj perstem root = do 
  { match root with 
    [ "cint" -> () (* no future *)
    | _ -> compute_peri_fut conj perstem root
    ]
  ; let inf_stem =  match conj with
        [ Primary -> (* Difference infinitive/tavya forms and peri-future *)
             match root with (* should rather appear in perstems *)
             [ "g.rr#1" -> revcode "giri" (* giritum, not gariitum *) 
             | "jak.s"  -> revcode "jagh" (* jagdhum \Pan{2,4,36} *)
             | "dabh"   -> revcode "dabh" (* dabhdum WR *)
             | "p.rr"   -> revcode "puuri" (* puuritum \Pan{7,1,102} *)
             | "sva~nj" -> revcode "svaj" (* svaktum *)
             | "sa~nj"  -> revcode "saj" (* saktum *)
             | "s.rp"   -> revcode "sarpi" (* sarpitum *)
             | ".dii"   -> revcode ".dii" (* .diitum *)
             | _ -> perstem
             ] 
        | _ -> perstem 
        ] in 
    build_infinitive conj inf_stem root (* pb saa1 setum WR -situm *)
  ; if admits_passive root then record_pfp_tavya conj perstem root else ()
  (* other pfps generated from [pfp_ya] et [pfp_aniiya] below *)
  }
;
(* Computes periphrastic future, infinitive and [pfp_tavya] Primary forms *)
value compute_perif rstem root =
  let pstems = perstems rstem root in 
  iter (fun st -> perif Primary (rev st) root) pstems
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
value pfp_ya rstem root =
  let (_, strong, long) = stems root in
  (* NB we do not use [weak_stem] and thus rstem is not mrijified/duhified *)
  let ya_stem = match rstem with
    [ [ 1 :: _ ] -> rstem 
    | [ 2 :: r ] 
    | [ 11 (* ai *) :: r ] 
    | [ 12 (* o *) :: r ] 
    | [ 13 (* au *) :: r ] -> match root with 
        [ "mnaa" | "zaa" | "saa#1" -> rstem (* mnaaya zaaya avasaaya *)
        | _ -> [ 10 :: r ] (* deya *)
        ]
    | [ 3 :: _ ] | [ 4 :: _ ] -> strong (* but zeya for zii1 not in Bucknell ? *)
    | [ 5 (* u *) :: r ] -> match root with 
        [ "stu" -> [ 45 :: [ 2 :: r ] ] (* u -> aav *)
        | "yu#1" -> [ 6 :: r ] (* u -> uu *)
        | "yu#2" -> raise Not_attested 
        | _ -> strong 
        ]
    | [ 6 (* uu *) :: _ ] -> match root with 
        [ "huu" -> revcode "hav" (* havya WR (?) *)
        | _ -> strong 
        ] 
    | [ 7 (* .r *) :: _ ] -> match root with 
        [ "dhv.r" -> strong (* dhvarya *)
        | "d.r#1" | "v.r#2" -> [ 32 :: rstem ] (* d.rtya v.rtya \Pan{3,1,109} *)
          (* others as supplementary forms with interc t in [record_pfp] below *)
        | _ -> long (* kaarya (k.rt .nyat) \Pan{3,1,124} *)
        ] 
    | [ 8 (* .rr *) :: _ ] ->  match root with 
        [ "st.rr" -> strong (* starya *)
        | _ -> long
        ] 
      (* now consonant rules - order of patterns important *)
    | [ 22; 7 ] (* .rc *) 
    | [ 24; 7 ] (* .rj *) -> strong (* arc arj *)
    | [ 24; 7; 41 ] (* m.rj *) -> long (* maarj \Pan{7,2,114} *)
    | [ 32; 7; 17 ] (* k.rt *) -> strong (* kartya WR *)
    | [ 47; 7 ] (* .r.sya autonomous *)
    | [ 48; 1 ] (* as1 *) -> 
            if root = "as#1" then raise Not_attested (* \Pan{2,4,52} use bhuu *) 
                             else rstem (* asya - may overgenerate *)   
    | [ 48; 1; 46 ] (* zas *) -> rstem 
    | [ 48; 2; 46 ] (* zaas *) -> rstem (* zaasya + zi.sya extra *)
    | [ 33; 36; 1; 43; 19 ] (* granth *) -> revcode "grath"  
    | [ 35; 1; 45 ] (* vadh/han *) -> rstem (* vadhya *) 
    | [ 36; 1; 49 ] (* han *) -> revcode "ghaat" (* (h=h') \Pan{7,3,32+54} *)
    | [ 35; 1; 42; 45 ] (* vyadh *) -> revcode "vedh"
    | [ 45; 4; 28; 47 ] (* .s.thiiv *) -> revcode ".s.thev" (* as .s.thiv *)
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
    | [ 31; 1; 43 ] (* ra.n *) -> rstem (* ra.nya *)
    | [ c :: [ 1 :: _ ] ] when labial c -> rstem  (* \Pan{3,1,98} -yat *) 
    | [ c :: [ 1 :: r ] ] -> [ c :: [ 2 :: r ] ] 
                      (* a lengthened if last non labial *)
                      (* above often optional, see [record_extra_pfp_ya] below *)
    | [ c :: [ 7 :: _ ] ] -> rstem (* d.rz1 v.r.s but NOT m.rj k.rt *)
    | [ c :: [ v :: _ ] ] when short_vowel v (* gunify *) -> strong
    | _ -> rstem
    ] in 
  record_pfp_ya Primary ya_stem root 
 ;
(* Primary conjugation pfp in -ya for gana 10 *)
value pfp_ya_10 rstem root =
  let pfp_stem = rfix rstem "ya" in
  record_part (Pfutp_ Primary pfp_stem root) 
 ;
(* Primary conjugation pfp in -aniiya *)
value pfp_aniiya rstem root =
  let iya_stem = 
     match root with 
     [ "uk.s" | "cint" -> rstem (*i others ? PB [strong_stem] ? i*)
     | "yu#1" | "yu#2" -> raise Not_attested 
     | "dham" -> revcode "dhmaa"  (* \Pan{7,3,78} *)
     | "vyadh" -> revcode "vedh"
     | _ -> match Word.mirror rstem with
            [ [ 4 :: _ ] | [ 6 :: _ ] -> rstem (* ii- uu- no guna *) 
            | _ -> strong_stem root rstem
            ]
     ] in 
  record_pfp_aniiya Primary iya_stem root 
 ;
value record_pfp_10 root rstem = do
  { try pfp_ya_10  rstem root with [ Not_attested -> () ]
  ; try pfp_aniiya rstem root with [ Not_attested -> () ]
  }
;

(**********************************)
(* Absolutive and Past Participle *)
(**********************************)

value record_part_ppp ppstem root = do 
  { record_part (Ppp_ Primary ppstem root)
  ; record_part (Pppa_ Primary ppstem root) (* pp-vat (krit tavat) *)
  }
;
(* No attested abso in classical language - Brocquet§4.5 *)
value no_abso = fun (* could add: "majj" *)
  [ "jan" | "pad#1" | "puuj" | "p.rr" | "bh.r" | "rak.s" -> True
  | _ -> False
  ]
;
value record_abso_ya form root =  (* lyap \Pan{7,1,37} *)
  if no_abso root then () else
  enter1 root (Invar (Primary,Absoya) form) 
and record_abso_tvaa form root = (* ktvaa \Pan{3,4,18+} *)
  if no_abso root then () else
  enter1 root (Absotvaa Primary form)
;
(* First absolutives in -ya \Pan{7,1,37} lyap *)
value record_abs_ya root rstem w = do
  (* intercalate t for light roots Kiparsky[159] Macdonell§165 *)
  { let absya = 
       if light rstem then fix w "tya" (* check test light *)
       else let rst = match root with
            [ (* roots in -m and -n in gana 8 \Pan{6,4,37} *)
                "van" | "man" | "tan#1" (* man also in gana 4 *)
            | "gam" | "nam" | "yam" | "han#1" (* anudatta ? *)
              (* next 2 needed to avoid aa like pp according to \Pan{6,4,15} *)
            | "kram" | "klam" | "cam" | "dam#1" | "dhvan" | "bhram" | "vam"
            | "zam#1" | "zam#2" | "zram"
            | "daa#2" | "saa#1" | "sthaa#1" | "maa#1" (* \Pan{7,4,40} *)
            | "daa#1" (* \Pan{7,4,46} *)
            | "dhaa#1" (* \Pan{7,4,42} *)
                   -> rstem 
            | "zii#1"  -> revcode "zay" (* \Pan{7,4,22} *)
            | "arh"    -> revcode "argh" (* arghya (h=h') *)
            | "k.rt#1" -> revcode "kart" (* strong to avoid k.rtya from k.r *)
            | "jak.s"  -> revcode "jagdh" (* \Pan{2,4,36} prajagdhya *)
            | _ -> w (* follows pp *)
            ] in match root with
                 [ "hi.ms" -> code "hi.msya" (* no retroflex s Whitney§183 *)
                 | _ -> fix rst "ya" 
                 ] in
    record_abso_ya absya root
  ; match root with (* alternate forms in -ya and -tvaa *)
    [ "gam" | "tan#1" | "nam" | "man" | "van" | "han#1" ->
      (* a+nasal optional assimilation to light roots *)
        record_abso_ya (fix w "tya") root
    | "dabh"   -> record_abso_tvaa (code "dambitvaa") root (* WR *)
    | "dhaa#1" -> record_abso_tvaa (code "dhitvaa") root 
    | "plu"    -> record_abso_ya (code "pluuya") root
    | "vad"    -> record_abso_ya (code "vadya") root (* anuvadya *)
    | "b.rh#1" -> record_part_ppp (revcode "b.r.mhita") root (* MW -WR *) 
    | "v.r#2" -> do { record_abso_tvaa (code "varitvaa") root
                    ; record_abso_tvaa (code "variitvaa") root 
                    }
    | "kram" -> record_abso_tvaa (code "krantvaa") root (* \Pan{6,4,18} *)
    | "bhram" -> record_abso_ya (code "bhraamya") root (* WR *)
    | "k.lp" -> record_abso_ya (code "kalpya") root (* parikalpya KuS{1,2} *)
    | "zaas" -> (* passive stem zi.s *)
        let w = revcode "zi.s" in do (* as if ipad=0 *)
        { record_part_ppp (rfix w "ta") root 
        ; record_abso_tvaa (fix w "tvaa") root
        ; record_abso_ya (fix w "ya") root
        }
    | _ -> ()
    ]  
  }
;
(* For absolutives of roots gana 10 - Macdonell§164a Whitney§1051d *) 
value light_10 = fun 
   [ [] -> False
   | [ c :: r ] -> if vowel c then False else match r with
          [ [] -> False
          | [ v :: _ ] -> short_vowel v (* opp guru Pan{1,4,11} *)
          ]
   ]
;
value alternate_pp = fun
  [ "m.r.s" | "svid#2" | "dh.r.s" | "puu#1" (*i \Pan{?} i*)
    (* next roots of gu.na 1 have penultimate "u" *)
  | "kul" | "k.sud" | "guh" | "jyut" | "dyut#1" | "mud#1" | "rud#1" | "ruh#1"
  | "lul" | "zuc#1" | "zubh#1" | "zu.s#1" -> True
  | _ -> False
  ]
;
(* Condition for extra abs in -tvaa with guna: root starts with consonant
   and ends in any consonant but y or v and has i or u as penultimate. 
   Given by \Pan{1,2,26}. Example: sidh1 likh *)
value alternate_tvaa root rstem =
  match Word.mirror rstem with (* double rev *)
  [ [ c :: _ ] -> consonant c && match rstem with
      [ [ 42 (* y *) :: _ ] | [ 45 (* v *) :: _ ] -> False
      | [ c' :: rest ] -> consonant c' && match rest with 
          [ [ 3 (* i *) :: _ ] | [ 5 (* u *) :: _ ] -> True | _ -> False ]
      | _ -> False
      ]
  | _ -> match root with
         [ "t.r.s#1" | "m.r.s" | "k.rz" (* \Pan{1,2,25} *)
         | "puu#1" (* \Pan{1,2,22} *) -> True 
         | _ -> False
         ]
  ]
;
(* Records the (reversed) ppp stem (computed by [compute_ppp_stems])
   and builds absolutives in -tvaa and -ya *)
value record_ppp_abs_stems root rstem ppstems =
  let process_ppstem = fun
     [ Na w -> do 
        { record_part_ppp (rfix w "na") root 
        ; match root with (* too obfuscated *) 
          [ "pii" -> () (* no absolutive *)
          | _ -> let stem = match root with (* roots in -d *) 
            [ "k.sud" | "chad#1" | "chid#1" | "ch.rd" | "tud#1" | "t.rd" | "nud" 
            | "pad#1" | "bhid#1" | "mid" | "vid#2" | "zad" | "sad#1" | "had" 
            | "svid#2" -> match w with 
                          [ [ 36 (* n *) :: r ] -> [ 34 (* d *) :: r ] 
                          | _ -> failwith "Anomaly Verbs"
                          ] 
            | "vrazc" -> revcode "v.rz" (* not v.rk *)
            | "und" | "skand" | "syand" -> [ 34 (* d *) :: w ]
            | _ -> w 
            ]    in match root with 
            [ "mid" -> let abs_mid st = record_abso_tvaa (fix st "itvaa") root in
                       do { abs_mid stem; abs_mid (revcode "med") (* guna *)}
            | _  -> do 
              { record_abso_tvaa (fix stem "tvaa") root (* ktvaa \Pan{3,4,18+} *)
              ; record_abso_ya (fix stem "ya") root (* lyap \Pan{7,1,37} *)
              }
            ]
         ]
        }
     | Ka w -> do (* zu.s1 *)
         { record_part_ppp (rfix w "ka") root (* zu.ska \Pan{8,2,51} *)
         ; record_abso_ya  (fix w "ya")  root (* -zu.sya *)
         }
     | Va w -> do 
         { record_part_ppp  (rfix w "va")  root 
         ; record_abso_tvaa (fix w "tvaa") root
         ; record_abso_ya   (fix w "ya")   root
         }
     | Ta w -> do
         { if is_anit_pp root rstem then record_part_ppp (rfix w "ta") root
           else ((* taken care of as Tia *))
         ; if is_anit_tvaa root rstem then record_abso_tvaa (fix w "tvaa") root
           else ((* taken care of as Tia *))
         ; (* abs -ya lyap computed whether set or anit *) 
           match root with 
           [ "av" -> record_abs_ya root rstem (revcode "aav") (* -aavya *)
           | "v.rj" -> record_abs_ya root rstem (revcode "varj") (* -varjya *)
           | _    -> record_abs_ya root rstem w
           ]
         }
     | Tia w -> let (ita,itvaa) = 
                if root = "grah" then ("iita","iitvaa")  (* \Pan{7,2,37} *)
                   else ("ita","itvaa") in do
         { if is_set_pp root rstem then 
              match root with
              [ "dh.r.s" | "zii#1" (* "svid#2" "k.svid" "mid" \Pan{1,2,19} *)
                 -> record_part_ppp (rfix (strong w) ita) root
              | _ -> do
                { record_part_ppp (rfix w ita) root
                ; if alternate_pp root then 
                     record_part_ppp (rfix (strong w) ita) root
                  else ()
                }
              ]
           else ()
         ; if is_set_tvaa root rstem then do 
              { let tstem = match root with
                    [ "m.rj" -> lengthened rstem (* maarj *)
                    | "yaj#1" | "vyadh" | "grah" | "vrazc" | "praz" | "svap"  
                    | "vaz" | "vac" | "vap" | "vap#1" | "vap#2" | "vad" 
                    | "vas#1" | "vas#4" -> w
                    | "siiv" -> revcode "sev" (* gu.na *)
                    | "v.rj" -> revcode "varj" (* gu.na *)
                    | "stambh" -> rstem (* stabhita but stambhitvaa! *)
                    | _ -> strong w 
                    ] in
                record_abso_tvaa (fix tstem itvaa) root
              ; if alternate_tvaa root rstem then 
                   record_abso_tvaa (fix w "itvaa") root
                else ()
              }
           else ()
         }
    ] in 
  iter process_ppstem ppstems
;
(* Simple version for denominatives - tentative *)
value record_ppp_abs_den ystem root = 
 let ppstem =  match root with 
    [ "cira" | "bhi.saj" | "vaira" -> ystem (* to be completed *)
    | _ -> trunc (revstem root) 
    ] in do  
  { record_part_ppp (rfix ppstem "ita") root 
  ; match root with
    [ "aakar.na" -> record_abso_tvaa (fix ppstem "ya") root (* fake abso-ya! *)
    | _ -> record_abso_tvaa (fix ystem "itvaa") root 
    ]
  (* no general [record_abso_ya] since usually no preverb to denominatives *)
  ; match root with
    [ "dhiira#1" -> record_abso_ya (code "dhiirya") root (* avadhiirya *)
    | _ -> () (* to be completed *)
    ]
  }
;
(* Absolutive in -am - Macdonell§166 Stenzler§288 \Pan{3,4,22} .namul          *)
(* Registered in Inv-Absoya and in Absotvaa, since may be used with preverbs.  *)
(* Used specially for verbs that may be iterated - having done again and again *)
(* "gaaya.m gaayam" ayant chanté et chanté; "paaya.m paayam" ayant bu et bu.   *)
value record_abso_am root = 
  let record form = let word = code form in do 
      { record_abso_tvaa word root (* no preverb *)
      ; record_abso_ya   word root (* some preverb *)
      } in 
  match root with (* gu.na + am frequent, but exceptions *)
  [ "as#2"    -> record "aasam" (* \Pan{3,4,57} may overgenerate *)
  | "ka.s"    -> record "kaa.sam" (* \Pan{3,4,34} *) 
  | "kuc"     -> record "kocam" (* \Pan{3,4,54} *)
  | "kram"    -> record "kraamam"
  | "k.r#1"   -> record "kaaram" (* \Pan{3,4,26-28+61} *)
  | "k.r.s"   -> record "kar.sam" (* \Pan{3,4,49} avec upa *)
  | "k.sp"    -> record "k.sepam" (* Deshpande *)
  | "khan"    -> record "khaanam"
  | "gaa#2"   -> record "gaayam" (* Deshpande *)
  | "grah"    -> record "graaham"(* \Pan{3,4,39+58} *)
  | "c.rt"    -> record "c.rtam"
  | "jiiv"    -> record "jiivam" (* \Pan{3,4,30} *)
  | "j~naa#1" -> record "j~naayam"
  | "ta.d"    -> record "taa.dam" (* Deshpande *)
  | "t.r.s#1" -> record "tar.sam"
  | "traa"    -> record "traayam" (* Deshpande *)
  | "da.mz"   -> record "da.mzam" (* \Pan{3,4,47} avec upa- *)
  | "dah#1"   -> record "daaham" (* Deshpande *)
  | "daa#1"   -> record "daayam" (* Deshpande *)
  | "diz#1"   -> record "dezam" (* \Pan{3,4,58} avec aa-*)
  | "d.rz#1"  -> record "darzam" (* \Pan{3,4,29} Apte§166 totalité *)
  | "dhaa#1"  -> record "dhaayam" (* \Pan{3,4,45} *)
  | "dhyaa"   -> record "dhyaayam" (* Deshpande *)
  | "naz#1"   -> record "naazam" (* \Pan{3,4,43+45} *)
  | "pat#1"   -> record "paatam" (* \Pan{3,4,56} *)
  | "pa.th"   -> record "paa.tham" (* Deshpande *)
  | "paa#1"   -> record "paayam" (* Deshpande *)
  | "pi.s"    -> record "pe.sam" (* \Pan{3,4,35+38+55} *)
  | "pii.d"   -> record "pii.dam" (* \Pan{3,4,49} avec upa *)
  | "pu.s#1"  -> record "po.sam" (* \Pan{3,4,40} *)
  | "p.rr"    -> record "puuram" (* \Pan{3,4,31+44} \Pan{7,1,102} *)
  | "praz"    -> record "p.rccham"
  | "bandh"   -> record "bandham" (* \Pan{3,4,41} *)
  | "bhuj#1"  -> record "bhojam"
  | "bhuu#1"  -> record "bhaavam"
  | "maa#1"   -> record "maayam" (* Deshpande *)
  | "m.r"     -> record "maaram" (* Deshpande *)
  | "rudh#2"  -> record "rodham" (* \Pan{3,4,49} avec upa *)
  | "vad"     -> record "vaadam"
  | "vah#1"   -> record "vaaham" (* \Pan{3,4,43} *)
  | "vid#1"   -> record "vedam" (* \Pan{3,4,29} Apte§166 totalité *)
  | "viz#1"   -> record "vezam" (* \Pan{3,4,56} *)
  | "v.rt#1"  -> record "vartam" (* \Pan{3,4,39} hastavartam *)
  | "zu.s#1"  -> record "zo.sam" (* \Pan{3,4,44} *)
  | "zru"     -> record "zraavam"
  | "sa~nj"   -> record "sa~ngam"
  | "s.r"     -> record "saaram"
  | "s.rp"    -> record "sarpam"
  | "skand"   -> record "skandam" (* \Pan{3,4,56} *)
  | "stambh"  -> record "stambham"
  | "sthaa#1" -> record "sthaayam" (* Bhate: zayyosthaayam sauté du lit *)
  | "sm.r"    -> record "smaaram" (* Deshpande *)
  | "han"     -> record "ghaatam" (* \Pan{3,4,36+37+48} *)
  | "knuu"    -> record "knopam" (* from causative *)
  | _ -> ()
  ]
(* NB Bandharkar: colloquial expressions iic+V.namul suivi de forme finie de V *)
(* eg "hastagraaha.m g.r.naati" il tient par la main *)
(* idem "zayyotthaaya.m bhufkte" sitôt levé du lit il mange \Pan{3,4,52} *)
(* also go.spadapuuram \Pan{3,4,32}. Could be recognized with extra phases, 
   or ad-hoc inclusion like "utthaayam" in [Nouns.enter_indecl_ifcs] - berk *)
(* Should be also definable for causative, eg knopam ca{knuu} \Pan{3,4,33} *)
;
(* absolutive of secondary conjugations *)
value record_absolutive c abs_stem_tvaa abs_stem_ya intercal root = 
  let record_abso_ya form = enter1 root (Invar (c,Absoya) form) 
  and record_abso_tvaa form = enter1 root (Absotvaa c form) in do
  { let sfx = if intercal then "itvaa" else "tvaa" in
    record_abso_tvaa (fix abs_stem_tvaa sfx)
  ; record_abso_ya   (fix abs_stem_ya "ya")
  }
;
value record_pppca cpstem cstem root =
  let ppstem = [ 1 :: [ 32 :: [ 3 :: cpstem ] ] ] (* cp-ita *) in do 
  { record_part (Ppp_ Causative ppstem root)
  ; record_part (Pppa_ Causative ppstem root) (* pp-vat *)
  ; let abs_stem_ya = match root with (* Whitney§1051d *)
        [ "aap" | ".r" | ".rc#1" | ".rdh" | "kal" | "k.lp" | "kram" | "gam" 
        | "jan" | "jval" | "dh.r" | "rac" | "zam#1" | "p.rr" | "bhak.s" | "v.rj" 
            -> cstem  (* retains ay: -gamayya to distinguish from -gamya *)
        | _ -> cpstem (* eg -vaadya -vezya -k.saalya *)
        ] 
    and abs_stem_tvaa = match root with 
        [ "smi" -> cpstem (* smaayitvaa not smaayayitvaa ? *)
        | _ -> cstem (* retains ay: gamayitvaa *) 
        ] in
    record_absolutive Causative abs_stem_tvaa abs_stem_ya True root 
       (* cp-ita -> cp-ayitvaa, -cp-ayya ou -cp-ya *)
  }
;
value record_pppdes stem root =
  let ppstem = [ 1 :: [ 32 :: [ 3 :: stem ] ] ] in (* s-ita *) do
  { record_part (Ppp_ Desiderative ppstem root)
  ; record_part (Pppa_ Desiderative ppstem root) (* pp-vat *)
  ; let abs_stem_tvaa = [ 3 :: stem ] (* s-i *) 
    and abs_stem_ya = stem in
    record_absolutive Desiderative abs_stem_tvaa abs_stem_ya False root 
       (* s-ita -> s-itvaa, -s-iya *)
  }
;

(******************************)
(* Intensive or frequentative *)
(******************************)

value compute_intensive_presenta strong weak iiflag root =
(* info not used for check because of ambiguity of third sg - we want no
   error message in the conjugation engine display *)
  let conjugs person suff = (person,fix strong suff) 
  and conjugw person suff = (person,fix3w weak iiflag False suff) in do
  { enter1 root (Conju intensa 
   [ (Singular, 
(* intercalate ii Wh§1006 dubious *daridreti (draa1) *jafgheti (han1) *)
        [ conjugs First  "mi"
        ; conjugw First  "iimi"
        ; conjugs Second "si"
        ; conjugw Second "iisi"
        ; conjugs Third  "ti" 
        ; conjugw Third  "iiti" 
        ])
   ; (Dual, 
        [ conjugw First  "vas"
        ; conjugw Second "thas"
        ; conjugw Third  "tas"
        ])
   ; (Plural,
        [ conjugw First  "mas"
        ; conjugw Second "tha"
        ; conjugw Third (if root="han#1" then "anti" (* jafghanti *)
                         else "ati")
        ])
   ])
  ; (* Tentative for ppr - needs iiflag setting *)
    let wk = if iiflag then Some (trunc_ii weak) 
                (* ii disappears before vowels in special roots *)
             else if consonantal weak then Some weak (* 3rd pl weak stem *)
             else None (* problematic. Concerns 
                          daridraa for draa1
                          naanada for nad
                          bobho for bhuu1
                          yaayajyaa for yaj1
                          jafgha for han1 *) in 
     match wk with [ Some weak -> record_part (Pprared_ Intensive weak root) 
                   | _ -> ()
                   ]
  }
;
value compute_intensive_impfta strong weak iiflag root =
  let conjugs person suff = (person,fix_augment strong suff)
  and conjugw person suff = (person,fix3w_augment weak iiflag False suff) in
  enter1 root (Conju intimpfta 
   [ (Singular, 
        [ conjugs First  "am"
        ; conjugs Second "s" 
        ; conjugw Second "iis"
        ; conjugs Third  "t"
        ; conjugw Third "iit"
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
value compute_intensive_optativea weak iiflag root =
  let conjugw person suff = (person,fix3w weak iiflag False suff) in
  enter1 root (conjug_optativea int_gana Intensive conjugw)
;
value compute_intensive_imperativea strong weak iiflag root =
  let conjugs person suff = (person,fix strong suff)
  and conjugw person suff = (person,fix3w weak iiflag False suff) 
  and vowel = match weak with 
              [ [ c :: _  ] when vowel c -> True
              | _ -> False
              ] in
  enter1 root (Conju intimpera
   [ (Singular, 
        [ conjugs First "aani"
        ; conjugw Second (if vowel then "hi" else "dhi")
        ; conjugs Third  "tu"
        ; conjugw Third  "iitu"
        ])
   ; (Dual, 
        [ conjugs First  "aava"
        ; conjugw Second "tam"
        ; conjugw Third  "taam"
        ])
   ; (Plural,
        [ conjugs First  "aama"
        ; conjugw Second "ta"
        ; conjugw Third  (if vowel then "tu" else "atu") 
        ])
   ])
;
(* Reduplication for the intensive conjugation - TODO Macdonell§173 
[value redup_int root = ...]
For the moment, the reduplicated stem is read from the lexicon. 
It is not clear whether there are enough intensive forms to warrant a paradigm
rather than a table. *)
(* missing paradigm: Pprp e.g. jalgalyamaana for gal *)

(* Similar to [compute_active_present3] with Intensive, plus optional ii forms *)
value compute_intensivea wstem sstem root third = 
  let iiflag = False in (*i TEMP - TODO i*)
  (* [let (sstem,wstem) = redup_int root in] *) do 
  { compute_intensive_presenta sstem wstem iiflag root (* no third *)
  ; compute_intensive_impfta sstem wstem iiflag root
  ; compute_intensive_optativea wstem iiflag root 
  ; compute_intensive_imperativea sstem wstem iiflag root 
  ; if root="bhuu#1" (* bobhoti *) then
       let stem = revcode "bobhav" in 
       build_perpft Intensive stem root
    else () (* EXPERIMENTAL *)
  ; if root = "draa#1" then
       let ppstem = revcode "daridrita" in
       record_part (Ppp_ Intensive ppstem root) 
    else ((* TODO *))
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
value compute_present_system root rstem gana pada third = 
  try
   (* pada=True for active (parasmaipade), False for middle (aatmanepade) *)
   let padam = if third=[] then False else pada in (* artifact for fake below *)
   match gana with
   [ 1 | 4 | 6 | 10 (* thematic conjugation *) -> 
     let compute_thematic_present stem = 
         match voices_of_gana gana root with
         [ Para -> (* active only *) if pada then
             compute_thematic_active gana Primary stem root third
             else emit_warning ("Unexpected middle form: " ^ root)
         | Atma -> (* middle only *)  
             if padam then emit_warning ("Unexpected active form: " ^ root) 
             else compute_thematic_middle gana Primary stem root third
         | Ubha -> 
             let thirda = if pada then third else []
             and thirdm = if pada then [] else third in do
             { compute_thematic_active gana Primary stem root thirda
             ; compute_thematic_middle gana Primary stem root thirdm
             }
         ] in 
     match gana with
     [ 1 -> match root with 
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
               (* [ compute_thematic_middle 1 Primary (strong rstem) root 
                    (if pada then [] else third) ] (* ohate ved *) *)
               }
            | "huu" -> do (* 2 forms *) (* hvayati, havate *)
               { compute_thematic_present (revcode "hve") 
               ; compute_thematic_middle 1 Primary (revcode "hav") root
                    (if pada then [] else third) (* havate *)
               }
            | _ -> let stem = match root with 
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
                 (* also "s.r"  -> "dhau" (corresponds to dhaav1) *)
              | "dhmaa"   -> revcode "dham" 
              | "dhaa#1"  -> revcode "dadh" (* id *) 
              | "paa#1"   -> revcode "pib" (* fake 3rd gana \Pan{7,3,78} *)
              | "ghraa"   -> revcode "jighr"          (* id \Pan{7,3,78} *)
              | "sthaa#1" -> revcode "ti.s.th"        (* id \Pan{7,3,78} *)
              | "d.rh"    -> revcode "d.r.mh" (* .rh -> .r.mh *)
              | "b.rh#1"  -> revcode "b.r.mh" (* WR; Bucknell adds barhati *)
              | "iir.s" | "gaa#2" (* = gai *)
              | "daa#3" | "dyaa"  | "zyaa" | "sphaa" 
                        -> [ 42 (* y *) :: rstem ](* aa/ai add y *)
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
                  trunc_a rstem (* since thematic a added *)
              | "k.rp" -> rstem 
              | _ -> strong rstem (* default *)
              ] in compute_thematic_present stem  
            ]
     | 4 -> let weak = match root with
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
            | [ 8 :: rest ] -> match root with
                   [ "p.rr" -> revcode "p.r.n" (* ugly duckling *)
                   | _ -> [ 43 :: [ 3 :: rest ] ] (* .rr/ir *)
                   ]
              (* -.rr -> -ir eg [t.rr] *)
            | _ -> match root with
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
                 ; build_perpft Primary y_stem root
                 ; let perstem = [ 3 :: y_stem ] (* -ayi *) in
                   perif Primary perstem root 
                 } in 
        match root with 
        [ "tul" -> do (* 2 forms *)
            { process10 (revcode "tulay")
            ; process10 (revcode "tolay") (* guna *)
            }
        | "mid" -> process10 (revcode "minday") (* nasal *)
        | _ -> let base_stem = strengthen_10 rstem root in 
               let ystem = rev (sandhi base_stem [ 1; 42 ] (* ay *)) in 
               process10 ystem
        ] 
     | _ -> failwith "Anomaly Verbs"
     ] (* end of thematic conjugation *) 
   | 2 -> (* athematic conjugation: 2nd class (root class) *)
     let set = augment_ii root 
     and sstem = strong_stem root rstem 
     and wstem = if root="as#1" then [ 48 ] (* rare archaic forms *)
         (* svap opt supyaat MW but Kane§42  and DRK do not support weak "sup" *)
                 else weak_stem root rstem in do (* note: no samprasaarana *)
     { match voices_of_gana 2 root with
       [ Para -> (* active only *) if pada then
          compute_active_present2 sstem wstem set root third
          else emit_warning ("Unexpected middle form: " ^ root)
       | Atma (* middle only *) -> 
          if padam then emit_warning ("Unexpected active form: " ^ root) else 
             compute_middle_present2 sstem wstem set root third
       | Ubha ->
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present2 sstem wstem set root thirda
          ; compute_middle_present2 sstem wstem set root thirdm
          }
       ]
     ; match root with (* special cases *)
       [ "as#1" -> (* rare middle forms of as *)
         compute_athematic_present2m sstem [ 48 ] set root (code "ste")
(*[    | "vac" -> let weak = revcode "vaz" (* douteux -WR *) in
                  compute_athematic_present2m sstem weak set root [] ]*)
       | _ -> ()
       ]
     }
   | 3 -> let (sstem,wstem,iiflag) = redup3 root rstem in
          match voices_of_gana 3 root with
       [ Para -> if pada then
          compute_active_present3 sstem wstem iiflag root third
          (* TODO allow bibhi for weak bibhii root bhii1 Whitney§679 *)
          else emit_warning ("Unexpected middle form: " ^ root)
       | Atma -> 
          if padam then emit_warning ("Unexpected active form: " ^ root)
          else compute_middle_present3 sstem wstem iiflag root third
       | Ubha ->
          let thirda = if pada then third else []
          and thirdm = if pada then [] else third in do
          { compute_active_present3 sstem wstem iiflag root thirda
          ; compute_middle_present3 sstem wstem iiflag root thirdm
          }
       ] 
   | 5 -> (* athematic conjugation: 5th class *)
     let (stem,vow) = match rstem with 
         [ [ 36; 3 ]     (* in *)  -> ([ 3 ] (* i *),True) (* Whitney§716a *)
         | [ 5; 43; 46 ] (* zru *) -> ([ 7; 46 ] (* z.r *),True) (*\Pan{3,1,74}*)
         | [ 40 :: [ 41 :: r ] ] -> ([ 40 :: r ],False) (* skambh stambh *)
           (* possibly other penultimate nasal lopa ? *)
         | [ c :: rest ] -> if vowel c then ([ short c :: rest ],True)
                            else (rstem,False)  
         | [] -> error_empty 29
         ] in
     let wstem = rev (sandhi stem [ 36; 5 ]) (* stem-nu *)
     and sstem = rev (sandhi stem [ 36; 12 ]) (* stem-no *) in do
     { compute_present5 5 sstem wstem vow root third pada padam
     ; if root = "v.r#1" then (* extra derivation *)
          let wstem = revcode "uur.nu" and sstem = revcode "uur.no" in 
          compute_present5 5 sstem wstem True root third pada padam
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
         if root = "t.rh" then revcode "t.rfh" (* guttural nasal f *)
         else [ c :: rev (sandhi stem [ nasal ]) ] (* nasalized stem *) 
       and sstem = 
         if root = "t.rh" then [ c :: rev (sandhi stem [ 36; 10 (* -ne *)]) ] 
         else [ c :: rev (sandhi stem [ 36; 1 ]) ] (* stem-na *) in 
       compute_present7 sstem wstem root third pada padam 
     | _ -> warning (roman root ^ " atypic 7\n")
     ]
   | 8 -> (* k.r1 k.san tan1 man san1 *)
     match rstem with 
     [ [ 36 (* n *) :: rest ] -> 
       let wstem = rev (sandhi rest [ 36; 5 ]) (* stem-nu *)
       and sstem = rev (sandhi rest [ 36; 12 ]) (* stem-no *) in
       compute_present5 8 sstem wstem True root third pada padam
     | [ 7; 17 ] (* k.r *) -> 
       let wstem = revcode "kuru"
       and short = revcode "kur" (* before suffix -m -y -v Macdonell§134E *)
       and sstem = revcode "karo" in
       compute_presentk sstem wstem short root third
     | _ -> warning (roman root ^ " atypic 8\n")
     ]
   | 9 -> let (stem,vow) = match root with (* vow = vowel ending root *)
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
        if root = "k.subh" then 36 (* n *) else 31 (* .n *)  else 36 (* n *) 
     and glue = if root = "k.subh" then List2.unstack else sandhi in
     let sstem = rev (glue stem [ 36; 2 ]) (* stem-naa *) (* naa accented *) 
     and wstem = rev (glue stem [ 36; 4 ]) (* stem-nii *) (* nii unaccented *)
     and short = [ retn :: stem ] (* stem-n *) in do
     { compute_present9 sstem wstem short vow stem root third pada padam
     ; if root = "grah" then (* ved alternative form "g.rbh" Vt1 \Pan{8,2,35} *)
         let stem = revcode "g.rbh" in 
         let sstem = rev (sandhi stem [ 36; 2 ]) (* stem-naa *) 
         and wstem = rev (sandhi stem [ 36; 4 ]) (* stem-nii *) 
         and short = [ 31 :: stem ] (* stem-.n *) in
         compute_present9 sstem wstem short vow stem root [] pada padam
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
value record_pfp root rstem = do
  { try pfp_ya rstem root with [ Not_attested -> () ]
  ; try pfp_aniiya rstem root with [ Not_attested -> () ]
  ; (* Supplements *)
    let record_extra_pfp_ya form = 
        record_part (Pfutp_ Primary (revcode form) root) in
    match root with
    [ "k.r#1" (* \Pan{3,1,120} .duk.r~n + kyap *)
    | "stu" | "bh.r" | "i" | "m.r" -> (* \Pan{3,1,109} Renou§155e *)
      (* intercalate t after roots ending in short vowel Renou§146 *)
      let pfp_tya = rfix rstem "tya" in (* k.rtya stutya bh.rtya itya m.rtya *)
      record_part (Pfutp_ Primary pfp_tya root)
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
    | "mad#1"  -> record_extra_pfp_ya "madya" (* maadya for pv- \Pan{3,1,100} *)
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
   arth ka.n.d kath kal kiirt kuts ga.n garh gup gha.t.t cint cur .damb 
   tandr tark tul bharts mantr m.r.d rac rah ruup var.n lok suud sp.rh *)
(* Also gave.s, because possible ga.na 1 and pp - should be added separately *)
(* Also lelaa, which has a strange status (marked as verb rather than root) *)
(* asu is bizarre, lexicalized under asuuya *)

(* The next two functions are used only by the Grammar interface, the forms
   memorized are computed from the lexicalized 3rd sg form *)  
(* BEWARE. the root forms given in the next two functions must be in normalized
   form - no non-genuine anusvaara 
   This should be replaced by the recording of the 3rd sg form, like others. *)
value den_stem_a root = (* in general transitive Whitney§1059c *)
   let rstem = revstem root in 
   match root with
   [ "putrakaama" | "rathakaama" (* \Pan{3,1,9} *)
   | "sukha" | "du.hkha" (* also "adhvara" "m.rga" below *)
   (* but "sukha" forms also sukhayati *)
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
   | "mantu" | "valgu" | "sakhi" | "samudra#1" 
   | "niila" | "manda" | "lohita" (* G{lohita} to become \Pan{3,1,13} kya.s *)
   | "asu" (* lexicalized under "asuuya" *) | "cira" 
       -> lengthen rstem (* lengthening -aayati *) 
   | "asuuya" (* "asu" lengthened *) | "gomaya" | "vyaya" (* euphony *)
       -> trunc (trunc rstem) 
   | (* "artha" |*) "veda" | "satya" (* \Pan{3,1,25} Vt. *)
       -> [ 1 :: [ 37 :: [ 2 :: trunc rstem ] ] ] (* -aapayati - interc p *) 
   (* |  (* very rare Whitney§1059d e.g. "putra" *)
       -> [ 3 :: trunc_a rstem ] (* -()iyati *) *)
   | "adhvara" | "tavi.sa" | "putra" | "praasaada" 
   | "mitra" (* treat as \Pan{3,1,10} *)
   | "udaka" | "kavi" | "dhana" | "maa.msa" | "vastra" (* desire Kale§643 *) 
       -> [ 4 :: trunc rstem ] (* -()iiyati *) (* \Pan{3,1,8} kyac *)
   | "kart.r" -> [ 4 :: [ 43 :: trunc rstem ] ] (* .r -> rii  Kale§642 *)
   | "go"     -> [ 45 :: [ 1 :: trunc rstem ] ] (* o -> av    Kale§642 *) 
   | "nau#1"  -> [ 45 :: [ 2 :: trunc rstem ] ] (* au -> aav  Kale§642 *)
   | "raajan" -> [ 4 :: trunc (trunc rstem) ]   (* nasal amui Kale§642 *)
     (* now the general case: keep the nominal stem - to cause (transitive) *)
   | "a.mza" | "afka" | "afkha" | "andha" | "aparok.sa" | "apahasta" | "amitra"
   | "aakar.na" | "aakula" | "aahvaana" | "aavila" | "i.sa" | "unmuula" 
   | "upahasta" | "ka.thora" | "kadartha" | "kar.na" | "kalafka" | "kalu.sa"
   | "kavala" | "kusuma" | "kha.da" | "garva" | "gocara" | "gopaa" 
   | "cuur.na" | "chala" | "chidra" | "tantra" | "tapas" | "tarafga" | "taru.na"
   | "tuhina" | "da.n.da" | "deva" | "dola" | "dravat" | "dhiira#1"
   | "nirmuula" | "nuutana" | "pa.tapa.taa" | "pallava"
   | "pavitra" | "paaza" | "pi.n.da" | "pulaka" | "puula" | "pratikuula" 
   | "prati.sedha" | "pradak.si.na" | "prasaada" | "bhi.saj" 
   | "malina" | "mizra" | "mukula" | "mukhara" | "mu.n.da" | "muutra" 
   | "m.rga" | "yantra" | "rasa" | "ruuk.sa" | "lagha" (* u -> a *)
   (*| "var.na" now varn:10 *) | "vizada" | "vra.na" | "zaanta" | "zithila"
   | "zyena" | ".sa.n.dha" | "sapi.n.da" | "saphala" | "sabhaaja" | "saantva" 
   | "saavadhaana" | "suutra" | "stena" (* practice \Pan{3,1,15} *)
   | "u.sas" | "namas" | "varivas" (* do \Pan{3,1,19} *)
   | "utpuccha" (* do \Pan{3,1,20} *)
   | "vrata" | "zlak.s.na" (* make \Pan{3,1,21} *)
   | "lava.na" (* desire Kale§645 \Pan{3,1,21} *)
   | "udan" (* Kale§645 *)
   | "hala" (* take \Pan{3,1,21} *)
   | "kelaa" | "rekhaa" | "tiras" | "uras" | "payas" (* Kale§660 *)
   | "vaac" (* consonant Kale§642 *)
   | "dantura" (* possess *)
   | "k.r.s.na" (* act as *)
   | "vikaca#2" (* become *)
   | "viira" | "zabda" | "tira" (* MW *) | "ma~njara" | "sraja" | "manas" 
       -> rstem (* -yati *) (* standard causative meaning *)  
   | "putras" | "lava.nas" -> rstem (* trick for redundancy *) 
   | "madhu" | "v.r.sa" (* also madhvasyati v.r.siiyati *) 
   | "k.siira" 
       -> [ 48 :: rstem ] (* -syati *) (* wish for \Pan{7,1,51} Kale§643 *)
   | _ -> failwith ("Unknown denominative " ^ root)
   ] 
;
value den_stem_m root = (* in general intransitive or reflexive Whitney§1059c *)
   let rstem = revstem root in 
   match root with 
   [ "i.sa" | "utpuccha" | "kuha" | "manas" | "muutra" 
     (* "artha" | "mantra" now ga.na 10 arth mantr *)
   | "m.rga" | "viira" | "safgraama" | "suutra" (* also zithila below *)
       -> rstem (* (a)-yate *) 
   | "asuuya" (* "asu" lengthened *) | "vyaya" (* euphony *)
       -> trunc (trunc rstem) 
   | "tavi.sa" | "citra" (* do \Pan{3,1,19} *) | "sajja"
       -> [ 4 :: trunc_a rstem ] (* -()iiyate *)
(* | "arth" -> [ 1 :: rstem ] - arthayate for lexicon access - now ga.na 10 *)
   | "apsaras" | "sumanas" (* act as, become \Pan{3,1,11-12} *) 
   | "unmanas" 
   | "uu.sman" (* emit \Pan{3,1,16} *)
   | "raajan" (* play the role of *)
       -> lengthen (trunc rstem) (* final consonant dropped *)
     (* now the general case: lengthen the nominal vowel stem *)
   | "pa.tapa.taa" | "mahii#2" | "m.r.saa" 
   | "laalaa" | "svalpazilaa" | "vimanaa" 
   | "ajira" | "kalu.sa" | "k.rpa.na" | "kliiba" | "garva" | "jala" | "jihma"
   | "taru.na" | "nika.sa" | "parok.sa" | "piiyuu.savar.sa" | "pu.spa" | "priya"
   | "bh.rza" | "maalyagu.na" | "zalabha" | "zithila" | "ziighra" 
   | "zyaama" | "zyena" | "safka.ta"
   | "ka.n.du" | "karu.na" | "sukha" | "du.hkha" (* feel \Pan{3,1,18} *)
(* G{sukhaadi} take suffix kyaf in -aayate :
   {sukha,du.hkha,t.rpta,k.rcchra,asra,aasra,aliika,pratiipa,karu.na,so.dha} *)
   | "t.rpta" (* -MW *)
   | "abhra" | "ka.nva" | "kalaha" | "k.sepa" | "megha" | "vaira" | "zabda" 
   | "z.rfga" (* do \Pan{3,1,17} *)
   | "durdina" | "sudina" | "niihaara" (* id. vaartika *)
   | "ka.s.ta" | "k.rcchra" (* strive to \Pan{3,1,14} *)
   | "romantha" (* practice \Pan{3,1,15} *)
   | "dhuuma" | "baa.spa" | "phala" | "phena" (* emit \Pan{3,1,16} *)
   | "kurafga" | "pu.skara" | "yuga" | "vi.sa" | "zizu"  | "samudra#1" 
   | "gomaya" | "bh.rtya" | "sa.mdhyaa"  (* resemble *)
   | "puru.sa" (* imitate *)
   | "k.r.s.na" | "bhuusvarga" (* to become *) | "manda" | "niila" | "lohita"
   | "harita" | "piita" (* G{lohita} to become \Pan{3,1,13} *)
       -> lengthen rstem (* -aayate *)
   | _ -> failwith ("Unknown denominative " ^ root)
   ] 
;
value compute_denom stem ystem root = do (* other than present system - rare *)
  { build_perpft Primary ystem root 
  ; let fsuf = revcode "i.sy" in (* rare - similar to [compute_future_10] *)
    compute_future (fsuf @ ystem) root 
  ; let perstem = [ 3 :: ystem ] (* -yi *) in  
    perif Primary perstem root 
  ; match stem with
    [ [ 1 :: rest ] -> 
        match root with
        [ "asuuya" -> () (* wrong asya *)
        | "m.rga" -> () (* from m.rg *)
        | "raajan" -> () (* from raaj2 *)
        | "medhaa" -> () (* overgenerates with medhya *)
        | _ -> do (* experimental - rare acc. to Whitney *)
               { compute_passive_11 root rest
               ; record_pfp_10 root rest
               }
        ]
    | _ -> () (* specially wrong for consonant stems *)
    ]
  }
;
value compute_denominative_a root third = 
  match Word.mirror third with
      [ [ 3 :: [ 32 :: [ 1 :: ([ 42 :: s ] as ystem) ] ] ] (* -yati *) -> do
            { compute_thematic_active 11 Primary ystem root third
            ; compute_denom s ystem root 
            ; record_ppp_abs_den ystem root
            }
      | _ -> failwith ("Anomalous denominative " ^ Canon.decode third)
      ]
and compute_denominative_m root third = 
  match Word.mirror third with
      [ [ 10 :: [ 32 :: [ 1 :: ([ 42 :: s ] as ystem) ] ] ] (* -yate *) -> do
            { compute_thematic_middle 11 Primary ystem root third
            ; compute_denom s ystem root
            ; record_ppp_abs_den ystem root
            }
      | _ -> failwith ("Anomalous denominative " ^ Canon.decode third)
      ]
;
(* We use the lexicalized third stem *)
value compute_denominative root pada third = 
  match third with
  [ [] (* fake *) -> do (* pada not informative, we try both *)
     { try let stem = den_stem_a root in 
           let ystem = [ 42 :: stem ] in do
           { compute_thematic_active 11 Primary ystem root third
           ; compute_denom stem ystem root
           ; record_ppp_abs_den ystem root
           }
       with [ Failure _ -> () ]
     ; try let stem = den_stem_m root in 
           let ystem = [ 42 :: stem ] in do
           { compute_thematic_middle 11 Primary ystem root third
           ; compute_denom stem ystem root
           ; record_ppp_abs_den ystem root
           }
       with [ Failure _ -> () ]
     }
  | _ -> if pada then (* Para *) compute_denominative_a root third  
                 else (* Atma *) compute_denominative_m root third  
  ]
;
value compute_other_systems root rstem = do
   { (* Future and Conditional *) 
     match root with
     [ "as#1" (* bhuu *) | "ah" | "ifg" | "paz" (* d.rz *)| "cint" (* cit *) 
     | "bruu" (* vac *) | "kan" | "k.saa" | "cud" | "chur" | "dhii#1" | "pat#2"
     | "pii" | "praa#1" | "vidh#1" | "zlath" | "spaz#1" -> () (* no future *)
     | "tud#1" | "cakaas" -> () (* only periphrastic *)
     | "bharts" -> compute_future_gen rstem root (* exception gana 10 *)
     | "umbh" -> do { compute_future_gen (revcode "ubh") root (* 2 forms *)
                    ; compute_future_gen rstem root
                    }
     | "saa#1" -> do { compute_future_gen (revcode "si") root
                     ; compute_future_gen rstem root
                     }
     | "vyadh" -> compute_future_gen (revcode "vidh") root 
     | "zuu" -> compute_future_gen (revcode "zve") root 
     | ".s.thiiv" -> compute_future_gen (revcode ".s.thiv") root 
     | "knuu" -> compute_future_gen (revcode "knuuy") root 
     | _ -> compute_future_gen rstem root 
     ]
   ; (* Periphrastic future, Infinitive, Passive future part. in -tavya *)
     match root with
     [ "ah" | "ifg" | "kan" | "paz" (* for d.rz *) | "bruu" (* for vac *) 
     | "k.saa" | "cud" | "dhii#1" | "pat#2" | "pii" | "praa#1" | "vidh#1"
     | "spaz#1" | "haa#2" -> () (* no perif *)
     | "saa#1" -> do { compute_perif (revcode "si") root 
                     ; compute_perif rstem root
                     }
     | "vyadh" -> compute_perif (revcode "vidh") root 
     | "zuu"   -> compute_perif (revcode "zve") root 
     | ".s.thiiv" -> compute_perif (revcode ".s.thiv") root 
     | "knuu"   -> compute_perif (revcode "knuuy") root 
     | "stambh" -> compute_perif (revcode "stabh") root 
     | _ -> compute_perif rstem root 
     ]
   ; (* Precative/Benedictive active rare, middle very rare in classical Skt *)
      match root with 
      [ "as#1" | "ah" -> () (* uses bhuu1 bruu *) (* but Zriivara: staat *)
      | "kan" | "k.r#2" | ".s.thiiv" -> () (* unattested - to be added *)
      | _ -> compute_benedictive rstem root 
      ]
   ; (* Passive *)
     if admits_passive root then 
        let ps_stem = passive_stem root rstem in do
        { if root = "arh" || root = "k.lp" then () (* admits pfp but no ps *)
          else compute_passive Primary root ps_stem 
          (* Passive future participle (gerundive) in -ya and -aniiya *)
        ; record_pfp root rstem 
        }
     else ()
   ; (* Ppp computation and recording (together with absolutives) *)
     if admits_ppp_abs root then do 
        { let ppstems = compute_ppp_stems root rstem in 
          record_ppp_abs_stems root rstem ppstems
        ; record_abso_am root (* rare .namul *)
        }
     else ()
   ; (* Perfect *) 
     match root with
     [ "paz"  (* d.rz *) | "bruu" (* vac *) | "ma.mh" (* mah *) | "ind" 
     | "indh" | "inv" | "k.r#2" | "k.saa" | "cakaas" | "dhii#1" | "vidh#1" 
        -> () (* no perfect *)
     | "uuh" -> () (* periphrastic *)
     | _ -> compute_perfect root
     ] (* NB perfect forms may have a passive meaning *)
   ; (* Periphrastic Perfect *) (* on demand - except gana 10 above *)
     try let stem = peri_perf_stem root in
         build_perpft Primary stem root
     with [ Not_attested -> () ]
   ; (* Aorist *) compute_aorist root
   ; (* Injunctive *) compute_injunctive root
   }
;
(***************************)
(* Main conjugation engine *)
(***************************)
(* [compute_conjugs_stems : string -> Conj_infos.vmorph -> unit]           *)
(* Called by [compute_conjugs] and [fake_compute_conjugs] below            *)
(*        and [Conjugation.secondary_conjugs]                              *)
value compute_conjugs_stems root (vmorph,aa) = do (* main *)
  { admits_aa.val := aa (* sets the flag for phantom forms for aa- preverb *)
  ; match vmorph with
 [ (* 1. Denominatives *)
   Conj_infos.Prim 11 pada third -> 
      (* note: pada of denominative verbs is lexicalized *)
      compute_denominative root pada third
   (* 2. Roots of gana 10 *)
 | Conj_infos.Prim 10 pada third -> 
   (* root in gana 10, pada is True for Para, False for Atma of third form *)
   let rstem = revstem root in (* root stem reversed *)  
   try do
   { (* Present system plus perif pft and future, infinitives and pfp-tavya *)
     compute_present_system root rstem 10 pada third 
     (* missing: imperative in -taat Whitney§570-1 (post-vedic rare) *)
     (* Future and Conditional *) 
   ; compute_future_10 rstem root 
     (* Passive *)
   ; let ps_stem = passive_stem root rstem in 
     compute_passive_10 root (strong ps_stem) 
   ; record_pfp_10 root rstem  
     (* Ppp and Absolutives *)
   ; let ystem = rfix rstem "ay" 
     and ppstem = rfix rstem "ita" in do  
     { record_part_ppp ppstem root 
     ; record_abso_tvaa (fix ystem "itvaa") root
     ; let ya_stem = if light_10 rstem then ystem else rstem in
       record_abso_ya (fix ya_stem "ya") root (* lyap should follow [record_pppca] *)
     }
     (* No Perfect -- periphrastic perfect generated by process10 above *)
   ; (* Aorist *) compute_aorist root
   ; (* Injunctive *) compute_injunctive root
   }
   with [ Control.Warning s -> output_string stdout (s ^ "\n") ]
   (* 3. Roots of gana <10 *)
 | Conj_infos.Prim gana pada third -> 
   (* gana is root class, pada is True for Para, False for Atma of third form *)
   (* Primary conjugation *)
   let rstem = revstem root in (* root stem reversed *)  
   try do
   { if gana=0 then () (* root with no present system *)
     else compute_present_system root rstem gana pada third (* Present system *)
   ; compute_other_systems root rstem 
   }
   with [ Control.Warning s -> output_string stdout (s ^ "\n") ]
   (* 4. Causatives  *)
 | Conj_infos.Causa third -> 
     (* Here we extract the causative stem from the third given in Dico *)
     (* rather than implementing all special cases of Whitney§1042.     *)
     (* Alternative: compute cstem instead of reading it from the lexicon.    
        See Panini krit{.ni} \Pan{7,3,36-43}                            *)
     let (cstem,active) = match Word.mirror third with
         [ [ 3 :: [ 32 :: [ 1 :: st ] ] ]  (* remove -ati *)
             -> (st,True)
         | [ 10 :: [ 32 :: [ 1 :: st ] ] ] (* remove -ate *)
             -> (st,False)
           (* We lose some information, but generate both active and middle *)
         | _ -> failwith ("Weird causative " ^ Canon.decode third)
         ] in
     let cpstem = match cstem with (* tentative, should be checked *)
         [ [ 42 :: [ 1 :: st ] ] (* -ay *) -> match root with
            [ "dhvan" -> revcode "dhvaan"
            | _ -> st 
            (* doubt: ambiguity in ps when the ca stem is not lengthened       *)
            (* eg gamyate. Whitney§1052a says "causatively strengthened stem"? *)
            ]
         (* Why no ca in -aayati while such forms exist for ga.na 10 and 11 ?  *)
         | _ -> failwith ("Anomalous causative " ^ Canon.decode third)
         ] in
     let compute_causative stem = do (* both active and middle are generated *)
         { compute_causativea stem root (if active then third else [])
         ; compute_causativem stem root (if active then [] else third)
         } in 
     do (* active, middle, passive present; active middle future, aor *)
     { compute_causative cstem
     ; compute_passive Causative root cpstem (* adapt [compute_passive_10]? *)
     ; let fsuf = revcode "i.sy" in
       let fustem = fsuf @ cstem in 
       compute_future_ca fustem root 
     ; compute_aor_ca cpstem root (* Whitney§861b Henry§339 *)
     ; (* Passive future participle in -ya *)
       match root with
       [ "gad" | "yam" | "has" -> () (* to avoid redundancy with Primary pfp *)
       (* zi.s : justified redundancy with Primary pfp *)
       (* car :  redundancy with Primary pfp to be justified *)
       | _ -> record_pfp_ya Causative cpstem root 
       ]
     ; (* Passive future participle in -aniiya *)
       record_pfp_aniiya Causative cpstem root
       (* Passive past participle and absolutives *)
     ; record_pppca cpstem cstem root
       (* Periphrastic future, Infinitive, Gerundive/pfp in -tavya *)
     ; let icstem = [ 3 :: stem ] where stem = 
          match root with [ "smi" -> cpstem (* -i Z tentative *) 
                          | _ -> cstem (* -ayi *) 
                          ] in
       perif Causative icstem root 
       (* Periphrastic perfect Whitney§1045 *)
     ; build_perpft Causative cstem root (* gamayaa.mcakaara *)
     } 
   (* 5. Intensives  *)
 | Conj_infos.Inten third -> (* TODO passive, perfect, future, aorist, parts *) 
     match Word.mirror third with (* active or middle are generated on demand *)
     (* paras. in -ati, -iiti, -arti (k.r2), -aati (draa1, yaj1), -etti (vid1) *)
     [ [ 3 :: [ 32 :: [ 4 :: ([ 45 :: [ 1 :: w ] ] as wk) ] ] ] (* x-aviiti *) ->
         let st = [ 12 :: w ] in
         (* x-o eg for hu johavitti -> joho -> johomi johavaani *)
         compute_intensivea wk st root third
     | [ 3 :: [ 32 :: [ 4 :: wk ] ] ] (* other -iiti *) ->
         let st = strong wk in
         compute_intensivea wk st root third
     | [ 3 :: [ 32 :: st ] ] (* ti *) 
     | [ 3 :: [ 27 :: st ] ] (* .ti eg veve.s.ti *) ->
         let wk = st in (* TEMP - no easy way to get weak stem from strong one *)
                        (* eg vevid from vevetti=veved+ti nenij from nenekti *)
         compute_intensivea wk st root third 
     | [ 10 :: [ 32 :: [ 1 :: st ] ] ] -> (* -ate *) 
         compute_intensivem st root third
     | [ 10 :: [ 32 :: st ] ] -> (* -te : nenikte *) 
         compute_intensivem2 st root third
     | _ -> failwith ("Weird intensive " ^ Canon.decode third)
     ] 
   (* 6. Desideratives  *)
 | Conj_infos.Desid third -> (* TODO passive, future, aorist, more parts *)
     let compute_krid st = do (* ppp pfp inf *)
         { record_pppdes st root
         ; record_pfp_aniiya Desiderative st root 
         ; record_pfp_ya Desiderative st root 
(*i      ; record_des_aa Desiderative st root (* Des k.rdantas lexicalized *) 
         ; record_des_u Desiderative st root i*)
         ; perif Desiderative [ 3 :: st ] root 
         } in
     match Word.mirror third with (* active or middle are generated on demand *)
       [ [ 3 :: [ 32 :: [ 1 :: st ] ] ] -> do 
           { compute_desiderativea st root third
           ; compute_passive Desiderative root st 
           ; compute_futurea Desiderative [ 42 :: st ] root 
           ; compute_perfect_desida st root 
           ; compute_krid st 
           }
       | [ 10 :: [ 32 :: [ 1 :: st ] ] ] -> do 
           { compute_desiderativem st root third
           ; compute_passive Desiderative root st 
           ; compute_futurem Desiderative [ 42 :: st ] root 
           ; compute_perfect_desidm st root 
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

(* Various Vedic subjunctives needed for citations Whitney§557-562 *)
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
  ; subj_sg "i" Third "ayati" (* primary endings *) 
  ; subj_sg "i" Third "ayat" (* secondary endings *) 
  ; subj_sg "bhuu#1" Third "bhavaat" (* Varenne§129 *)
  ; subj_sg "su#2" Third "sunavat" (* Varenne§129 *)
  ; subj_sg "pat#1" Third "pataati"
  ; subj_pl "gam" Third "gman" (* for apigman *) 
  ; subj_cau_sg "jan" Second "janayaas"  
  ; subj_cau_sg "cud" Third "codayaat" (* Gaayatrii pracodayaat *)
  ; subj_int_sg "vi.s#1" Third "vevi.sati"
(*; [subj_sg] "k.r#1" First "karavaa.ni" (* became imp Whitney§578 *) *)
  ; subjm_sg3 "aas#2" "aasate" 
  ; subjm_sg3 "aas#2" "aasaatai"
  }
;

value compute_auxi_kridantas () = 
  let stems str = let st = revstem str in match st with
      [ [ 1 :: rst ] -> (rst,Word.mirror st) 
      | _ -> failwith "auxi_kridantas" 
      ] in do (* A few auxiliary action nouns are generative for cvi compounds *)
  (* Problem: this induces ambiguities with lexicalized kara.na etc. which are 
     not treated as participles and should be placed in a different phase, not
     attainable from preverbs, inducing a patch in [Dispatcher.validate_pv_k] *)
  { let (rst,st) = stems "kara.na" in 
    build_part_a_n (Primary,Action_noun) rst st "k.r#1" 
  ; let (rst,st) = stems "kaara" in 
    build_part_a_m (Primary,Agent_noun) rst st "k.r#1" (* also n. f. in -ii? *)
  ; let (rst,st) = stems "bhaavana" in
    build_part_a_n (Primary,Action_noun) rst st "bhuu#1" (* also Agent mnf ? *)
  ; let (rst,st) = stems "bhaava" in 
    build_part_a_m (Primary,Action_noun) rst st "bhuu#1"
  ; let (rst,st) = stems "bhuuya" in 
    build_part_a_n (Primary,Action_noun) rst st "bhuu#1"
  }
;
(* Called by [Make_roots.roots_to_conjugs] *)
value compute_conjugs root_word (infos : Conj_infos.root_infos) = 
  let root = Canon.decode root_word in compute_conjugs_stems root infos
;
(* Supplementary forms *)
value compute_extra_rc () = (* vedic - \Pan{7,1,38} *)
  enter1 ".rc#1" (Absotvaa Primary (code "arcya")) (* abs -ya with no preverb *)
and compute_extra_kan () = do (* Lanmann "can" *)
  { enter1 "kan" (Conju (aora 5) [ (Singular,[ (Third, code "acaniit") ]) ])
  ; record_part (Pprm_ 4 Primary (revcode "kaayamaan") "kan") 
  }
and compute_extra_kri () = (* Atharva Veda *)
  enter1 "k.r#1" (Conju (impera 2) [ (Singular,[ (Second, code "k.rdhi") ]) ])
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
  let root = "j~naa#1" in (* j~napta vet \Pan{7,2,27} *)
  let cstem = revcode "j~nap" in 
  let ppstem = [ 1 :: [ 32 :: cstem ] ] (* j~napta *) in do 
  { record_part (Ppp_ Causative ppstem root)
  ; record_part (Pppa_ Causative ppstem root) (* pp-vat *)
  ; perif Causative cstem root 
  }
and compute_extra_trr () = do 
      { build_infinitive Primary (revcode "tarii") "t.rr" (* id. *)
      ; build_infinitive Primary (revcode "tar") "t.rr" (* Whitney roots *)
      ; enter1 "t.rr" (Conju perfa (* archaic forms Whitney§794k *)
             [ (Plural,[ (Third, code "terus") ])
             ; (Plural,[ (Second, code "tera") ])
             ; (Plural,[ (First, code "terima") ])
             ; (Dual,[ (Third, code "teratus") ])
             ; (Dual,[ (Second, code "terathus") ])
             ; (Dual,[ (First, code "teriva") ])
             ])
      }
and compute_extra_dri () = do 
  { compute_passive_raw "d.r#1" (* aadriyate *)
  ; record_pfp "d.r#1" (revcode "d.r")
  ; record_abso_ya (code "d.rtya") "d.r#1" (* aad.rtya *)
  ; record_part_ppp (revstem "d.rta") "d.r#1"  (* aad.rta *)
  }
and compute_extra_dham () = let stem = revcode "dhmaa" in do
    { compute_future_gen stem "dham" (* Bucknell Pan{7,3,78} *)
    ; compute_passive Primary "dham" (revstem "dham") (* WR *)
    }
and compute_extra_dhmaa () = let stem = revcode "dham" in do 
    { compute_future_gen stem "dhmaa" (* Id *)
    ; compute_passive Primary "dhmaa" (revstem "dham")
    }
and compute_extra_dhaa () = do 
    { (* Gaayatrii dhiimahi precative m. Whitney§837b *)
      enter1 "dhaa#1" (Conju benem [ (Plural,[ (First, code "dhiimahi") ]) ])
(* [; record_part (Ppp_ Primary (revcode "dhita") "dhaa#1") (* alter hita *)] *)
    } (* also "vidmahi" on yantra ? *)
and compute_extra_nind () = (* WR: RV *)
  enter1 "nand" (Conju perfa [ (Plural,[ (Third, code "ninidur") ])
                             ; (Plural,[ (First, code "nindimas") ]) 
                             ])
and compute_extra_pat () = (* WR: RV Henry: paptur véd. Varenne§39 *)
  enter1 "pat#1" (Conju perfa [ (Plural,[ (Third, code "paptur") ])
                              ; (Plural,[ (First, code "paptima") ])
                              ])
and compute_extra_prr () = (* paaryate as well as puuryate above *) 
    let stem = revcode "paar" in compute_passive Primary "p.rr" stem
and compute_extra_bhaas () = do 
    { enter1 "bhaa.s" (Invar (Primary,Infi) (code "bhaa.s.tum")) (* WR epic *)
    ; enter1 "bhaa.s" (Conju perfm [ (Plural,[ (Third, code "bhaa.sire") ]) ]) 
      (* Mah{18,2,40} ava-bhaa.sire haplology *)
    }
and compute_extra_bhuj2 () = 
    enter1 "bhuj#2" (Conju (Primary,voa 7) (* epics Wh{688a} *) 
                           [ (Singular,[ (First, code "bhu~njiiyaam") ])
                           ; (Singular,[ (Second, code "bhu~njiiyaas") ])
                           ; (Singular,[ (Third, code "bhu~njiiyaat") ])
                           ])
and compute_extra_bhr () = (* Epics sa.mbhriyantu Oberlies 8.7 *) 
   enter1 "bh.r" (Conju (Primary,vmp) [ (Plural,[ (Third, code "bhriyantu") ]) ])
and compute_extra_bhram () = (* MW: Mah *)
  enter1 "bhram" (Conju perfa [ (Plural,[ (Third, code "bhremur") ]) ])
and compute_extra_muc () = do 
  { (* ved precative `fasse que je sois libéré' *)
    enter1 "muc#1" (Conju benem [ (Singular,[ (First, code "muk.siiya") ]) ])
  ; build_infinitive Causative (revcode "moci") "muc#1"    (* Whitney§1051c *)
  }
and compute_extra_yu2 () = (* Maitreya Sa.mhita MS{1,1.11} *)
  enter1 "yu#2" (Conju (imperm 3) [ (Plural,[ (Second, code "yuyudhvam") ]) ])
and compute_extra_zaas () = 
   let e = "zaas" in do (* epics zaasyate + Renou gram §29 *) 
     { let stem = revcode e in compute_passive Primary e stem 
     ; enter1 e (Conju (Primary,via 2) [ (Singular,[ (Second, code "azaat") ]) ])
     }
and compute_extra_zru () = (*i was zrudhi but Whitney§594a zrudhii i*)
  enter1 "zru" (* ved écoute *) (* Whitney§704 z.r.nuhi z.r.nudhi *)
         (Conju (impera 5) [ (Singular,[ (Second, code "z.rnuhi") ]) ])
and compute_extra_sad () = (* WR E. Mah(1.214.027c) (Gretil) sa.mni.siidatu.h *)
  enter1 "sad#1" (Conju (Primary,Conjug Perfect Active) 
                        [ (Dual,[ (Third, code "siidatus") ]) ])
and compute_extra_sanj () = (* WR Oberlies p LI but maybe prm of variant sajj *)
  let root = "sa~nj" 
  and conj = Primary
  and pastem = revcode "sajj" (* "y" replaced by j in passive *) in 
  compute_passive_system conj root pastem 
and compute_extra_suu () = (* BhG{3,10} *)
  enter1 "suu#1" (Conju benem [ (Plural,[ (Second, code "savi.syadhvam") ]) ])
and compute_extra_skand () = do (* WR *)  
  { enter1 "skand" (Invar (Primary,Infi) (code "skanditum")) 
  ; record_abso_ya (code "skadya") "skand"
  }
and compute_extra_smi () = do (* WR *)
  { record_abso_tvaa (code "smayitvaa") "smi" 
  ; record_abso_ya (code "smayitya") "smi" (* ? *)
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
  { record_part_ppp (revstem "zaata") "zaa" 
  ; record_part_ppp (revstem "k.sita") "k.sii" 
  ; record_part_ppp (revstem "diipita") "diip" 
  ; record_part_ppp (revstem "cintaaratnaayita") "cintaaratna"
  ; record_part (Ppra_ 1 Primary (revstem ".dam") (revstem ".damat") ".dam")
  ; record_part (Ppra_ 1 Intensive (revstem "jaajam") (revstem "jaajamat") "jam")
  ; record_part (Pprm_ 1 Intensive (revstem "cekitaan") "cit#1")
  }
;
(* For verbs without present forms and variants, *)
(* called by [Make_roots.roots_to_conjugs] at generation time *)
value compute_extra () = do (* Extra forms for specific roots *)
  { compute_extra_rc () 
  ; compute_extra_kan ()
  ; compute_extra_kri () 
  ; compute_extra_khan ()
  ; compute_extra_car () 
  ; compute_extra_jnaa () 
  ; compute_extra_trr () 
  ; compute_extra_dri () 
  ; compute_extra_dham () 
  ; compute_extra_dhaa () 
  ; compute_extra_nind () 
  ; compute_extra_pat () 
  ; compute_extra_prr () 
  ; compute_extra_bhaas () 
  ; compute_extra_bhuj2 ()
  ; compute_extra_bhr ()
  ; compute_extra_bhram ()
  ; compute_extra_muc () 
  ; compute_extra_yu2 ()
  ; compute_extra_zaas () 
  ; compute_extra_zru () 
  ; compute_extra_sanj ()
  ; compute_extra_sad ()
  ; compute_extra_suu ()
  ; compute_extra_skand () 
  ; compute_extra_smi ()
  ; compute_extra_syand ()
  ; compute_extra_hims ()
  ; compute_extra_huu ()
  ; build_infinitive Primary (revcode "rami") "ram"
  ; build_infinitive Primary (revcode "aas") "aas#2"       (* Whitney§968d *)
  ; build_infinitive Causative (revcode "bhaavi") "bhuu#1" (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "dhaari") "dh.r"   (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "ze.si") "zi.s"    (* Whitney§1051c *)
  ; build_infinitive Causative (revcode "j~naap") "j~naa#1" (* WR epics *)
    (* Infinitives in -as krit{kasun} \Pan{3,4,17} *)
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
value fake_compute_conjugs (gana : int) (root : string) = do
  { morpho_gen.val := False (* Do not generate phantom forms *) 
  ; let no_third = [] and pada = True in (* hacks to disable check warning *)
    let vmorph = Conj_infos.Prim gana pada no_third in do
    { compute_conjugs_stems root (vmorph,False) (* False since no-op in fake *)
    ; match root with (* extra forms - to be completed from [compute_extra] *)
      [ ".rc#1"  -> compute_extra_rc ()
      | "kan"    -> compute_extra_kan ()
      | "k.r#1"  -> compute_extra_kri () 
      | "k.sii"  -> record_part_ppp (revcode "k.sita") root
      | "khan"   -> compute_extra_khan ()
      | "car"    -> compute_extra_car ()
      | "j~naa#1"-> compute_extra_jnaa () 
      | "t.rr"   -> compute_extra_trr () 
      | "d.r#1"  -> compute_extra_dri () 
      | "dham"   -> compute_extra_dham () 
      | "dhmaa"  -> compute_extra_dhmaa () 
      | "dhaa#1" -> compute_extra_dhaa () 
      | "nind"   -> compute_extra_nind ()
      | "pat#1"  -> compute_extra_pat ()
      | "p.rr"   -> compute_extra_prr ()
      | "bhaa.s" -> compute_extra_bhaas ()
      | "bhuj#2" -> compute_extra_bhuj2 ()
      | "bh.r"   -> compute_extra_bhr ()
      | "bhram"  -> compute_extra_bhram ()
      | "muc#1"  -> compute_extra_muc ()
      | "yu#2"   -> compute_extra_yu2 ()
      | "zaa"    -> record_part_ppp (revcode "zaata") root
      | "zaas"   -> compute_extra_zaas ()
      | "zru"    -> compute_extra_zru () 
      | "sa~nj"  -> compute_extra_sanj () 
      | "sad#1"  -> compute_extra_sad ()
      | "suu#1"  -> compute_extra_suu ()
      | "smi"    -> compute_extra_smi ()
      | "syand"  -> compute_extra_syand ()
      | "hi.ms"  -> compute_extra_hims ()
      | "huu"    -> compute_extra_huu ()
      | _ -> ()
      ]
    }
  }
;

(*i end; i*)
