(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit morphology interface *)

type deictic = [ Speaker | Listener | Self | Numeral ]
;
(* Deictics have their gender determined from the context for pronouns 
of 1st and 2nd person, or the reflexive pronoun "aatman", or numerals over 4 *)
type gender = [ Mas | Neu | Fem | Deictic of deictic ]
and genders = list gender 
;
type number = [ Singular | Dual | Plural ] 
;
type case = [ Nom (* nominatif *)
            | Acc (* accusatif *)
            | Ins (* instrumental *) (* comitatif (Henry) *)
            | Dat (* datif *)
            | Abl (* ablatif *)
            | Gen (* génitif *)
            | Loc (* locatif *)
            | Voc (* vocatif *)
            ]
;
(* The verb system *)
type gana = int (* present class: 1 to 10, plus 11 for denominatives *)
and aor_class = int (* aorist class: 1 to 7 *)
;
type person = [ First | Second | Third ] (* Indian Third, Second and First *)
;
type conjugation = [ Primary | Causative | Desiderative | Intensive ]
(*i NB fake gana by convention :  12           13            14  (unused) i*)
;
type finite = (conjugation * paradigm) (* finite forms of verbs *)
and paradigm = 
  [ Presenta of gana and pr_mode (* parasmaipade *)
  | Presentm of gana and pr_mode (* aatmanepade *)
  | Presentp of pr_mode (* passive of present system *)
  | Conjug of tense and voice (* other tenses/aspects *)
  | Perfut of voice (* periphrastic futur (lu.t) *)
  ]
and voice = [ Active | Middle | Passive ] (* diathesis (pada: Para Atma Ubha) *)
and pr_mode = 
  [ Present (* (la.t) *)
  | Imperfect (* Preterit (laf) *)
  | Imperative (* (lo.t) *)
  | Optative (* Potential (lif) *) 
  ]
and tense = 
  [ Future (* (l.r.t) *)
  | Perfect (* Remote past - resultative aspect (li.t) *)
  | Aorist of aor_class (* Immediate past or future - perfective aspect (luf) *)
  | Injunctive of aor_class (* (le.t) - also Prohibitive with maa *) 
  | Benedictive (* Precative: optative aorist (aazirlif) *) 
  | Conditional (* Preterit of future (l.rf) *)
  ]
;
(* NB from Indo-European: the present stem has the imperfective aspect,
   the aorist one the perfective aspect, and the perfect one the resultative. *)
(* Vedic Subjunctive and Pluperfect are not yet taken into account. The only
   non-present passive forms are some passive aorist forms in 3rd sg. *)

(* Verbal adjectives *)
type kritya = int (* shades of intention of passive future/potential participle: 
   1 -ya (obligation, necessity or possibility, potentiality) (yat kyap .nyat)
   2 -aniiya (fitness, desirability, effectivity) (aniiyar)
   3 -tavya (necessity, unavoidability) (tavyat) *)
;
type verbal = (conjugation * participle)  
and participle = (* participles *)
(* These are the kridanta stems (primary verbal derivatives) with participial 
   value. They act as adjectives or gendered nouns. But [Ppra] does not qualify 
   as a noun, but as an adverb, signifying simultaneous action. *)
  [ Ppp          (* passive past participle *)
  | Pppa         (* active past participle *) 
  | Ppra of gana (* active present participle *)
  | Pprm of gana (* middle present participle *)
  | Pprp  (* passive present participle *)
  | Ppfta (* active perfect participle *) 
  | Ppftm (* middle perfect participle *) (* no passive *)
  | Pfuta (* active future participle *)
  | Pfutm (* middle future participle *)
  | Pfutp of kritya (* passive future/potential participle - 3 forms *)
    (* Pfutp is called gerundive in old grammars *)
  | Action_noun (* generative only for auxiliaries, for cvi compounds *)
  (* Other krit stems are not generative, but are assumed lexicalized *)
  ]
;
(* Invariable verbal forms.
   Such forms are indeclinable and have their own inflected forms constructors.
   Infinitives are similar to dative substantival forms, periphrastic perfect
   forms are associated with an auxiliary verb in the perfect.
   Absolutives split into root absolutives in -tvaa and absolutives in -ya 
   that must be prefixed with a preverb. *)
type modal = (conjugation * invar) 
and invar =
  [ Infi    (* infinitive (tumun) *)
  | Absoya  (* absolutive (gerund, invariable participle) (lyap) *) 
  | Perpft  (* periphrastic perfect (li.t) *)
  ]
;
type sadhana = (* karaka, action or absolutive - coarser than krit *)
  [ Agent
  | Action
  | Object
  | Instr
  | Loca
  | Absolu
  ]
;
(* Primary nominal formations *)
type nominal = (conjugation * krit)  
and krit = (* coarser than Paninian krit suffixes *) 
  [ Agent_aka (* .nvul \Pan{3,1,133} \Pan{3,3,108-109} -aka -ikaa v.rddhi 
                 .svun \Pan{3,1,145} trade gu.na f. -akii  
                 vu~n \Pan{3,1,146-147} *)
  | Agent_in  (* .nini \Pan{3,1,134} \Pan{3,2,78-86}   -in  -inii v.rddhi 
                 ghinu.n \Pan{3,2,141-145} 
                 ini \Pan{3,2,93} ifc. -vikrayin past *)
  | Agent_tri (* t.rc \Pan{3,1,133} t.rn \Pan{3,2,135} -t.r gu.na  *)
  | Agent_ana (* lyu \Pan{3,1,134} yuc \Pan{3,2,148}     -ana a.   
                 .nyu.t \Pan{3,1,147-148} profession f. -anii *)
  | Agent_root (* kvip \Pan{3,2,61} ifc + \Pan{3,2,76} adja  
                        ifc. mnf. \Pan{6,1,67} amuis de v
                        \Pan{3,2,76} root autonomous mnf. 
                 + .tak \Pan{3,2,8} root ifc (f. -ii) 
                 + .ta \Pan{3,2,20} -kara ifc (f. -ii) habitual, enjoy
                 + ka \Pan{3,2,3} root -aa, amuie, ifcno (no Preverb) f. ii *)
  | Agent_a (* ac \Pan{3,1,134} gu.na m. -a f. -aa   
               .na \Pan{3,1,140-143} v.rddhi (f. -aa)
               ka \Pan{3,1,135-136;144} -gu.na 
                  \Pan{3,2,3-7} m. -a (f. -aa) metaphoric use
               za \Pan{3,1,137-139} idem ka but (f. -aa) nb present stem *)
  | Agent_nu (* i.s.nu \Pan{3,2,136}    
                i.s.nuc \Pan{3,2,136-138} -i.s.nu gu.na (habit)
                khi.s.nuc \Pan{3,2,57} -i.s.n'u gu.na 
                knu \Pan{3,2,140} ksnu \Pan{3,2,139}  -nu -gu.na     *)
  | Action_ana (* lyu.t \Pan{3,3,115-117}            -ana n.        *)
  | Action_na (* naf \Pan{3,3,90} nan \Pan{3,3,91}    -na m. -naa f. *)
  | Action_a (* gha~n \Pan{3,3,18-}               -a m. v.rddhi *)
  | Action_ya (* kyap \Pan{3,1,107} -ya n. -yaa f.              *)
  | Action_root (* ?                                      f.   *)
  | Action_ti (* ktin \Pan{3,3,94}                      -ti f.  *)
  | Action_i (* ki \Pan{3,3,92-93}                     -i f.    *)
  | Object_root 
  | Object_a (* ka                                    -a n.    *)
  | Instrument (* ka \Pan{3,1,136}                  0/amui n.   *)
  | Instra (* .s.tran -tra n.                -trii f. traa f.  *)
  | Agent_u   (* san+u                             -u on des stem *)
  | Action_aa (* san+a+.taap  \Pan{3,3,102}        -aa on des stem *)
  | Abstract (* abstract nouns n.           -as u.naadi suffix *)
  (* More to come *)
  ]
  ;

type ind_kind = 
  [ Adv       (* adverb *)
  | Avya      (* turned into an adverb by avyayiibhaava compounding *)
  | Abs       (* root absolutive in -tvaa *)
  | Tas       (* tasil taddhita *)
  | Part      (* particule *)
  | Prep      (* preposition *) 
  | Conj      (* conjunction *)
  | Nota      (* notation *)
  | Infl      (* inflected form *)
  | Interj    (* interjection *)
  | Default   (* default - inherits its role *)
  ]
;
