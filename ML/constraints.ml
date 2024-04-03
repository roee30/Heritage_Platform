(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Syntactico/semantic analysis and penalty computations. *)
(* This is the 2005 design of a constraint machinery working on some kind
   of linear logic graph matching of semantic roles. Verbs are assigned arities 
   of needed complements, seen as roles with a negative polarity. It does not 
   really use the karaka theory, the role of a nominative is mediated through 
   the voice. This is very primitive, and works only for toy examples. 
   It merely gives a proof of feasability. 
   A more serious machinery should work on discourse, deal with ellipses, and
   possibly use optimality theory with matrix computations. *)

(* We need to enrich this parser with kridantas which have their own
aaka.mk.saa , eg participles. Then we must recognize that certain passive
constructs, such ppp, may be use in the active sense to indicate past
e.g. with verbs of mouvement *)

(*i module Constraints = struct i*)

open Skt_morph;
open Morphology; (* [inflexion_tag] *)
open Html;

(* Constraints analysis *)

(* Nouns *)
type noun_role = (* not karaka *)
  [ Subject of person and number (* agent of active or patient of passive *)
  | Object (* patient or goal of active or adverb of manner *)
  | Instrument (* agent of passive or instrument of active or adverb of manner *)
  | Destination (* receiver or goal *)
  | Origin (* origin of action or adverb of manner *)
  | Possessor (* dual role as verb complement or noun attribution *)
  | Circumstance (* adverb of time or location *)
  ]
and demand = list noun_role
;
value person_of_subst = fun
  [ "aham" -> First | "tvad" -> Second | _ -> Third ]
;
value gram_role num entry = fun
  [ Nom -> Subject (person_of_subst entry) num 
  | Acc -> Object (* Patient or adverb of manner *)
  | Ins -> Instrument (* Agent or adverb of instrument *)
  | Dat -> Destination
  | Abl -> Origin
  | Gen -> Possessor
  | Loc -> Circumstance
  | Voc -> failwith "Unexpected vocative (gram_role)"
  ]
and case_of = fun (* inverse of [gram_role] *)
  [ Subject _ _ -> Nom 
  | Object -> Acc 
  | Instrument -> Ins 
  | Destination -> Dat 
  | Origin -> Abl 
  | Possessor -> Gen 
  | Circumstance -> Loc 
  ]
;
type mood = 
  [ Indicative
  | Imper of bool (* True: Imperative False: Injunctive *)
  ]
;
(* mood processing - pertains to maa management *)
value ini_mood = (0,0)
and add_mood m moods = match m with
   [ Imper b -> let (imp,inj)=moods in if b then (imp+1,inj) else (imp,inj+1)
   | _ -> moods
   ]
;

(* Part of speech *)
type pos =
  [ Process of demand and mood    (* roles governed by a verb form *)
  | Subprocess of demand          (* verbal subphrase *)
  | Actor of noun_role and gender and number (* noun form with morphology *)
  | Addressee                     (* vocative *)
  | Tool of tool                  (* grammatical word *)
  | Compound                      (* iic *)
  | Number of gender and case and number  (* number (gender for eka) *)
  | Ignored                       (* indeclinable not known as tool *)
  ]
(* Combinatorial tools *)
and tool = 
  [ Coordination (* ca *)
  | Post_instrument (* saha vinaa prep *)
  | Not_Post_instrument (* saha adv *)
  | Prohibition (* maa *)
  | Post_genitive (* varam TODO *)
  | Todo (* to avoid warning *)
  ]
;
(* Verb valencies - Very experimental. *)
(* The serious version will have to make computations with preverbs *)
(* and will accommodate several sememes with different valencies for a
   given lexeme - e.g. "dhaav#1.1" intransitive, "dhaav#1.2" transitive.
   The paraphrase will be associated with sememes and not just lexemes. *)
type regime =
  [ Transitive   (* transitive verbs in active and middle *)
  | Intransitive (* intransitive verbs in active and middle *)
  | Factitive    (* impersonal - no subject *)
  | Quotative    (* aahur - it is said *) 
(*| Bitransitive - use of transitive with 2 accusatives *)
(*| Regime of (list case * list case) - specific regime - unused so far *)
(* Actually a root should have a valency list like [ 0; 1; 2 ] for "bhaa.s" *)
  ]
;

(* We simplify by assuming equal valency of atmanepade and parasmaipade. *)
(* Also we assume (to be revised) that valency is independent of preverb. *)
value root_regime = fun
  [ (* akarmaka roots, checked by Pawan Goyal *)
    (* more exactly, these are the roots that may be used akarmaka *)
    "an#2" | "as#1" | "as#2" | "aas#2" | "iih" | "uc" | "uurj#1" | ".rdh" 
  | "edh" | "kamp" | "kaaz" | "kuc" | "ku.t" | "kup" | "kul" | "kuuj" | "k.lp"
  | "krii.d" | "krudh#1" | "klid" | "kvath" | "k.sar" | "k.si" | "k.su" 
  | "k.sudh#1" | "k.subh" | "khel" | "gaj" | "garj" | "gard" | "galbh" | "gu~nj" 
  | "gur" | "g.rr#2" | "glai" | "gha.t" | "gha.t.t" | "ghuur.n" | "cakaas" 
  | "ca~nc" | "cal" | "cit#1" | "ce.s.t" | "jan" | "jaag.r" | "jiiv" | "j.rmbh" 
  | "j.rr" | "jyaa#1" | "jvar" | "jval" | ".dii" | "tan#2" | "tam" | "tu.s" 
  | "t.r.s#1" | "trap" | "tras" | "tvar" | "tsar" | "dak.s" | "dal" | "das" 
  | "dah#1" | "dih" | "diik.s" | "diip" | "du.s" | "d.rh" | "dev#1" | "dyut#1" 
  | "draa#1" | "draa#2" | "dhaav#1" | "dhru" | "dhvan" | "dhv.r" | "na.t" 
  | "nand" | "nard" | "naz#1" | "nah" | "nii#1" | "n.rt" | "pat#1" | "pii" 
  | "puuy" | "p.r#2" | "pyaa" | "prath" | "phal" | "ba.mh" | "bal" | "bha.n.d"
  | "bhand" | "bha.s" | "bhaa#1" | "bhaas#1" | "bhii#1" | "bhuj#1" | "bhuu#1" 
  | "bhra.mz" | "bhram" | "bhraaj" | "ma.mh" | "majj" | "mad#1" | "mud#1" | "muh"
  | "muurch" | "m.r" | "m.rdh" | "mre.d" | "mlaa" | "yabh" | "yas" | "yu#2" 
  | "yudh#1" | "ra~nj" | "ra.n" | "ram" | "raaj#1" | "ru" | "ruc#1" | "rud#1"
  | "ru.s#1" | "ruh#1" | "lag" | "lamb" | "lal" | "las" | "vak.s" | "vas#1" 
  | "vah#1" (* nadii vahati *) | "vaa#2" | "vaaz" | "vij" | "vip" | "viz#1" 
  | "v.rt#1" | "v.rdh#1" | "vyath" | "zak" | "zad" | "zam#1" | "zii#1" | "ziil"
  | "zuc#1" | "zudh" | "zubh#1" | "zu.s#1" | "zuu" | "zram" | "zrambh" | "zvas#1"
  | "zvit#1" | "sap#1" | "saa#1" | "sidh#1" | "sur" | "skhal" | "stan" | "stu" 
  | "stubh" | "sthaa#1" | "snih#1" | "snu" | "spand" | "spardh" | "sphaa"
  | "sphu.t" | "sphur" | "smi" | "syand" | "sra.ms" | "svap" | "svar#1" 
  | "svar#2" | "had" | "has" | "hikk" | "h.r.s" | "hras" | "hraad" | "hrii#1"
  | "hlaad" | "hval" -> Intransitive 
  | "baa.sp" | "zyaam" (* nominal verbs *) -> Intransitive 
  | "v.r.s" -> Factitive
  | "ah" -> Quotative
  | _ -> (* sakarmaka in all usages *) Transitive
(* But "bhaa.s" is Transitive, even though he may be used with 0 or 2 objects *)
(* Thus a penalty should not occur if he has no object or 2 objects *)
  ]
;
(* But valency may depend on gana for the present system *)
value root_regime_gana k = fun
  [ "i"      -> match k with [ 2 | 4 -> Intransitive | _ -> Transitive ]
  | "daa#1"  -> match k with [ 3 -> Intransitive | _ -> Transitive ]
  | "b.rh#1" -> match k with [ 1 -> Intransitive | _ -> Transitive ]
  | "maa#1"  -> match k with [ 2 -> Intransitive | _ -> Transitive ]
  | "tap" | "pac" | "raadh" | "svid#2" -> match k with 
      [ 4 -> Intransitive | _ -> Transitive ]
  | root -> root_regime root
  ]
;
(* Certain roots marked as Transitive are in fact Intransitive for some of
their meanings:
[ "kruz" | "gh.r" | "jak.s" | "ji" | "t.rp#1" | "d.rp" | "dhva.ms" | "pi~nj" 
| "bhas" | "mand#1" | "radh" | "lafgh" | "lu.n.th" | "vii#1" | "zumbh" 
| "sad#1" | "su#2" | "svan" | "ha.th" | "hi#2" | "hu.n.d" | "huu" ]
When used intransitively, the parser will look for a missing object and may
penalize correct sentences. For roots marked as Intransitive, but nonetheless
used transitively in a sentence, the parser will consider their accusative
object, in the active voice, as an adverb, but no penalty will incur.
NB. dvikarmaka roots are just treated as Transitive in this version. 
*)

value agent_of_passive = fun 
  [ "vid#2" -> [] (* ellipsed impersonal agent "it is known that" *)
  | _ -> [ Instrument ]  (* Agent at instrumental in passive voice *)
  ]
;
(* The following type actually combines aspect, voice and mood *)
type aspect = 
  [ Imperfectif (* active or middle indicative *)
  | Impersonal  (* intransitive passive *)
  | Perfectif   (* transitive passive *)
  | Statif      (* factitive *)
  ]
;
(* Computes aspect valency and mood of a verbal finite form as a triple *)
value regime entry (cj,t) = 
  (* conjugation cj and possible preverb sequence ignored in first version *)
  let regime = root_regime entry in (* TODO dependency on k *)
  match t with 
     [ Conjug _ Passive | Presentp _ -> 
        let aspect = match regime with
                     [ Intransitive -> Impersonal
                     | Factitive -> Statif
                     | _ -> Perfectif
                     ] 
        and valency = agent_of_passive entry in
        (aspect,valency,Indicative)
     | Conjug t _ -> 
        let aspect = if regime=Factitive || regime=Quotative then Statif 
                     else Imperfectif
        and valency = match regime with 
                      [ Transitive -> [ Object ] | _ -> [] ] 
        and mood = match t with [ Injunctive _ -> Imper False
                                | _ -> Indicative
                                ] in
        (aspect,valency,mood) 
     | Presenta k m | Presentm k m -> (* on affine le regime gana et mode *)
        let regime = root_regime_gana k entry in
        let aspect = if regime=Factitive || regime=Quotative then Statif 
                     else (* if m=Optative then Statif (* NEW bruyaat *)
                     else *) Imperfectif
        and valency = match regime with 
                      [ Transitive -> [ Object ] | _ -> [] ] 
        and mood = match m with 
                   [ Imperative -> Imper True
                   | _ -> Indicative (* now, only Imperative for Present *) 
                   ] in
        (aspect,valency,mood) 
(* OBS     | Perfut _ -> (if regime=Factitive then Statif else Imperfectif, 
                    match regime with [ Transitive -> [ Object ] | _ -> [] ],
                    Indicative) *)
     ] 
;
value get_fin_roles entry f n p = 
  let (aspect,valency,mood) = regime entry f in 
  let demand = match aspect with 
       [ Statif | Impersonal -> valency
       | _ -> [ Subject p n :: valency ] (* anaphoric subject reference *)
    (* | Imperfectif -> [ Subject p n :: valency ] (* subject is agent *)
       | Perfectif -> [ Subject p n :: valency ] (* subject is goal/patient *) *)
       ] in
  Process demand mood
and get_abs_roles entry = 
  let demand = match root_regime entry with 
       [ Intransitive | Factitive -> []
       | _ -> [ Object ] 
       ] in
  Subprocess demand 
;
(* Present participle active defines an auxiliary clause, like Absolutive *)
(* It denotes simultaneity rather than sequentiality/causality *)
value is_ppra (_,v) = match v with (* TEMP *)
  [ Ppra _ -> True | _ -> False ]
;
(* [get_roles] assigns roles to morphological items. *)
(* Some tool words are processed here and numbers are recognized. *)
value get_roles entry = fun
  [ Part_form v g n c 
             -> if c = Voc then Addressee 
                else if is_ppra v then Subprocess [] (* should lookup root *)
                else Actor (gram_role n entry c) g n (* beware n duplication *)
  | Noun_form g n c 
             -> if c = Voc then Addressee 
                else if g=Deictic Numeral || entry="eka" then Number g c n
                else Actor (gram_role n entry c) g n (* beware n duplication *)
  | Verb_form f n p -> get_fin_roles entry f n p 
(*  | Abs_root _ -> get_abs_roles entry  *)
  | Ind_form Conj -> match entry with
                     [ "ca" -> Tool Coordination
                     | _ -> Ignored (* TODO vaa etc *)
                     ]
  | Ind_form Prep -> if entry = "saha" || entry = "vinaa" || entry = "satraa" 
                        then Tool Post_instrument
                     else Ignored
  | Ind_form Adv -> if entry = "saha" then Tool Not_Post_instrument
                    else Ignored
  | Ind_form Abs -> get_abs_roles entry 
  | Ind_form Part -> match entry with
                     [ "maa#2" -> Tool Prohibition
                     | _ -> Ignored
                     ] 
  | Bare_stem -> Compound
  | _ -> Ignored
  ]
;
(* Used in Parser, Reader *)
value roles_of seg word tags = 
  let distrib (sub,res) (delta,morphs) = 
    let entry = Canon.decode (Word.patch delta word) in
    let roles = List.map (get_roles entry) morphs in
    let (_,r) = List.fold_left label (1,res) roles
            where label (i,l) role = (i+1,[ ((seg,sub,i),role) :: l ]) in
    (sub+1,r) in
  let (_,rls) = List.fold_left distrib (1,[]) tags in 
  rls
;
(*i Auxiliary list commodity - flattening a list of lists 
(* flatten [[2; 3; 4]; [5; 6]] = (* distribute *)
   [[2; 5]; [3; 5]; [4; 5]; [2; 6]; [3; 6]; [4; 6]] *)
value rec flatten = fun
  [ [] -> [[]]
  | [ l :: r ] -> let flatr = flatten r in 
                  let distr f res = (List.map (fun x -> [ x :: f ]) l) @ res in
                  List.fold_right distr flatr []
  ]
; 
(* Flattening with index management - with weird re-ordering within choices *)
(* flatteni [[("a",2); ("b",3); ("c",4)]; [("x",5); ("y",6)]] = 
            [[("c",4); ("x",5)]; [("b",3); ("x",5)]; [("a",2); ("x",5)];
             [("c",4); ("y",6)]; [("b",3); ("y",6)]; [("a",2); ("y",6)]] *)
value rec flatteni = fun
  [ [] -> [[]]
  | [ l :: r ] -> 
    let flatr = flatteni r in 
    let distr res f = 
       let prefix acc x = [ [ x :: f ] :: acc ] in
       let result = List.fold_left prefix [] l in 
       result @ res in
    List.fold_left distr [] flatr 
  ]

(* Inspired by these simpler flattening primitives, we define now 
   [flatten_add] for lists of roles: *) 
i*)

(* We flatten the role matrix into a list of sequences. *)
(* This is potentially exponential, since we multiply choices. *)

type label = (int * int * int) (* (segment number, homonymy index, tag index) *)
and roles = list (label * pos)
;
(* Combinator [flatten_add] is for the brave. Do not attempt to understand 
   this code if you have not already mastered flatten and flatteni above. *) 
(* [flatten_add : list roles -> list roles] *)
value rec flatten_add = fun       (* arg goes backward in time *)
  [ [] -> [ [] ]
  | [ l :: r ] ->                                 (* l: roles *)
    let flatr = flatten_add r  
    and distr res f =                             (* f: roles *)
        let prefix acc x = [ [ x :: f ] :: acc ] in
        let result = List.fold_left prefix [] l in 
        result @ res in 
    List.fold_left distr [] flatr 
  ]
; 
(* Tool words as semantic combinators - reverse role stream transducers *)

(* Coordination tool *)
exception No_coord (* Coordination failure *)
;
(* future deictic gender context, here assumed all male *)
value context d = Mas
;
(* abstract interpretation of coordination *)
value merge = fun (* persons priorities *)
  [ First -> fun _ -> First
  | Second -> fun [ First -> First | _ -> Second ]
  | Third -> fun [ First -> First | Second -> Second | _ -> Third ]
  ]
and add = fun (* numbers additions *)
  [ Plural | Dual -> fun _ -> Plural
  | Singular -> fun [ Plural | Dual -> Plural | Singular -> Dual ]
  ]
;
value rec dom = fun (* male dominance *)
  [ Mas -> fun _ -> Mas
  | Fem -> fun [ Mas -> Mas | Deictic d -> dom Fem (context d) | _ -> Fem ]
  | Neu -> fun [ Deictic d -> context d | g -> g ]
  | Deictic d -> dom (context d)
(* Unsatisfactory - numbers ought to be treated as Neu. *)
(* The gender is used only for possible adjective agreement, 
   not for verb government *)
  ]
;
(* Coordination recognizes noun phrases (N = IIc*.Noun@nom)
   N1 N2 ca ... Np ca
   N1 ca N2 ca ... Np ca
   with N = C* S  C = iic, S = Subst
 NB negation not yet accounted for (naca etc);
    also is missing N1 N2 ... Np ca avec Ni homogène en nb - adjectival cascade.
 We synthesize a multiple homogeneous substantive in the output stream *)

value coord_penalty = 1
;
(* removing possible compound prefixes *)
value end_coord kar acc p g n = rem_iic 
  where rec rem_iic cur = match cur with 
   [ [ Compound :: rest ] -> rem_iic rest 
   | _ -> match kar with (* Synthesis of compound kar *)
       [ Subject _ _ -> ([ Actor (Subject p n) g n :: acc ],cur) 
       | kar -> ([ Actor kar g n :: acc ],cur) 
       ]
   ]
;
value agree_deictic g = fun
   [ Deictic _ -> True
   | g1 -> g=g1
   ]
;
(* Remove compound formation and possible adjectival number word. *)
value skim c g n context = skim_rec context 
  where rec skim_rec con = match con with
   [ [ Compound :: rest ] -> skim_rec rest (* skip possible iic - compounding *)
   | [ Number g1 c1 n1 :: rest ] -> 
       if agree_deictic g g1 && c=c1 && n=n1 (* agreement of Number *)
          then rest
          else raise No_coord
   | _ -> con
   ]
;
value rec coord1 kar acc p g n = fun 
  (* searching for closest noun phrase *)
  [ [] -> raise No_coord
  | [ np :: rest ] -> match np with 
     [ Actor (Subject p1 _) g1 n1 -> match kar with 
       [ Subject _ _ -> 
         coord2 kar acc (merge p p1) (dom g g1) (add n n1) rest
       | _ -> raise No_coord
       ]
     | Actor k g1 n1 when k = kar ->
       coord2 kar acc Third (dom g g1) (add n n1) rest
     | _ -> raise No_coord
     ]
   ]
and coord2 kar acc p g n cur = match cur with 
    (* searching for previous noun phrases *)
  [ [] -> raise No_coord
  | [ np :: rest ] -> match np with 
     [ Actor (Subject p1 _) g1 n1 -> match kar with 
       [ Subject _ _ -> 
         let before = skim Nom g1 n1 rest in
         end_coord kar acc (merge p p1) (dom g g1) (add n n1) before
       | _ -> raise No_coord
       ]
     | Actor k g1 n1 -> 
       if k = kar then let before = skim (case_of k) g1 n1 rest in
                       (* additive interpretation of ca *)
                       end_coord kar acc Third (dom g g1) (add n n1) before
                  else raise No_coord
     | Tool Coordination -> coord1 kar acc p g n rest (* iterate the tool *)
     | _ -> raise No_coord
     ]
   ]
;
(* Coordination: the ca tool constructs a composite tag from its predecessors *)
value coordinate acc = fun 
  (* searching for first noun phrase *)
  [ [] -> raise No_coord
  | [ np :: rest ] -> match np with 
    [ Actor (Subject p1 _ as kar) g1 n1 -> 
         let before = skim Nom g1 n1 rest in
         coord2 kar acc p1 g1 n1 before
    | Actor kar g1 n1 -> 
         let before = skim Nom g1 n1 rest in
         coord2 kar acc Third g1 n1 before
    | _ -> raise No_coord
    ]
   ]
;
(* Bumping the current penalty by a given malus *)
value penalize malus (roles,pen) = (roles,pen+malus)
;

(* Ugly experimental management of "maa" negative particle - temporary *)
value maa_counter = ref 0 
;
value reset_maa () = maa_counter.val := 0
;
(* apply tools on the list of roles, read from right to left *)
(* tools are piped as role streams transducers - res is accumulated output 
   of the form (list role,penalty). *)
value rec use_tools res = fun
  [ [] -> res
  | [ r :: iroles ] -> match r with 
     [ Tool Coordination -> (* ca *)
       try let (oroles,penalty) = res in
           let (result,left) = coordinate oroles iroles in
           use_tools (result,penalty) left with
       [ No_coord -> use_tools (penalize coord_penalty res) iroles ] 
     | Tool Post_instrument -> (* saha vinaa prep *)
       match iroles with
       [ [] -> penalize 1 res
       | [ r :: previous ] -> match r with 
            [ Actor Instrument _ _ -> use_tools res previous (* <i.-saha> *)
            | _ -> use_tools (penalize 1 res) iroles 
            ]
       ]
     | Tool Not_Post_instrument -> (* saha adv *)
       match iroles with
       [ [] -> res
       | [ r :: _ ] -> match r with 
            [ Actor Instrument _ _ -> use_tools (penalize 1 res) iroles
            | _ -> use_tools res iroles 
            ]
       ]
     | Tool Prohibition  -> (* maa *) do
       { maa_counter.val := maa_counter.val + 1
       ; res
       }
     | Tool _  (* not yet implemented *)
     | Ignored (* noop *)
     | Compound -> use_tools res iroles (* compounds are skipped *)
       (* ordinary roles are processed as Identity tools *)
     | _ -> let (oroles,p) = res in (* otherwise we take role as is *)
            use_tools ([ r :: oroles ],p) iroles 
     ]
  ]
;

(* We construct a list [neg] of expected [noun_roles], a list [pos] of 
   available ones, a counter [pro] of processes, a boolean [subpro] indicating
   the need of a finite verb form, a mood integrator [moo] *)
value process_role (neg,pos,pro,subpro,moo) role =
  match role with
    [ Process noun_roles m -> (noun_roles @ neg,pos,pro+1,subpro,add_mood m moo)
      (* pro+1 is problematic, it does not account for relative clauses *)
    | Subprocess noun_roles -> (noun_roles @ neg,pos,pro,True,moo)
    | Actor noun_role gender number -> 
           (neg,[ (noun_role, number, gender) :: pos ],pro,subpro,moo)
    | _ -> (neg,pos,pro,subpro,moo)
    ]
;
exception Missing
;
type triple = (noun_role * number * gender) 
     (* NB there is redundancy in the case (Subject p n,n',g) since n'=n *)
;
value subject_agreement (noun_role,_,_) p n = 
  noun_role = Subject p n
;
(* Tries to find a matching agent: looks into the list of leftover given roles 
   for an expected agent with person p and number n, returns it paired with 
   the rest of given roles if found, raises exception Missing otherwise *)
value remove_subj p n = remrec [] 
  where rec remrec acc = fun
    [ [] -> raise Missing
    | [ triple :: rest ] -> 
      if subject_agreement triple p n then (triple,List2.unstack acc rest)
      else remrec [ triple :: acc ] rest
    ]
;
(* Tries to find a matching role for a non-agent [noun_role] *)
value remove_matching kar = remrec [] 
  where rec remrec acc = fun
    [ [] -> raise Missing
    | [ ((k,_,_) as triple) :: rest ] -> 
      if k = kar then 
         (triple,List2.unstack acc rest) (* we choose latest matching *)
      else remrec [ triple :: acc ] rest
    ]
;
(* [missing] is the list of missing expectancies [noun_roles]
   [taken] is the list of found expectancies [noun_roles]
   [left] is the list of found unexpected [noun_roles] *)
value process_exp (missing,taken,left) = fun
  (* for each expected [noun_role] we look for a matching given one *)
  [ Subject p n -> (* verb subject has p and n *)
    try let (found,remain) = remove_subj p n left in
        (missing,[ found :: taken ],remain)
    with [ Missing -> (missing,taken,left) ] (* subject is optional *)
  | kar -> try let (found,remain) = remove_matching kar left in
               (missing,[ found :: taken ],remain)
           with [ Missing -> ([ kar :: missing ],taken,left) ] (* mandatory *)
  ]
;
(* Contraction corresponding to agreement between phrase-forming chunks. *)
(* Items agreeing with an already taken item are removed from leftovers. *)
value contract taken = List.fold_left filter [] 
  where filter left triple = if List.mem triple taken then left
                             else [ triple :: left ]
;
(* Penalty parameters in need of tuning by training *)
value missing_role_penalty _ = 1
and excess_subject_penalty = 1
and np_penalty = 2
and absol_penalty = 2 (* absolutive without finite verb *)
;
(* remaining extra nominatives give penalty *)
value count_excess pen = fun
  [ (Subject p n,_,_) -> pen+excess_subject_penalty
  | triple -> pen (* taken as adverbs or genitive noun phrases *)
  ]
;
(* We count all persons with same person and number *)
value count_subj persons = fun
  [ Subject p n -> List2.union1 (p,n) persons
  | _ -> persons
  ]
;
value count_missing pen k = pen+missing_role_penalty k 
; 
value missing_penalty = List.fold_left count_missing 0
and excess_penalty = List.fold_left count_excess 0 
;
type penalty =
  [ Sentence of (int * int * int * int)
  | Copula of (int * int * int * int * int) 
  | NP of penalty
  ]
;
value rec show_penalty = fun (* explicit vector for debug *)
  [ Sentence (p1,p2,p3,p4) -> 
     "S(" ^ string_of_int p1 ^ "," ^ string_of_int p2 ^ "," 
          ^ string_of_int p3 ^ "," ^ string_of_int p4 ^ ")" 
  | Copula (p1,p2,p3,p4,p5) -> 
     "C(" ^ string_of_int p1 ^ "," ^ string_of_int p2 ^ "," 
          ^ string_of_int p3 ^ "," ^ string_of_int p4 ^ ","
          ^ string_of_int p5 ^ ")"
  | NP p -> string_of_int np_penalty ^ "+" ^ show_penalty p  
  ]
;
(* Ad-hoc linear penalty function - to be optimized by corpus training *)
value rec eval_penalty = fun
  [ Sentence (pen1,pen2,pen3,pen4) -> pen1+pen2+pen3+pen4
  | Copula (pen1,pen2,pen3,pen4,pen5) -> pen1+pen2+pen3+pen4+pen5
  | NP pen -> np_penalty + eval_penalty pen
  ]
;
value balance_process pro subpro = 
  if pro>1 then pro-1 (* TEMP, to be adjusted with relative clauses *)
  else if pro=0 then if subpro then absol_penalty else 0
  else 0
;
(* Delay dealing with nominatives in order to favor Acc over Nom for neuters *)
value sort_kar = sort_rec [] [] 0
  where rec sort_rec nomins others n = fun
  [ [] -> (List2.unstack others nomins,n)
  | [ (Subject _ _ as kar) :: rest ] -> 
      sort_rec [ kar :: nomins ] others (n+1) rest
  | [ kar :: rest ] -> sort_rec nomins [ kar :: others ] n rest 
  ]
;
value check_sentence pen1 neg pos pro subpro =
  let (missing,taken,left) = List.fold_left process_exp ([],[],pos) neg in
  let contracted = contract taken left in
  let pen2 = missing_penalty missing 
  and pen3 = excess_penalty contracted 
  and pen4 = balance_process pro subpro in 
  Sentence (pen1,pen2,pen3,pen4)
;
(* Given a list of remaining roles, tries to find a matching Subject; 
   returns (missing,taken,rest) where either taken is the singleton found, 
   rest is the list of remaining roles, and missing is empty, or else taken 
   is empty, missing is the singleton not found, and rest is all roles *)
value process_exp_g p n roles = 
  let remove_matching = remrec [] 
  where rec remrec acc = fun
    [ [] -> raise Missing
    | [ triple :: rest ] -> match triple with
        [ (Subject p n',_,_) when n'=n -> (triple,List2.unstack acc rest)
                             (* NB there is no mandatory concord of genders *)
        | _ -> remrec [ triple :: acc ] rest
        ]
    ] in
(* we look for a matching nominative *)
    try let (found,remain) = remove_matching roles in
        ([],[ found ],remain)
    with [ Missing -> (* First and Second persons Subjects are optional *)
           if p = First || p = Second then ([],[],roles)
           else ([ Subject p n ],[],roles) ]
;
value check_copula_sentence pen1 p n pos subpro = 
  let (missing,taken,left) = process_exp_g p n pos in
  let contracted = contract taken left in 
  let pen2 = missing_penalty missing 
  and pen3 = excess_penalty contracted 
  and pen4 = if subpro then absol_penalty else 0 in
  Copula (pen1,pen2,pen3,pen4,0)
;
(* [get_predicate] returns the first available Subject (backward from the end)
   if there is one, else raises Missing *)
value get_predicate = search_subject [] 
  where rec search_subject acc = fun
  [ [] -> raise Missing
  | [ ((kar,_,_) as triple) :: rest ] -> match kar with
      [ Subject p n -> (p, n, List2.unstack acc rest)
      | _ -> search_subject [ triple :: acc ] rest
      ]
  ]
(* NB adding a topic amounts to replacing [get_predicate pos] 
   by (Third,Singular,pos) below *)
;
(* We enforce that maa must correspond to an injunctive or an imperative
   and that injunctives occur only with maa. TODO: allow also optative,
   subjunctive and augmentless imperfect with maa. UGLY *)
value rec mood_correction (imp,inj) pen = 
  let maa_tokens = maa_counter.val in (* counted by Prohibition tool *)
  let maa_pen = if maa_tokens>imp+inj then maa_tokens-(imp+inj)
                else if inj>maa_tokens then inj-maa_tokens 
                else 0 in match pen with
  [ Sentence (p1,p2,p3,p4) -> Sentence (p1,p2,p3,p4+maa_pen) (* p4=0 *)
  | Copula (p1,p2,p3,p4,p5) -> Copula (p1,p2,p3,p4,p5+maa_pen) (* p5=0 *)
  | NP pen -> mood_correction (imp,inj) pen (* weird *)
  ]
;
value inspect pen (neg,pos,pro,subpro,md) = mood_correction md pens 
  where pens =
  if neg=[] (* no overt verb, we conjecture copula (pro=0) *) then
     try let (p,n,rest) = get_predicate pos in 
         check_copula_sentence pen p n rest subpro
     with [ Missing -> (* maybe noun phrase *)
              NP (check_sentence pen [] pos pro subpro) ] (* 2+ *)
  else check_sentence pen neg pos pro subpro (* verbal predicate exists *)
;
(* We compute a path penalty by applying [use_tools] from right to left 
   to the given path, then iterating [process_role] on the resulting roles,
   then inspecting and weighting the resulting constraints *)
value penalty rev_path =
  let right_left_roles = List.map snd rev_path in do
  { reset_maa () (* horreur *)
  ; let (roles,pen_tools) = use_tools ([],0) right_left_roles in 
    let constraints = 
        List.fold_left process_role ([],[],0,False,ini_mood) roles in
    inspect pen_tools constraints 
  }
;
type flattening = list (penalty * list roles)
;
(* We flatten all choices in the chunked solution *)
(* [sort_flatten: list roles -> flattening] *)
value sort_flatten groups =           (* groups goes backward in time *)
  let parses = flatten_add groups in  (* each parse goes backward in time *)
  let insert_in sorted_buckets rev_path = 
      let p = penalty rev_path in 
      let ep = eval_penalty p in
      ins_rec [] sorted_buckets
      where rec ins_rec acc = fun
      [ [] -> List2.unstack acc [ (p,[ rev_path ]) ]
      | ([ ((pk,b_k) as b) :: r ] as buckets) -> 
        let ek = eval_penalty pk in (* recomputation to avoid *)
        if ek=ep then List2.unstack acc [ (p,[ rev_path :: b_k ]) :: r ]
        else if ek<ep then ins_rec [ b :: acc ] r
        else List2.unstack acc [ (p,[ rev_path ]) :: buckets ]
      ] in
  let sort_penalty = List.fold_left insert_in [] in
  sort_penalty parses
;

(* Output truncated to avoid choking on immense web page.
   Returns penalty threshold if truncation. Used in Reader and Parser *)
value truncate_groups buckets = match buckets with
  [ [ best :: [ next :: rest ] ] -> 
    let top = [ best; next ] in (* top 2 buckets *)
    let threshold = 
      match rest with 
      [ [] -> None
      | [ (p,_) :: _ ] -> Some (eval_penalty p)
      ] in 
    (top, threshold)
  | _  -> (buckets, None)
  ]
; 

value extract str ((seg,sub,ind),_) = (* construct tag projections *)
  let m = string_of_int sub (* segment number [seg] is redundant *)
  and n = string_of_int ind in 
  let proj = m ^ "," ^ n in 
  if str="" then proj else proj ^ "|" ^ str
; 

(* end; *)
