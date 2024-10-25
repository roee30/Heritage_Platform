(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2024 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Nouns = struct i*)

(* Computes the declensions of substantives, adjectives, pronouns, numerals
   and records the nominal inflected forms in databases by [Inflected.enter]. 
   It is called from [Make_nouns] nominal generation process. *)

open List; (* exists, iter *)
open Word; (* mirror *)
open Skt_morph (* morphology, datatypes *);
open Phonetics; (* [finalize, finalize_r] *) 
open Inflected; (* [Declined, Bare, Cvi, enter, enter1, morpho_gen,
reset_nominal_databases, nominal_databases] *)

(**** Error handling ****)
exception Report of string
;
value report revstem gen =
  let stem = Canon.rdecode revstem
  and gender_str = match gen with 
      [ Mas -> "M" | Neu -> "N" | Fem -> "F" | Deictic _ -> "*" ] in 
  let message = stem ^ " missing gender " ^ gender_str in
  raise (Report message)
;
value warn revstem str =
  let stem = Canon.decode (mirror revstem) in 
  let message = stem ^ " is declined as " ^ str in
  raise (Report message)
;
value print_report s = 
  output_string stderr (s ^ "\n")
;

(* Word encodings of strings *)
value code = Encode.code_string (* normalized *)
and revcode = Encode.rev_code_string (* reversed (mirror of code) *)
and revstem = Encode.rev_stem (* stripped of homo counter *)
and normal_stem = Encode.normal_stem 
;
(* Declension generators *)
type declension_class = 
  [ Gender of gender  (* declined substantive, adjective, number, pronoun *)
  | Ind of ind_kind   (* indeclinable form *)
  ]
and nmorph = (string * declension_class)
;
(* Affix a suffix string to a rstem word using internal sandhi *)
(* [fix: Word.word -> string -> Word.word] *)
value fix rstem suff = 
  Int_sandhi.int_sandhi rstem (code suff) 
;
(* raw affixing for [build_han] Whitney§195a *)
value fixno rstem suff = List2.unstack rstem (code suff) 
;
value wrap rstem c = mirror [ c :: rstem ]
;
(* monosyllabic stems, for feminine in ii or uu *)
(* NB - condition not preserved by prefixing and compounding.
   See Whitney§352 for differing opinions of grammarians *)
value monosyl = Phonetics.all_consonants (* Z NOT Phonetics monosyllabic *)
;
(* An attempt at treating a few compounds of monosyllabic in -ii    *)
(* This question is not clear at all, cf. mail by Malhar Kulkarni   *)
(* eg loc sg fem abhii = abhiyi (Zukla) or abhyaam (Malhar) ?       *)
(* Malhar actually says: 3 forms abhyi according to commentators    *)
(* if consonant clutter before ii or uu, then not nadii \Pan{1.4.4} *)
(* This is dubious, see -vii -nii below *)
(* See Kale §76 §77 *)
value compound_monosyl_ii = fun
  [ [ 40 :: l ] (* -bhii *) -> match l with  
      [ [ 1 ] | [ 1; 37; 1 ] -> True (* abhii apabhii *)
      | _ -> False 
      ]
  | [ 35 :: l ] (* -dhii *) -> match l with
      [ [ 1; 37; 44; 1 ] | [ 2; 33; 32; 3 ] | [ 5; 17 ] | [ 43; 5; 34 ]
      | [ 5; 48 ] -> True (* alpa- itthaa- ku- dur- su- *)
      | _ -> False 
      ]
  | [ 43 :: [ 37 :: l ] ] (* -prii *) -> match l with
      [ [ 2 ] (* aaprii *) -> True
      | _ -> False 
      ]
  | [ 43 :: [ 46 :: _ ] ] (* -zrii *) -> True (* ma~njuzrii *)
(*| [ 31 :: l ] (* -.nii for -nii *) -> match l with
      [ [ 1; 41; 2; 43; 19 ] (* graama- *) -> True (* wrong - \Pan{6,4,82} *)
      | _ -> False 
      ] *)
(*| [ 36 :: l ] (* -nii *) -> match l with
      [ [ 2; 36; 10; 48 ] (* senaa- *) -> True (* wrong Deshpande gr p146 *)
      | _ -> False 
      ] *)
(*| [ 45 :: l ] (* -vii *) -> match l with (* wrong: padaviim *)
      [ [ 1; 34; 1; 37 ] -> True (* pada- *)
      | _ -> False 
      ] *)
  | _ -> False (* to be completed for other roots *)
  ]
;
(* Similarly for -uu roots *)
value compound_monosyl_uu = fun
  [ [ 37 :: _ ] (* -puu *)  (* khalapuu DespGram p146 *) 
  | [ 40 :: _ ] (* -bhuu2 *) (* abhiibhuu manobhuu pratibhuu DespGram p146 *) 
  | [ 48 :: _ ] (* -suu2 *)  (* prasuu  *)
  | [ 43 :: [ 40 ::  _ ] ]  (* -bhruu *) -> True (* subhruu *)
  | _ -> False 
  ]
;

(* Stems with possible pronominal declension *)
value pronominal_usage = fun 
  [ "prathama" | "dvitaya" | "t.rtiiya" | "apara" 
  | "alpa" | "ardha" | "kevala" | "baahya" -> True (* Whitney§526 *)
  | _ -> False
  ]
;
(* The following restrict the generative capacity of certain entries,
   in order to reduce overgeneration. 
   Such information should ultimately be lexicalized *)

(* Masculine a-entries may be all used as iiv (inchoative cvi suffix) *)
(* NB pronouns "eka" and "sva" produces cvi form in [build_pron_a] *)
(* idem for masculines in -i and -in *)
(* Now for neuter stems (ad-hoc - should be more general) *)
value a_n_iiv = fun
  [ "aaspada" | "kara.na" | "go.spada" | "t.r.na" | "nimitta" | "paatra" 
  | "pi~njara" | "pratibimba" | "pratyak.sa" | "pramaa.na" | "prahara.na"
  | "yuddha" | "vahana" | "vize.sa.na" | "vi.sa" | "vyajana" | "zayana"
  | "zo.na" | "sukha" 
  | (* NavyaNyaaya *) "adhikara.na" | "kaara.na" | "saadhana"
    (* missing compound: "si.mhavyaaghraami.sa" *)
      -> True
  | _ -> False 
  ]
and man_iiv = fun (* sn *)
  [ "karman" | "bhasman" 
      -> True
  | _ -> False
  ]
and as_iiv = fun (* sn *)
  [ "unmanas" | "uras" | "cetas" | "manas" | "rajas" | "rahas" 
      -> True
  | _ -> False
  ]
and aa_iiv = fun 
  [ "kathaa" | "parikhaa" | "mak.sikaa" -> True  (* to be completed *)
  | _ -> False   
  ]
(* NB [aa_iic] obsolete, now use separate entry femcf marked fstem and 
[extract_fem_stems] will generate its iic. *)
;
(*************************************)
(************* Paradigms *************)
(*************************************)

(* For use in mono-entries paradigms *)
value register case form = (case,code form)
;
value build_mas_a stem entry = 
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun Mas 
   [ (Singular,if entry = "ubha"  (* dual only *)
               || entry = "g.rha" (* plural only *)
               || entry = "daara" then [] else 
        [ decline Voc "a"
        ; decline Nom "as"
        ; decline Acc "am"
        ; decline Ins "ena"
        ; decline Dat "aaya"
        ; decline Abl "aat"
        ; decline Gen "asya"
        ; decline Loc "e" 
        ])
   ; (Dual, if entry = "g.rha" 
            || entry = "daara" then [] else 
        [ decline Voc "au"
        ; decline Nom "au"
        ; decline Acc "au"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, if entry = "ubha" then [] else 
      let l =
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aan"
        ; decline Ins "ais" 
        ; decline Ins "ebhis" (* Vedic eg kar.nebhi.h *) 
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "aanaam"
        ; decline Loc "esu"
        ] in 
      if pronominal_usage entry then [ decline Nom "e" :: l ] else l)
   ] 
   ; Bare Noun (wrap stem 1)
   ; Avyayaf (fix stem "am"); Avyayaf (fix stem "aat") (* avyayiibhaava *)
   ; Indecl Tas (fix stem "atas") (* tasil productive *)
   ; Cvi (wrap stem 4) (* cvi productive *)
   ]) 
;
value build_mas_i stem trunc entry = (* declension of "ghi" class *) 
  let declines case suff = (case,fix stem suff) 
  and declineg case suff = (case,fix [ 10 :: trunc ] suff) 
  and declinel case suff = (case,fix [ 4 :: trunc ] suff) 
  and declinau case = (case,wrap trunc 13) in
  enter entry (
   [ Declined Noun Mas
   [ (Singular, 
        [ declineg Voc ""
        ; declines Nom "s"
        ; declines Acc "m"
        ; declines Ins "naa"
        ; declineg Dat "e"
        ; declineg Abl "s"
        ; declineg Gen "s" (* but avi: avyas Burrow p177 ? *)
        ; declinau Loc 
        ])
   ; (Dual, 
        [ declinel Voc ""
        ; declinel Nom ""
        ; declinel Acc ""
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam" 
        ; declines Abl "bhyaam"
        ; declines Gen "os"
        ; declines Loc "os"
        ])
   ; (Plural, 
        [ declineg Voc "as"
        ; declineg Nom "as"
        ; declinel Acc "n"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]
   ; Bare Noun (mirror stem)
   ; Avyayaf (mirror stem)
   ; Indecl Tas (fix stem "tas")
   ; Cvi (wrap trunc 4) (* "aadhi1" "pratinidhi" *)
   ])
;
value build_sakhi stem entry sakhi = (* Whitney§343a *)
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun Mas 
   [ (Singular,
        [ decline Voc "e"
        ; decline Nom "aa"
        ; decline Acc "aayam"
        ; decline Ins "yaa"
        ; decline Dat "ye"
        ; decline Abl "yus"
        ; decline Gen "yus"
        ; decline Loc "yau"
        ])
   ; (Dual, 
        [ decline Voc "aayau"
        ; decline Nom "aayaa" (* ved. Whitney§343b *)
        ; decline Nom "aayau"
        ; decline Acc "aayau"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "yos"
        ; decline Loc "yos"
        ])
   ; (Plural, 
        [ decline Voc "aayas"
        ; decline Nom "aayas"
        ; decline Acc "iin"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "iinaam"
        ; decline Loc "isu"
        ])
   ]
   ; Avyayaf (wrap stem 3)
(* ; Cvi (wrap stem 4) *)
   ] @ (if sakhi then [ Bare Noun (wrap stem 1) ] (* sakha *) else []))
;
value build_mas_u stem trunc entry = (* similar to [build_mas_i] *)
  let declines case suff = (case,fix stem suff) 
  and declineg case suff = (case,fix [ 12 :: trunc ] suff) 
  and declinel case suff = (case,fix [ 6 :: trunc ] suff) 
  and declinau case = (case,wrap trunc 13) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ declineg Voc "" 
        ; declines Nom "s"
        ; declines Acc "m"
        ; declines Ins "naa"
        ; declineg Dat "e"
        ; declineg Abl "s"
        ; declineg Gen "s"
        ; declinau Loc 
        ])
   ; (Dual, 
        [ declinel Voc "" 
        ; declinel Nom ""
        ; declinel Acc ""
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam"
        ; declines Gen "os" 
        ; declines Loc "os"
        ]) 
   ; (Plural,
        [ declineg Voc "as" 
        ; declineg Nom "as"
        ; declinel Acc "n" 
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]
   ; Bare Noun (mirror stem)
   ; Cvi (wrap trunc 6) (* .rju maru m.rdu laghu *)
   ; Avyayaf (mirror stem)
   ; Indecl Tas (fix stem "tas")
   ]
;
value build_mas_ri_v stem entry = (* vriddhi in strong cases *)
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 7 in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "aa"
        ; decline Acc "aaram"
        ; decline Ins "raa"
        ; decline Dat "re"
        ; decline Abl "ur"
        ; decline Gen "ur"
        ; decline Loc "ari"
        ])
   ; (Dual,
        [ decline Voc "aarau"
        ; decline Nom "aarau"
        ; decline Acc "aarau"
        ; decline Ins ".rbhyaam"
        ; decline Dat ".rbhyaam"
        ; decline Abl ".rbhyaam"
        ; decline Gen "ros"
        ; decline Loc "ros"
        ])
   ; (Plural,
        [ decline Voc "aaras"
        ; decline Nom "aaras"
        ; decline Acc ".rrn"
        ; decline Ins ".rbhis"
        ; decline Dat ".rbhyas"
        ; decline Abl ".rbhyas"
        ; decline Gen ".rr.naam"
        ; decline Loc ".r.su"
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare
   ]
;
(* kro.s.t.r irregular with stem krostu Muller§236 \Pan{7,1,95-97} *)
value build_krostu stem entry = 
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 5 in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "o"
        ; decline Nom "aa"
        ; decline Acc "aaram"
        ; decline Ins "unaa"
        ; decline Ins "raa"
        ; decline Dat "ave"
        ; decline Dat "re"
        ; decline Abl "or"
        ; decline Abl "ur"
        ; decline Gen "or"
        ; decline Gen "ur"
        ; decline Loc "au"
        ; decline Loc "ari"
        ])
   ; (Dual,
        [ decline Voc "aarau"
        ; decline Nom "aarau"
        ; decline Acc "aarau"
        ; decline Ins "ubhyaam"
        ; decline Dat "ubhyaam"
        ; decline Abl "ubhyaam"
        ; decline Gen "vos"
        ; decline Gen "ros"
        ; decline Loc "vos"
        ; decline Loc "ros"
        ])
   ; (Plural,
        [ decline Voc "aaras"
        ; decline Nom "aaras"
        ; decline Acc "uun"
        ; decline Ins "ubhis"
        ; decline Dat "ubhyas"
        ; decline Abl "ubhyas"
        ; decline Gen "uunaam"
        ; decline Loc "u.su"
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare
   ]
;
value build_mas_ri_g stem entry = (* parenté avec gu.na *)
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 7 in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "aa"
        ; decline Acc "aram"
        ; decline Ins "raa"
        ; decline Dat "re"
        ; decline Abl "ur"
        ; decline Gen "ur"
        ; decline Loc "ari"
        ])
   ; (Dual,
        [ decline Voc "arau"
        ; decline Nom "arau"
        ; decline Acc "arau"
        ; decline Ins ".rbhyaam"
        ; decline Dat ".rbhyaam"
        ; decline Abl ".rbhyaam"
        ; decline Gen "ros"
        ; decline Loc "ros"
        ])
   ; (Plural,
        [ decline Voc "aras"
        ; decline Nom "aras"
        ; decline Acc ".rrn"
        ; decline Acc "aras" (* epics Whitney§373c *)
        ; decline Ins ".rbhis"
        ; decline Dat ".rbhyas"
        ; decline Abl ".rbhyas"
        ; decline Gen ".rr.naam"
        ; decline Loc ".r.su"
        ])
   ]
   ; Bare Noun bare
   ; Bare Noun (wrap stem 2) (* for dvandva eg ved hotaapotarau \Pan{6,3,47} *)
   ; Avyayaf bare
   ; Indecl Tas (fix stem ".rtas") (* pit.rtas *)
   ]
;
value build_nri stem entry = (* currently disabled by skip in Dico *)
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 7 in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Nom "aa"  (* other cases from nara like naram *)
        ; decline Loc "ari" (* MacDonell§101b *)
        ])
   ; (Dual, 
        [ decline Voc "arau"
        ; decline Nom "arau"
        ; decline Acc "arau"
        ; decline Ins ".rbhyaam"
        ; decline Dat ".rbhyaam"
        ; decline Abl ".rbhyaam"
        ; decline Gen "ros"
        ; decline Loc "ros"
        ])
   ; (Plural,
        [ decline Voc "aras"
        ; decline Nom "aras"
        ; decline Acc ".rrn"
        ; decline Ins ".rbhis"
        ; decline Dat ".rbhyas"
        ; decline Abl ".rbhyas"
        ; decline Gen ".rr.naam"
        ; decline Gen ".r.naam" (* Veda, but .r metrically long *)
        ; decline Loc ".r.su"
        ])
   ] 
   ; Bare Noun bare
   ]
;
value build_mas_red stem entry = 
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "t"
        ; decline Nom "t"
        ; decline Acc "tam"
        ; decline Ins "taa"
        ; decline Dat "te"
        ; decline Abl "tas"
        ; decline Gen "tas"
        ; decline Loc "ti"
        ])
   ; (Dual, 
        [ decline Voc "tau"
        ; decline Nom "tau"
        ; decline Acc "tau"
        ; decline Ins "dbhyaam"
        ; decline Dat "dbhyaam"
        ; decline Abl "dbhyaam"
        ; decline Gen "tos"
        ; decline Loc "tos"
        ])
   ; (Plural,
        [ decline Voc "tas"
        ; decline Nom "tas"
        ; decline Acc "tas"
        ; decline Ins "dbhis"
        ; decline Dat "dbhyas"
        ; decline Abl "dbhyas"
        ; decline Gen "taam"
        ; decline Loc "tsu"
        ])
   ] 
   ; Indecl Tas (fix stem "tas")
   ]
;
value build_mas_at stem entry = 
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "n"
        ; decline Nom "n"
        ; decline Acc "ntam"
        ; decline Ins "taa"
        ; decline Dat "te"
        ; decline Abl "tas"
        ; decline Gen "tas"
        ; decline Loc "ti"
        ])
   ; (Dual, 
        [ decline Voc "ntau"
        ; decline Nom "ntau"
        ; decline Acc "ntau"
        ; decline Ins "dbhyaam"
        ; decline Dat "dbhyaam"
        ; decline Abl "dbhyaam"
        ; decline Gen "tos"
        ; decline Loc "tos"
        ])
   ; (Plural,
        [ decline Voc "ntas"
        ; decline Nom "ntas"
        ; decline Acc "tas"
        ; decline Ins "dbhis"
        ; decline Dat "dbhyas"
        ; decline Abl "dbhyas"
        ; decline Gen "taam"
        ; decline Loc "tsu"
        ])
   ]
   ; Bare Noun (wrap stem 32) (* at - e.g. b.rhadazva *)
   ; Avyayaf (fix stem "ntam") (* tam ? *)
   ]
;
value build_mas_mat stem entry = (* poss adj mas in -mat or -vat *)
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "an"
        ; decline Nom "aan"
        ; decline Acc "antam"
        ; decline Ins "ataa"
        ; decline Dat "ate"
        ; decline Abl "atas"
        ; decline Gen "atas"
        ; decline Loc "ati"
        ])
   ; (Dual, 
        [ decline Voc "antau"
        ; decline Nom "antau"
        ; decline Acc "antau"
        ; decline Ins "adbhyaam"
        ; decline Dat "adbhyaam"
        ; decline Abl "adbhyaam"
        ; decline Gen "atos"
        ; decline Loc "atos"
        ])
   ; (Plural,
        [ decline Voc "antas"
        ; decline Nom "antas"
        ; decline Acc "atas"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ] 
   ; Bare Noun (mirror [ 32 :: [ 1 :: stem ] ]) (* mat - e.g. zriimat *)
   ; Avyayaf (fix stem "antam") (* atam ? *)
   ]
;
value build_mas_mahat stem entry = 
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "aan"
        ; decline Nom "aan"
        ; decline Acc "aantam"
        ; decline Ins "ataa"
        ; decline Dat "ate"
        ; decline Abl "atas"
        ; decline Gen "atas"
        ; decline Loc "ati"
        ])
   ; (Dual, 
        [ decline Voc "aantau"
        ; decline Nom "aantau"
        ; decline Acc "aantau"
        ; decline Ins "adbhyaam"
        ; decline Dat "adbhyaam"
        ; decline Abl "adbhyaam"
        ; decline Gen "atos"
        ; decline Loc "atos"
        ])
   ; (Plural,
        [ decline Voc "aantas"
        ; decline Nom "aantas"
        ; decline Acc "atas"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ]
   ; Bare Noun (wrap stem 2) (* mahaa- *)
   ; Bare Noun (mirror [ 32 :: [ 1 :: stem ] ]) (* mahat- rarer *) 
   ; Cvi (wrap stem 4)
   ; Avyayaf (fix stem "aantam") (* atam ? *)
   ]
;
(* stems having a consonant before man or van have vocalic endings an *)
value avocalic = fun
  [ [ last :: _ ] -> not (Phonetics.vowel last)
  | [] -> failwith "Nouns.avocalic: empty stem"
  ]
;
(* NB impossible to factorise with [build_van] because "mne" and not "nne" *) 
value build_man g stem entry = 
  let vedic_drop = match entry with (* Whitney§425e *)
      [ "mahiman" | "prathiman" | "variman" | "daaman" | "preman" | "bhuuman"
          -> True 
      | _ -> False
      ] 
  and avoc = avocalic stem in 
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun g
   [ (Singular,  
        [ decline Voc "man"
        ; decline Nom (if g=Neu then "ma" else "maa")
        ; decline Acc (if g=Neu then "ma" else "maanam")
        ; decline Ins (if avoc then "manaa" else "mnaa")
        ; decline Dat (if avoc then "mane" else "mne")
        ; decline Abl (if avoc then "manas" else "mnas")
        ; decline Gen (if avoc then "manas" else "mnas")
        ; decline Loc "mani"
        ] @ (if g=Neu then if entry = "naaman" then [] 
                           else [ decline Voc "ma" ] (* Kaatyaayana *)
             else []) 
          @ (if vedic_drop then [ decline Ins "naa" ] else [])
          @ (if avoc then [] else [ decline Loc "mni" ]))
   ; (Dual, (if g=Neu then 
        [ decline Voc (if avoc then "manii" else "mnii")
        ; decline Nom (if avoc then "manii" else "mnii")
        ; decline Acc (if avoc then "manii" else "mnii")
        ] 
             else 
        [ decline Voc "maanau"
        ; decline Nom "maanau"
        ; decline Acc "maanau"
        ]) @
        [ decline Ins "mabhyaam"
        ; decline Dat "mabhyaam"
        ; decline Abl "mabhyaam"
        ; decline Gen (if avoc then "manos" else "mnos")
        ; decline Loc (if avoc then "manos" else "mnos")
        ])
   ; (Plural, if g=Neu then 
        [ decline Voc "maani"
        ; decline Nom "maani"
        ; decline Acc "maani"
        ]     else
        [ decline Voc "maanas"
        ; decline Nom "maanas"
        ; decline Acc (if avoc then "manas" else "mnas")
        ]) 
   ; (Plural,
        [ decline Ins "mabhis"
        ; decline Dat "mabhyas"
        ; decline Abl "mabhyas"
        ; decline Gen (if avoc then "manaam" else "mnaam")
        ; decline Loc "masu"
        ])
   ]
   ; Avyayaf (fix stem "mam")
   ; Indecl Tas (fix stem "matas")
   ] @ (if entry = "dharman" then [] (* redundant with dharma *)
        else [ Bare Noun (mirror [ 1 :: [ 41 :: stem ]]) ])
     @ (if g=Neu && man_iiv entry then [ Cvi (mirror [ 4 :: [ 41 :: stem ]]) ] 
        else [])
     @ if g=Neu then [ Avyayaf (fix stem "ma") ] else []) (* \Pan{5,4,109} *)
;
value build_man_god stem entry = (* Aryaman Whitney §426a; Kale §118 *)
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun Mas
   [ (Singular,  
        [ decline Voc "man"
        ; decline Nom "maa"
        ; decline Acc "manam"
        ; decline Ins "mnaa" (* aryam.naa and not *arya.n.naa *)
        ; decline Dat "mne"  (* above forbids merging with [build_an_god] *)
        ; decline Abl "mnas"
        ; decline Gen "mnas" 
        ; decline Loc "mani"
        ; decline Loc "mni"
        ])
   ; (Dual, 
        [ decline Voc "manau"
        ; decline Nom "manau"
        ; decline Acc "manau"
        ; decline Ins "mabhyaam"
        ; decline Dat "mabhyaam"
        ; decline Abl "mabhyaam"
        ; decline Gen "mnos"
        ; decline Loc "mnos"
        ])
   ; (Plural, 
        [ decline Voc "manas"
        ; decline Nom "manas"
        ; decline Acc "mnas"
        ; decline Ins "mabhis"
        ; decline Dat "mabhyas"
        ; decline Abl "mabhyas"
        ; decline Gen "mnaam"
        ; decline Loc "masu"
        ])
   ]
   ; Bare Noun (mirror [ 1 :: [ 41 :: stem ]])
   ])
;
value build_van g stem entry = 
  let avoc = avocalic stem in
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun g
   [ (Singular,  
        [ decline Voc "van"
        ; decline Nom (if entry = "piivan" then "vaan" (* Gonda *)
                       else if g=Neu then "va" else "vaa")
        ; decline Acc (if g=Neu then "va" else "vaanam")
        ; decline Ins (if avoc then "vanaa" else "vnaa")
        ; decline Dat (if avoc then "vane" else "vne")
        ; decline Abl (if avoc then "vanas" else "vnas")
        ; decline Gen (if avoc then "vanas" else "vnas")
        ; decline Loc "vani"
        ] @ (if g=Neu then [ decline Voc "va" ] else [])
          @ (if avoc then [] else [ decline Loc "vni" ]))
   ; (Dual, (if g=Neu then 
        [ decline Voc "vanii"
        ; decline Voc "vnii" (* if avoc ? *)
        ; decline Nom "vanii"
        ; decline Nom "vnii" (* if avoc ? *)
        ; decline Acc "vanii"
        ; decline Acc "vnii" (* if avoc ? *)
        ] 
            else 
        [ decline Voc "vaanau"
        ; decline Nom "vaanau"
        ; decline Acc "vaanau"
        ]) @
        [ decline Ins "vabhyaam"
        ; decline Dat "vabhyaam"
        ; decline Abl "vabhyaam"
        ; decline Gen (if avoc then "vanos" else "vnos")
        ; decline Loc (if avoc then "vanos" else "vnos")
        ])
   ; (Plural, if g=Neu then 
        [ decline Voc "vaani"
        ; decline Nom "vaani"
        ; decline Acc "vaani"
        ] else
        [ decline Voc "vaanas"
        ; decline Nom "vaanas"
        ; decline Acc (if avoc then "vanas" else "vnas")
        ]) 
   ; (Plural,
        [ decline Ins "vabhis"
        ; decline Dat "vabhyas"
        ; decline Abl "vabhyas"
        ; decline Gen (if avoc then "vanaam" else "vnaam")
        ; decline Loc "vasu"
        ])
   ]
   ; Bare Noun (mirror [ 1 :: [ 45 :: stem ]])
   ; Avyayaf (fix stem "vam")
   ; Indecl Tas (fix stem "vatas")
   ]
   @ if g=Neu then [ Avyayaf (fix stem "va") ] else []) (* \Pan{5,4,109} *)

;
value build_an g stem entry = 
  let decline case suff = (case,fix stem suff) in
  enter entry (
   [ Declined Noun g
   [ (Singular,  
        [ decline Voc "an"
        ; decline Nom (if g=Neu then "a" else "aa")
        ; decline Acc (if g=Neu then "a" else "aanam")
        ; decline Ins "naa"
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "ani"
        ; decline Loc "ni"
        ] @ (if g=Neu then 
        [ decline Voc "a" ] else []))
   ; (Dual, (if g=Neu then 
        [ decline Voc "anii"
        ; decline Voc "nii"
        ; decline Nom "anii"
        ; decline Nom "nii"
        ; decline Acc "anii"
        ; decline Acc "nii"
        ] 
             else 
        [ decline Voc "aanau"
        ; decline Nom "aanau"
        ; decline Acc "aanau"
        ]) @
        [ decline Ins "abhyaam"
        ; decline Dat "abhyaam"
        ; decline Abl "abhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ])
   ; (Plural, if g=Neu then 
        [ decline Voc "aani"
        ; decline Nom "aani"
        ; decline Acc "aani"
        ]     else
        [ decline Voc "aanas"
        ; decline Nom "aanas"
        ; decline Acc "nas"
        ]) 
   ; (Plural,
        [ decline Ins "abhis"
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "naam"
        ; decline Loc "asu"
        ])
   ]
   ; Bare Noun (wrap stem 1)
   ; Avyayaf (fix stem "am") 
   ; Indecl Tas (fix stem "atas")
   ] @ if g=Neu then [ Avyayaf (fix stem "a") ] else []) (* \Pan{5,4,109} *)
;
value build_an_god stem entry = (* Whitney §426a Un{1,157} *)
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,  
        [ decline Voc "an"
        ; decline Nom "aa"
        ; decline Acc "anam"
        ; decline Ins "naa"
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "ani"
        ; decline Loc "ni"
        ])
   ; (Dual, 
        [ decline Voc "anau"
        ; decline Nom "anau"
        ; decline Acc "anau"
        ; decline Ins "abhyaam"
        ; decline Dat "abhyaam"
        ; decline Abl "abhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ])
   ; (Plural, 
        [ decline Voc "anas"
        ; decline Nom "anas"
        ; decline Acc "nas"
        ; decline Ins "abhis"
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "naam"
        ; decline Loc "asu"
        ])
   ]
   ; Bare Noun (wrap stem 1)
   ] 
;
value build_sp_an stem entry = 
(* Whitney§432 these stems substitute the following for Voc Nom Acc :
   "yakan" \R "yak.rt"
   "zakan" \R "zak.rt"
   "udan" \R "udaka"
   "yuu.san" \R "yuu.sa"
   "do.san" \R "dos"
   "asan" \R "as.rj"
   "aasan" \R "aasya" 
   Kale§129 Renou§241d *)
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Ins "naa"
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "ani"
        ])
   ; (Dual, 
        [ decline Ins "abhyaam"
        ; decline Dat "abhyaam"
        ; decline Abl "abhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ])
   ; (Plural,
        [ decline Acc "aani" (* Kale§129 zakan but not yakan, Renou: trouble *)
        ; decline Ins "abhis" 
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "naam"
        ; decline Loc "asu"
        ])
   ]
   ; Bare Noun (wrap stem 1)
(* ; Avyayaf ? *)
   ]
;
value build_han stem entry = (* stem = ...-han Whitney§402 *)
  (* g=Mas only, since g=Neu is dubious specially -ha *)
  let decline case suff = (case,fix stem suff) 
  and declino case suff = (case,fixno stem suff) in (* no retroflexion of n *)
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "han"
        ; decline Nom "haa"   (* if g=Neu then "ha" else "haa" *)
        ; decline Acc "hanam" (* if g=Neu then "ha" else "hanam" *)
        ; declino Ins "ghnaa" (* v.rtraghnaa, not *v.rtragh.naa Whitney§195a *)
        ; declino Dat "ghne"
        ; declino Abl "ghnas"
        ; declino Gen "ghnas"
        ; declino Loc "ghni"
        ; decline Loc "hani"
        ])                 (* @ (if g=Neu then [ decline Voc "ha" ] else [])) *)
   ; (Dual, (* if g=Neu then 
                  [ decline Voc "hanii"
                  ; declino Voc "ghnii"
                  ; decline Nom "hanii"
                  ; declino Nom "ghnii"
                  ; decline Acc "hanii"
                  ; declino Acc "ghnii"
                  ]
               else *)
        [ decline Voc "hanau"
        ; decline Nom "hanau"
        ; decline Acc "hanau"
        ; decline Ins "habhyaam"
        ; decline Dat "habhyaam"
        ; decline Abl "habhyaam"
        ; declino Gen "ghnos"
        ; declino Loc "ghnos"
        ])
   ; (Plural, (* if g=Neu then 
                    [ decline Voc "haani"
                    ; decline Nom "haani"
                    ; decline Acc "haani"
                    ]
                 else *)
        [ decline Voc "hanas"
        ; decline Nom "hanas"
        ; declino Acc "ghnas"
        ; decline Ins "habhis"
        ; decline Dat "habhyas"
        ; decline Abl "habhyas"
        ; declino Gen "ghnaam"
        ; decline Loc "hasu"
        ])
   ] 
   ; Avyayaf (fix stem "hanam")
   ]
;
value build_mas_zvan stem entry =  (* \Pan{6,4,133} Whitney§427 *)
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular, 
        [ decline Voc "van"
        ; decline Nom "vaa"
        ; decline Acc "vaanam"
        ; decline Ins "unaa"
        ; decline Dat "une"
        ; decline Abl "unas"
        ; decline Gen "unas"
        ; decline Loc "uni"
        ])
   ; (Dual,
        [ decline Voc "vaanau"
        ; decline Nom "vaanau"
        ; decline Acc "vaanau"
        ; decline Ins "vabhyaam"
        ; decline Dat "vabhyaam"
        ; decline Abl "vabhyaam"
        ; decline Gen "unos"
        ; decline Loc "unos"
        ])
   ; (Plural,
        [ decline Voc "vaanas"
        ; decline Nom "vaanas"
        ; decline Acc "unas"
        ; decline Ins "vabhis"
        ; decline Dat "vabhyas"
        ; decline Abl "vabhyas"
        ; decline Gen "unaam"
        ; decline Loc "vasu"
        ])
   ]
   (* Bare Noun (code "zunas") abl/gen pour zuna.hzepa non génératif *)
   (* Bare Noun (code "zvaa") zvaapada avec nom. non génératif *)
   ; Bare Noun (mirror [ 1 :: [ 45 :: stem ] ]) (* eg zva-v.rtti *)
   ; Avyayaf (fix stem "vaanam") (* "vam" ? *)
   ]
;
value build_athin stem entry = (* pathin, supathin, mathin *)
(* \Pan{7.1.85}  pathi-mathy-.rbhuk.saam aat *)
  let decline case suff = (case,fix stem suff) in (* stem = pa for pathin *)
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "nthaas"
        ; decline Nom "nthaas"
        ; decline Acc "nthaanam"
        ; decline Ins "thaa"
        ; decline Dat "the"
        ; decline Abl "thas"
        ; decline Gen "thas"
        ; decline Loc "thi"
        ])
   ; (Dual,
        [ decline Voc "nthaanau"
        ; decline Nom "nthaanau"
        ; decline Acc "nthaanau"
        ; decline Ins "thibhyaam"
        ; decline Dat "thibhyaam"
        ; decline Abl "thibhyaam"
        ; decline Gen "thos"
        ; decline Loc "thos"
        ])
   ; (Plural,
        [ decline Voc "nthaanas"
        ; decline Nom "nthaanas"
        ; decline Acc "thas"
        ; decline Ins "thibhis"
        ; decline Dat "thibhyas"
        ; decline Abl "thibhyas"
        ; decline Gen "thaam"
        ; decline Loc "thisu"
        ])
   ]
   ; Bare Noun (fix stem "thi")  (* pathi- *)
   ; Avyayaf (fix stem "tham") (* upapatham *)
   ]
;
value build_ribhuksin stem entry =
  let decline case suff = (case,fix stem suff) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aanam"
        ; decline Acc "anam" (* \Pan{6,4,9} *)
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Voc "aanau"
        ; decline Nom "aanau"
        ; decline Acc "aanau"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural,
        [ decline Voc "aanas"
        ; decline Nom "aanas"
        ; decline Acc "as"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "aam"
        ; decline Loc "asu"
        ])
   ]
   ]
;
value build_mas_yuvan entry =  (* \Pan{6,4,133} *)
  let stem = [ 42 ] (* y *) in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "uvan"
        ; decline Nom "uvaa"
        ; decline Acc "uvaanam"
        ; decline Ins "uunaa"
        ; decline Dat "uune"
        ; decline Abl "uunas"
        ; decline Gen "uunas"
        ; decline Loc "uuni"
        ])
   ; (Dual, 
        [ decline Voc "uvaanau"
        ; decline Nom "uvaanau"
        ; decline Acc "uvaanau"
        ; decline Ins "uvabhyaam"
        ; decline Dat "uvabhyaam"
        ; decline Abl "uvabhyaam"
        ; decline Gen "uunos"
        ; decline Loc "uunos"
        ])
   ; (Plural,
        [ decline Voc "uvaanas"
        ; decline Nom "uvaanas"
        ; decline Acc "uunas"
        ; decline Ins "uvabhis"
        ; decline Dat "uvabhyas"
        ; decline Abl "uvabhyas"
        ; decline Gen "uunaam"
        ; decline Loc "uvasu"
        ])
   ]
   ; Bare Noun (code "yuva") 
   ; Avyayaf (code "yuvam") (* ? *)
   ]
;
value build_mas_maghavan entry = (* \Pan{6,4,133} *)
  let stem = revcode "magh" in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "avan"
        ; decline Nom "avaa"
        ; decline Acc "avaanam"
        ; decline Ins "onaa"
        ; decline Dat "one"
        ; decline Abl "onas"
        ; decline Gen "onas"
        ; decline Loc "oni"
        ])
   ; (Dual, 
        [ decline Voc "avaanau"
        ; decline Nom "avaanau"
        ; decline Acc "avaanau"
        ; decline Ins "avabhyaam"
        ; decline Dat "avabhyaam"
        ; decline Abl "avabhyaam"
        ; decline Gen "onos"
        ; decline Loc "onos"
        ])
   ; (Plural, 
        [ decline Voc "avaanas"
        ; decline Nom "avaanas"
        ; decline Acc "onas"
        ; decline Ins "avabhis"
        ; decline Dat "avabhyas"
        ; decline Abl "avabhyas"
        ; decline Gen "onaam"
        ; decline Loc "avasu"
        ])
   ] 
   ; Avyayaf (fix stem "avam") (* ? *)
];
 
value build_mas_in stem entry = 
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 3 in 
  enter entry (
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "in"
        ; decline Nom "ii"
        ; decline Acc "inam"
        ; decline Ins "inaa"
        ; decline Dat "ine"
        ; decline Abl "inas"
        ; decline Gen "inas"
        ; decline Loc "ini"
        ])
   ; (Dual,
        [ decline Voc "inau"
        ; decline Nom "inau"
        ; decline Acc "inau"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "inos"
        ; decline Loc "inos"
        ])
   ; (Plural,
        [ decline Voc "inas"
        ; decline Nom "inas"
        ; decline Acc "inas"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "inaam"
        ; decline Loc "i.su"
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare
   ; Cvi (wrap stem 4) (* "saak.sin" "sthaayin" overgenerates with vi.saayin *)
   ])
;
value build_as gen stem entry = 
  let decline case suff = (case,fix stem suff) 
  and rstem = [ 48 :: [ 1 :: stem ] ] in 
  enter entry (
   [ Declined Noun gen
   [ (Singular, let l =
        [ decline Voc "as"
        ; decline Nom (match gen with
           [ Mas -> match entry with (* gram Muller p72,  Whitney §416-419 *)
                    [ "anehas" | "uzanas" | "da.mzas" (* Puruda.mzas *) -> "aa" 
                    | _ -> "aas" (* Kane§108 candramas vedhas su/dur/unmanas *)
                    ]  
           | Fem -> match entry with
                    [ "anehas" -> "aa" (* Whitney §419 *)
                    | _ -> "aas" 
                    ]
           | Neu -> "as" (* manas payas vyas? avas1 zreyas saras vacas *)
           | _ -> raise (Control.Anomaly "Nouns")
           ])
        ; decline Acc (match gen with
           [ Mas | Fem -> "asam"
           | Neu  -> "as"
           | _ -> raise (Control.Anomaly "Nouns")
           ])
        ; decline Ins "asaa"
        ; decline Dat "ase"
        ; decline Abl "asas"
        ; decline Gen "asas"
        ; decline Loc "asi"
        ] in if entry = "uzanas" && gen = Mas then (* gram Muller p 72 *)
                [ decline Voc "a"; decline Voc "an" ] @ l 
             else l)
   ; (Dual,
        let direct = match gen with
           [ Mas | Fem -> "asau"
           | Neu  -> "asii"
           | _ -> raise (Control.Anomaly "Nouns")
           ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "obhyaam"
        ; decline Dat "obhyaam"
        ; decline Abl "obhyaam"
        ; decline Gen "asos"
        ; decline Loc "asos"
        ])
   ; (Plural, 
        let direct = match gen with
          [ Mas | Fem -> "asas"
          | Neu  -> "aa.msi" 
(* eg chandaa.msi: chandas-as Pan{7,1,20}{1,1,42} chandas-zi Pan{7,1,72} 
      chandans-i Pan{6,4,10} chandaans-i Pan{8,3,24} chandaa.msi *)
          | _ -> raise (Control.Anomaly "Nouns")
          ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "obhis"
        ; decline Dat "obhyas"
        ; decline Abl "obhyas"
        ; decline Gen "asaam"
        ; decline Loc "a.hsu" (* decline Loc "assu" *) (* Kane§108 opt "astu" *)
        ])
   ]            
   ; Bare Noun (mirror rstem) (* as *)
   ; Indecl Tas (fix rstem "tas") (* eg manastas *)
   ]
     @ (match entry with 
         ["uras" | "manas" -> [ Bare Noun (wrap stem 1) ] (* ura- mana- *)
         | _ -> []
         ])
     @ (match entry with
         [ "anas" | "manas" | "cetas" | "jaras" -> [ Avyayaf (fix stem "asam") ]
         | "nabhas" -> [ Avyayaf (fix stem "as"); Avyayaf (fix stem "yam") ]
         | _ -> []
         ])
     @ (if gen=Neu && as_iiv entry then [ Cvi (wrap stem 4) ] else []))
;
value build_maas () = 
  let decline case form = (case,code form) in 
  enter "maas"
   [ Declined Noun Mas
   [ (Singular, 
        [ decline Nom "maas" (* no Acc Voc ? *)
        ; decline Ins "maasaa"
        ; decline Dat "maase"
        ; decline Abl "maasas"
        ; decline Gen "maasas"
        ; decline Loc "maasi"
        ])
   ; (Dual,
        [ decline Ins "maadbhyaam" (* ou "maabhyaam" ?? *)
        ; decline Ins "maabhyaam" (* Siddhaanta kaumudii - Jha *)
        ; decline Dat "maadbhyaam"
        ; decline Abl "maadbhyaam"
        ; decline Gen "maasos"
        ; decline Loc "maasos"
        ])
   ; (Plural, 
        [ decline Ins "maadbhis"
        ; decline Dat "maadbhyas"
        ; decline Abl "maadbhyas"
        ; decline Gen "maasaam"
        ; decline Loc "maa.hsu" (* maassu *)
        ])
   ]  
   ]
;
value build_nas entry = 
  let decline case form = (case,code form) in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular, 
        [ decline Ins "nasaa"
        ; decline Dat "nase"
        ; decline Abl "nasas"
        ; decline Gen "nasas"
        ; decline Loc "nasi"
        ])
   ; (Dual,
        [ decline Nom "naasaa" (* RV narines Whitney§397 *)
        ; decline Gen "nasos"
        ; decline Loc "nasos"
        ])
   ]            
   ]
;
value build_dos gen entry = (* Kale§108a *)
  let decline case form = (case,code form) in 
  enter entry 
   [ Declined Noun gen
   [ (Singular, 
        [ decline Voc "dos"
        ; decline Nom "dos"
        ; decline Acc "dos"
        ; decline Ins "do.saa"
        ; decline Dat "do.se"
        ; decline Abl "do.sas"
        ; decline Gen "do.sas"
        ; decline Loc "do.si"
        ])
   ; (Dual, let form = match gen with 
                       [ Mas | Fem -> "do.sau"
                       | Neu  -> "do.sii"
                       | _ -> raise (Control.Anomaly "Nouns")
                       ] in
        [ decline Voc form
        ; decline Nom form
        ; decline Acc form
        ; decline Ins "dorbhyaam"
        ; decline Dat "dorbhyaam"
        ; decline Abl "dorbhyaam"
        ; decline Gen "dor.sos"
        ; decline Loc "dor.sos"
        ])
   ; (Plural, let form = match gen with
          [ Mas | Fem -> "do.sas"
          | Neu  -> "do.m.si"
          | _ -> raise (Control.Anomaly "Nouns")
          ] in
        [ decline Voc form
        ; decline Nom form
        ; decline Acc form
        ; decline Ins "dorbhis"
        ; decline Dat "dorbhyas"
        ; decline Abl "dorbhyas"
        ; decline Gen "do.saam"
        ; decline Loc "do.h.su"
        ])
   ]            
   ]
;
value build_is gen stem entry =
  let decline case suff = (case,fix stem suff) 
  and rstem = [ 48 :: [ 3 :: stem ] ] in 
  let bare = mirror rstem in
  enter entry 
   [ Declined Noun gen
   [ (Singular, 
        [ decline Voc "is"
        ; decline Nom "is"
        ; decline Acc (match gen with
          [ Mas | Fem -> "i.sam"
          | Neu  -> "is"
          | _ -> raise (Control.Anomaly "Nouns")
          ])
        ; decline Ins "i.saa"
        ; decline Dat "i.se"
        ; decline Abl "i.sas"
        ; decline Gen "i.sas"
        ; decline Loc "i.si"
        ])
   ; (Dual, 
        let direct = match gen with
           [ Mas | Fem -> "i.sau"
           | Neu  -> "i.sii"
           | _ -> raise (Control.Anomaly "Nouns")
           ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "irbhyaam"
        ; decline Dat "irbhyaam"
        ; decline Abl "irbhyaam"
        ; decline Gen "i.sos"
        ; decline Loc "i.sos"
        ])
   ; (Plural,
        let direct = match gen with
           [ Mas | Fem -> "i.sas"
           | Neu  -> "ii.msi"
           | _ -> raise (Control.Anomaly "Nouns")
           ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "irbhis"
        ; decline Dat "irbhyas"
        ; decline Abl "irbhyas"
        ; decline Gen "i.saam"
        ; decline Loc "i.h.su" (* decline Loc "i.s.su" *)
        ])
   ]             
   ; Bare Noun bare (* is *)
   ; Indecl Tas (fix rstem "tas") 
   ; Avyayaf bare
   ]
;
value build_us gen stem entry = 
  let decline case suff = (case,fix stem suff) 
  and rstem = [ 48 :: [ 5 :: stem ] ] in 
  let bare = mirror rstem in 
  enter entry 
   [ Declined Noun gen
   [ (Singular,
        [ decline Voc "us"
        ; decline Nom "us"
        ; decline Acc (match gen with
          [ Mas | Fem -> "u.sam"
          | Neu  -> "us"
          | _ -> raise (Control.Anomaly "Nouns")
          ])
        ; decline Ins "u.saa"
        ; decline Dat "u.se"
        ; decline Abl "u.sas"
        ; decline Gen "u.sas"
        ; decline Loc "u.si"
        ])
   ; (Dual, 
        let direct = match gen with
           [ Mas | Fem -> "u.sau"
           | Neu  -> "u.sii"
           | _ -> raise (Control.Anomaly "Nouns")
           ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "urbhyaam"
        ; decline Dat "urbhyaam"
        ; decline Abl "urbhyaam"
        ; decline Gen "u.sos"
        ; decline Loc "u.sos"
        ])
   ; (Plural,
      let direct = match gen with
          [ Mas | Fem -> "u.sas"
          | Neu  -> "uu.msi"
          | _ -> raise (Control.Anomaly "Nouns")
          ] in
        [ decline Voc direct
        ; decline Nom direct
        ; decline Acc direct
        ; decline Ins "urbhis"
        ; decline Dat "urbhyas"
        ; decline Abl "urbhyas"
        ; decline Gen "u.saam"
        ; decline Loc "u.h.su" (* decline Loc "u.s.su" *)
        ])
   ]             
   ; Bare Noun bare (* us *)
   ; Cvi (wrap stem 6) (* arus cak.sus *)
   ; Indecl Tas (fix rstem "tas") 
   ; Avyayaf bare 
   ]
;
value build_mas_yas stem entry = 
  let bare = fix stem "as"
  and decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "an"
        ; decline Nom "aan"
        ; decline Acc "aa.msam"
        ; decline Ins "asaa"
        ; decline Dat "ase"
        ; decline Abl "asas"
        ; decline Gen "asas"
        ; decline Loc "asi"
        ])
   ; (Dual, 
        [ decline Voc "aa.msau"
        ; decline Nom "aa.msau"
        ; decline Acc "aa.msau"
        ; decline Ins "obhyaam"
        ; decline Dat "obhyaam"
        ; decline Abl "obhyaam"
        ; decline Gen "asos"
        ; decline Loc "asos"
        ])
   ; (Plural,
        [ decline Voc "aa.msas"
        ; decline Nom "aa.msas"
        ; decline Acc "asas"
        ; decline Ins "obhis"
        ; decline Dat "obhyas"
        ; decline Abl "obhyas"
        ; decline Gen "asaam"
        ; decline Loc "a.hsu" (* decline Loc "assu" *)
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare
   ]
;
value build_mas_vas stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "van"
        ; decline Nom "vaan"
        ; decline Acc "vaa.msam"
        ; decline Ins "u.saa"
        ; decline Dat "u.se"
        ; decline Abl "u.sas"
        ; decline Gen "u.sas"
        ; decline Loc "u.si"
        ])
   ; (Dual, 
        [ decline Voc "vaa.msau"
        ; decline Nom "vaa.msau"
        ; decline Acc "vaa.msau"
        ; decline Ins "vadbhyaam"
        ; decline Dat "vadbhyaam"
        ; decline Abl "vadbhyaam"
        ; decline Gen "u.sos"
        ; decline Loc "u.sos"
        ])
   ; (Plural,
        [ decline Voc "vaa.msas"
        ; decline Nom "vaa.msas"
        ; decline Acc "u.sas"
        ; decline Ins "vadbhis"
        ; decline Dat "vadbhyas"
        ; decline Abl "vadbhyas"
        ; decline Gen "u.saam"
        ; decline Loc "vatsu"
        ])
   ]
   ; Bare Noun (fix stem "vat") (* eg vidvat- *)
   ; Avyayaf (fix stem "vas")
   ]
;
(* i is dropped before u.s - Macdonnel §89a *)
value build_mas_ivas stem entry = 
  let decline case suff = (case,fix stem suff) 
  and declinev case suff = (case,fix stem ("i" ^ suff)) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ declinev Voc "van"
        ; declinev Nom "vaan"
        ; declinev Acc "vaa.msam"
        ; decline Ins "u.saa"
        ; decline Dat "u.se"
        ; decline Abl "u.sas"
        ; decline Gen "u.sas"
        ; decline Loc "u.si"
        ])
   ; (Dual, 
        [ declinev Voc "vaa.msau"
        ; declinev Nom "vaa.msau"
        ; declinev Acc "vaa.msau"
        ; declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ; decline Gen "u.sos"
        ; decline Loc "u.sos"
        ])
   ; (Plural,
        [ declinev Voc "vaa.msas"
        ; declinev Nom "vaa.msas"
        ; decline Acc "u.sas"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; decline Gen "u.saam"
        ; declinev Loc "vatsu"
        ])
   ] 
  ; Bare Noun (fix stem "ivat") 
  ; Avyayaf (fix stem "ivas")
   ]
;
value build_mas_aac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "f"
        ; decline Nom "f"
        ; decline Acc "~ncam"
        ; decline Ins "caa"
        ; decline Dat "ce"
        ; decline Abl "cas"
        ; decline Gen "cas"
        ; decline Loc "ci"
        ])
   ; (Dual, 
        [ decline Voc "~ncau"
        ; decline Nom "~ncau"
        ; decline Acc "~ncau"
        ; decline Ins "gbhyaam"
        ; decline Dat "gbhyaam"
        ; decline Abl "gbhyaam"
        ; decline Gen "cos"
        ; decline Loc "cos"
        ])
   ; (Plural,
        [ decline Voc "~ncas"
        ; decline Nom "~ncas"
        ; decline Acc "cas"
        ; decline Ins "gbhis"
        ; decline Dat "gbhyas"
        ; decline Abl "gbhyas"
        ; decline Gen "caam"
        ; decline Loc "k.su"
        ])
   ]
   ; Bare Noun (fix stem "f") (* nasale gutturale *)
   ; Avyayaf (fix stem "~nc") (* ? *)
   ]
;
value build_mas_yac stem entry = 
  let decline case suff = (case,fix stem suff) 
  and prevoc = if stem = revcode "tir" then "azc"
                                       else "iic" in
      (* exception tiryac -> weakest stem tiriic in prevocalic flexions *)
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "yaf"
        ; decline Nom "yaf"
        ; decline Acc "ya~ncam"
        ; decline Ins (prevoc ^ "aa")
        ; decline Dat (prevoc ^ "e")
        ; decline Abl (prevoc ^ "as")
        ; decline Gen (prevoc ^ "as")
        ; decline Loc (prevoc ^ "i")
        ])
   ; (Dual, 
        [ decline Voc "ya~ncau"
        ; decline Nom "ya~ncau"
        ; decline Acc "ya~ncau"
        ; decline Ins "yagbhyaam"
        ; decline Dat "yagbhyaam"
        ; decline Abl "yagbhyaam"
        ; decline Gen (prevoc ^ "os")
        ; decline Loc (prevoc ^ "os")
        ])
   ; (Plural,
        [ decline Voc "ya~ncas"
        ; decline Nom "ya~ncas"
        ; decline Acc (prevoc ^ "as")
        ; decline Ins "yagbhis"
        ; decline Dat "yagbhyas"
        ; decline Abl "yagbhyas"
        ; decline Gen (prevoc ^ "aam")
        ; decline Loc "yak.su"
        ])
   ] 
   ; Bare Noun (fix stem "yak") 
   ; Avyayaf (fix stem "yaf") (* ? *)
   ]
;
value build_mas_vac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "vaf"
        ; decline Nom "vaf"
        ; decline Acc "va~ncam"
        ; decline Ins "uucaa"
        ; decline Dat "uuce"
        ; decline Abl "uucas"
        ; decline Gen "uucas"
        ; decline Loc "uuci"
        ])
   ; (Dual, 
        [ decline Voc "va~ncau"
        ; decline Nom "va~ncau"
        ; decline Acc "va~ncau"
        ; decline Ins "vagbhyaam"
        ; decline Dat "vagbhyaam"
        ; decline Abl "vagbhyaam"
        ; decline Gen "uucos"
        ; decline Loc "uucos"
        ])
   ; (Plural, 
        [ decline Voc "va~ncas"
        ; decline Nom "va~ncas"
        ; decline Acc "uucas"
        ; decline Ins "vagbhis"
        ; decline Dat "vagbhyas"
        ; decline Abl "vagbhyas"
        ; decline Gen "uucaam"
        ; decline Loc "vak.su"
        ])
   ] 
   ; Bare Noun (fix stem "vak") 
   ; Avyayaf (fix stem "vaf") (* ? *)
   ]
;
value build_mas_ac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "af"
        ; decline Nom "af"
        ; decline Acc "a~ncam"
        ; decline Ins "iicaa"
        ; decline Dat "iice"
        ; decline Abl "iicas"
        ; decline Gen "iicas"
        ; decline Loc "iici"
        ])
   ; (Dual,
        [ decline Voc "a~ncau"
        ; decline Nom "a~ncau"
        ; decline Acc "a~ncau"
        ; decline Ins "agbhyaam"
        ; decline Dat "agbhyaam"
        ; decline Abl "agbhyaam"
        ; decline Gen "iicos"
        ; decline Loc "iicos"
        ])
   ; (Plural,
        [ decline Voc "a~ncas"
        ; decline Nom "a~ncas"
        ; decline Acc "iicas"
        ; decline Ins "agbhis"
        ; decline Dat "agbhyas"
        ; decline Abl "agbhyas"
        ; decline Gen "iicaam"
        ; decline Loc "ak.su"
        ])
   ] 
   ; Bare Noun (fix stem "ak") 
   ; Avyayaf (fix stem "af") (* ? *)
   ]
;
value build_pums pum pums entry = (* for pu.ms et napu.ms *)
(* hi.ms pu.ms no retroflexion of s - Whitney§183a Kale §113 *)
  let decline case suff = (case,List2.unstack pum (code suff)) 
  and declines case suff = (case,List2.unstack pums (code suff)) in
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline  Voc "an"
        ; decline  Nom "aan"
        ; decline  Acc "aa.msam"
        ; declines Ins "aa"
        ; declines Dat "e"
        ; declines Abl "as"
        ; declines Gen "as"
        ; declines Loc "i"
        ])
   ; (Dual, 
        [ decline  Voc "aa.msau"
        ; decline  Nom "aa.msau"
        ; decline  Acc "aa.msau"
        ; decline  Ins "bhyaam"
        ; decline  Dat "bhyaam"
        ; decline  Abl "bhyaam"
        ; declines Gen "os"
        ; declines Loc "os"
        ])
   ; (Plural, 
        [ decline  Voc "aa.msas"
        ; decline  Nom "aa.msas"
        ; declines Acc "as"
        ; decline  Ins "bhis"
        ; decline  Dat "bhyas"
        ; decline  Abl "bhyas"
        ; declines Gen "aam"
        ; declines Loc "u"
        ])
   ] 
   ; Bare Noun (mirror pum) (* for pul~~lifga *)
   ; Bare Noun (mirror pums) (* for pu.mzcala *)
   (* ; Avyayaf ? *)
   ]
;
value build_mas_vah stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "van"
        ; decline Nom "vaa.t"
        ; decline Acc "vaaham"
        ; decline Ins "ohaa" (* becomes auhaa by sandhi with a-   *)
        ; decline Dat "ohe"  (* Whitney 403 gives uuhaa etc       *)
        ; decline Abl "ohas" (* but has special sandhi rule §137c *)
        ; decline Gen "ohas"
        ; decline Loc "ohi"
        ])
   ; (Dual,
        [ decline Voc "vaahau"
        ; decline Nom "vaahau"
        ; decline Acc "vaahau"
        ; decline Ins "vaa.dbhyaam"
        ; decline Dat "vaa.dbhyaam"
        ; decline Abl "vaa.dbhyaam"
        ; decline Gen "ohos"
        ; decline Loc "ohos"
        ])
   ; (Plural,
        [ decline Voc "vaahas"
        ; decline Nom "vaahas"
        ; decline Acc "ohas"
        ; decline Ins "vaa.dbhis"
        ; decline Dat "vaa.dbhyas"
        ; decline Abl "vaa.dbhyas"
        ; decline Gen "ohaam"
        ; decline Loc "vaa.tsu"
        ])
   ] 
   ; Avyayaf (fix stem "vah")
   ]
;
value build_anadvah stem entry = (* ana.dvah *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "van"
        ; decline Nom "vaan"
        ; decline Acc "vaaham"
        ; decline Ins "uhaa"
        ; decline Dat "uhe"
        ; decline Abl "uhas"
        ; decline Gen "uhas"
        ; decline Loc "uhi"
        ])
   ; (Dual, 
        [ decline Voc "vaahau"
        ; decline Nom "vaahau"
        ; decline Acc "vaahau"
        ; decline Ins "udbhyaam"
        ; decline Dat "udbhyaam"
        ; decline Abl "udbhyaam"
        ; decline Gen "uhos"
        ; decline Loc "uhos"
        ])
   ; (Plural,
        [ decline Voc "vaahas"
        ; decline Nom "vaahas"
        ; decline Acc "uhas"
        ; decline Ins "udbhis"
        ; decline Dat "udbhyas"
        ; decline Abl "udbhyas"
        ; decline Gen "uhaam"
        ; decline Loc "utsu"
        ])
   ] 
   ]
;
value build_neu_a stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry (
   [ Declined Noun Neu
   [ (Singular,if entry = "ubha"  (* dual only *) then [] else 
        [ decline Voc "a"
       (* decline Voc "am" - rare - disconnected for avoiding overgeneration *)
        ; decline Nom "am"
        ; decline Acc "am"
        ; decline Ins "ena"
        ; decline Dat "aaya"
        ; decline Abl "aat"
        ; decline Gen "asya"
        ; decline Loc "e"
        ])
   ; (Dual, if entry = "eka" (* singular only *) then [] else 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, if entry = "ubha"  (* dual only *) 
              || entry = "eka"   (* singular only *) then [] else let l =
        [ decline Voc "aani"
        ; decline Nom "aani"
        ; decline Acc "aani"
        ; decline Ins "ais"
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "aanaam"
        ; decline Loc "esu"
        ] in if entry = "durita" then [ decline Nom "aa" :: l ] (* vedic *)
             else l)
   ]             
   ; Bare Noun (wrap stem 1) 
   ; Avyayaf (fix stem "am"); Avyayaf (fix stem "aat")
   ; Indecl Tas (fix stem "atas")
   ] @ (if a_n_iiv entry then [ Cvi (wrap stem 4) ] else []))
;
value adj_neu_i = fun (* Kale§70 *)
  [ "zuci" -> True | _ -> False ] (* add on demand *)
;
value build_neu_i trunc entry = (* stems in -i and -ii *)
  let stems = [ 3 :: trunc ] 
  and steml = [ 4 :: trunc ] in 
  let rstems = mirror stems
  and declines case suff = (case,fix stems suff) 
  and declinel case suff = (case,fix steml suff) 
  and declinem case suff = (case,fix trunc suff) in
  enter entry 
   [ Declined Noun Neu
   [ (Singular, let l = 
        [ declines Voc ""
        ; declines Nom ""
        ; declines Acc ""
        ; declines Ins "naa"
        ; declines Dat "ne"
        ; declines Abl "nas"
        ; declines Gen "nas"
        ; declines Loc "ni"
        ] in if adj_neu_i entry then (* Kale§70 : like Mas *)
             let l' = [ declinem Voc "e"
                      ; declinem Dat "aye"
                      ; declinem Abl "es"
                      ; declinem Gen "es"
                      ; declinem Loc "au"
                      ] in l @ l' 
             else l)
   ; (Dual, let l =
        [ declines Voc "nii"
        ; declines Nom "nii"
        ; declines Acc "nii"
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam"
        ; declines Gen "nos"
        ; declines Loc "nos"
        ] in if adj_neu_i entry then (* Kale§70 *)
             let l' = [ declinem Gen "yos"
                      ; declinem Loc "yos"
                      ] in l @ l' 
             else l)
   ; (Plural, 
        [ declinel Voc "ni"
        ; declinel Nom "ni"
        ; declinel Acc "ni"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]             
   ; Bare Noun rstems
   ; Avyayaf rstems
   ]
;
value adj_neu_u = fun (* Kale§70 *)
  [ "guru" -> True | _ -> False ] (* add on demand *)
;
value build_neu_u trunc entry = (* stems in -u and -uu *)
  let stems = [ 5 :: trunc ] 
  and steml = [ 6 :: trunc ] in 
  let declines case suff = (case,fix stems suff) 
  and declinel case suff = (case,fix steml suff) 
  and declinem case suff = (case,fix trunc suff) in
  enter entry 
   [ Declined Noun Neu
   [ (Singular, let l =
        [ declines Voc ""
        ; declines Nom ""
        ; declines Acc ""
        ; declines Ins "naa"
        ; declines Dat "ne"
        ; declines Abl "nas"
        ; declines Gen "nas"
        ; declines Loc "ni"
        ] in if adj_neu_u entry then (* Kale§70 : like Mas *)
             let l' = [ declinem Voc "o"
                      ; declinem Dat "ave"
                      ; declinem Abl "es"
                      ; declinem Gen "es"
                      ; declinem Loc "au"
                      ] in l @ l' 
             else l)
   ; (Dual, let l =
        [ declines Voc "nii"
        ; declines Nom "nii"
        ; declines Acc "nii"
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam"
        ; declines Gen "nos"
        ; declines Loc "nos"
        ] in if adj_neu_u entry then (* Kale§70 *)
             let l' = [ declinem Gen "vos"
                      ; declinem Loc "vos"
                      ] in l @ l' 
             else l)
   ; (Plural, 
        [ declinel Voc "ni"
        ; declinel Nom "ni"
        ; declinel Acc "ni"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]            
   ; Bare Noun (mirror stems)
   ; Avyayaf (mirror stems)
   ; Indecl Tas (fix stems "tas") (* eg vastutas *)
   ]
;
value build_neu_ri trunc entry = 
  let stems = [ 7 :: trunc ]  
  and steml = [ 8 :: trunc ] in 
  let declines case suff = (case,fix stems suff) 
  and declinel case suff = (case,fix steml suff) in
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ declines Voc ""
        ; declines Nom ""
        ; declines Acc ""
        ; declines Ins "naa"
        ; declines Dat "ne"
        ; declines Abl "nas"
        ; declines Gen "nas"
        ; declines Loc "ni"
        ])
   ; (Dual,
        [ declines Voc "nii"
        ; declines Nom "nii"
        ; declines Acc "nii"
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam"
        ; declines Gen "nos"
        ; declines Loc "nos"
        ])
   ; (Plural, 
        [ declinel Voc "ni"
        ; declinel Nom "ni"
        ; declinel Acc "ni"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ] 
   ; Bare Noun (mirror stems)
   ; Avyayaf (mirror stems)
   ]
;
value build_neu_yas stem entry =
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "as"
        ; decline Nom "as"
        ; decline Acc "as"
        ; decline Ins "asaa"
        ; decline Dat "ase"
        ; decline Abl "asas"
        ; decline Gen "asas"
        ; decline Loc "asi"
        ])
   ; (Dual,
        [ decline Voc "asii"
        ; decline Nom "asii"
        ; decline Acc "asii"
        ; decline Ins "obhyaam"
        ; decline Dat "obhyaam"
        ; decline Abl "obhyaam"
        ; decline Gen "asos"
        ; decline Loc "asos"
        ])
   ; (Plural,
        [ decline Voc "aa.msi"
        ; decline Nom "aa.msi"
        ; decline Acc "aa.msi"
        ; decline Ins "obhis"
        ; decline Dat "obhyas"
        ; decline Abl "obhyas"
        ; decline Gen "asaam"
        ; decline Loc "a.hsu" (* decline Loc "assu" *)
        ])
   ] 
   ; Bare Noun (fix stem "as")
   ; Avyayaf (fix stem "as")
   ]
;
value build_neu_vas stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "vat"
        ; decline Nom "vat"
        ; decline Acc "vat"
        ; decline Ins "u.saa"
        ; decline Dat "u.se"
        ; decline Abl "u.sas"
        ; decline Gen "u.sas"
        ; decline Loc "u.si"
        ])
   ; (Dual, 
        [ decline Voc "u.sii"
        ; decline Nom "u.sii"
        ; decline Acc "u.sii"
        ; decline Ins "vadbhyaam"
        ; decline Dat "vadbhyaam"
        ; decline Abl "vadbhyaam"
        ; decline Gen "u.sos"
        ; decline Loc "u.sos"
        ])
   ; (Plural, 
        [ decline Voc "vaa.msi"
        ; decline Nom "vaa.msi"
        ; decline Acc "vaa.msi"
        ; decline Ins "vadbhis"
        ; decline Dat "vadbhyas"
        ; decline Abl "vadbhyas"
        ; decline Gen "u.saam"
        ; decline Loc "vatsu"
        ])
   ] 
   ; Bare Noun (fix stem "vat") (* eg vidvat- *)
   ; Avyayaf (fix stem "vat") (* ? *)
   ]
;
(* i is dropped before u.s - Macdonnel §89a *)
value build_neu_ivas stem entry = 
  let decline case suff = (case,fix stem suff) 
  and declinev case suff = (case,fix stem ("i" ^ suff)) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ declinev Voc "vat"
        ; declinev Nom "vat"
        ; declinev Acc "vat"
        ; decline Ins "u.saa"
        ; decline Dat "u.se"
        ; decline Abl "u.sas"
        ; decline Gen "u.sas"
        ; decline Loc "u.si"
        ])
   ; (Dual, 
        [ decline Voc "u.sii"
        ; decline Nom "u.sii"
        ; decline Acc "u.sii"
        ; declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ; decline Gen "u.sos"
        ; decline Loc "u.sos"
        ])
   ; (Plural, 
        [ declinev Voc "vaa.msi"
        ; declinev Nom "vaa.msi"
        ; declinev Acc "vaa.msi"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; decline Gen "u.saam"
        ; declinev Loc "vatsu"
        ])
   ] 
   ; Bare Noun (fix stem "ivat")
   ; Avyayaf (fix stem "ivas") (* why not ivat Acc ? *)
   ]
;
value build_neu_red stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "t"
        ; decline Nom "t"
        ; decline Acc "tam"
        ; decline Ins "taa"
        ; decline Dat "te"
        ; decline Abl "tas"
        ; decline Gen "tas"
        ; decline Loc "ti"
        ])
   ; (Dual, 
        [ decline Voc "tii"
        ; decline Nom "tii"
        ; decline Acc "tii"
        ; decline Ins "dbhyaam"
        ; decline Dat "dbhyaam"
        ; decline Abl "dbhyaam"
        ; decline Gen "tos"
        ; decline Loc "tos"
        ])
   ; (Plural, 
        [ decline Voc "ti"
        ; decline Voc "nti"
        ; decline Nom "ti"
        ; decline Nom "nti"
        ; decline Acc "ti"
        ; decline Acc "nti"
        ; decline Ins "dbhis"
        ; decline Dat "dbhyas"
        ; decline Abl "dbhyas"
        ; decline Gen "taam"
        ; decline Loc "tsu"
        ])
   ]
   ; Avyayaf (fix stem "tam") 
   ]
;
value build_neu_at stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "t"
        ; decline Nom "t"
        ; decline Acc "t"
        ; decline Ins "taa"
        ; decline Dat "te"
        ; decline Abl "tas"
        ; decline Gen "tas"
        ; decline Loc "ti"
        ])
   ; (Dual, 
        [ decline Voc "tii"
        ; decline Voc "ntii" 
        ; decline Nom "tii"
        ; decline Nom "ntii"
        ; decline Acc "tii"
        ; decline Acc "ntii"
        ; decline Ins "dbhyaam"
        ; decline Dat "dbhyaam"
        ; decline Abl "dbhyaam"
        ; decline Gen "tos"
        ; decline Loc "tos"
        ])
   ; (Plural,
        [ decline Voc "nti"
        ; decline Nom "nti"
        ; decline Acc "nti"
        ; decline Ins "dbhis"
        ; decline Dat "dbhyas"
        ; decline Abl "dbhyas"
        ; decline Gen "taam"
        ; decline Loc "tsu"
        ])
   ] 
   ; Avyayaf (fix stem "tam") (* why not "t" Acc ? *)
   ]
;
value build_neu_mahat stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "at"
        ; decline Nom "at"
        ; decline Acc "at"
        ; decline Ins "ataa"
        ; decline Dat "ate"
        ; decline Abl "atas"
        ; decline Gen "atas"
        ; decline Loc "ati"
        ])
   ; (Dual,
        [ decline Voc "atii"
        ; decline Nom "atii"
        ; decline Acc "atii"
        ; decline Ins "adbhyaam"
        ; decline Dat "adbhyaam"
        ; decline Abl "adbhyaam"
        ; decline Gen "atos"
        ; decline Loc "atos"
        ])
   ; (Plural, 
        [ decline Voc "aanti"
        ; decline Nom "aanti"
        ; decline Acc "aanti"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ]
   ; Avyayaf (fix stem "at") (* Z *)
   ]
;
(* pronominal use of aatman in sg for refl use of 3 genders and 3 numbers *)
value build_aatman entry = 
  let stem = revcode "aatm" in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun (Deictic Self)
   [ (Singular,
        [ decline Voc "an"
        ; decline Nom "aa"
        ; decline Acc "aanam"
        ; decline Ins "anaa"
        ; decline Dat "ane"
        ; decline Abl "anas"
        ; decline Gen "anas"
        ; decline Loc "ani"
        ])
   ]             
   ; Bare Noun (code "aatma") 
   ; Avyayaf (code "aatmam") (* aatmaanam Acc ? *)
   ; Cvi (code "aatmii") 
   ]
;
value build_neu_yuvan entry = 
  let stem = [ 42 ] (* y *) in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "uva"
        ; decline Voc "uvan"
        ; decline Nom "uva"
        ; decline Acc "uva"
        ; decline Ins "uunaa"
        ; decline Dat "uune"
        ; decline Abl "uunas"
        ; decline Gen "uunas"
        ; decline Loc "uuni"
        ])
   ; (Dual, 
        [ decline Voc "uvanii"
        ; decline Nom "uvanii"
        ; decline Acc "uvanii"
        ; decline Ins "uvabhyaam"
        ; decline Dat "uvabhyaam"
        ; decline Abl "uvabhyaam"
        ; decline Gen "uunos"
        ; decline Loc "uunos"
        ])
   ; (Plural, 
        [ decline Voc "uvaani"
        ; decline Nom "uvaani"
        ; decline Acc "uvaani"
        ; decline Ins "uvabhis"
        ; decline Dat "uvabhyas"
        ; decline Abl "uvabhyas"
        ; decline Gen "uunaam"
        ; decline Loc "uvasu"
        ])
   ] 
   ; Avyayaf (fix stem "uvam") (* uva Acc ? *)
   ]
;
value build_neu_brahman entry = 
  let stem = revcode "brahm" in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "a"
        ; decline Nom "a"
        ; decline Acc "a"
        ; decline Ins "a.naa"
        ; decline Dat "a.ne"
        ; decline Abl "a.nas"
        ; decline Gen "a.nas"
        ; decline Loc "a.ni"
        ])
   ; (Dual, 
        [ decline Voc "a.nii"
        ; decline Nom "a.nii"
        ; decline Acc "a.nii"
        ; decline Ins "abhyaam"
        ; decline Dat "abhyaam"
        ; decline Abl "abhyaam"
        ; decline Gen "a.nos"
        ; decline Loc "a.nos"
        ])
   ; (Plural,
        [ decline Voc "aa.ni"
        ; decline Nom "aa.ni"
        ; decline Acc "aa.ni"
        ; decline Ins "abhis"
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "a.naam"
        ; decline Loc "asu"
        ])
   ]             
   ; Bare Noun (code "brahma") 
   ; Avyayaf (code "brahma") (* Acc *)
   ]
;
value build_aksan stem entry = 
  (* stem = ak.san, asthan, dadhan, sakthan Whitney §431 Kale§126 *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "e"
        ; decline Voc "i"
        ; decline Nom "i"
        ; decline Acc "i"
        ; decline Ins "naa"
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "ni"
        ; decline Loc "ani" (* \Pan{7,1,75} *)
        ])
   ; (Dual, let l = 
        [ decline Voc "inii"
        ; decline Nom "inii"
        ; decline Acc "inii"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ] in if entry="ak.san" || entry="sakthan" then 
        [ decline Voc "ii"  (* ak.sii Vedic: Sun and moon *)
        ; decline Nom "ii"  (* sakthii les deux cuisses *)
        ; decline Acc "ii"
        ] @ l
      else l)
   ; (Plural, 
        [ decline Voc "iini"
        ; decline Nom "iini"
        ; decline Acc "iini"
        ; decline Acc "aani" (* MW véd. sakthaani RV{10,86,16} AV{6,9,1} *)
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "naam"
        ; decline Loc "isu"
        ])
   ] 
   ; Bare Noun (fix stem "i") (* also indirectly generated by var subentry *)
   ; Avyayaf (fix stem "i") (* Acc *) 
   ]
;
value build_ahan stem entry = (* stem = "ah" *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "ar"
        ; decline Acc "ar"
        ; decline Ins "naa"
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "ni"
        ; decline Loc "ani"
        ])
   ; (Dual,
        [ decline Voc "nii"
        ; decline Voc "anii"
        ; decline Nom "nii"
        ; decline Nom "anii"
        ; decline Acc "nii"
        ; decline Acc "anii"
        ; decline Ins "obhyaam"
        ; decline Dat "obhyaam"
        ; decline Abl "obhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ])
   ; (Plural, 
        [ decline Voc "aani"
        ; decline Nom "aani"
        ; decline Acc "aani"
        ; decline Ins "obhis"
        ; decline Dat "obhyas"
        ; decline Abl "obhyas"
        ; decline Gen "naam"
        ; decline Loc "a.hsu" (* decline Loc "assu" *)
        ])
   ]             
   ; Bare Noun (fix stem "ar")
   ; Bare Noun (fix stem "as") (* before r Pan{8;2;68} *)
   (* Avyayaf (fix stem "am") NO pratyaham Acc of pratyaha *)
   ; Avyayaf (fix stem "ar") (* Acc pratyaha.h *) 
   ]
;
value build_uudhan stem entry = (* stem = "uudh" *) (* Whitney §430d *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "ar"
        ; decline Acc "ar"
(*      ; decline Voc "as" redundant *)
(*      ; decline Nom "as" redundant *)
(*      ; decline Acc "as" redundant *)
        ; decline Ins "naa" 
        ; decline Dat "ne"
        ; decline Abl "nas"
        ; decline Gen "nas"
        ; decline Loc "an"
        ; decline Loc "ani"
        ])
   ; (Dual,
        [ decline Voc "nii"
        ; decline Voc "anii"
        ; decline Nom "nii"
        ; decline Nom "anii"
        ; decline Acc "nii"
        ; decline Acc "anii"
        ; decline Ins "abhyaam"
        ; decline Dat "abhyaam"
        ; decline Abl "abhyaam"
        ; decline Gen "nos"
        ; decline Loc "nos"
        ])
   ; (Plural, 
        [ decline Voc "aani"
        ; decline Nom "aani"
        ; decline Acc "aani"
        ; decline Ins "abhis"
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "naam"
        ; decline Loc "a.hsu" (* decline Loc "assu" *)
        ])
   ]             
   ; Bare Noun (code "uudhar")
   ; Avyayaf (code "uudham") 
   ; Avyayaf (code "uudha") 
   ]
;
value build_neu_in stem entry = 
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 3 in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "in"
        ; decline Voc "i"
        ; decline Nom "i"
        ; decline Acc "i"
        ; decline Ins "inaa"
        ; decline Dat "ine"
        ; decline Abl "inas"
        ; decline Gen "inas"
        ; decline Loc "ini"
        ])
   ; (Dual,
        [ decline Voc "inii"
        ; decline Nom "inii"
        ; decline Acc "inii"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "inos"
        ; decline Loc "inos"
        ])
   ; (Plural, 
        [ decline Voc "iini"
        ; decline Nom "iini"
        ; decline Acc "iini"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "inaam"
        ; decline Loc "i.su"
        ])
   ]             
   ; Bare Noun bare (* same as Acc *)
   ; Avyayaf bare 
   ]
;
value build_neu_aac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "k"
        ; decline Nom "k"
        ; decline Acc "~ncam"
        ; decline Ins "caa"
        ; decline Dat "ce"
        ; decline Abl "cas"
        ; decline Gen "cas"
        ; decline Loc "ci"
        ])
   ; (Dual, 
        [ decline Voc "cii"
        ; decline Nom "cii"
        ; decline Acc "cii"
        ; decline Ins "gbhyaam"
        ; decline Dat "gbhyaam"
        ; decline Abl "gbhyaam"
        ; decline Gen "cos"
        ; decline Loc "cos"
        ])
   ; (Plural, 
        [ decline Voc "~nci"
        ; decline Nom "~nci"
        ; decline Acc "~nci"
        ; decline Ins "gbhis"
        ; decline Dat "gbhyas"
        ; decline Abl "gbhyas"
        ; decline Gen "caam"
        ; decline Loc "k.su"
        ])
   ] 
   ; Bare Noun (fix stem "k") (* eg praaguttara *)
   ]
;
value build_neu_yac stem entry = 
  let prevoc = if stem = revcode "tir" then "azc" 
                                       else "iic" in
               (* exception tiryac -> tiriic in prevocalic flexions *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "yak"
        ; decline Nom "yak"
        ; decline Acc "yak"
        ; decline Ins (prevoc ^ "aa")
        ; decline Dat (prevoc ^ "e")
        ; decline Abl (prevoc ^ "as")
        ; decline Gen (prevoc ^ "as")
        ; decline Loc (prevoc ^ "i")
        ])
   ; (Dual, 
        [ decline Voc (prevoc ^ "ii")
        ; decline Nom (prevoc ^ "ii")
        ; decline Acc (prevoc ^ "ii")
        ; decline Ins "yagbhyaam"
        ; decline Dat "yagbhyaam"
        ; decline Abl "yagbhyaam"
        ; decline Gen (prevoc ^ "os")
        ; decline Loc (prevoc ^ "os")
        ])
   ; (Plural,
        [ decline Voc "ya~nci"
        ; decline Nom "ya~nci"
        ; decline Acc "ya~nci"
        ; decline Ins "yagbhis"
        ; decline Dat "yagbhyas"
        ; decline Abl "yagbhyas"
        ; decline Gen (prevoc ^ "aam")
        ; decline Loc "yak.su"
        ])
   ] ]
;
value build_neu_vac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "vak"
        ; decline Nom "vak"
        ; decline Acc "vak"
        ; decline Ins "uucaa"
        ; decline Dat "uuce"
        ; decline Abl "uucas"
        ; decline Gen "uucas"
        ; decline Loc "uuci"
        ])
   ; (Dual,
        [ decline Voc "uucii"
        ; decline Nom "uucii"
        ; decline Acc "uucii"
        ; decline Ins "vagbhyaam"
        ; decline Dat "vagbhyaam"
        ; decline Abl "vagbhyaam"
        ; decline Gen "uucos"
        ; decline Loc "uucos"
        ])
   ; (Plural, 
        [ decline Voc "va~nci"
        ; decline Nom "va~nci"
        ; decline Acc "va~nci"
        ; decline Ins "vagbhis"
        ; decline Dat "vagbhyas"
        ; decline Abl "vagbhyas"
        ; decline Gen "uucaam"
        ; decline Loc "vak.su"
        ])
   ]
   ; Avyayaf (code "vacam") (* check *)
   ]
;
value build_neu_ac stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Voc "ak"
        ; decline Nom "ak"
        ; decline Acc "ak"
        ; decline Ins "iicaa"
        ; decline Dat "iice"
        ; decline Abl "iicas"
        ; decline Gen "iicas"
        ; decline Loc "iici"
        ])
   ; (Dual, 
        [ decline Voc "iicii"
        ; decline Nom "iicii"
        ; decline Acc "iicii"
        ; decline Ins "agbhyaam"
        ; decline Dat "agbhyaam"
        ; decline Abl "agbhyaam"
        ; decline Gen "iicos"
        ; decline Loc "iicos"
        ])
   ; (Plural, 
        [ decline Voc "a~nci"
        ; decline Nom "a~nci"
        ; decline Acc "a~nci"
        ; decline Ins "agbhis"
        ; decline Dat "agbhyas"
        ; decline Abl "agbhyas"
        ; decline Gen "iicaam"
        ; decline Loc "ak.su"
        ])
   ]
   ; Avyayaf (code "acam") 
   ]
;
value build_neu_aas stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Neu
   [ (Singular,
        [ decline Ins "aa" 
        ; decline Ins "ayaa"
        ; decline Abl "as"
        ])
   ] ]
;
value build_fem_aa stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry (
   [ Declined Noun Fem
   [ (Singular, if entry = "ubha" then [] else let l = 
        [ if entry = "allaa" || entry = "akkaa" (* Pan{7,3,107} *)
          then decline Voc "a"
          else decline Voc "e"
        ; decline Nom "aa"
        ; decline Acc "aam"
        ; decline Ins "ayaa"
        ; decline Dat "aayai"
        ; decline Abl "aayaas"
        ; decline Gen "aayaas"
        ; decline Loc "aayaam"
        ] in if entry = "ambaa" then 
        [ decline Voc "a" :: l ] (* Pan{7,3,107} but also ambe vedic *)
             else if entry = "guha" then (* guhaa fde guha *)
        [ decline Loc "aa" :: l ] (* Vedic *)
             else l)
   ; (Dual, if entry = "eka" then [] else 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, if entry = "ubha" || entry = "eka" then [] else 
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aas"
        ; decline Ins "aabhis"
        ; decline Dat "aabhyas"
        ; decline Abl "aabhyas"
        ; decline Gen "aanaam"
        ; decline Loc "aasu"
        ])
   ] 
(* No Bare iic, it will be generated by [compute_decls with extract_fem_stems] *)
   ; Avyayaf (fix stem "am") (* acc of neuter stem with hrasva of vowel *)
   ] @ (if aa_iiv entry then [ Cvi (wrap stem 4) ] else []))
; 
(* vedic g = Fem, rare (jaa) Whitney 351 *)
value build_mono_aa g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aam"
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Voc "au"
        ; decline Nom "au"
        ; decline Acc "au"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural, 
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aas" (* Whitney *)
        ; decline Acc "as" (* Paninian form, according to Deshpande *)
        ; decline Ins "aabhis" 
        ; decline Dat "aabhyas" 
        ; decline Abl "aabhyas"
        ; decline Gen "aam"
        ; decline Gen "anaam"
        ; decline Loc "aasu"
        ])
   ] 
   ; Bare Noun (fix stem "aa")
   ; Avyayaf (fix stem "am") (* acc of neuter stem with hrasva of vowel *)
   ]
;
(* gandharva Haahaa [Tirupati] and pkt raa.naa *)
value build_mas_aa_no_root stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aam"
        ; decline Ins "aa"
        ; decline Dat "ai"
        ; decline Abl "aas"
        ; decline Gen "aas"
        ; decline Loc "e"
        ])
   ; (Dual,
        [ decline Voc "au"
        ; decline Nom "au"
        ; decline Acc "au"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "aus" 
        ; decline Loc "aus"
        ]) 
   ; (Plural,  
        [ decline Voc "aas" 
        ; decline Nom "aas" 
        ; decline Acc "aan"
        ; decline Ins "aabhis" 
        ; decline Dat "aabhyas" 
        ; decline Abl "aabhyas"
        ; decline Gen "aam" 
        ; decline Loc "aasu" 
        ])
   ]
   ; Bare Noun (fix stem "aa")
   ] 
;
(* Special for gandharva Huuhuu [Tirupati] *)
(* Also a few exceptions *)
value build_huuhuu entry = 
  let stem = revcode "huuh" in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Singular,
        [ decline Voc "uus"
        ; decline Nom "uus"
        ; decline Acc "uum"
        ; decline Ins "vaa"
        ; decline Dat "ve"
        ; decline Abl "vas"
        ; decline Gen "vas"
        ; decline Loc "vi"
        ])
   ; (Dual,
        [ decline Voc "vau"
        ; decline Nom "vau"
        ; decline Acc "vau"
        ; decline Ins "uubhyaam"
        ; decline Dat "uubhyaam"
        ; decline Abl "uubhyaam"
        ; decline Gen "vau"
        ; decline Loc "vau"
        ])
   ; (Plural, 
        [ decline Voc "vas"
        ; decline Nom "vas"
        ; decline Acc "uun"
        ; decline Ins "uubhis"
        ; decline Dat "uubhyas"
        ; decline Abl "uubhyas"
        ; decline Gen "vaam"
        ; decline Loc "uu.su"
        ])
   ] ]
;
value build_fem_i stem trunc entry = 
  let declines case suff = (case,fix stem suff) 
  and declineg case suff = (case,fix [ 10 :: trunc ] suff) 
  and declinel case suff = (case,fix [ 4 :: trunc ] suff) 
  and declinau case = (case,wrap trunc 13) in
  enter entry (
   [ Declined Noun Fem
   [ (Singular,
        [ declineg Voc ""
        ; declines Nom "s"
        ; declines Acc "m"
        ; declines Ins "aa"
        ; declines Dat "ai"
        ; declineg Dat "e"
        ; declines Abl "aas"
        ; declineg Abl "s"
        ; declines Gen "aas"
        ; declineg Gen "s" 
        ; declines Loc "aam"
        ; declinau Loc 
        ])
   ; (Dual, 
        [ declinel Voc "" 
        ; declinel Nom ""
        ; declinel Acc "" 
        ; declines Ins "bhyaam" 
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam" 
        ; declines Gen "os"
        ; declines Loc "os" 
        ]) 
   ; (Plural, 
        [ declineg Voc "as"
        ; declineg Nom "as"
        ; declinel Acc "s"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]             
   ; Bare Noun (mirror stem)
   ; Avyayaf (mirror stem) (* actually acc of neuter stem *)
   ; Indecl Tas (fix stem "tas")
   ] @ (if entry = "vi.mzati"
        then [ Bare Noun (mirror trunc) (* vi.mzat *) ]
        else []))

;
(* NB concerning Avyayaf of stems ending in long vowels. According to Pan{2,4,18}
   avyayiibhaava compounds are of neuter gender, incurring hrasva of ifc stem *)
value build_fem_ii trunc entry = 
  let stems = [ 3 :: trunc ]  
  and steml = [ 4 :: trunc ] in 
  let declines case suff = (case,fix stems suff) 
  and declinel case suff = (case,fix steml suff) in
  enter entry (
   [ Declined Noun Fem
   [ (Singular,
        [ declines Voc ""
        ; declinel Nom ""
        ; declinel Acc "m"
        ; declines Ins "aa"
        ; declines Dat "ai"
        ; declines Abl "aas"
        ; declines Gen "aas"
        ; declines Loc "aam"
        ])
   ; (Dual, if entry = "ubhayii" then [] else 
        [ declines Voc "au" 
        ; declines Nom "au"
        ; declines Acc "au"
        ; declinel Ins "bhyaam"
        ; declinel Dat "bhyaam"
        ; declinel Abl "bhyaam"
        ; declines Gen "os"
        ; declines Loc "os"
        ])
   ; (Plural, 
        [ declines Voc "as"
        ; declines Nom "as"
        ; declinel Acc "s"
        ; declinel Ins "bhis"
        ; declinel Dat "bhyas"
        ; declinel Abl "bhyas"
        ; declinel Gen "naam"
        ; declinel Loc "su"
        ])
   ]             
   ; Bare Noun (mirror steml) 
   ; Avyayaf (mirror stems) 
   ] @ match entry with 
       [ "nadii" | "paur.namasii" | "aagrahaaya.nii" 
         -> [ Avyayaf (fix trunc "am") ]
       | _ -> []
       ]
     @ match entry with 
       [ "abhii#2" -> [] (* avoids overgeneration abhi- *)
       | _ -> [ Bare Noun (mirror stems) ] (* Pan{6,3,61} *)
       ])
;
(* g = Fem, rarely Mas *)
value build_mono_ii g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g 
   [ (Singular,
        [ decline Voc "iis"
        ; decline Nom "iis"
        ; decline Acc "iyam"
        ; decline Ins "iyaa"
        ; decline Dat "iye"
        ; decline Dat "iyai"
        ; decline Abl "iyas"
        ; decline Abl "iyaas"
        ; decline Gen "iyas"
        ; decline Gen "iyaas"
        ; decline Loc "iyi"
        ; decline Loc "iyaam" (* niyaam Kale§77 p46 *)
        ])
   ; (Dual,
        [ decline Voc "iyau"
        ; decline Nom "iyau"
        ; decline Acc "iyau"
        ; decline Ins "iibhyaam"
        ; decline Dat "iibhyaam"
        ; decline Abl "iibhyaam"
        ; decline Gen "iyos"
        ; decline Loc "iyos"
        ])
   ; (Plural, 
        [ decline Voc "iyas"
        ; decline Nom "iyas"
        ; decline Acc "iyas"
        ; decline Ins "iibhis"
        ; decline Dat "iibhyas"
        ; decline Abl "iibhyas"
        ; decline Gen "iyaam"
        ; decline Gen "iinaam"
        ; decline Loc "ii.su"
        ])
   ]
   ; Bare Noun (wrap stem 4) (* productive ? shortened ? *)
   ; Avyayaf (wrap stem 3) 
   ]
;
(* g = Mas scheme [42] Bucknell p26 p90 *)
value build_bicons_ii g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g 
   [ (Singular,
        [ decline Voc "iis"
        ; decline Nom "iis"
        ; decline Acc "iyam"
        ; decline Ins "iyaa"
        ; decline Dat "iye"
        ; decline Abl "iyas"
        ; decline Gen "iyas"
        ; decline Loc "iyi"
        ])
   ; (Dual,
        [ decline Voc "iyau"
        ; decline Nom "iyau"
        ; decline Acc "iyau"
        ; decline Ins "iibhyaam"
        ; decline Dat "iibhyaam"
        ; decline Abl "iibhyaam"
        ; decline Gen "iyos"
        ; decline Loc "iyos"
        ])
   ; (Plural, 
        [ decline Voc "iyas"
        ; decline Nom "iyas"
        ; decline Acc "iyas"
        ; decline Ins "iibhis"
        ; decline Dat "iibhyas"
        ; decline Abl "iibhyas"
        ; decline Gen "iyaam"
        ; decline Loc "ii.su"
        ])
   ]
   ; Bare Noun (wrap stem 4)
   ; Avyayaf (wrap stem 3) 
   ]
;
(* vedic forms g = Fem, rarely Mas (rathii senanii) *)
value build_poly_ii g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g 
   [ (Singular,
        [ decline Voc "i"
        ; decline Voc "iis" (* Bucknell senaanii.h Table 7 Deshpande p146 *)
        ; decline Nom "iis"
        ; decline Acc "yam"
        ; decline Ins "yaa"
        ; decline Dat "ye"
        ; decline Abl "yas"
        ; decline Gen "yas"
        ; decline Loc "yi"
        ; decline Loc "yaam" (* Bucknell senaanyaam Table 7 Deshpande p146 *)
        ])
   ; (Dual, 
        [ decline Voc "yaa"
        ; decline Nom "yaa"
        ; decline Acc "yaa"
        ; decline Ins "iibhyaam"
        ; decline Dat "iibhyaam"
        ; decline Abl "iibhyaam"
        ; decline Gen "yos"
        ; decline Loc "yos"
        ])
   ; (Plural,
        [ decline Voc "yas"
        ; decline Nom "yas"
        ; decline Acc "yas"
        ; decline Ins "iibhis"
        ; decline Dat "iibhyas"
        ; decline Abl "iibhyas"
        ; decline Gen "iinaam"
        ; decline Loc "ii.su"
        ])
   ]
   ; Bare Noun (wrap stem 4) 
   ; Bare Noun (wrap stem 3) (* kumaarimataa Pan{6,3,42} senaanigraama.nii *)
   ; Avyayaf (wrap stem 3) 
   ]
;
value build_strii stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "i"
        ; decline Nom "ii"
        ; decline Acc "iyam"
        ; decline Acc "iim"
        ; decline Ins "iyaa"
        ; decline Dat "iyai"
        ; decline Abl "iyaas"
        ; decline Gen "iyaas"
        ; decline Loc "iyaam"
        ])
   ; (Dual, 
        [ decline Voc "iyau"
        ; decline Nom "iyau"
        ; decline Acc "iyau"
        ; decline Ins "iibhyaam"
        ; decline Dat "iibhyaam"
        ; decline Abl "iibhyaam"
        ; decline Gen "iyos"
        ; decline Loc "iyos"
        ])
   ; (Plural, 
        [ decline Voc "iyas"
        ; decline Nom "iyas"
        ; decline Acc "iyas"
        ; decline Acc "iis"
        ; decline Ins "iibhis"
        ; decline Dat "iibhyas"
        ; decline Abl "iibhyas"
        ; decline Gen "iinaam"
        ; decline Loc "ii.su"
        ])
   ]
   ; Bare Noun (wrap stem 4) 
   ; Avyayaf (wrap stem 3) 
   ]
;
value build_fem_u stem trunc entry = 
  let declines case suff = (case,fix stem suff) 
  and declineg case suff = (case,fix [ 12 :: trunc ] suff) 
  and declinel case suff = (case,fix [ 6 :: trunc ] suff) 
  and declinau case = (case,wrap trunc 13) in
  enter entry (
   [ Declined Noun Fem 
   [ (Singular,
        [ declineg Voc ""
        ; declines Nom "s"
        ; declines Acc "m"
        ; declines Ins "aa"
        ; declines Dat "ai"
        ; declineg Dat "e"
        ; declines Abl "aas"
        ; declineg Abl "s"
        ; declines Gen "aas"
        ; declineg Gen "s"
        ; declines Loc "aam"
        ; declinau Loc 
        ])
   ; (Dual, 
        [ declinel Voc ""
        ; declinel Nom ""
        ; declinel Acc ""
        ; declines Ins "bhyaam"
        ; declines Dat "bhyaam"
        ; declines Abl "bhyaam"
        ; declines Gen "os"
        ; declines Loc "os"
        ])
   ; (Plural,
        [ declineg Voc "as"
        ; declineg Nom "as"
        ; declinel Acc "s"
        ; declines Ins "bhis"
        ; declines Dat "bhyas"
        ; declines Abl "bhyas"
        ; declinel Gen "naam"
        ; declines Loc "su"
        ])
   ]
   ; Avyayaf (mirror stem)
   ] @ (if entry="ku#2" || entry="go" then [] (* avoids overgeneration *)
        else [ Bare Noun (mirror stem) ]))
;
value build_fem_uu stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "u"
        ; decline Nom "uus"
        ; decline Acc "uum"
        ; decline Ins "vaa"
        ; decline Dat "vai"
        ; decline Abl "vaas"
        ; decline Gen "vaas"
        ; decline Loc "vaam"
        ])
   ; (Dual,
        [ decline Voc "vau"
        ; decline Nom "vau"
        ; decline Acc "vau"
        ; decline Ins "uubhyaam"
        ; decline Dat "uubhyaam"
        ; decline Abl "uubhyaam"
        ; decline Gen "vos"
        ; decline Loc "vos"
        ])
   ; (Plural,
        [ decline Voc "vas"
        ; decline Nom "vas"
        ; decline Acc "uus"
        ; decline Ins "uubhis"
        ; decline Dat "uubhyas"
        ; decline Abl "uubhyas"
        ; decline Gen "uunaam"
        ; decline Loc "uu.su"
        ])
   ]
   ; Bare Noun (wrap stem 6) 
   ; Bare Noun (wrap stem 5) (* Pan{6,3,61} *)
   ; Avyayaf (wrap stem 5) 
   ]
;
(* g = Fem, rarely Mas *)
value build_mono_uu g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "uus"
        ; decline Voc "u" (* alternative Renou §234 MW gram §126h Vopadeva *)
        ; decline Nom "uus"  
        ; decline Acc "uvam" (* \Pan{6,4,77} *)
        ; decline Ins "uvaa"
        ; decline Dat "uve"
        ; decline Dat "uvai"
        ; decline Abl "uvas"
        ; decline Abl "uvaas"
        ; decline Gen "uvas"
        ; decline Gen "uvaas"
        ; decline Loc "uvi"
        ; decline Loc "uvaam"
        ])
   ; (Dual, 
        [ decline Voc "uvau"
        ; decline Nom "uvau"
        ; decline Acc "uvau"
        ; decline Ins "uubhyaam"
        ; decline Dat "uubhyaam"
        ; decline Abl "uubhyaam"
        ; decline Gen "uvos"
        ; decline Loc "uvos"
        ])
   ; (Plural,
        [ decline Voc "uvas"
        ; decline Nom "uvas"
        ; decline Acc "uvas"
        ; decline Ins "uubhis"
        ; decline Dat "uubhyas"
        ; decline Abl "uubhyas"
        ; decline Gen "uvaam"
        ; decline Gen "uunaam"
        ; decline Loc "uu.su"
        ])
   ]
   ; Bare Noun (wrap stem 6) 
   ; Avyayaf (wrap stem 5) 
   ]
;
value poly_uu_decls decline = 
   [ (Singular,
        [ decline Voc "u"
        ; decline Nom "uus"
        ; decline Acc "vam"
        ; decline Ins "vaa"
        ; decline Dat "ve"
        ; decline Abl "vas"
        ; decline Gen "vas"
        ; decline Loc "vi"
        ])
   ; (Dual, 
        [ decline Voc "vaa"
        ; decline Nom "vaa"
        ; decline Acc "vaa"
        ; decline Ins "uubhyaam"
        ; decline Dat "uubhyaam"
        ; decline Abl "uubhyaam"
        ; decline Gen "vos"
        ; decline Loc "vos"
        ])
   ; (Plural, 
        [ decline Voc "vas"
        ; decline Nom "vas"
        ; decline Acc "vas"
        ; decline Ins "uubhis"
        ; decline Dat "uubhyas"
        ; decline Abl "uubhyas"
        ; decline Gen "uunaam"
        ; decline Loc "uu.su"
        ])
   ]
;
(* vedic forms g = Fem, very rarely Mas (praazuu) *)
value build_poly_uu g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g (poly_uu_decls decline)
   ; Bare Noun (wrap stem 6) 
   ; Avyayaf (wrap stem 5) 
   ]
;
value build_fem_ri_v stem entry = (* vriddhi in strong cases *)
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 7 in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "aa"
        ; decline Acc "aaram"
        ; decline Ins "raa"
        ; decline Dat "re"
        ; decline Abl "ur"
        ; decline Gen "ur"
        ; decline Loc "ari"
        ])
   ; (Dual,
        [ decline Voc "aarau"
        ; decline Nom "aarau"
        ; decline Acc "aarau"
        ; decline Ins ".rbhyaam"
        ; decline Dat ".rbhyaam"
        ; decline Abl ".rbhyaam"
        ; decline Gen "ros"
        ; decline Loc "ros"
        ])
   ; (Plural, 
        [ decline Voc "aaras"
        ; decline Nom "aaras"
        ; decline Acc ".rrs"
        ; decline Ins ".rbhis"
        ; decline Dat ".rbhyas"
        ; decline Abl ".rbhyas"
        ; decline Gen ".rr.naam"
        ; decline Loc ".r.su"
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare 
   ]
;
value build_fem_ri_g stem entry = (* lien de parenté avec gu.na *)
  let decline case suff = (case,fix stem suff) 
  and bare = wrap stem 7 in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "ar"
        ; decline Nom "aa"
        ; decline Acc "aram"
        ; decline Ins "raa"
        ; decline Dat "re"
        ; decline Abl "ur"
        ; decline Gen "ur"
        ; decline Loc "ari"
        ])
   ; (Dual, 
        [ decline Voc "arau"
        ; decline Nom "arau"
        ; decline Acc "arau"
        ; decline Ins ".rbhyaam"
        ; decline Dat ".rbhyaam"
        ; decline Abl ".rbhyaam"
        ; decline Gen "ros"
        ; decline Loc "ros"
        ])
   ; (Plural, 
        [ decline Voc "aras"
        ; decline Nom "aras"
        ; decline Acc ".rrs"
        ; decline Acc "aras" (* epics Whitney 373c *)
        ; decline Ins ".rbhis"
        ; decline Dat ".rbhyas"
        ; decline Abl ".rbhyas"
        ; decline Gen ".rr.naam"
        ; decline Loc ".r.su"
        ])
   ]             
   ; Bare Noun bare
   ; Avyayaf bare 
   ; Indecl Tas (fix stem ".rtas") (* maat.rtas *)
   ]
;
value build_fem_ir stem entry = (* gir *)
  let decline case suff = (case,fix stem suff) 
  and short = fix stem "ir"
  and long = fix stem "iir" in 
  enter entry
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "iir"
        ; decline Nom "iir"
        ; decline Acc "iram"
        ; decline Ins "iraa"
        ; decline Dat "ire"
        ; decline Abl "iras"
        ; decline Gen "iras"
        ; decline Loc "iri"
        ])
   ; (Dual,
        [ decline Voc "irau"
        ; decline Nom "irau"
        ; decline Acc "irau"
        ; decline Ins "iirbhyaam"
        ; decline Dat "iirbhyaam"
        ; decline Abl "iirbhyaam"
        ; decline Gen "iros"
        ; decline Loc "iros"
        ])
   ; (Plural, 
        [ decline Voc "iras"
        ; decline Nom "iras"
        ; decline Acc "iras"
        ; decline Ins "iirbhis"
        ; decline Dat "iirbhyas"
        ; decline Abl "iirbhyas"
        ; decline Gen "iraam"
        ; decline Loc "iir.su"
        ])
   ] 
   ; Bare Noun short (* gir- *)
   ; Bare Noun long (* giir- *)
   ; Avyayaf short
   ]
;
(* Similar to preceding paradigm - for aazis fem et niraazis adj *)
value build_aazis g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "iis"
        ; decline Nom "iis"
        ; decline Acc "i.sam"
        ; decline Ins "i.saa"
        ; decline Dat "i.se"
        ; decline Abl "i.sas"
        ; decline Gen "i.sas"
        ; decline Loc "i.si"
        ])
   ; (Dual,
        [ decline Voc "i.sau"
        ; decline Nom "i.sau"
        ; decline Acc "i.sau"
        ; decline Ins "iirbhyaam"
        ; decline Dat "iirbhyaam"
        ; decline Abl "iirbhyaam"
        ; decline Gen "i.sos"
        ; decline Loc "i.sos"
        ])
   ; (Plural, 
        [ decline Voc "i.sas"
        ; decline Nom "i.sas"
        ; decline Acc "i.sas"
        ; decline Ins "iirbhis"
        ; decline Dat "iirbhyas"
        ; decline Abl "iirbhyas"
        ; decline Gen "i.saam"
        ; decline Loc "ii.h.su" 
        ; decline Loc "ii.s.su" (* necessary *)
        ])
   ]             
   ; Bare Noun (fix stem "iir") (* aazis1- *)
   ; Bare Noun (fix stem "ii")  (* aazis2- *)
   ; Avyayaf (fix stem "is") 
   ]
;
value build_fem_ur stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Voc "uur"
        ; decline Nom "uur"
        ; decline Acc "uram"
        ; decline Ins "uraa"
        ; decline Dat "ure"
        ; decline Abl "uras"
        ; decline Gen "uras"
        ; decline Loc "uri"
        ])
   ; (Dual, 
        [ decline Voc "urau"
        ; decline Nom "urau"
        ; decline Acc "urau"
        ; decline Ins "uurbhyaam"
        ; decline Dat "uurbhyaam"
        ; decline Abl "uurbhyaam"
        ; decline Gen "uros"
        ; decline Loc "uros"
        ])
   ; (Plural, 
        [ decline Voc "uras"
        ; decline Nom "uras"
        ; decline Acc "uras"
        ; decline Ins "uurbhis"
        ; decline Dat "uurbhyas"
        ; decline Abl "uurbhyas"
        ; decline Gen "uraam"
        ; decline Loc "uur.su"
        ]) 
   ]             
   ; Bare Noun (fix stem "uur") (* dhuur- *)
   ; Avyayaf (fix stem "ur") 
   ]
;
(* This paradigm could be obtained by implementing Macdonell§59, see
   [Phonetics.diphthong_split] and the code commented out in [Int_sandhi] *)
value build_rai g stem entry = (* stem = raa g = Mas or Fem (rare) *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "s"
        ; decline Nom "s"
        ; decline Acc "yam"
        ; decline Ins "yaa"
        ; decline Dat "ye"
        ; decline Abl "yas"
        ; decline Gen "yas"
        ; decline Loc "yi"
        ])
   ; (Dual, 
        [ decline Voc "yau"
        ; decline Nom "yau"
        ; decline Acc "yau"
        ; decline Ins "bhyaam"
        ; decline Dat "bhyaam"
        ; decline Abl "bhyaam"
        ; decline Gen "yos"
        ; decline Loc "yos"
        ])
   ; (Plural, 
        [ decline Voc "yas"
        ; decline Nom "yas"
        ; decline Acc "yas"
        ; decline Ins "bhis"
        ; decline Dat "bhyas"
        ; decline Abl "bhyas"
        ; decline Gen "yaam"
        ; decline Loc "su"
        ])
   ] 
   ; Avyayaf (code "ri") 
   ]
;
value build_e g stem entry =
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "es"
        ; decline Voc "e" (* Kale 33 *)
        ; decline Nom "es"
        ; decline Acc "am"
        ; decline Ins "ayaa"
        ; decline Dat "aye"
        ; decline Abl "es"
        ; decline Gen "es"
        ; decline Loc "ayi"
        ])
   ; (Dual,
        [ decline Voc "ayau"
        ; decline Nom "ayau"
        ; decline Acc "ayau"
        ; decline Ins "ebhyaam"
        ; decline Dat "ebhyaam"
        ; decline Abl "ebhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, 
        [ decline Voc "ayas"
        ; decline Nom "ayas"
        ; decline Acc "ayas"
        ; decline Ins "ebhis"
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "ayaam"
        ; decline Loc "e.su"
        ])
   ]            
   ; Bare Noun (fix stem "aya")
   ; Avyayaf (fix stem "i") 
   ]
;
value build_o g stem entry =
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "aus"
        ; decline Nom "aus"
        ; decline Acc "aam"
        ; decline Ins "avaa"
        ; decline Dat "ave"
        ; decline Abl "os"
        ; decline Gen "os"
        ; decline Loc "avi"
        ])
   ; (Dual,
        [ decline Voc "aavau"
        ; decline Nom "aavau"
        ; decline Acc "aavau"
        ; decline Ins "obhyaam"
        ; decline Dat "obhyaam"
        ; decline Abl "obhyaam"
        ; decline Gen "avos"
        ; decline Loc "avos"
        ])
   ; (Plural, 
        [ decline Voc "aavas"
        ; decline Nom "aavas"
        ; decline Acc "aas"
        ; decline Ins "obhis"
        ; decline Dat "obhyas"
        ; decline Abl "obhyas"
        ; decline Gen "avaam"
        ; decline Loc "o.su"
        ])
   ]            
   ; Bare Noun ((mirror stem) @ (code "o"))   (* go- *)
   ; Bare Noun ((mirror stem) @ (code "ava")) (* go -> gava- *)
   ; Avyayaf (fix stem "u") (* upagu *)
   ]
;
value build_div g stem entry = (* stem = "d" *)
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "yaus"
        ; decline Nom "yaus"
        ; decline Acc "ivam"
        ; decline Acc "yaam"
        ; decline Ins "ivaa"
        ; decline Dat "ive"
        ; decline Dat "yave"
        ; decline Abl "ivas"
        ; decline Abl "yos"
        ; decline Gen "ivas"
        ; decline Gen "yos"
        ; decline Loc "ivi"
        ; decline Loc "yavi"
        ])
   ; (Dual,
        [ decline Nom "yaavau"
        ; decline Nom "ivau" (* Renou *)
        ; decline Acc "yaavau"
        ; decline Acc "ivau" (* Renou *)
        ])
   ; (Plural, 
        [ decline Voc "ivas"
        ; decline Nom "ivas"
        ; decline Nom "yaavas"
        ; decline Acc "ivas"
        ; decline Ins "yubhis"
        ; decline Dat "yubhyas"
        ; decline Abl "yubhyas"
        ; decline Gen "ivaam"
        ; decline Loc "yu.su"
        ])
   ] 
   ; Avyayaf (fix stem "iv") 
   ]
;
value build_diiv entry = (* diiv\#2 *)
  let decline case form = (case,code form) in 
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ decline Acc "dyuvam" 
        ; decline Ins "diivnaa" (* for pratidiivnaa (par l'adversaire) *)
        ; decline Dat "diive"
        ; decline Dat "dyuve"
        ; decline Loc "diivi"
        ])
   ] ]
;
value build_au g stem entry =
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc "aus"
        ; decline Nom "aus"
        ; decline Acc "aavam"
        ; decline Ins "aavaa"
        ; decline Dat "aave"
        ; decline Abl "aavas"
        ; decline Gen "aavas"
        ; decline Loc "aavi"
        ])
   ; (Dual,
        [ decline Voc "aavau"
        ; decline Nom "aavau"
        ; decline Acc "aavau"
        ; decline Ins "aubhyaam"
        ; decline Dat "aubhyaam"
        ; decline Abl "aubhyaam"
        ; decline Gen "aavos"
        ; decline Loc "aavos"
        ])
   ; (Plural, 
        [ decline Voc "aavas"
        ; decline Nom "aavas"
        ; decline Acc "aavas"
        ; decline Ins "aubhis"
        ; decline Dat "aubhyas"
        ; decline Abl "aubhyas"
        ; decline Gen "aavaam"
        ; decline Loc "au.su"
        ])
   ]
   ; Avyayaf (fix stem "u") 
   ]
;
value build_ap entry = 
  enter entry 
   [ Declined Noun Fem
   [ (Plural,
        [ register Voc "aapas"
        ; register Nom "aapas"
        ; register Acc "apas"
        ; register Ins "adbhis"
        ; register Dat "adbhyas"
        ; register Abl "adbhyas"
        ; register Gen "apaam"
        ; register Loc "apsu"
        ])
   ]
   ; Bare Noun (code "ap") (* e.g. abja *)
   ; Avyayaf (code "apam") 
   ]
;
(* Root word declension. Finalization ensures the initial aspiration by
Phonetics.asp, in order to transform eg duk in dhuk (Whitney §155) *)
value build_root g stem entry =
  let decline case suff = (case,fix stem suff)
  and decline_nasalise case suff = 
      let nstem = match stem with
        [ [ c :: r ] -> if nasal c then stem else 
                           try [ c :: [ (homonasal c) :: r ]]
                           with [ Failure _ -> stem ]
        | _ -> failwith "build_root"
        ] in (case,fix nstem suff)
  and declfin case suff = 
      (* [finalize_r] for doubling of vowel in r roots Whitney §245b *)
      (case,fix (finalize_r stem) suff) 
  and bare = mirror (finalize_r stem) in 
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ declfin Voc ""
        ; declfin Nom ""
        ; if g=Neu then declfin Acc "" else decline Acc "am"
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Voc (if g=Neu then "ii" else "au")
        ; decline Nom (if g=Neu then "ii" else "au")
        ; decline Acc (if g=Neu then "ii" else "au")
        ; declfin Ins "bhyaam"
        ; declfin Dat "bhyaam"
        ; declfin Abl "bhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural, 
        [ if g=Neu then decline_nasalise Voc "i" else decline Voc "as"
        ; if g=Neu then decline_nasalise Nom "i" else decline Nom "as"
        ; if g=Neu then decline_nasalise Acc "i" else decline Acc "as"
   (* Voc Nom Acc Neu ought to have nasal : v.rnti Whitney§389c p. 145 *)
   (* Acc. vaacas with accent on aa or on a        Whitney§391  p. 147 *)
        ; declfin Ins "bhis"
        ; declfin Dat "bhyas"
        ; declfin Abl "bhyas"
        ; decline Gen "aam"
        ; declfin Loc "su" 
         (* viz2 -> vi.tsu but also véd. vik.su Whitney§218a [compute_extra] *)
        ])
   ]             
   ; Bare Noun bare (* thus hutabhuj -> hutabhuk+dik -> ...gdik *)
   ; Avyayaf bare
   ]
;
(* special case for iid.rz and kiid.rz considered as pronouns *)
value build_root_pn g stem entry =
  let decline case suff = (case,fix stem suff)
  and decline_nasalise case suff = 
      let nstem = match stem with
        [ [ c :: r ] -> if nasal c then stem else 
                           try [ c :: [ (homonasal c) :: r ]]
                           with [ Failure _ -> stem ]
        | _ -> failwith "build_root"
        ] in (case,fix nstem suff)
  and declfin case suff = 
      (* [finalize_r] for doubling of vowel in r roots Whitney §245b *)
      (case,fix (finalize_r stem) suff) 
  and bare = mirror (finalize_r stem) in 
  enter entry 
   [ Declined Pron g
   [ (Singular,
        [ declfin Nom ""
        ; if g=Neu then declfin Acc "" else decline Acc "am"
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Nom (if g=Neu then "ii" else "au")
        ; decline Acc (if g=Neu then "ii" else "au")
        ; declfin Ins "bhyaam"
        ; declfin Dat "bhyaam"
        ; declfin Abl "bhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural, 
        [ if g=Neu then decline_nasalise Nom "i" else decline Nom "as"
        ; if g=Neu then decline_nasalise Acc "i" else decline Acc "as"
   (* Voc Nom Acc Neu ought to have nasal : v.rnti Whitney§389c p. 145 *)
   (* Acc. vaacas with accent on aa or on a        Whitney§391  p. 147 *)
        ; declfin Ins "bhis"
        ; declfin Dat "bhyas"
        ; declfin Abl "bhyas"
        ; decline Gen "aam"
        ; declfin Loc "su" 
         (* viz2 -> vi.tsu but also véd. vik.su Whitney§218a [compute_extra] *)
        ])
   ]             
   ; Bare Noun bare
(* ; Avyayaf bare -- attested ? *)
   ]
;
value build_root_m g trunc stem entry = (* Kale§107 prazaam *)
  let decline case suff = (case,fix stem suff)
  and declcon case suff = (case,fix [ 36 (* n *) :: trunc ] suff) in
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ declcon Voc ""
        ; declcon Nom ""
        ; if g=Neu then declcon Acc "" else decline Acc "am"
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Voc (if g=Neu then "ii" else "au")
        ; decline Nom (if g=Neu then "ii" else "au")
        ; decline Acc (if g=Neu then "ii" else "au")
        ; declcon Ins "bhyaam"
        ; declcon Dat "bhyaam"
        ; declcon Abl "bhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural, 
        [ decline Voc (if g=Neu then "i" else "as")
        ; decline Nom (if g=Neu then "i" else "as")
        ; decline Acc (if g=Neu then "i" else "as")
        ; declcon Ins "bhis"
        ; declcon Dat "bhyas"
        ; declcon Abl "bhyas"
        ; decline Gen "aam"
        ; declcon Loc "su" 
        ])
   ]             
   ]
;
value build_archaic_yuj stem (* yu~nj remnant nasal Kale§97 *) g entry =
  let decline case suff = (case,fix stem suff) 
  and declfinal case = (case, [ 42; 5; 21 (* yuf *) ]) in (* Whitney§386 *)
  enter entry  
   [ Declined Noun g 
   [ (Singular,
        [ declfinal Voc 
        ; declfinal Nom 
        ; if g=Neu then declfinal Acc else decline Acc "am" 
        ])
   ; (Dual, 
        [ decline Voc "au" (* Kale§97 but Whitney§386 "aa" ? *)
        ; decline Nom "au"
        ; decline Acc "au" 
        ])
   ; (Plural, 
        [ decline Voc "as"
        ; decline Nom "as"
        ])
   ]             
   ]
;
(* Root words opt. substitutes in weak cases \Pan{6,1,63} Whitney§397 *)
value build_root_weak g stem entry = 
  let declinev case suff = (case,fix stem suff) 
  and declinec case suff = (case,fix (finalize stem) suff) (* ni.dbhyas *)
  and bare = mirror (finalize stem) in 
  enter entry (* strong stem entry paada danta etc. *)
   [ Declined Noun g
   [ (Singular,
        [ declinev Ins "aa"
        ; declinev Dat "e"
        ; declinev Abl "as"
        ; declinev Gen "as"
        ; declinev Loc "i"
        ])
   ; (Dual, 
        [ declinec Ins "bhyaam"
        ; declinec Dat "bhyaam"
        ; declinec Abl "bhyaam"
        ; declinev Gen "os"
        ; declinev Loc "os"
        ])
   ; (Plural, 
        [ declinev Acc "as"
        ; declinec Ins "bhis"
        ; declinec Dat "bhyas"
        ; declinec Abl "bhyas"
        ; declinev Gen "aam"
        ; declinec Loc "su"
        ])
   ] 
   ; Bare Noun bare
   ; Avyayaf bare
   ]
;
value build_pad g stem entry = (* for catu.spad and other -pad compounds *)
  let decline case form = (case,fix stem form) 
  and bare = fix stem "pat" in 
  enter entry 
   [ Declined Noun g 
   [ (Singular,
        [ decline Nom "paat"
        ; decline Voc "paat"
        ; decline Acc "paadam"
        ; decline Ins "padaa"
        ; decline Dat "pade"
        ; decline Abl "padas"
        ; decline Gen "padas"
        ; decline Loc "padi"
        ] @ if g=Fem then 
        [ decline Nom "padii" ] else [])
   ; (Dual,
        [ decline Nom (if g=Neu then "paadii" else "paadau")
        ; decline Voc (if g=Neu then "paadii" else "paadau")
        ; decline Acc (if g=Neu then "paadii" else "paadau")
        ; decline Ins "paadbhyaam"
        ; decline Dat "paadbhyaam"
        ; decline Abl "paadbhyaam"
        ; decline Gen "paados"
        ; decline Loc "paados"
        ])
   ; (Plural,
        [ decline Nom "paadas"
        ; decline Voc "paadas"
        ; decline Acc "paadas"
        ; decline Ins "paadbhis"
        ; decline Dat "paadbhyas"
        ; decline Abl "paadbhyas"
        ; decline Gen "paadaam"
        ; decline Loc "paatsu"
        ])
   ]
   ; Bare Noun bare
   ; Avyayaf bare
   ]
;
value build_sap g st entry = (* MW saap in strong cases *)
  let decline case suff = (case,fix [ 37 :: [ 1 :: [ 48 :: st ] ] ] suff)
  and declinestr case suff = (case,fix [ 37 :: [ 2 :: [ 48 :: st ] ] ] suff) in
  enter entry 
   [ Declined Noun g
   [ (Singular,
        [ decline Voc ""
        ; declinestr Nom ""
        ; declinestr Acc "am"
        ; decline Ins "aa"
        ; decline Dat "e"
        ; decline Abl "as"
        ; decline Gen "as"
        ; decline Loc "i"
        ])
   ; (Dual,
        [ decline Voc (if g=Neu then "ii" else "au")
        ; declinestr Nom (if g=Neu then "ii" else "au")
        ; declinestr Acc (if g=Neu then "ii" else "au")
        ; decline Ins "bhyaam"
        ; decline Dat "bhyaam"
        ; decline Abl "bhyaam"
        ; decline Gen "os"
        ; decline Loc "os"
        ])
   ; (Plural, 
        [ decline Voc (if g=Neu then "i" else "as")
        ; declinestr Nom (if g=Neu then "i" else "as")
        ; decline Acc (if g=Neu then "i" else "as")
        ; decline Ins "bhis"
        ; decline Dat "bhyas"
        ; decline Abl "bhyas"
        ; decline Gen "aam"
        ; decline Loc "su" 
        ])
   ]
   ]
;
value build_dam entry = (* vedic - unused *)
  let decline case form = (case,code form) in 
  enter entry 
   [ Declined Noun Mas (* arbitrary *)
   [ (Singular,
        [ decline Gen "dan" ])
   ; (Plural,
        [ decline Gen "damaam" ])
   ]
   ; Bare Noun (revcode "dam")
   ]
;
value build_upaanah trunc stem entry = (* Kale§101 trunc = mirror(upaana) *) 
  let bare = [ 32 (* t *) :: trunc ] (* upaanat *) in
  let declineh case suff = (case,fix stem suff)
  and declinet case suff = (case,fix bare suff) in
  enter entry 
   [ Declined Noun Fem
   [ (Singular,
        [ declinet Voc ""
        ; declinet Nom ""
        ; declineh Acc "am"
        ; declineh Ins "aa"
        ; declineh Dat "e"
        ; declineh Abl "as"
        ; declineh Gen "as"
        ; declineh Loc "i"
        ])
   ; (Dual,
        [ declineh Voc "au"
        ; declineh Nom "au"
        ; declineh Acc "au"
        ; declinet Ins "bhyaam"
        ; declinet Dat "bhyaam"
        ; declinet Abl "bhyaam"
        ; declineh Gen "os"
        ; declineh Loc "os"
        ])
   ; (Plural, 
        [ declineh Voc "as"
        ; declineh Nom "as"
        ; declineh Acc "as"
        ; declinet Ins "bhis"
        ; declinet Dat "bhyas"
        ; declinet Abl "bhyas"
        ; declineh Gen "aam"
        ; declinet Loc "su" 
        ])
   ]             
   ; Bare Noun (mirror bare)
   ]
;
(* reduplicated ppr of class 3 verbs or intensives: no nasal in strong stem *)
(* should be replaced by proper tag, rather than matching stem *)
value is_redup = fun (* reduplicating roots, possibly with preverb *)
    [ [ 41 :: [ 3 :: [ 41 :: r ] ] ] when r = revstem "raz" 
        -> False (* razmimat protected from compounds of mimat *)
    | [ 34 :: [ 1 :: [ 34 :: _ ] ] ]           (* daa\#1 -> dadat *)
    | [ 35 :: [ 1:: [ 34 :: _ ] ] ]            (* dhaa\#1 -> dadhat *)   
    | [ 41 :: [ 3 :: [ 41 :: _ ] ] ]           (* maa\#1 -> mimat *) 
    | [ 42 :: [ 5 :: [ 42 :: _ ] ] ]           (* yu\#2 -> yuyat *)
    | [ 43 :: [ 19 :: [ 2 :: [ 24 :: _ ] ] ] ] (* g.r int -> jaagrat *)
    | [ 43 :: [ 20 :: [ 3 :: [ 24 :: _ ] ] ] ] (* gh.r -> jighrat *)
    | [ 43 :: [ 37 :: [ 3 :: [ 37 :: _ ] ] ] ] (* p.r\#1 -> piprat *)
    | [ 43 :: [ 40 :: [ 3 :: [ 39 :: _ ] ] ] ] (* bh.r -> bibhrat *)
    | [ 45 :: [ 49 :: [ 5 :: [ 24 :: _ ] ] ] ] (* hu -> juhvat *) 
    | [ 46 :: [ 3 :: [ 46 :: _ ] ] ]           (* zaa -> zizat *) 
    | [ 48 :: [ 3 :: [ 48 :: _ ] ] ]           (* s.r -> sisrat *) 
    | [ 49 :: [ 1 :: [ 24 :: _ ] ] ]           (* haa\#1 -> jahat *) 
(*  | [ 49 :: [ 3 :: [ 24 :: _ ] ] ]           (* haa\#? -> jihat *) ? *) 
    | [ 49 :: [ 12 :: [ 24 :: _ ] ] ]          (* hu int. -> johvat *)
(*  | [ 19 :: [ 1 :: [ 24 :: _ ] ] ]           (* gam -> jagat *) ? *)
    | [ 41 :: [ 1 :: [ 43 :: [ 17 :: [ 21 :: [ 1 :: [ 22 :: _ ] ] ] ] ] ] ]  
          (* kram int. -> cafkramat *)
    | [ 34 :: [ 1 :: [ 45 :: [ 2 :: [ 45 :: _ ] ] ] ] ](* vad int. -> vaavadat *)
        -> True 
(* Whitney says add: cak.sat daazat daasat zaasat sazcat dhak.sat vaaghat *)
    | _ -> False
    ]
;
value build_auduloma g stem pstem entry = (* au.duloma Kale 26 *)
  let decline case suff = (case,fix stem suff) 
  and declinep case suff = (case,fix pstem suff) in 
  enter entry 
   [ Declined Noun g
   [ (Singular, 
        [ decline Voc "e"
        ; decline Nom "is"
        ; decline Acc "im"
        ; decline Ins "inaa"
        ; decline Dat "aye"
        ; decline Abl "es"
        ; decline Gen "es"
        ; decline Loc "au"
        ])
   ; (Dual, 
        [ decline Voc "ii"
        ; decline Nom "ii"
        ; decline Acc "ii"
        ; decline Ins "ibhyaam"
        ; decline Dat "ibhyaam"
        ; decline Abl "ibhyaam"
        ; decline Gen "yos"
        ; decline Loc "yos"
        ])
   ; (Plural, 
        [ declinep Voc "aas"
        ; declinep Nom "aas"
        ; declinep Acc "aan"
        ; declinep Ins "ais"
        ; declinep Dat "ebhyas"
        ; declinep Abl "ebhyas"
        ; declinep Gen "aanaam"
        ; declinep Loc "esu"
        ])
   ] ]
;

(* Pronouns *)
value build_sa_tad g stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry (
   [ Declined Pron g
   [ (Singular, let l =
        [ decline Nom (if g=Mas then "sas" (* gives e.sa.h for etad *)
                                else "tat") (* final *)
        ; decline Acc (if g=Mas then "tam" else "tat")
        ; decline Ins "tena"
        ; decline Dat "tasmai"
        ; decline Abl "tasmaat"
        ; decline Gen "tasya"
        ; decline Loc "tasmin"
        ] in if g=Mas then 
        [ decline Nom "sa" :: l ] (* usable before consonants, see Dispatcher *)
             else l) 
   ; (Dual, 
        [ decline Nom (if g=Mas then "tau" else "te")
        ; decline Acc (if g=Mas then "tau" else "te")
        ; decline Ins "taabhyaam"
        ; decline Dat "taabhyaam"
        ; decline Abl "taabhyaam"
        ; decline Gen "tayos"
        ; decline Loc "tayos"
        ])
   ; (Plural,
        [ decline Nom (if g=Mas then "te" else "taani")
        ; decline Acc (if g=Mas then "taan" else "taani")
        ; decline Ins "tais"
        ; decline Dat "tebhyas"
        ; decline Abl "tebhyas"
        ; decline Gen "te.saam"
        ; decline Loc "te.su"
        ])
   ] 
   ; Indecl Tas (fix stem "tatas") 
   ] @ (if g=Neu && stem = [ 10 ] then [ Bare Pron (code "etat") ]
          else []))
;
value build_sya_tyad g entry = (* Vedic Whitney §499a actually skipped *)
  let decline case form = (case,code form) in 
  enter entry  
   [ Declined Pron g
   [ (Singular, let l =
        [ decline Nom (if g=Mas then "syas" else "tyat")
        ; decline Acc (if g=Mas then "tyam" else "tyat")
        ; decline Ins "tyena"
        ; decline Dat "tyasmai"
        ; decline Abl "tyasmaat"
        ; decline Gen "tyasya"
        ; decline Loc "tyasmin"
        ] in if g=Mas then 
        [ decline Nom "sya" :: l ]
                      else l)
   ; (Dual,
        [ decline Nom (if g=Mas then "tyau" else "tye")
        ; decline Acc (if g=Mas then "tyau" else "tye")
        ; decline Ins "tyaabhyaam"
        ; decline Dat "tyaabhyaam"
        ; decline Abl "tyaabhyaam"
        ; decline Gen "tyayos"
        ; decline Loc "tyayos"
        ])
   ; (Plural,
        [ decline Nom (if g=Mas then "tye" else "tyaani")
        ; decline Acc (if g=Mas then "tyaan" else "tyaani")
        ; decline Ins "tyais"
        ; decline Dat "tyebhyas"
        ; decline Abl "tyebhyas"
        ; decline Gen "tye.saam"
        ; decline Loc "tye.su"
        ])
   ] 
   ; Indecl Tas (code "tyatas")
   ]
;
(* Pronominal stems (mirror+lopa) of pronouns usable as nominals. 
   When used as pronouns, they denote the relative position. *)
(* Bhat: puurva, para, avara, dak.si.na, uttara, apara, adhara, sva, antara:
   when used pronominally use optionally pronominal endings. 
   Missing from his list: aneka, pazcima, nema, ubhaya, sarva, vizva *)
value pseudo_nominal_basis = fun 
  [ [ 17; 10; 36; 1 ] (* aneka *) (* possibly also eka, anya ? *)
  | [ 31; 3; 47; 17; 1; 34 ] (* dak.si.na *) 
  | [ 41; 3; 22; 46; 1; 37 ] (* pazcima *) 
  | [ 41; 10; 36 ] (* nema Whitney§525c *)
  | [ 42; 1; 40; 5 ] (* ubhaya *) 
  | [ 43; 1; 32; 32; 5 ] (* uttara *) 
  | [ 43; 1; 32; 36; 1 ] (* antara *)
  | [ 43; 1; 35; 1 ] (* adhara *)
  | [ 43; 1; 37 ] (* para *)
  | [ 43; 1; 37; 1 ] (* apara *)
  | [ 43; 1; 45; 1 ] (* avara *)
  | [ 45; 43; 1; 48 ] (* sarva *)
  | [ 45; 43; 6; 37 ] (* puurva Whitney§524 *)
(* NB ga.na {puurva, paraavara, dak.si.na, uttara, apara, adhara} -: paraavara 
      +: aneka, pazcima, nema, ubhaya, antara, para, avara, sarva, vizva, sva *)
  | [ 45; 46; 3; 45 ] (* vizva *) 
  | [ 45; 48 ] (* sva *) -> True
  | _ -> False
  ] 
;
value tasil_gen = fun 
  (* lexicalized tasils among pseudo_nominal stems *)
  [ [ 42; 1; 40; 5 ] (* ubhaya *) 
  | [ 43; 1; 37 ] (* para *)
  | [ 45; 46; 3; 45 ] (* vizva *) 
  | [ 45; 48 ] (* sva *) -> []
  | stem -> [ Indecl Tas (fix stem "atas") ] (* dak.si.natas *) 
  ] 
;

(* builds existentials with -cit and -cana from inflected forms of kim *)
value existential part =
  let entry = "ki~n" ^ part (* ad-hoc hand made e-sandhi *)
  and glue case form = (case,code (form ^ part)) in
  enter entry (
    [ Declined Noun Mas  
    [ (Singular, 
         [ glue Nom "kaz" (* kazcit *)
         ; glue Acc "ka~n" (* ? *)
         ; glue Ins "kena"
         ; glue Dat "kasmai"
         ; glue Abl "kasmaac"
         ; glue Gen "kasya"
         ; glue Loc "kasmi.mz"
         ])
    ; (Plural,
         [ glue Nom "ke" (* kecit *)
         ; glue Acc "kaa.mz" (* ? *)
         ])
    ]
    ; Declined Noun Neu 
    [ (Singular, 
         [ glue Nom "ki~n" (* ki.mcit *)
         ; glue Acc "ki~n"
         ; glue Ins "kena"
         ; glue Dat "kasmai"
         ; glue Abl "kasmaac"
         ; glue Gen "kasya"
         ; glue Loc "kasmi.mz"
         ])
    ; (Plural,
         [ glue Nom "kaani" (* kaanicit *)
         ; glue Acc "kaani" 
         ; glue Loc "ke.su" 
         ])
    ]
    ; Declined Noun Fem
    [ (Singular, 
         [ glue Nom "kaa" (* ? *)
         ; glue Acc "kaa~n"
         ; glue Ins "kayaa"
         ; glue Dat "kasyai"
         ; glue Abl "kasyaa.mz"
         ; glue Gen "kasyaa.mz"
         ; glue Loc "kasyaa~n" (* kasyaa~ncid attested *)
         ])
    ; (Plural,
         [ glue Nom "kaaz" (* ? *)
         ; glue Acc "kaaz" 
         ])
    ]
    ; Bare Noun (code entry)
    ])
;
value build_pron_a g stem entry = (* g=Mas ou g=Neu *)
  let pseudo_nominal = pseudo_nominal_basis stem 
  and neu_nom_acc = match stem with
      [ [ 17 ] -> (* kim *) "im"
      | [ 42 ] (* yad *) 
      | [ 43; 1; 32; 1; 17 ] (* katara *)
      | [ 41; 1; 32; 1; 17 ] (* katama *)
      | [ 43; 1; 32; 3 ] (* itara *)
      | [ 42; 36; 1 ] (* anya *) 
      | [ 43; 1; 32; 1; 42; 36; 1 ] (* anyatara *)
      | [ 45; 32 ] (* tva *)-> "at" (* Whitney§523 *)
      | _ -> (* eka, ekatara, vizva, sva, sarva, ... *) "am"
      ] in
  let decline case suff = (case,fix stem suff) 
  and phase = if pseudo_nominal then Noun else Pron in 
  enter entry (
   [ Declined phase g 
   [ (Singular, let l =
        [ decline Nom (if g=Mas then "as" else neu_nom_acc)
        ; decline Acc (if g=Mas then "am" else neu_nom_acc)
        ; decline Ins "ena"
        ; decline Dat "asmai"
        ; decline Abl "asmaat"
        ; decline Gen "asya"
        ; decline Loc "asmin"
        ] in if pseudo_nominal then 
        [ decline Abl "aat" :: [ decline Loc "e" :: 
        [ decline Voc "a" :: l ] ] ] else l)
   ; (Dual, if entry = "eka" (* singular only *) 
            || entry = "ubhaya"  (* no dual - dubious *) then [] 
            else let l = 
        [ decline Nom (if g=Mas then "au" else "e")
        ; decline Acc (if g=Mas then "au" else "e")
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ] in if pseudo_nominal then 
        [ decline Voc (if g=Mas then "au" else "e") :: l ] else l)
   ; (Plural, if entry = "eka" (* singular only *) then [] 
              else let l = 
        [ decline Nom (if g=Mas then "e" else "aani")
        ; decline Acc (if g=Mas then "aan" else "aani")
        ; decline Ins "ais"
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "e.saam"
        ; decline Loc "e.su"
        ] in if pseudo_nominal then 
                if g=Mas then [ decline Nom "aas" :: [ decline Voc "aas" :: l ] ]
                else (* g=Neu *) [ decline Voc "aani" :: l ] 
             else l)
   ] ] @ (if g=Neu then 
             let iic = match stem with 
                       [ [ 17 ] (* kim *) -> code "kim"
                       | [ 42 ] (* yad *) -> code "yat"
                       | [ 42; 36; 1 ] (* anyad *) -> code "anyat"
                       | _ -> mirror [ 1 :: stem ]
                       ] in 
             [ Bare phase iic ]
          else (* g=Mas *) if stem = [ 42; 36; 1 ] (* anya *) 
               then [ Bare phase (code "anya") ] (* optional anya- *)
          else if pseudo_nominal then
                  [ Avyayaf (fix stem "am"); Avyayaf (fix stem "aat") ] @ 
                  tasil_gen stem 
               else [])
       @ (if g=Mas then match entry with
                       [ "eka" -> [ Cvi (code "ekii") ] 
                       | "sva" -> [ Cvi (code "svii") ] 
                       | _ -> [] 
                       ]
          else []))
;
value build_saa stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Pron Fem
   [ (Singular,
        [ decline Nom "saa"
        ; decline Acc "taam"
        ; decline Ins "tayaa"
        ; decline Dat "tasyai"
        ; decline Abl "tasyaas"
        ; decline Gen "tasyaas"
        ; decline Loc "tasyaam"
        ])
   ; (Dual, 
        [ decline Nom "te"
        ; decline Acc "te"
        ; decline Ins "taabhyaam"
        ; decline Dat "taabhyaam"
        ; decline Abl "taabhyaam"
        ; decline Gen "tayos"
        ; decline Loc "tayos"
        ])
   ; (Plural,
        [ decline Nom "taas"
        ; decline Acc "taas"
        ; decline Ins "taabhis"
        ; decline Dat "taabhyas"
        ; decline Abl "taabhyas"
        ; decline Gen "taasaam"
        ; decline Loc "taasu"
        ])
   ] ]
;
value build_syaa stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Pron Fem
   [ (Singular,
        [ decline Nom "syaa"
        ; decline Acc "tyaam"
        ; decline Ins "tyayaa"
        ; decline Dat "tyasyai"
        ; decline Abl "tyasyaas"
        ; decline Gen "tyasyaas"
        ; decline Loc "tyasyaam"
        ])
   ; (Dual, 
        [ decline Nom "tye"
        ; decline Acc "tye"
        ; decline Ins "tyaabhyaam"
        ; decline Dat "tyaabhyaam"
        ; decline Abl "tyaabhyaam"
        ; decline Gen "tyayos"
        ; decline Loc "tyayos"
        ])
   ; (Plural,
        [ decline Nom "tyaas"
        ; decline Acc "tyaas"
        ; decline Ins "tyaabhis"
        ; decline Dat "tyaabhyas"
        ; decline Abl "tyaabhyas"
        ; decline Gen "tyaasaam"
        ; decline Loc "tyaasu"
        ])
   ] ]
;
value build_pron_aa stem entry = 
  let pseudo_nominal = pseudo_nominal_basis stem in
  let decline case suff = (case,fix stem suff)  
  and phase = if pseudo_nominal then Noun else Pron in 
  enter entry 
   [ Declined phase Fem
   [ (Singular, let l = 
        [ decline Nom "aa"
        ; decline Acc "aam"
        ; decline Ins "ayaa"
        ; decline Dat "asyai"
        ; decline Abl "asyaas"
        ; decline Gen "asyaas"
        ; decline Loc "asyaam"
        ] in if pseudo_nominal then 
        [ decline Voc "e" :: l ] else l)
   ; (Dual, if entry = "ekaa" then (* singular only *) [] else let l =
        [ decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ] in if pseudo_nominal then 
        [ decline Voc "e" :: l ] else l)
   ; (Plural, if entry = "ekaa" then (* singular only *) [] else let l =
        [ decline Nom "aas"
        ; decline Acc "aas"
        ; decline Ins "aabhis"
        ; decline Dat "aabhyas"
        ; decline Abl "aabhyas"
        ; decline Gen "aasaam"
        ; decline Loc "aasu"
        ] in if pseudo_nominal then 
        [ decline Voc "aas" :: l ] else l)
   ] ]
;
value build_ayam_idam g = (* g=Mas or Neu *)
  enter "idam"
   [ Declined Pron g
   [ (Singular,
        [ register Nom (if g=Mas then "ayam" else "idam")
        ; register Acc (if g=Mas then "imam" else "idam")
        ; register Ins "anena"
        ; register Dat "asmai"   (* also "atas" *)
        ; register Abl "asmaat"
        ; register Gen "asya"
        ; register Loc "asmin"
        ])
   ; (Dual,
        [ register Nom (if g=Mas then "imau" else "ime")
        ; register Acc (if g=Mas then "imau" else "ime")
        ; register Ins "aabhyaam"
        ; register Dat "aabhyaam"
        ; register Abl "aabhyaam"
        ; register Gen "anayos"
        ; register Loc "anayos"
        ])
   ; (Plural,
        [ register Nom (if g=Mas then "ime" else "imaani")
        ; register Acc (if g=Mas then "imaan" else "imaani")
        ; register Ins "ebhis"
        ; register Dat "ebhyas"
        ; register Abl "ebhyas"
        ; register Gen "e.saam"
        ; register Loc "e.su"
        ])
   ] ]
;
value build_iyam () =
  enter "idam"
   [ Declined Pron Fem
   [ (Singular, 
        [ register Nom "iyam"
        ; register Acc "imaam"
        ; register Ins "anayaa"
        ; register Dat "asyai"
        ; register Abl "asyaas"
        ; register Gen "asyaas"
        ; register Loc "asyaam"
        ])
   ; (Dual,
        [ register Nom "ime"
        ; register Acc "ime"
        ; register Ins "aabhyaam"
        ; register Dat "aabhyaam"
        ; register Abl "aabhyaam"
        ; register Gen "anayos"
        ; register Loc "anayos"
        ])
   ; (Plural, 
        [ register Nom "imaas"
        ; register Acc "imaas"
        ; register Ins "aabhis"
        ; register Dat "aabhyas"
        ; register Abl "aabhyas"
        ; register Gen "aasaam"
        ; register Loc "aasu"
        ])
   ] ]
;
value build_asau_adas g  = 
  enter "adas"
   [ Declined Pron g
   [ (Singular, let accu = 
        [ register Nom (if g=Mas then "asau" else "adas")
        ; register Acc (if g=Mas then "amum" else "adas")
        ; register Ins "amunaa"
        ; register Dat "amu.smai"
        ; register Abl "amu.smaat"
        ; register Gen "amu.sya"
        ; register Loc "amu.smin"
        ] in if g=Mas then [ register Nom "asakau" :: accu ] 
                           (* Pan{7,2,107} with yaka.h/yakaa *)       
             else accu)    
   ; (Dual, 
        [ register Nom "amuu"
        ; register Acc "amuu"
        ; register Ins "amuubhyaam"
        ; register Dat "amuubhyaam"
        ; register Abl "amuubhyaam"
        ; register Gen "amuyos"
        ; register Loc "amuyos"
        ])
   ; (Plural,
        [ register Nom (if g=Mas then "amii" else "amuuni")
        ; register Acc (if g=Mas then "amuun" else "amuuni")
        ; register Ins "amiibhis"
        ; register Dat "amiibhyas"
        ; register Abl "amiibhyas"
        ; register Gen "amii.saam"
        ; register Loc "amii.su"
        ])
   ] ]
;
value build_asau_f () = 
  enter "adas" 
   [ Declined Pron Fem      
   [ (Singular,  
        [ register Nom "asau"
        ; register Nom "asakau" (* Pan{7,2,107} with yaka.h/yakaa *)
        ; register Acc "amuum"
        ; register Ins "amuyaa" 
        ; register Dat "amu.syai"
        ; register Abl "amu.syaas"
        ; register Gen "amu.syaas"
        ; register Loc "amu.syaam"
        ])
   ; (Dual, 
        [ register Nom "amuu"
        ; register Acc "amuu"
        ; register Ins "amuubhyaam"
        ; register Dat "amuubhyaam"
        ; register Abl "amuubhyaam"
        ; register Gen "amuyos"
        ; register Loc "amuyos"
        ])
   ; (Plural, 
        [ register Nom "amuus"
        ; register Acc "amuus"
        ; register Ins "amuubhis"
        ; register Dat "amuubhyas"
        ; register Abl "amuubhyas"
        ; register Gen "amuu.saam"
        ; register Loc "amuu.su"
        ])
   ] ]
;
value build_ena g entry = 
  enter "idam" (* Whitney§500 *)
   [ Declined Pron g
   [ (Singular, 
    (* No nominative - anaphoric pronoun - in non accented position *) 
        [ register Acc (match g with
            [ Mas -> "enam"
            | Neu -> "enat"
            | Fem -> "enaam"
            | _ -> raise (Control.Anomaly "Nouns")
            ])
        ; register Ins (match g with
            [ Mas -> "enena"
            | Neu -> "enena"
            | Fem -> "enayaa"
            | _ -> raise (Control.Anomaly "Nouns")
            ])
        ])
   ; (Dual,
        [ register Acc (match g with
            [ Mas -> "enau"
            | Neu -> "ene"
            | Fem -> "ene"
            | _ -> raise (Control.Anomaly "Nouns")
            ])
        ; register Gen "enayos"
        ; register Loc "enayos"
        ])
   ; (Plural, 
        [ register Acc (match g with
            [ Mas -> "enaan"
            | Neu -> "enaani"
            | Fem -> "enaas"
            | _ -> raise (Control.Anomaly "Nouns")
            ])
        ])
   ] ]
;
value build_aham () = 
  let decline case form = (case,code form) in 
  enter "asmad" (* Paninian entry *)
   [ Declined Pron (Deictic Speaker)
   [ (Singular, 
        [ decline Nom "aham"
        ; decline Acc "maam"
        ; decline Acc "maa" (* encl *)
        ; decline Ins "mayaa"
        ; decline Dat "mahyam"
        ; decline Dat "me" (* encl *)
        ; decline Abl "mat"
        ; decline Abl "mattas" 
        ; decline Gen "mama"
        ; decline Gen "me" (* encl *)
        ; decline Loc "mayi"
        ])
   ; (Dual,
        [ decline Nom "aavaam" (* Vedic "aavam" \Pan{7.2.88}  Burrow p267 *)
        ; decline Acc "aavaam" 
        ; decline Acc "nau" (* encl *)
        ; decline Ins "aavaabhyaam"
        ; decline Dat "aavaabhyaam"
        ; decline Dat "nau" (* encl *)
        ; decline Abl "aavaabhyaam"
        ; decline Gen "aavayos"
        ; decline Gen "nau" (* encl *)
        ; decline Loc "aavayos"
        ])
   ; (Plural, 
        [ decline Nom "vayam"
        ; decline Acc "asmaan"
        ; decline Acc "nas" (* encl *)
        ; decline Ins "asmaabhis"
        ; decline Dat "asmabhyam"
        ; decline Dat "nas" (* encl *)
        ; decline Abl "asmat"
        ; decline Abl "asmattas"
        ; decline Gen "asmaakam"
        ; decline Gen "nas" (* encl *)
        ; decline Loc "asmaasu"
        ])
   ]             
   ; Bare Pron (code "aham")
   ; Bare Pron (code "mat") (* \Pan{7,2,98} when meaning is singular *)
   ; Bare Pron (code "asmat") (* \Pan{7,2,98} when meaning is plural *)
   ]
;
value build_tvad () = 
  let decline case form = (case,code form) in 
  enter "yu.smad" (* Paninian entry *)
   [ Declined Pron (Deictic Listener)
   [ (Singular, 
        [ decline Nom "tvam"
        ; decline Acc "tvaam"
        ; decline Acc "tvaa" (* encl *)
        ; decline Ins "tvayaa"
        ; decline Dat "tubhyam"
        ; decline Dat "te" (* encl *)
        ; decline Abl "tvat"
        ; decline Abl "tvattas"
        ; decline Gen "tava"
        ; decline Gen "te" (* encl *)
        ; decline Loc "tvayi"
        ])
   ; (Dual, 
        [ decline Nom "yuvaam" (* Vedic "yuvam" \Pan{7.2.88} Burrow p267 *)
        ; decline Acc "yuvaam"
        ; decline Acc "vaam" (* encl *)
        ; decline Ins "yuvaabhyaam"
        ; decline Dat "yuvaabhyaam"
        ; decline Dat "vaam" (* encl *)
        ; decline Abl "yuvaabhyaam"
        ; decline Gen "yuvayos"
        ; decline Gen "vaam" (* encl *)
        ; decline Loc "yuvayos"
        ])
   ; (Plural, 
        [ decline Nom "yuuyam"
        ; decline Acc "yu.smaan"
        ; decline Acc "vas" (* encl *)
        ; decline Ins "yu.smaabhis"
        ; decline Dat "yu.smabhyam"
        ; decline Dat "vas" (* encl *)
        ; decline Abl "yu.smat"
        ; decline Abl "yu.smattas"
        ; decline Gen "yu.smaakam"
        ; decline Gen "vas" (* encl *)
        ; decline Loc "yu.smaasu"
        ])
   ] 
   ; Bare Pron (code "tvat") (* \Pan{7,2,98} when meaning is singular *)
   ; Bare Pron (code "yu.smat") (* \Pan{7,2,98} when meaning is plural *)
   ]
;

(* Numerals *)

value build_dvi entry = 
  let stem = revcode "dv" in 
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Dual,
        [ decline Voc "au"
        ; decline Nom "au"
        ; decline Acc "au"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ]
   ; Declined Noun Neu
  [ (Dual, 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ]
   ; Declined Noun Fem
  [ (Dual, 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ]
   ; Bare Noun (code "dvaa")
   ; Bare Noun (code "dvi")
   ]
;
value build_tri entry = 
  let decline case suff = 
     (case,fix (revcode "tr") suff)
  and declinf case suff = 
     (case,fix (revcode "tis") suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Plural,
        [ decline Voc "ayas"
        ; decline Nom "ayas"
        ; decline Acc "iin"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "ayaa.naam"
        ; decline Loc "i.su"
        ])
   ]
   ; Declined Noun Neu
   [ (Plural,
        [ decline Voc "ii.ni"
        ; decline Nom "ii.ni"
        ; decline Acc "ii.ni"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "ayaa.naam"
        ; decline Loc "i.su"
        ])
   ]
   ; Declined Noun Fem
   [ (Plural,
        [ declinf Voc "ras"
        ; declinf Nom "ras"
        ; declinf Acc "ras"
        ; declinf Ins ".rbhis"
        ; declinf Dat ".rbhyas"
        ; declinf Abl ".rbhyas"
        ; declinf Gen ".r.naam"
        ; declinf Loc ".r.su"
        ])
   ]             
   ; Bare Noun (code "tri")
   ; Bare Noun (code "tis.r") (* tis.rdhanva Whitney§482f *)
   ] 
;
value build_catur entry = 
  let decline case suff = 
   (case,fix (revcode "cat") suff)
  and declinf case suff = 
   (case,fix (revcode "catas") suff) in 
  enter entry 
   [ Declined Noun Mas
   [ (Plural,
        [ decline Voc "vaaras"
        ; decline Nom "vaaras"
        ; decline Acc "uras"
        ; decline Ins "urbhis"
        ; decline Dat "urbhyas"
        ; decline Abl "urbhyas"
        ; decline Gen "ur.naam"
        ; decline Loc "ur.su"
        ])
   ]
   ; Declined Noun Neu
   [ (Plural,
        [ decline Voc "vaari"
        ; decline Nom "vaari"
        ; decline Acc "vaari"
        ; decline Ins "urbhis"
        ; decline Dat "urbhyas"
        ; decline Abl "urbhyas"
        ; decline Gen "ur.naam"
        ; decline Loc "ur.su"
        ])
   ]
   ; Declined Noun Fem
   [ (Plural,
        [ declinf Voc "ras"
        ; declinf Nom "ras"
        ; declinf Acc "ras"
        ; declinf Ins ".rbhis"
        ; declinf Dat ".rbhyas"
        ; declinf Abl ".rbhyas"
        ; declinf Gen ".r.naam"
        ; declinf Loc ".r.su"
        ])
   ]
   ; Bare Noun (code "catur")
   ; Avyayaf (code "caturam")
   ]
;
value build_sat entry = 
  let stem = revcode ".sa" in
  let decline case suff = (case,fix stem suff) in 
  enter entry 
   [ Declined Noun (Deictic Numeral)
   [ (Plural,
        [ decline Voc ".t"
        ; decline Nom ".t"
        ; decline Acc ".t"
        ; decline Ins ".dbhis"
        ; decline Dat ".dbhyas"
        ; decline Abl ".dbhyas"
        ; decline Gen ".n.naam"
        ; decline Loc ".tsu"
        ])
   ]         
   ; Bare Noun (code ".sa.t")
   ]
;
(* Numerals 5, 7, 8, 9, 10, 11-19 *)
value build_num stem entry = 
  let decline case suff = (case,fix stem suff) in 
  enter entry (
   [ Declined Noun (Deictic Numeral)
   [ (Plural, let l =
        [ decline Nom "a" (* plural although no proper plural form Whitney§483 *)
        ; decline Acc "a" 
        ; decline Ins "abhis"
        ; decline Dat "abhyas"
        ; decline Abl "abhyas"
        ; decline Gen "aanaam"
        ; decline Loc "asu"
        ] in if entry = "a.s.tan" then 
        [ decline Nom "au" (* remains of dual form 8 as a pair of 4 (Vedic) *)
        ; decline Acc "au" 
        ; decline Ins "aabhis" (* Pan{7,2,84} opt discussed Subrahmanyam p130 *)
        ; decline Dat "aabhyas"
        ; decline Abl "aabhyas"
        ; decline Loc "aasu"
        ] @ l   else l)
   ]
   ; Bare Noun (wrap stem 1) 
   ; Cvi (wrap stem 4) 
   ] @ (if entry = "a.s.tan" then
           [ Bare Noun (wrap stem 2) (* a.s.taa *) ] 
        else []))
;
value build_kati entry = (* MW Gram§227a *)
  let decline case suff = 
     (case,fix (revcode "kat") suff) in 
  enter1 entry
   ( Declined Noun (Deictic Numeral)
   [ (Plural,
        [ decline Nom "i"
        ; decline Acc "i"
        ; decline Ins "ibhis"
        ; decline Dat "ibhyas"
        ; decline Abl "ibhyas"
        ; decline Gen "iinaam"
        ; decline Loc "i.su"
        ])
   ]
   )
;
value build_katicit entry = (* MW Gram§230 *)
  let decline case suff = 
     (case,fix (revcode "kat") suff) in 
  enter1 entry
   ( Declined Noun (Deictic Numeral)
   [ (Plural,
        [ decline Nom "icit"
        ; decline Acc "icit"
        ; decline Ins "ibhizcit"
        ; decline Dat "ibhyazcit"
        ; decline Abl "ibhyazcit"
        ; decline Gen "iinaa~ncit"
        ; decline Loc "i.sucit"
        ])
   ]
   )
;
(* Here end the declension tables *)

(* The next two functions, as well as the special cases for -vas ought to
   disappear, when declension will be called with a fuller morphological tag, 
   and not just the gender *)
value pprvat = fun 
  [ "avat" | "aapnuvat" | "kurvat" | "jiivat" | "dhaavat" | "dhaavat#1"
  | "dhaavat#2" | "bhavat#1" | "z.r.nvat" | "zaknuvat" -> True
  | "azaknuvat" -> True (* privative of ppr - to be completed *)
  | _ -> False
  ]
and pprmat = fun 
  [ "jamat" | "dyumat" | "bhaamat" -> True
  | _ -> False
  ]
;
(* tad -> tat yad -> yat cid -> cit etc mais pas de visarga pour r ou s *)
value terminal_form = fun
  [ [ 34 :: w ] -> [ 32 :: w ]
  | w -> w
  ]
;
(* Big switch between paradigms. [e:string] is the entry, [stem:word] one of 
   its (reversed) stems, [d:declension_class] gives gender or indeclinable, 
   [p:string] provides morphology or is empty if not known *)
value compute_nouns_stem_form e stem d p = 
  try match d with 
  [ Gender g -> match g with 
    [ Mas -> match stem with 
      [ [ 1 :: r1 ] (* -a *) ->  match r1 with
            [ [ 17 ] (* ka as mas stem of kim *) 
            | [ 17; 10 ] (* eka *) 
            | [ 17; 10; 36; 1 ] (* aneka *) 
            | [ 31; 3; 47; 17; 1; 34 ] (* dak.si.na *)
            | [ 41; 1; 32; 1; 17 ] (* katama *)
            | [ 41; 3; 22; 46; 1; 37 ] (* pazcima *)
            | [ 41; 10; 36 ] (* nema Whitney§525c *)
            | [ 42 ] (* ya\#1 *) 
            | [ 42; 1; 40; 5 ] (* ubhaya *)
            | [ 42; 36; 1 ] (* anya *) 
            | [ 43; 1; 32; 1; 17 ] (* katara *)
            | [ 43; 1; 32; 1; 17; 10 ] (* ekatara *) 
            | [ 43; 1; 32; 3 ] (* itara *)
            | [ 43; 1; 32; 1; 42; 36; 1 ] (* anyatara *) (* Whitney§523 *)
            | [ 43; 1; 32; 32; 5 ] (* uttara *)
            | [ 43; 1; 32; 36; 1 ] (* antara *)
            | [ 43; 1; 35; 1 ] (* adhara *)
            | [ 43; 1; 37 ] (* para *)
            | [ 43; 1; 37; 1 ] (* apara *)
            | [ 43; 1; 45; 1 ] (* avara *)
            | [ 45; 43; 1; 48 ] (* sarva *)
            | [ 45; 43; 6; 37 ] (* puurva *)
            | [ 45; 46; 3; 45 ] (* vizva *) 
            | [ 45; 32 ] (* tva *) 
            | [ 45; 48 ] (* sva *) -> build_pron_a Mas r1 e
            | [ 36; 10 ] (* ena *) -> build_ena Mas "idam"
            | [ 47; 10 ] (* e.sa *) when (e="etad" || e="e.sa#1" || e="e.sa")
                 -> build_sa_tad Mas [ 10 ] e 
            | [ 48 ] (* sa *) when (e="tad" || e="sa#2" || e="sa")
                 -> build_sa_tad Mas [] e 
            | [ 42; 48 ] (* sya *) -> build_sya_tyad Mas e
            | [ 41; 12; 44; 5; 29; 13 ] (* au.duloma *) -> (* Kale 26 *)
              let ps = revcode "u.duloma" in build_auduloma Mas r1 ps e
            | _ -> build_mas_a r1 e
            ]
      | [ 2 :: r1 ] (* -aa - rare *) -> match r1 with
          [ [ 19 :: [ 1 :: [ 41 :: [ 2 :: [ 48 ] ] ] ] ] (* saamagaa *)
          | [ 28 :: [ 47 :: _ ] ] (* -.s.thaa savya.s.thaa *)
          | [ 33 :: [ 48 :: _ ] ] (* -sthaa (?) *)
          | [ 34 :: _ ] (* -daa yazodaa *)
          | [ 35 :: _ ] (* -dhaa yazodhaa *)
          | [ 37 :: _ ] (* -paa gopaa vizvapaa dhenupaa somapaa etc Kale *) 
          | [ 40 :: _ ] (* vibhaa2 *)
          | [ 41 :: _ ] (* pratimaa and -dhmaa:
                           pa.nidhmaa zafkhadhmaa mukhadhmaa agnidhmaa *)
          | [ 42 :: [ 14 :: _ ] ] (* zubha.myaa *) 
          | [ 43 :: [ 17 :: _ ] ] (* -kraa dadhikraa *) 
          | [ 43 :: _ ] (* -raa2 *) 
              -> build_mono_aa Mas r1 e 
          | [ 49; 2; 49 ] (* haahaa *) 
          | [ 31; 2; 43 ] (* raa.naa *) -> build_mas_aa_no_root r1 e
          | _ -> report stem g (* monitoring *)
          ]
      | [ 3 :: r1 ] (* -i *) -> match e with 
          [ "sakhi" -> build_sakhi r1 e True
          | "pati" -> (* \Pan{I.4.8,9} optional ghi *)
                      do { build_sakhi r1 e False; build_mas_i stem r1 e }
          | _ -> build_mas_i stem r1 e (* agni, etc (ghi) *) 
          ]
      | [ 4 :: r1 ] (* -ii - rare *) -> 
          if bi_consonantal r1 then build_bicons_ii Mas r1 e (* yavakrii *) 
          else if monosyl r1 || compound_monosyl_ii r1 
               then build_mono_ii Mas r1 e 
               else build_poly_ii Mas r1 e (* rathii sudhii *)
      | [ 5 :: r1 ] (* -u *) -> match r1 with
          [ [ 27; 47; 12; 43; 17 ] -> build_krostu r1 e (* = kro.s.t.r *)
          | _ -> build_mas_u stem r1 e (* vaayu, etc (ghi) *) 
          ]
      | [ 6; 49; 6; 49 ] (* huuhuu *) -> build_huuhuu e  
      | [ 6 :: r1 ] (* -uu - rare *) -> 
          if monosyl r1 || compound_monosyl_uu r1 then build_mono_uu Mas r1 e
          else build_poly_uu Mas r1 e (* sarvatanuu *)
               (* vedic polysyllabic in uu are of utmost rarity - Whitney §355 *)
      | [ 7 :: r1 ] (* -.r *) -> match r1 with
          [ [ 27; 47; 12; 43; 17 ] -> build_krostu r1 e (* kro.s.t.r Muller§236 *)
          | [ 32 :: r2 ] (* -t.r *) -> match r2 with
               [ [ 3; 37 ] (* pit.r *) (* relationships McDonell §101 *)
               | [ 2; 41; 2; 24 ] (* jaamaat.r *)
               | [ 36; 1; 42; 1; 37; 3 ] (* upayant.r *) 
               | [ 2; 43; 40 ] (* bhraat.r *) -> build_mas_ri_g r1 e
               (* napt.r bhart.r pari.net.r dev.r: parenthood relation follow: *)
               | _ -> (* dhaat.r general agent paradigm *) build_mas_ri_v r1 e
             ]
          | [ 36 ] (* n.r *) -> build_nri r1 e
          |  _ -> build_mas_ri_v r1 e
          ]
      | [ 8 :: _ ] 
      | [ 9 :: _ ] -> report stem g
      | [ 10 :: r1 ] (* -e *) -> build_e Mas r1 e (* apte (?) *)
      | [ 11 :: r1 ] -> match r1 with
            [ [ 43 ] (* rai *) -> build_rai Mas [ 2; 43 ] e
            | _ -> report stem g
            ]
      | [ 12 :: r1 ] (* -o *) -> build_o Mas r1 e
      | [ 13 :: r1 ] (* -au *) -> match r1 with
            [ [ 48; 1 ] (* asau *) -> build_asau_adas Mas 
            | _ -> build_au Mas r1 e
            ]
      | [ 22 :: r1 ] (* -c *) -> match r1 with
            [ [ 1 :: r2 ] (* -ac *) -> match r2 with
                 [ [] -> () (* ac utilisé seulement avec px *)
                 | [ 42 :: r3 ] (* yac *) -> build_mas_yac r3 e
                 | [ 45 :: r3 ] (* vac *) -> build_mas_vac r3 e
                 | [ 37 :: r3 ] (* pac *) -> build_root Mas stem e
                 |  _ (* udac ... *) -> build_mas_ac r2 e 
                 ]
            | [ 2 :: r2 ] (* -aac *) -> match r2 with
                 [ [ 37; 1 ] (* apa-ac *)
                 | [ 42; 48; 1; 17 ] (* kasya-ac *)
                 | [ 43; 1; 37 ] (* para-ac *)
                 | [ 43; 37 ] (* pra-ac *)
                 | [ 45; 1 ] (* ava-ac *)
                 | [ 45; 34; 1; 10; 34 ] (* devadra-ac *)
                 | [ 45; 43; 1 ] (* arva-ac *)
                 | [ 45; 43; 1; 48 ] (* sarva-ac *)
                   -> build_mas_aac r1 e
                 | _ -> build_root Mas stem e
                 ]
            | [ 26 :: [ 1 :: r2 ] ] (* -a~nc *) -> build_mas_ac r2 e 
                 (* for Declension using uda~nc ... *)
            | [ 26 :: ([ 2 :: r2 ] as r)] (* -aa~nc *) -> match r2 with
                 [ [ 37; 1 ] (* apa-ac *)
                 | [ 42; 48; 1; 17 ] (* kasya-ac *)
                 | [ 43; 1; 37 ] (* para-ac *)
                 | [ 43; 37 ] (* pra-ac *)
                 | [ 45; 1 ] (* ava-ac *)
                 | [ 45; 34; 1; 10; 34 ] (* devadra-ac *)
                 | [ 45; 43; 1 ] (* arva-ac *)
                 | [ 45; 43; 1; 48 ] (* sarva-ac *)
                   -> build_mas_aac r e
                 | _ -> build_root Mas stem e
                 ]
            | _ -> build_root Mas stem e
            ] 
      | [ 24 :: r1 ] (* -j *) -> match r1 with (* mrijify *)
            [ [ 1 :: [ 42 :: _ ] ] (* -yaj2 upaya.t *) 
            | [ 1 :: [ 43 :: [ 40 :: _ ] ] ] (* -bhraj *) 
            | [ 2 :: [ 42 :: _ ] ] (* -yaaj2 *) (* but not -bhaaj *)
            | [ 2 :: [ 43 :: _ ] ] (* -raaj2 viraaj2 *) 
            | [ 7 :: [ 40 :: _ ] ] (* -bh.rj *) 
(*          | [ 7 :: [ 48 :: _ ] ] (* -s.rj2 *) changed 10/10/2023 
               doubt: WR in aor. gives both asraak and asraat *)
                -> build_root Mas [ 124 (* j' *) :: r1 ] e 
            | [ 5; 42 ] (* yuj2 *) -> do 
                { build_root Mas stem e
                ; build_archaic_yuj [ 24; 26; 5; 42 ] (* yu~nj *) Mas e
                }
            | _ -> build_root Mas stem e 
            ] 
      | [ 32 :: r1 ] (* -t *) -> match r1 with
            [ [ 1 :: r2 ] (* -at *) -> if is_redup r2 then build_mas_red r1 e
                                       else match r2 with
                 [ [ 41 :: r3 ] (* -mat *) -> 
                   if p="Ppra" || pprmat e then build_mas_at r1 e
                                           else build_mas_mat r2 e
                   (* Whitney§451 : yat iyat kiyat *)
                 | [ 42 ] | [ 42; 3 ] | [ 42; 3; 17 ] -> 
                   if p="Ppra" then build_mas_at r1 e (* yat2 *)
                   else build_mas_mat r2 e
                 | [ 45 :: r3 ] (* -vat *) -> 
                   if p="Ppra" || pprvat e then build_mas_at r1 e
                   else if e="maghavat" then build_mas_maghavan e                                   else build_mas_mat r2 e
                 | [ 49 :: [ 1 :: [ 41 :: _ ] ] ] (* mahat, sumahat *) 
                          -> build_mas_mahat r2 e
                 | [ 34 ] (* dat *) -> build_root_weak Mas stem "danta" 
                 | _ -> build_mas_at r1 e (* p.r.sat, jagat, like ppr *)
                 ] 
            | [ 2 :: r2 ] (* -aat *) -> match r2 with
                 [ [ 37; 1; 36 ] (* vedic napaat *) -> build_root Mas stem e
                 | _ -> build_mas_at r1 e (* ppr in aat/aant ? *)
                 ] 
            | _ -> build_root Mas stem e
            ] 
      | [ 34 :: r1 ] (* -d *) -> match r1 with
          [ [ 1; 37 ] (* pad *) -> build_root_weak Mas stem "paada"
          | [ 1 :: [ 37 :: s ] ] (* -pad *) -> build_pad Mas s e
          | _ -> build_root Mas stem e
          ]
      | [ 36 :: r1 ] (* -n *) -> match r1 with
            [ [ 1 :: r2 ] (* -an *) -> match r2 with
               [ [ 47 :: [ 6 :: [ 37 ] ] ]  (* puu.san *) 
                   -> build_an_god r2 e (* Whitney §426a *)
               | [ 41 :: r3 ] (* -man *) -> match r3 with
                     [ [ 1 :: [ 42 :: [ 43 :: [ 1 ] ] ] ] (* aryaman *)
                         -> build_man_god r3 e (* Whitney §426a *)
                     | _ -> build_man Mas r3 e
                     ]
               | [ 45 :: ([ 46 :: _ ] as r3) ] (* -zvan Whitney§427 *) 
                         -> build_mas_zvan r3 e (* takes care of eg dharmazvan *)
               | [ 45 :: r3 ] (* -van *) -> match e with
                  [ "yuvan" -> build_mas_yuvan e (* Whitney§427 *)
                  | "maghavan" -> build_mas_maghavan e (* Whitney§428 *)
                    (* NB: entry is maghavat but interface allows maghavan *)
                  | _ -> build_van Mas r3 e
                  ]
               | [ 49 :: r3 ] (* -han *) -> build_han r3 e
               | _  -> build_an Mas r2 e (* raajan *)
               ]
            | [ 3 :: r2 ] (* -in *) -> match r2 with
               [ [ 33 :: r3 ] (* -thin *) -> match r3 with
                  [ [ 1 :: [ 37 :: _ ] ]   (* -pathin *) (* \Pan{7,1,85} *)
                  | [ 1 :: [ 41 :: _ ] ]   (* -mathin *) 
                     -> build_athin r3 e
                  | _ -> build_mas_in r2 e
                  ]
               | [ 47; 17; 5; 40; 7 ]  (* -.rbhuk.sin *) (* \Pan{7,1,85} *)
                   -> build_ribhuksin r2 e
               | _ -> build_mas_in r2 e
               ] 
            | _ -> report stem g
            ]       
      | [ 37 :: [ 1 :: [ 48 :: r ] ] ] (* -sap *) -> build_sap Mas r e
      | [ 41 :: r1 ] (* -m *) -> match r1 with
           [ [ 1; 42; 1 ] (* ayam *) -> build_ayam_idam Mas 
           | [ 1; 34 ] (* dam2 *) -> (* [build_dam e] *) 
                 () (* skipped - only gen. vedic forms except dam-pati *)
           | _ -> build_root_m Mas r1 stem e (* was report stem g *)
           ]
      | [ 45 :: r1 ] (* -v *) -> match r1 with
           [ [ 3; 34 ] (* div *) -> build_div Mas [ 34 ] e
           | [ 4; 34 ] (* diiv *) -> () (* avoids reporting bahu *) 
           | _ -> report stem g
           ]
      | [ 46 :: [ 7 :: [ 34 :: [ 4 :: _ ] ] ] ] (* -(k)iid.rz *) -> 
           build_root_pn Mas stem e
      | [ 47 :: r1 ] (* .s *) -> match r1 with
            [ [ 3 :: r2 ] -> match r2 with
                [ [ 45; 1; 19 ] (* gavi.s *)
                | [ 45; 34 ] (* dvi.s *)
                | [ 45; 34; 3; 45 ] (* vidvi.s *)
                | [ 45; 34; 1; 32; 1; 49 ] (* hatadvi.s *)
                | [ 28; 1; 37; 3; 37 ] (* pipa.thi.s *)
                    -> build_is Mas r2 e (* Kale §114 *)
                | _ -> build_root Mas stem e
                ]
            | [ 5 :: r2 ] -> match r2 with
                [ [ 24 :: [ 1 :: [ 48 ] ] ] (* saju.s *)
                    -> build_us Mas r2 e (* Kale§114 *)
                | _ -> build_root Mas stem e
                ]
            | _ -> build_root Mas stem e
            ]
      | [ 48 :: r1 ] (* -s *) -> match r1 with
            [ [ 1 :: r2 ] (* -as *) -> match r2 with
               [ [ 42 :: _ ] (* -yas *) -> build_mas_yas r2 e
               | [ 45 :: r3 ] (* -vas *) -> 
            (* OBS  if p = "Ppfta" then build_mas_vas r3 e else *) 
                   match r3 with 
                   [ [ 1 :: [ 43 :: _ ] ] (* -ravas *) -> build_as Mas r2 e   
                     (* uccaisravas, puruuravas, ugrazravas, vizravas non ppf *)
                   | [ 3 :: r4 ] (* -ivas *) -> build_mas_ivas r4 e
                   | [ 35 :: _ ] (* -dhvas *)  
                   | [ 5 :: [ 48 :: _ ] ] (* -suvas *) -> build_root Mas stem e 
                   | _       (* other ppf *) -> build_mas_vas r3 e
                   ]
               | [ 43 :: [ 48 :: _ ]] (* -sras *) -> build_root Mas stem e
(*             | [[ 46; 1; 33; 17; 5 ] (* ukthazas *) -> build_ukthazas Mas e] *)
(*             | [[ 46 :: _ ] (* -zas *) -> build_root Mas stem e] *)
               | _  -> build_as Mas r2 e
               ] 
            | [ 2; 41 ] (* maas *) -> build_maas ()
            | [ 2 :: _ ] (* -aas *) -> () (* avoids reporting bahu aas bhaas *) 
            | [ 3 :: r2 ] (* -is *) -> match r2 with
                [ [ 46; 2 :: _ ] (* niraazis *) -> build_aazis Mas r2 e
                | _ -> build_is Mas r2 e  (* udarcis *)
                ]
            | [ 5 :: r2 ] (* -us *) -> build_us Mas r2 e (* acak.sus *)
            | [ 12; 34 ] (* dos *) -> build_dos Mas e
            | [ 14; 5; 37 ] (* pu.ms *) -> build_pums [ 41; 5; 37 ] stem e
            | [ 14; 5; 37; 1; 36 ] (* napu.ms *) 
              -> build_pums [ 41; 5; 37; 1; 36 ] stem e 
            | [ 14; 2; 41 ] (* maa.ms *) -> () (* avoids reporting bahu *) 
            | _ -> report stem g
            ]
      | [ 49 :: r1 ] (* -h *) -> match r1 with
            [ [ 1 :: [ 45 :: r3 ] ] (* vah2 *) -> match e with
                [ "ana.dvah" -> build_anadvah r3 e
                | _ -> build_mas_vah r3 e
                ]
            | [ 1 :: [ 34 :: _ ] ] (* dah2 *) (* mandatory duhify *)
            | [ 5 :: [ 34 :: _ ] ] (* duh2 *) -> 
                build_root Mas [ 149 (* h' *) :: r1 ] e 
            | [ 3 :: [ 36 :: [ 48 :: _ ] ] ] (* -snih2 *) 
            | [ 5 :: [ 36 :: [ 48 :: _ ] ] ] (* -snuh2 *) 
            | [ 5 :: [ 43 :: [ 34 :: _ ] ] ] (* -druh2 *) -> do
                { build_root Mas [ 149 (* h' *) :: r1 ] e 
                ; build_root Mas stem e (* optionally duhify *)
                }
            | _ -> build_root Mas stem e
            ]
      | _ -> build_root Mas stem e
      ]
    | Neu -> match stem with 
      [ [ 1 :: r1 ] (* -a *) -> match r1 with
            [ [ 17; 10 ]               (* eka *) (* pronouns *)
            | [ 17; 10; 36; 1 ]        (* aneka *) 
            | [ 31; 3; 47; 17; 1; 34 ] (* dak.si.na *)
            | [ 41; 1; 32; 1; 17 ]     (* katama *) 
            | [ 41; 3; 22; 46; 1; 37 ] (* pazcima *)
            | [ 42; 1; 40; 5 ]         (* ubhaya *)
            | [ 43; 1; 32; 1; 17 ]     (* katara *)
            | [ 43; 1; 32; 1; 17; 10 ] (* ekatara *)
            | [ 43; 1; 32; 3 ]         (* itara *) 
            | [ 43; 1; 32; 32; 5 ]     (* uttara *)
            | [ 43; 1; 32; 36; 1 ]     (* antara *)
            | [ 43; 1; 35; 1 ]         (* adhara *)
            | [ 43; 1; 37 ]            (* para *)
            | [ 43; 1; 37; 1 ]         (* apara *)
            | [ 43; 1; 45; 1 ]         (* avara *)
            | [ 45; 43; 6; 37 ]        (* puurva *)
            | [ 45; 46; 3; 45 ]        (* vizva *)
            | [ 45; 43; 1; 48 ]        (* sarva *)
            | [ 45; 48 ]               (* sva *) 
            (* | [ 45; 32 ] cf tvad clash with tva taddhita ending *) 
                -> build_pron_a Neu r1 e 
            | _ -> build_neu_a r1 e
            ]
      | [ 2 :: _ ] -> report stem Neu (* (missing) ahigopaa raa vibhaa sthaa *)
      | [ 4; 40; 1 ] (* abhii2 *) -> () (* overgenerates *)
      | [ 3 :: r1 ] (* -i *) -> match r1 with 
               [ [ 33; 17; 1; 48 ] (* sakthan/sakthi *) 
               | [ 33; 48; 1 ] (* asthan/asthi *) 
               | [ 35; 1; 34 ] (* dadhan/dadhi *)
               | [ 47; 17; 1 ] (* ak.san/ak.si *) -> build_aksan r1 e     
               | _ ->  build_neu_i r1 e
               ]
      | [ 4 :: r1 ] (* -ii - rare *) -> build_neu_i r1 e
      | [ 5 :: r1 ] (* -u *) 
      | [ 6 :: r1 ] (* -uu - rare *) -> build_neu_u r1 e
      | [ 7 :: r1 ] (* -.r *) -> build_neu_ri r1 e
      | [ 11; 43 ] (* rai *) 
      | [ 12; 19 ] (* go *) 
      | [ 13; 36 ] (* nau *) 
      | [ 13; 44; 19 ] (* glau *) 
      | [ 13; 48; 1 ] (* asau *) -> () (* avoids reporting bahu *) 
      | [ 8 :: _ ] 
      | [ 9 :: _ ] 
      | [ 10 :: _ ]
      | [ 11 :: _ ] 
      | [ 12 :: _ ] 
      | [ 13 :: _ ] -> report stem g
      | [ 22 :: r1 ] (* -c *) -> match r1 with
            [ [ 1 :: r2 ] (* -ac *) -> match r2 with
                 [ [] -> () (* ac utilisé seulement avec px *)
                 | [ 42 :: r3 ] -> build_neu_yac r3 e
                 | [ 45 :: r3 ] -> build_neu_vac r3 e
                 | [ 37 :: r3 ] (* pac *) -> build_root Neu stem e
                 |  _ (* udac ... *) -> build_neu_ac r2 e 
                 ]
            | [ 2 :: _ ] (* -aac *) -> build_neu_aac r1 e
            | [ 26 :: [ 1 :: r2 ] ] (* -a~nc *) -> build_neu_ac r2 e 
                 (* for Declension using uda~nc ... *)
            | [ 26 :: ([ 2 :: r2 ] as r) ] (* -aa~nc *) -> build_neu_aac r e
            | _ -> build_root Neu stem e
            ]
      | [ 24 :: r1 ] (* -j *) -> match r1 with (* mrijify *)
            [ [ 1 :: [ 42 :: _ ] ] (* -yaj2 upaya.t *) 
            | [ 1 :: [ 43 :: [ 40 :: _ ] ] ] (* -bhraj *) 
            | [ 2 :: [ 42 :: _ ] ] (* -yaaj2 *) (* but not -bhaaj *)
            | [ 2 :: [ 43 :: _ ] ] (* -raaj2 viraaj2 *) 
            | [ 7 :: [ 40 :: _ ] ] (* -bh.rj *) 
(*          | [ 7 :: [ 48 :: _ ] ] (* -s.rj2 *) Not *as.r.t but as.rk *)
                -> build_root Neu [ 124 (* j' *) :: r1 ] e
            | [ 5; 42 ] (* yuj2 *) -> do  
                { build_root Neu stem e
                ; build_archaic_yuj [ 24; 26; 5; 42 ] (* yu~nj *) Neu e
                }
            | _ -> build_root Neu stem e (* -s.rk as.rjk *)
            ] 
       | [ 32 :: r1 ] (* -t *) -> match r1 with
            [ [ 1 :: r2 ] (* -at *) -> if is_redup r2 then build_neu_red r1 e
                                       else match r2 with
               [ [ 49 :: [ 1 :: [ 41 :: _ ] ] ] (* mahat, sumahat *) 
                   -> build_neu_mahat r2 e
               | _ -> build_neu_at r1 e (* e.g. jagat *)
               ]
            | [ 2 :: r2 ] (* -aat *) -> build_neu_at r1 e (* ppr in aat/aant ? *)
            | _ -> build_root Neu stem e 
            ] 
      | [ 34 :: r1 ] (* -d *) -> match r1 with
          [ [ 1 :: r2 ] (* -ad *) -> match r2 with
            [ [ 32 ] (* tad *) -> do
                 { build_sa_tad Neu [] e
                 ; enter e [ Bare Noun (code "tat") ]
                 }
            | [ 32; 10 ] (* etad *) -> build_sa_tad Neu [ 10 ] e
            | [ 42; 32 ] (* tyad *) -> build_sya_tyad Neu e
            | [ 36; 10 ] (* enad *) -> build_ena Neu "idam"
            | [ 37 ] (* pad *) -> build_root_weak Neu stem "paada" 
            | [ 37 :: s ]  (* -pad *) -> build_pad Neu s e
            | [ 42 ] (* yad *) 
            | [ 45; 32 ] (* tvad *) 
            | [ 42; 36; 1 ] (* anyad *) 
            | [ 43; 1; 32; 1; 42; 36; 1 ] (* anyatarad *) (* Whitney§523 *)
                -> build_pron_a Neu r2 e
            | _ -> build_root Neu stem e
            ] 
          | [ 7; 49 ] (* h.rd *)
              -> build_root_weak Neu stem "h.rdaya" (* \Pan{6,1,63} Whitney§397 *)
          | _ -> build_root Neu stem e
          ]
      | [ 36 :: r1 ] (* -n *) -> match r1 with
            [ [ 1 :: r2 ] (* -an *) -> match r2 with
               [ [ 33; 17; 1; 48 ] (* sakthan *) 
               | [ 33; 48; 1 ] (* asthan *) 
               | [ 47; 17; 1 ] (* ak.san *) 
               | [ 35; 1; 34 ] (* dadhan *) -> build_aksan r2 e
               | [ 17; 1; 42 ] (* yakan *)
               | [ 17; 1; 46 ] (* zakan *)
               | [ 34; 5 ] (* udan *)
               | [ 47; 6; 42 ] (* yuu.san *)
               | [ 47; 12; 34 ] (* do.san *)
               | [ 48; 1 ] (* asan *)
               | [ 48; 2 ] (* aasan *) -> build_sp_an r2 e (* Whitney§432 *)
               | [ 35; 6 ] (* uudhan *) -> build_uudhan r2 e
               | [ 41 :: r3 ] (* -man *) -> build_man Neu r3 e
               | [ 45 :: r3 ] (* -van *) -> match e with
                  [ "yuvan" -> build_neu_yuvan e (* Whitney§427 *)
                  | _ -> build_van Neu r3 e
                  ]
               | [ 49 :: r3 ] (* -han *) -> match r3 with
                  [ [ 1 :: _ ] (* -ahan *) 
                  | [ 2; 42; 2; 48 ] (* saayaahan *) -> build_ahan r2 e
                  | _ (* -han2 *) -> build_an Neu r2 e
                  ]
               | _  -> build_an Neu r2 e
               ]
            | [ 3 :: r2 ] (* -in *) -> build_neu_in r2 e
            | _ -> report stem g
            ]       
      | [ 37 :: [ 1 :: [ 48 :: r ] ] ] (* -sap *) -> build_sap Neu r e
      | [ 41 :: r1 ] (* -m *) -> match r1 with
           [ [ 1; 34; 3 ] (* idam *) -> build_ayam_idam Neu 
           | [ 3; 17 ] (* kim *) -> build_pron_a Neu [ 17 ] e
           | _ -> build_root_m Neu r1 stem e (* was report stem g *)
           ]
      | [ 45 :: r1 ] (* -v *) -> match r1 with
           [ [ 3; 34 ] (* div *) -> build_div Neu [ 34 ] e
           | [ 4; 34 ] (* diiv *) -> () (* avoids reporting bahu *) 
           | _ -> report stem g
           ]
     | [ 46 :: [ 7 :: [ 34 :: [ 4 :: _ ] ] ] ] (* -(k)iid.rz *) -> 
           build_root_pn Neu stem e
     | [ 47 :: r1 ] (* .s *) -> match r1 with
            [ [ 3 :: r2 ] -> match r2 with
                [ [ 45; 1; 19 ] (* gavi.s *)
                | [ 45; 34; 1; 32; 1; 49 ] (* hatadvi.s *)
                | [ 28; 1; 37; 3; 37 ] (* pipa.thi.s *)
                    -> build_is Neu r2 e
                | _ -> build_root Neu stem e
                ]
            | [ 5 :: r2 ] -> match r2 with
                [ [ 24 :: [ 1 :: [ 48 ]]] (* saju.s *)
                    -> build_us Neu r2 e
                | _ -> build_root Neu stem e
                ]
            | _ -> build_root Neu stem e
            ]
      | [ 48 :: r1 ] (* -s *) -> match r1 with
            [ [ 1 :: r2 ] (* -as *) -> match r2 with
               [ [ 34; 1 ] (* adas *) -> build_asau_adas Neu 
               | [ 42 :: _ ] (* -yas *) -> build_neu_yas r2 e
               | [ 45 :: r3 ] (* -vas *) -> 
                 (* OBS if p = "Ppfta" then build_neu_vas r3 e else *)
                 match r3 with 
                 [ [ 1 ] (* avas1 - non ppf *)
                 | [ 1 :: [ 43 :: _ ] ] (* -ravas eg zravas, sravas - non ppf *)
                 | [ 5 :: [ 48 :: _ ] ] (* -suvas *) 
                 | [ 3; 43; 1; 45 ] (* varivas *) -> build_as Neu r2 e
                 | [ 3 :: r4 ] (* ivas *) -> build_neu_ivas r4 e 
                 | [ 35 :: _ ] (* -dhvas *) -> build_root Neu stem e
                 | _      (* other ppf *) -> build_neu_vas r3 e 
                 ]
               | [ 43 :: [ 48 :: _ ]] (* -sras *) -> build_root Neu stem e
               | _ (* manas, ziras, ... *) -> build_as Neu r2 e 
               ]
            | [ 2 :: r2 ] (* -aas *) -> match r2 with
               [ [] -> build_neu_aas stem e (* aas3 irregular *)
               | [ 17 ] (* kaas2 *) 
               | [ 41 ] (* maas *) -> () (* avoids reporting bahu *) 
               | [ 40 :: _ ] (* bhaas aabhaas *) -> () (* missing paradigm *) 
               | _ -> report stem Neu
               ]
            | [ 3 :: r2 ] (* -is *) -> build_is Neu r2 e (* jyotis havis *)
            | [ 5 :: r2 ] (* -us *) -> build_us Neu r2 e (* cak.sus dhanus *)
            | [ 12; 34 ] (* dos *) -> build_dos Neu e 
            | _ -> build_root Neu stem e 
            ]
      | [ 49 :: r1 ] (* -h *) -> match r1 with
            [ [ 1 :: [ 34 :: _ ] ] (* dah2 -dah *)
            | [ 5 :: [ 34 :: _ ] ] (* duh2 -duh *) ->
              build_root Neu [ 149 (* h' *) :: r1 ] e (* duhify *)
            | [ 5 :: [ 43 :: [ 34 :: _ ] ] ] (* -druh2 *) -> do
                { build_root Neu [ 149 (* h' *) :: r1 ] e (* optionally duhify *)
                ; build_root Neu stem e 
                }
            | _ -> build_root Neu stem e
            ]
      | _ -> build_root Neu stem e
      ]
    | Fem -> match stem with 
      [ [ 1 :: _ ] -> report stem g
      | [ 2 :: r1 ] (* -aa *) -> match r1 with 
            (*i [ [ 24 ] (* -jaa *)  (* no - ajaa etc *)
                | [ 19 ] (* -gaa *) -> build_mono_aa Fem r1 e (* gaa\#3 *) i*)
            [ [ 42 ] (* yaa *) -> match e with
                [ "ya#1" | "yad" | "yaa#2" -> build_pron_aa r1 e (* pn yaa\#2 *)
                | "ya#2" | "yaa#3" -> build_fem_aa r1 e (* ifc. -yaa\#3 *)
                | _ -> report stem g
                ] 
            | [ 17 ]                   (* kaa *) 
            | [ 17; 10 ]               (* ekaa *) 
            | [ 17; 10; 36; 1 ]        (* anekaa *) 
            | [ 31; 3; 47; 17; 1; 34 ] (* dak.si.naa *)
            | [ 41; 1; 32; 1; 17 ]     (* katamaa *) 
            | [ 41; 3; 22; 46; 1; 37 ] (* pazcimaa *)
            | [ 42; 36; 1 ]            (* anyaa *) 
            | [ 43; 1; 32; 1; 17 ]     (* kataraa *)
            | [ 43; 1; 32; 1; 17; 10 ] (* ekataraa *) 
            | [ 43; 1; 32; 1; 42; 36; 1 ] (* anyataraa *) (* Whitney§523 *)
            | [ 43; 1; 32; 3 ]         (* itaraa *)
            | [ 43; 1; 32; 32; 5 ]     (* uttaraa *)
            | [ 43; 1; 32; 36; 1 ]     (* antaraa *)
            | [ 43; 1; 35; 1 ]         (* adharaa *)
            | [ 43; 1; 37 ]            (* paraa *)
            | [ 43; 1; 37; 1 ]         (* aparaa *)
            | [ 43; 1; 45; 1 ]         (* avaraa *)
            | [ 45; 43; 1; 48 ]        (* sarvaa *) 
            | [ 45; 43; 6; 37 ]        (* puurvaa *)
            | [ 45; 46; 3; 45 ]        (* vizvaa *)
            | [ 45; 48 ]               (* svaa *) 
            | [ 45; 32 ]               (* tvaa *) 
                  -> build_pron_aa r1 e
            | [ 36; 10 ] (* enaa *)  -> build_ena Fem "idam"
            | [ 47; 10 ] (* e.saa *) when e="etad" || e="e.saa"
                  -> build_saa [ 10 ] e  
            | [ 48 ] (* saa *)       -> build_saa [] e 
            | [ 42 ; 48 ] (* syaa *) -> build_syaa [] e
            | _ -> build_fem_aa r1 e
            ]
      | [ 3 :: r1 ] (* -i *) -> build_fem_i stem r1 e
      | [ 4 :: r1 ] (* -ii *) -> 
              (* [match r1 with 
              [ [ 37 :: [ 2 :: _ ] ] (* -aapii *) 
              | _ -> ]] *)
          if monosyl r1 || compound_monosyl_ii r1 then match r1 with
             [ [ 43; 32; 48 ] (* strii *) -> build_strii r1 e
             | [ 43; 46 ] (* zrii *) -> do
               { build_mono_ii Fem r1 e     (* nom. ii.h *)
               ; build_fem_ii r1 e (* MW *) (* nom. ii Pan{6,1,68} sulopa *)
               }
            | _ -> build_mono_ii Fem r1 e (* dhii hrii bhii2 *)
             ]
          else do
               { if r1=[ 22; 1 ] (* -acii *)  then () (* seulement avec px *)
                 else build_fem_ii r1 e (* nom. ii Pan{6,1,68} sulopa *)
               ; match r1 with (* vedic forms Whitney§355-356 *) 
                 [ [ 45; 1 ]             (* avii *)
                 | [ 34; 1; 36 ]         (* nadii *) 
                 | [ 41; 43; 6; 48 ]     (* suurmii *) 
                 | [ 41; 47; 17; 1; 44 ] (* lak.smii *) 
                 | [ 43; 1; 32 ] (* tarii *) (* Whitney§363a *)
                 | [ 43; 32; 36; 1; 32 ] (* tantrii *)
                 | [ 43; 1; 32; 48 ] (* starii *) (* Deshpande u.naadisuutra *)
(* HN Bhat: avii tantrii tarii lak.smii hrii dhii zrii in u.naadi *)
(* autre liste: tantrii starii lak.smii tarii dhii hrii zrii *)
(* ci-dessus: + nadii suurmii - dhii hrii traités par [build_mono_ii] *) 
                   -> build_poly_ii Fem r1 e (* nom. ii.h *)
                 | _ -> () 
                 ]
               }
      | [ 5 :: r1 ] (* u *) -> build_fem_u stem r1 e
      | [ 6 :: r1 ] (* -uu *) -> 
          if monosyl r1 || compound_monosyl_uu r1 then build_mono_uu Fem r1 e
          else do
               { build_fem_uu r1 e
               ; match r1 with (* vedic forms Whitney§355-356 *)
                 [ [ 35; 1; 45 ] (* vadhuu *) 
                 | [ 36; 1; 32 ] (* tanuu *) 
                 | [ 41; 1; 22 ] (* camuu *) 
                   -> build_poly_uu Fem r1 e
                 | _ -> ()
                 ]
               }
      | [ 7 :: r1 ] (* -.r *) -> match r1 with
            [ [ 32 :: r2 ] (* -t.r *) -> match r2 with
               [ [ 2; 41 ] (* maat.r *) (* relationships McDonnel §101 *)
               | [ 3; 49; 5; 34 ] (* duhit.r *) -> build_fem_ri_g r1 e
               | _ -> build_fem_ri_v r1 e
               ]
            | [ 34; 36; 2; 36; 1; 36 ] (* nanaand.r *) 
            | [ 34; 36; 1; 36; 1; 36 ] (* nanaand.r *) 
                 -> build_fem_ri_g r1 e
            |  _ -> build_fem_ri_v r1 e (* including relationship svas.r *)
            ]
      | [ 8 :: _ ] 
      | [ 9 :: _ ] 
      | [ 10 :: _ ] -> report stem Fem
      | [ 11 :: r1 ](* -ai *) -> match r1 with
            [ [ 43 ] (* rai *) -> build_rai Fem [ 2; 43 ] e
(*          | [ 39; 41; 5; 41 ] (* mumbai *) -> (* TO DO *) *)
(*          | [ 48; 32; 32; 1; 48 ] (* sattasai *) -> (* idem *) *)
            | _ -> report stem Fem
            ]
      | [ 12 :: r1 ] (* -o *) ->  build_o Fem r1 e
      | [ 13 :: r1 ] (* -au *) -> match r1 with
            [ [ 48; 1 ] (* asau *) -> build_asau_f ()
            | _ -> build_au Fem r1 e
            ]
      | [ 24 :: r1 ] (* -j *) -> match r1 with (* mrijify *)
            [ [ 1 :: [ 42 :: _ ] ] (* -yaj2 upaya.t *) 
            | [ 1 :: [ 43 :: [ 40 :: _ ] ] ] (* -bhraj *) (* but not sraj! *)
            | [ 2 :: [ 42 :: _ ] ] (* -yaaj2 *) (* but not -bhaaj *)
            | [ 2 :: [ 43 :: _ ] ] (* -raaj2 viraaj2 *) 
            | [ 7 :: [ 40 :: _ ] ] (* -bh.rj *) 
(*          | [ 7 :: [ 48 :: _ ] ] (* -s.rj2 *) s.rk *)
                -> build_root Fem [ 124 (* j' *) :: r1 ] e
            | [ 5; 42 ] (* yuj2 *) -> do 
                { build_root Fem stem e
                ; build_archaic_yuj [ 24; 26; 5; 42 ] (* yu~nj *) Fem e
                }
            | _ -> build_root Fem stem e
            ] 
      | [ 32; 7; 37 ] (* p.rt *) -> build_root_weak Fem stem "p.rtanaa"
      | [ 34 :: r1 ] (* -d *) -> match r1 with
            [ [ 1; 37 ] (* pad *) -> build_root_weak Fem stem "paada"
            | [ 1; 37; 2 ] (* aapad *) 
            | [ 1; 37; 3; 45 ] (* vipad *)  
            | [ 1; 37; 41; 1; 48 ] (* sampad *) -> build_root Fem stem e 
            | [ 1 :: [ 37 :: s ] ] (* -pad *) -> build_pad Fem s e
            | _ -> build_root Fem stem e 
            ]
      | [ 36 :: r1 ] (* -n *) -> match r1 with
            [ [ 1 :: r2 ] (* -an *) -> match r2 with
               [ [ 41 :: r3 ] (* man *) -> match r3 with
                  [ [ 2; 48 ] (* saaman *) 
                  | [ 4; 48 ] (* siiman *) -> build_man Fem r3 e (* check *)
                  | _ -> report stem Fem
                  ]
               |  _ -> report stem Fem
               ]
            | _ -> report stem Fem
            ]       
      | [ 37; 1 ] (* ap *) -> build_ap e
      | [ 37 :: [ 1 :: [ 48 :: r ] ] ] (* -sap *) -> build_sap Fem r e
      | [ 41 :: r1 ] (* -m *) -> match r1 with
            [ [ 1; 42; 3 ] (* iyam *) ->  build_iyam ()
            | _ -> build_root_m Fem r1 stem e (* was report stem g *)
            ]
      | [ 43 :: r1 ] (* -r *) -> match r1 with
            [ [ 2 :: _ ] (* -aar *) -> build_root Fem stem e (* dvaar *)
            | [ 3 :: r2 ] (* -ir *) -> build_fem_ir r2 e (* gir *)
            | [ 5 :: r2 ] (* -ur *) -> build_fem_ur r2 e
            | [ 1 :: _ ] (* -praatar -sabar *) -> () 
            | _ -> report stem g
            ]
      | [ 45 :: r1 ] (* -v *) -> match r1 with
            [ [ 3; 34 ] (* div *) -> build_div Fem [ 34 ] e
            | [ 4; 34 ] (* diiv\#2 *) -> build_diiv e
            | _ -> report stem g
            ]
      | [ 46; 3; 36 ] (* niz *) -> build_root_weak Fem stem "niz"
      | [ 46 :: [ 7 :: [ 34 :: [ 4 :: _ ] ] ] ] (* -(k)iid.rz *) -> 
           build_root_pn Fem stem e
      | [ 47 :: r1 ] (* -.s *) -> match r1 with
            [ [ 3 :: r2 ] -> match r2 with
                [ [ 28 :: [ 1 :: [ 37 :: [ 3 :: [ 37 ] ] ] ] ] (* pipa.thi.s *)
                    -> build_is Fem r2 e
                | _ -> build_root Fem stem e
                ]
            | [ 5 :: r2 ] -> match r2 with
                [ [ 24 :: [ 1 :: [ 48 ] ] ] (* saju.s *)
                    -> build_us Fem r2 e
                | _ -> build_root Fem stem e
                ]
            | _ -> build_root Fem stem e
            ]
      | [ 48 :: r1 ] (* -s *) -> match r1 with
            [ [ 1; 36 ] (* nas *) -> build_nas e
            | [ 1 :: r2 ] (* -as *) -> match r2 with
                [ [ 45 :: [ 35 :: _ ] ] (* -dhvas *) 
                | [ 43 :: [ 48 :: _ ] ] (* -sras *) -> build_root Fem stem e
                | [ 34; 1 ] (* adas *) -> build_asau_f ()
                | _ -> build_as Fem r2 e
                ]
            | [ 2 :: r2 ] (* -aas *) -> build_root Fem stem e (* bhaas *)
            | [ 3 :: r2 ] (* -is *) -> match r2 with
                [ [ 46; 2 :: _ ] (* -aazis *) -> build_aazis Fem r2 e
                | _ -> build_is Fem r2 e
                ]
            | [ 5 :: r2 ] (* -us *) -> build_us Fem r2 e
            | [ 12; 34 ] (* dos *) -> build_dos Fem e 
            | [ 14; 2; 41 ] (* maa.ms *) -> () (* avoids reporting bahu *) 
            | [ 14 :: [ 5 :: _ ] ] -> () (* -pu.ms *)
            | _ -> report stem g
            ]
      | [ 49 :: r1 ] (* -h *) -> match r1 with
            [ [ 1 :: [ 34 :: _ ] ] (* dah2 -dah *)
            | [ 5 :: [ 34 :: _ ] ] (* duh2 -duh *) 
            | [ 3; 31; 47; 5 ] (* u.s.nih *) -> 
              build_root Fem [ 149 (* h' *) :: r1 ] e (* duhify *)
            | [ 3; 36; 48 ] (* snih2 *)
            | [ 5; 36; 48 ] (* snuh2 *) 
            | [ 5 :: [ 43 :: [ 34 :: _ ] ] ] (* druh2 -druh *) -> do
                { build_root Fem [ 149 (* h' *) :: r1 ] e (* optionally duhify *)
                ; build_root Fem stem e 
                }
            | [ 1; 36; 2; 37; 5 ] -> build_upaanah r1 stem e (* Kale§101 *)
            | _ -> build_root Fem stem e
            ]
      | _ -> build_root Fem stem e
      ]
    | Deictic _ -> match stem with 
      [ (* aham *) [ 41; 1; 49; 1 ] (* Dico *)
      | (* asmad *) [ 34; 1; 41; 48; 1 ] (* tradition *) -> build_aham () 
      | (* tvad *) [ 34; 1; 45; 32 ] (* Dico *)
      | (* yu.smad *) [ 34; 1; 41; 47; 5; 42 ] (* tradition *) -> build_tvad ()
      | (* aatman *) [ 36; 1; 41; 32; 2 ] -> build_aatman e
      | (* eka *) [ 1; 17; 10 ] -> warn stem "a Mas or Neu" (* pn in Dico *)
      | (* dvi *) [ 3; 45; 34 ] -> build_dvi e
      | (* tri *) [ 3; 43; 32 ] -> build_tri e
      | (* tis.r *) [ 7; 48; 3; 32 ]
      | (* trayas *) [ 48; 1; 42; 1; 43; 32 ] 
      | (* trii.ni *) [ 3; 31; 4; 43; 32 ] -> warn stem "tri" 
      | (* catur *) [ 43; 5; 32; 1; 22 ] -> build_catur e
      | (* catas.r *) [ 7; 48; 1; 32; 1; 22 ] 
      | (* catvaari *) [ 3; 43; 2; 45; 32; 1; 22 ] -> warn stem "catur"
      | (* .sa.s *) [ 47; 1; 47 ] -> build_sat e
      | (* -an (numbers) *) [ 36 :: [ 1 :: st ] ] -> match st with 
             [ (* pa~ncan *) [ 22; 26; 1; 37 ] 
             | (* saptan *) [ 32; 37; 1; 48 ]
             | (* a.s.tan *) [ 27; 47; 1 ] 
             | (* navan *) [ 45; 1; 36 ]
             | (* .so.dazan *) [ 46; 1; 29; 12; 47 ] 
             | (* -dazan *) [ 46 :: [ 1 :: [ 34 :: _ ] ] ] -> build_num st e 
             | _ -> report stem g
             ]
      | (* kati *) [ 3; 32; 1; 17 ] -> build_kati e
      | (* kazcit *) [ 32; 3; 22; 46; 1; 17 ] -> existential "cit" 
      | (* katicit *) [ 32; 3; 22; 3; 32; 1; 17 ] -> build_katicit e
      | (* vi.mzati *) [ 3; 32; 1; 46; 14; 3; 45 ] 
      | (* .sa.s.ti *) [ 3; 27; 47; 1; 47 ] 
      | (* saptati *) [ 3; 32; 1; 32; 37; 1; 48 ]
      | (* aziiti *) [ 3; 32; 4; 46; 1 ] 
      | (* navati *) [ 3; 32; 1; 45; 1; 36 ] 
      | (* -zat *) [ 32 :: [ 1 :: [ 46 :: _ ] ] ] 
                    (* -tri.mzat -catvaari.mzat -pa~ncaazat *)
                    -> warn stem "a Fem" 
      | (* zata *) [ 1; 32; 1; 46 ] (* actually also Mas *)
      | (* dvizata *) [ 1; 32; 1; 46; 3; 45; 34 ]
      | (* sahasra *) [ 1; 43; 48; 1; 49; 1; 48 ] -> warn stem "a Neu" 
      | (* adhika *) [ 1; 17; 3; 35; 1 ] -> warn stem "an adj" 
      | _ -> report stem g
      ]
    ] 
  | Ind k -> let form = mirror (terminal_form stem) in
             enter e [ Indecl k form ] 
  ] with
  [ Failure s -> do
      { output_string stdout "\n\n"
      ; flush stdout
      ; Printf.eprintf "Declension error for stem %s in entry %s\n%!" 
                       (Canon.decode (mirror stem)) e
      ; failwith s
      }
  ]
;
(* Main procedure, invoked by [compute_decls] and [fake_compute_decls] 
   with entry [e:string], [d:declension_class] which gives the gender [g], 
   [s:skt] is a stem of [e], [p:string] is a participle name or "" *)
value compute_decls_stem e (s,d) p = 
  let rstem = revstem s in (* remove homonym index if any *)
  compute_nouns_stem_form e rstem d p
  (* Only the normalized form is stored and thus extra sandhi rules such as 
     m+n->nn must be added in [Compile_sandhi] *)
;
(* We keep entries with only feminine stems, in order to put them in Iic *)
(* eg haridraa but durgaa must be noted \fstemi to keep its fem iic *)
value extract_fem_stems = extract_rec []
  where rec extract_rec acc = fun
     [ [] -> acc
     | [ (s,Gender Fem) :: rest ] -> extract_rec [ s :: acc ] rest
     | [ (_,Ind _) :: rest ] -> extract_rec acc rest (* eg yad.rcchaa *)
     | [ _ :: rest ] -> [] 
     ]
;
value enter_iic_stem entry (stem : string) = do
  { enter1 entry (Bare Noun (mirror (finalize (revstem stem)))) (* horror *)
  ; match entry with (* extra forms *)
    [ "viz#2" | "elaa" -> enter1 entry (Bare Noun (normal_stem entry)) 
     (* vizpati elaalataa *)
    | _ -> () 
    ]
  }
;
(* called by [Make_nouns.genders_to_nouns] twice, for nouns and then ifcs *)
value compute_decls word genders =
  let entry = Canon.decode word in 
  let compute_gender gen = compute_decls_stem entry gen ""
                           (* we do not know the morphology *) in do
  { try List.iter compute_gender genders
    with [ Report s -> print_report s 
         | Failure s -> print_report ("Anomaly: " ^ entry ^ " " ^ s)
         ]
  ; match extract_fem_stems genders with
    [ [] -> ()
    | fem_stems -> iter (enter_iic_stem entry) fem_stems
    ]
  }
;
value iic_indecl = (* should be lexicalized or completed *)
(* indeclinable stems used as iic of non-avyayiibhaava cpd *)
  [ "atra#1"    (* atrabhavat *)
  ; "adhas"     (* adha.hzaakha adhazcara.nam *)
  ; "antar"     (* antarafga *)
  ; "antaraa"   (* antarafga *)
  ; "anyatas"   (* anyatodvaara - tasil *)
  ; "arvaak"    (* arvaakkaalika *)
  ; "alam"      (* (gati) ala.mk.rta *)
  ; "alpaat"    (* alpaanmukta *)
  ; "asak.rt"   (* asak.rtsamaadhi *)
  ; "aajanma"   (* aajanmazuddha *)
  ; "iti"       (* ityukta *)  
(*; "ittham"    (* ittambhuuta *) ? *)
  ; "uccais"    (* uccaisziras *)  
  ; "upari"     (* uparicara *)  
  ; "upaa.mzu"  (* upaa.mzuda.n.da *)
  ; "ubhayatas" (* ubhayata.hsasya - tasil *)
  ; "evam"      (* eva.mvid *)
(*; "katham"    (* katha.mbhuuta *) ? *)
  ; "ki.mcid"   (* ki.mciccalana *)
  ; "k.rcchraat" (* k.rcchraadavaapta *)
  ; "ciram"     (* cira.mjiiva *)
  ; "tatra"     (* tatrabhavat *)
  ; "tathaa"    (* tathaagata *)
  ; "dak.si.natas" (* dak.si.nataskaparda - tasil *)
  ; "divaa"     (* divaanidraa *)
  ; "dhik"      (* dhikkaara *)
  ; "na~n"      (* na~nvaada *)
  ; "naktam"    (* nakta.mcara *)
  ; "naanaa"    (* naanaaruupa *)
  ; "niicais"   (* ? *)  
  ; "param"     (* para.mtapa *)
  ; "pazcaa"    (* pazcaardha *)
  ; "pazcaat"   (* pazcaadukti *)
  ; "punar"     (* punarukta *)
  ; "puras"     (* (gati) pura.hstha *)
  ; "p.rthak"   (* p.rthagjana *)
  ; "prati"     (* pratikuula *)
  ; "praatar"   (* praataraaza  *)
  ; "praayas"   (* praayazcitta *)
  ; "pha.t"     (* pha.dantena *)
  ; "bahis"     (* bahirafga *)
  ; "mithyaa"   (* mithyaak.rta *) (* G{saak.saat} *)
  ; "yathaa"    (* yathaanirdi.s.ta *)
  ; "lokasaat"  (* lokasaatk.rta *)
  ; "vinaa"     (* vinaabhava *)
  ; "vizvatas"  (* visvatomukha - tasil *)
  ; "vau.sa.t"  (* vau.sadantena *)
(*; "sak.rt"    - not needed, adj *)
  ; "satraa"    (* satraajit *)
  ; "sadaa"     (* sadaananda *)
  ; "sadyas"    (* sadya.hkaala *)
  ; "sanat"     (* sanatkumaara *)
  ; "sarvatas"  (* sarvatomukha - tasil *)
  ; "sarvatra"  (* sarvatraga *)
  ; "sarvathaa" (* sarvathaavi.saya *)
  ; "saha#2"    (* problematic -- overgenerates  *)
  ; "saak.saat"  (* saak.saadd.r.s.ta *) (* G{saak.saat} *)
  ; "saaci"
  ; "saamaanyatas" (* saamaanyatod.r.s.ta - tasil *)
  ; "saami"     (* saamipiita *)
  ; "saayam"    (* saayamazana *)
  ; "su.s.thu"  (* su.s.thuprayoga *)
  ; "svayam"    (* svaya.mvara *)
  ; "svar#3"    (* svargatim *)
  ; "hyas"      (* hyask.rta *)
  ]
;
value declined_indecls =
(* declined substantival forms used as adverbs - many could be added *)
  [ "a~njasaa"
  ; "antaraa"
  ; "astam"
  ; "uccais" 
  ; "kam#1"
  ; "kaamam"
  ; "divaa"
  ; "naktam"
  ; "niicais" 
  ; "param"
  ; "raatrim"
  ; "satatam" 
  ; "sahasaa" 
  ]
;
value compute_extra_indecls () =
  iter add_adv declined_indecls 
  where add_adv entry = enter1 entry (Indecl Adv (normal_stem entry)) 
;

(* Avyayiibhaava compounds *)
value iic_avyas = 
(* indeclinable stems used as iic of avyayiibhaava cpd *)
  [ "ati" (* atikambalam atinidram atyaasam atiniicam *)
  ; "adhas" (* adhazcara.nam *)
  ; "adhi" (* adhipaa.ni adhistri adhihari adhihasti adhyaatmam *)
  ; "abhi" (* abhyagni abhipuurvam *)
  ; "anu" (* anujye.s.tham anuk.sa.nam anugu anu.svadham (.) *)
  ; "antaraa" (* antaraaz.fgam *)
  ; "apa"
(*; "aa" -- overgenerates *)
  ; "iti"
  ; "upa" (* upakumbham upak.r.s.naat upagafgam upanadam upaagni upapatham *)
  ; "upari" (* uparibhuumi *)
  ; "dus" (* durbhik.sam *)
  ; "nis" (* nirmak.sikam *)
  ; "pari"
  ; "prati" (* pratyaham prativar.sam *)
  ; "paare" (* paaregafgam *)
  ; "praak"
  ; "bahir" (* bahirgraamam *)
  ; "madhye" (* madhyegafgam madhyejalaat *)
  ; "yathaa" (* yathaazakti yathaakaamam yathaagatam yathaanyaasam yathaav.rddham
                yathaazraddham yathaasthaanam ... *)
  ; "yaavat" (* yaavacchakyam yaavajjiivam \Pan{2,1,8} *)
  ; "sa#1" (* sak.satram sacakram sat.r.nam saak.siptam saak.saat *)
  ; "su#1"
(*  "dvyaha" (* dvyahatar.sam (adv+namul) dvyahaatyaasam (adv) *) *)
  ]
(* Avyayiibhaava compounds not recognized as such: 
   those should not be marked as avya (and thus skipped) in the lexicon 
1. missing iic:
   iic aa-: aakar.namuulam aacandram aadvaadazam aamuulam aasa.msaaram aasamudram
            aabrahmabhuvanaat
   iic. a-yathaa-:  ayathaamaatram 
   iic. ubhayatas-: ubhayata.hkaalam
   iic. dvyaha-: dvyahatar.sam dvyahaatyaasam
   iic. para-: parazvas 
   iic. paras-: parovaram
   iic. uccais-: uccai.hzabdam
   iic. mithyaa-: mithyaaj~naanam 
2. missing ifc:
   ifc. -prati: sukhaprati zaakaprati
   ifc. krid yathaav.rddham yathe.s.tam yaavacchakyam atyucchritam ziitam (TODO)
   ifc. also pv-kridanta (-aagata) yathaagatam 
   ifc. yatham: yathaayatham
3. misc: ti.s.thadgu anu.svadham var.sabhogye.na (retroflexion) *)
;
value enter_iic_avya entry = 
  enter1 entry (Avyayai (normal_stem entry)) (* stripped entry *)
;
(*i Taddhitas generation - unplugged at present 
[value gen_prefixes = (* productive prefixes *)
  [ "ku" (* rare *)
  ; "dus"
  ; "nis"
  ; "prati"
  ; "vi" (* may be privative or intensive *)
  ; "sa#1" (* substitute for saha - lookup Paa.nini *)
  ; "su#1" 
(* NB "mahaa" is taken into account by [build_mas_mahat]; *)
(* fems in -aa are included if nominal use of entry *)
(* privative "a/an1" not included because generated in Complete mode,
   idem for prepositions (generated by preverbs in case of kridantas). 
   praadi compounds must be lexicalized.
   Idem aa- prefixes not generative - cpd must be lexicalized (aaniila). *)
  ]
and gen_suffixes = (* productive suffixes cf. [Subst.taddhitas] *)
  [ "taa"
  ; "tva"
  ; "mat"
  ; "vat" 
  ; ... many other taddhitas ka ika aka/ikaa iika in iiya etc.
  ]] i*)

value enter_iic entry =  
  enter1 entry (Bare Noun (normal_stem entry)) (* stripped entry *) 
  (* NB This assumes the iic to be the entry stem - unsafe *)
;
value compute_extra_iic = iter enter_iic  
;
(* Feminine stems iic for productive adjectives                       *)
(* This is a generic weakness, to be remedied.                        *)
(* Generative stems are not inspected for feminine stems              *)
(* attested as substantives, and thus incurring a feminine iic stem.  *)
(* This concerns privative compounds and participles.                 *)
(* Equivalent to declaring a supplementary entry as icfc.             *)
value iicf_extra = 
  [ "abalaa" (* a-bala with fem abalaa *)  
  ; "ukhaa" (*  ukhaasrat *)
  ; "kaantaa" (* kaanta pp *)
  ; "draak.saa" (* draak.saaphala *) 
  ; "madhymaa" (* superlative tarjanī-madhyamā-anāman *)
(*i TODO: merge with [enter_iic_stem] above i*)
  ] 
;
(* Glitch to allow Cvi construction to kridanta entries, even though
   [Inflected.enter_form] called from [Parts] does not allow it. *)
(* Incomplete for compounds anyway: "si.mh'avyaaghraami.siik.r" *)
value iiv_krids = 
  [ "gupta"
  ; "yuddha"
  ; "lak.sya"
  ; "vibhinna"
  ; "vyakta"
  ; "ziir.na"
  ; "ziita"
  ; "zuddha"
  ; "spa.s.ta"
  ; "saaci" (* ind *) 
  ]
;
value enter_iiv entry = 
  match revstem entry with
  [ [ _ :: stem ] -> enter1 entry (Cvi (wrap stem 4 (* ii *)))
  | _ -> failwith "wrong stem enter_iiv"
  ]
;
value compute_extra_iiv = iter enter_iiv 
;

(* Gati forms used as prefixes of auxiliary verbs, like Iiv -- form Absya *)
value gatis = (* G{saak.sat} \Pan{1,4,74} + G(uurii) \Pan{1,4,61} *)
  [ "saak.saat" (* in the sense of cvi - becoming Wh§1078a *)
  ; "mithyaa" 
  ; "cintaa"  
  ; "bhadraa" 
  ; "locanaa" 
  ; "vibhaa.saa" (* sampatkaa ? *) 
  ; "aasthaa" 
  ; "amaa"
  ; "zraddhaa" (* praajaryaa praajaruhaa viijaryaa viijaruhaa sa.msaryaa *)
  ; "arthe"
  ; "lava.nam"
  ; "u.s.nam" (* u.s.na.mk.rtya \Pan{1,4,74} *)
  ; "ziitam"
  ; "udakam"
  ; "aardram" 
  ; "agnau"
  ; "vaze" (* vikampate vihasane prahasane pratapane *)
  ; "praadur" (* Wh§1078 *)
  ; "namas" (* namask.rtya Wh§1092a *)
  ; "aavis" (* aavisk.rtya Wh§1078 *)
  ; "urasi" (* \Pan{1,4,75} in the sense of anatyaadhaana cf Sharma *)
  ; "manasi" (* id. *)
  ; "anye" (* \Pan{1,4,76} id *)
  ; "pade" (* id. *)
  ; "madhye" (* id. *)
  ; "nivacane" (* id. *)
  ; "haste" (* \Pan{1,4,77} upayamana (mariage) *)
  ; "paa.nau" (* id. *)
  ; "svayam"
  ; "uurii" (* \Pan{1,4,61} G{uurii} uuriik.rtya but Wh§1094b says uriik.r *)
  (* other G{uurii}: yadurii,urarii,yadurarii,paapii,laalii,aattaalii,vetaalii,
     dhuurii,zakalii,sa.mzaklii,phaluu,phalii,viklii, etc. ignored or Cvi *)
(* The following gatis are treated as preverbs to specific roots:
  ; "astam" (* gam,i \Pan{1,4,68} asta.mgatya Wh§1092b *)
  ; "puras" (* k.r1,dhaa1,i \Pan{1,4,67} Wh§1078 *)
  ; "tiras" (* k.r1,dhaa1 \Pan{1,4,71-72} Wh§1078 *)
  ; "alam" (* ala.mk.rtya \Pan{1,4,64} Wh§1078a *)
  ; "bahis" (* k.r1 bhuu1 Wh§1078a *)
  ; "zrat" (* dhaa1 Wh§1079 *) 
  ; "sat" (* satk.rtya \Pan{1,4,63}, and "asat" recognized in a-satk.rtya *)
  ; "ka.ne"/"manas" ka.nehatya \Pan{1,4,66} 
  ; "antar" (* i gam dhaa han antarhatya \Pan{1,4,65} *)
  ; "paaram" (* i gam *) TODO *)
(*; "adas" ada.hk.rtya \Pan{1,4,70} TODO *)
(* Ignored at present
   accha acchaa acchagatya acchodya \Pan{1,4,69} Wh§1078 
   vinaa Wh§1078a 
   g.rhya abs used as ifc cvi{grah} hastag.rhya kar.nag.rhya RV Wh§990h
   also ignored onomatopeae pa.tapa.taakaroti etc. .daac \Pan{5,4,57-67}
   and samayaa for samaya in samayaakaroti  \Pan{5,4,61}
   also interjections like va.sa.t va.sa.tkaroti but va.sa.tkaara lexicalized *)
  ]
;
value enter_gati gati = (* assumes gati has lexical entry *)
  let stem = normal_stem gati in 
  enter1 gati (Cvi stem)
;
(* Now for the construction "reduced to" with auxiliaries *)

value gati_products = (* on demand for gati in -saat *)
  [ "agni"; "aatma"; "cuur.na"; "dasyu"; "bhasma"; "bhuumi"; "braahma.na" ]
;
(* Whitney§1108 -saat s does not go to retroflex .s *)
value enter_saat_gati product =  (* assumes gati has lexical entry *)
  let gati = product ^ "saat" in (* bhasmasaat = reducing to cinders *)
  let stem = normal_stem gati in 
  enter1 product (Cvi stem) 
(* NB There is a possible redundancy when the adverb in -saat is lexicalized,
and is immediately followed by a form of k.r, as or bhuu (without space).
The lexicalization is necessary when the construction is used with a different 
auxiliary, such as yaa (bhasmasaat) or nii (Whitney) or sampad (gr.).
Also necessary when non contiguous see cite{796} *)
;

(* Tasils are treated as adverbs. Here are the lexicalized ones: Whitney§1098 
   First tasils of pronouns, not needed if lexicalised 
 [; enter1 "tad"    (Indecl Tas (code "tatas"))   (* tasil on tad \Pan{5,3,7} *) 
  ; enter1 "ya#1"   (Indecl Tas (code "yatas"))   (* tasil on ya \Pan{5,3,7} *) 
  ; enter1 "ku#1"   (Indecl Tas (code "kutas"))   (* tasil on ku \Pan{5,3,7-8} *)
  ; enter1 "abhi"   (Indecl Tas (code "abhitas")) (* tasil on abhi \Pan{5,3,9} *)
  ; enter1 "pari"   (Indecl Tas (code "paritas")) (* tasil on pari \Pan{5,3,9} *)
  ; enter1 "anti"   (Indecl Tas (code "antitas")) (* tasil on pn \Pan{5,3,7} *)
  ; enter1 "ayam"   (Indecl Tas (code "atas"))    (* tasil on ayam \Pan{5,3,5} *)
  ; enter1 "idam"   (Indecl Tas (code "itas"))    (* tasil on idam id *)
  ; enter1 "adas"   (Indecl Tas (code "amutas"))    (* id *)
  ; enter1 "anya"   (Indecl Tas (code "anyatas"))   (* id *)
  ; enter1 "para"   (Indecl Tas (code "paratas"))   (* id *) 
  ; enter1 "vizva"  (Indecl Tas (code "vizvatas"))  (* id *) 
  ; enter1 "puurva" (Indecl Tas (code "puurvatas")) (* id *) 
  ; enter1 "sarva"  (Indecl Tas (code "sarvatas"))  (* id *) 
  ; enter1 "eka"    (Indecl Tas (code "ekatas"))    (* id *) 
  ; enter1 "sva"    (Indecl Tas (code "svatas"))    (* id *) 
  ; enter1 "anyatara" (Indecl Tas (code "anyataratas")) (* id *)
  ; enter1 "dak.si.na" (Indecl Tas (code "dak.si.natas"))  (* id *) 
  ; enter1 "avara"  (Indecl Tas (code "avaratas"))  (* \Pan{5,3,29} *)  
  ; enter1 "uttara#1" (Indecl Tas (code "uttaratas")) (* on pn \Pan{5,3,7} ? *)
  ; enter1 "ubhaya" (Indecl Tas (code "ubhayatas")) (* on pn \Pan{5,3,7} ? *)
  ; enter1 "puras" (Indecl Tas (code "puratas")) (* on indecl puras *)]
*)
value compute_extra_tasils () = do (* add non-generative tasils - ad-hoc *) 
  { enter1 "ekaruupa" (Indecl Tas (code "ekaruupatas")) (* tasil on cpd *)  
  ; enter1 "ekaanta" (Indecl Tas (code "ekaantatas")) (* tasil on cpd *)  
  ; enter1 "kaamacaara" (Indecl Tas (code "kaamacaaratas")) (* id *)  
(*; enter1 "d.r.s.taanta" (Indecl Tas (code "d.r.s.taantatas")) tasil on icpd *)
  ; enter1 "guruvaktra" (Indecl Tas (code "guruvaktratas")) (* id *) 
  ; enter1 "paramaartha" (Indecl Tas (code "paramaarthatas")) (* id *) 
  ; enter1 "praagbhaava" (Indecl Tas (code "praagbhaavatas")) (* id *) 
  ; enter1 "svabhaava" (Indecl Tas (code "svabhaavatas")) (* id *) 
  ; enter1 "svaravar.na" (Indecl Tas (code "svaravar.natas")) (* id *) 
  ; enter1 "gu.nabheda" (Indecl Tas (code "gu.nabhedatas")) (* id *) 
  ; enter1 "bhasad" (Indecl Tas (code "bhasattas")) (* tasil on consonant stem *)
(*; enter1 "nas#2" (Indecl Tas (code "nastas")) - idem but lexicalized *)
  ; enter1 "pratibhaa#2" (Indecl Tas (code "pratibhaatas")) (* tasil on fstem *)
  ; enter1 "yad.rcchaa" (Indecl Tas (code "yad.rcchaatas")) (* id *)
  ; enter1 "vivak.saa" (Indecl Tas (code "vivak.saatas")) (* id *)
(* NB bhii.smadro.napramukhatas BhG{1,25} treated in [enter_extra_ifcs] below *) 
  } 
; 
(* Supplementary forms - called by [Make_nouns.genders_to_nouns]
   with argument [iic_stems] contents of [iic_stems_file] dumped from
   [Subst.iic_stems] built by calling [Subst.record_iic] for iic only entries. *)
value compute_extra iic_only_stems = do 
  { enter1 "maas" (* Siddhaanta kaumudii *) decl (*i Jha - CHECK i*)
    where decl = Declined Noun Mas [ (Dual,[ (Ins,code "maabhyaam") ]) ] 
  ; enter1 "yuu.sa" (* Siddhaanta kaumudii *) decl
    where decl = Declined Noun Mas [ (Plural,[ (Loc,code "yuu.h.su") ]) ]
  ; enter1 "avanam" (Cvi (code "avanamii")) (* exception *)
  ; enter1 "nara" decl 
    where decl = Declined Noun Mas [ (Plural,[ (Gen,code "n.rr.naam") ]) ]
  ; enter1 "nara" decl (* \Pan{6,4,6} *)
    where decl = Declined Noun Mas [ (Plural,[ (Gen,code "n.r.naam") ]) ] 
  ; enter1 "nara" decl 
    where decl = Bare Noun (code "n.r") 
  ; enter1 "bhagavat" decl (* archaic vocative bhagavas *)
    where decl = Declined Noun Mas [ (Singular,[ (Voc,code "bhagavas") ]) ]
  ; enter1 "tak.san" decl (* \Pan{6,4,9} *) 
    where decl = Declined Noun Mas [ (Singular,[ (Acc,code "tak.sa.nam") ]) ]
  ; enter1 "bhuuman" decl (* dhruvaaya bhuumaaya nama.h *) 
    where decl = Declined Noun Mas [ (Singular,[ (Dat,code "bhuumaaya") ]) ]
  ; enter1 "sudhii" (* Monier *) decl
    where decl = Declined Noun Mas [ (Singular,[ (Nom,code "sudhi") ]) ]
  ; enter1 "viz#2" (* Vedic Whitney§218a *) decl
    where decl = Declined Noun Fem [ (Plural,[ (Loc,code "vik.su") ]) ]
  ; iter enter_iic_avya iic_avyas
  ; enter1 "tva" (* but skipped in dico *) decl
    where decl = Declined Noun Mas [ (Singular,[ (Nom,code "tvas") ]) ]
  ; enter1 "tva" decl
    where decl = Declined Noun Mas [ (Dual,[ (Nom,code "tvau") ]) ]
  ; enter1 "tva" decl
    where decl = Declined Noun Mas [ (Plural,[ (Nom,code "tve") ]) ]
  ; enter1 "giri" (Avyayaf (code "giram")) (* \Pan{5,4,112} upagiram *)
  ; iter enter_gati gatis
  ; iter enter_saat_gati gati_products
  ; compute_extra_indecls ()
  ; compute_extra_tasils ()
  ; compute_extra_iic iic_indecl (* antar *) 
  ; compute_extra_iic iic_only_stems (* aajaanu etc. *)
  ; compute_extra_iic iicf_extra (* abalaa etc. *)
  ; compute_extra_iiv iiv_krids (* zuddhii *) 
  ; enter1 "ekaika" decl (* ad hoc pronominal vibhakti *)
    where decl = Declined Noun Mas [ (Singular,[ (Dat,code "ekaikasmai") 
                                               ; (Abl,code "ekaikasmaat") 
                                               ; (Loc,code "ekaikasmin") 
                                               ]) ]
  ; enter1 "u" (* Vedic *) (Indecl Interj [ 5 ] (* u *))
  ; existential "cit" (* cid1 *)
  ; existential "cana"
    (* Unplugged presently because of overgeneration
  ; [compute_extra_iic gen_prefixes] 
  ; [compute_extra_ifc bahu_suffixes] eg Fem -padaa for meter formation *) 
  }
;
(* Used in [Make_nouns] in Phase 2 ifc pass *)
value enter_extra_ifcs () = do (* archaic retroflexion in cpds \Pan{8,4,13} *)
  { let entry = "bhogya" in (* var.sabhogye.na Meghaduuta 1b *)
        let ins_sg = [ (Singular,[ (Ins,code "bhogye.na") ]) ]
        and gen_pl = [ (Plural,  [ (Gen,code "bhogyaa.naam") ]) ] in do
        { enter1 entry (Declined Noun Mas ins_sg) (* Meghaduuta{1} *)
        ; enter1 entry (Declined Noun Mas gen_pl)
        ; enter1 entry (Declined Noun Neu ins_sg)
        ; enter1 entry (Declined Noun Neu gen_pl)
        ; enter1 entry (Declined Noun Fem gen_pl)
        }
  ; let entry = "yogin" in (* pu.spayogi.nah Renou yogi-fleur? *)
        let form = code "yogi.nas" in do
        { enter1 entry (Declined Noun Mas [ (Singular, [ (Gen,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Singular, [ (Abl,form) ]) ])
        ; enter1 entry (Declined Noun Neu [ (Singular, [ (Gen,form) ]) ])
        ; enter1 entry (Declined Noun Neu [ (Singular, [ (Abl,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Plural,   [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Plural,   [ (Acc,form) ]) ])
        }
  ; let entry = "yuga" in do 
      { let form = code "yugaa.ni" in do (* v.r.sabhayugaa.ni vastrayugaa.ni *)
        { enter1 entry (Declined Noun Neu [ (Plural,   [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Neu [ (Plural,   [ (Acc,form) ]) ])
        }
      ; let form = code "yuge.na" in do (* vastrayuge.na kharayuge.na *)
        { enter1 entry (Declined Noun Neu [ (Singular, [ (Ins,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Singular, [ (Ins,form) ]) ])(*bahu*)
        } 
      } (* NB "vastrayugi.nas", "vastrayugi.nau" etc. OK since stem autonomous *)
  ; let entry = "kaamin" in (* svargakaami.nau *)
        let form = code "kaami.nau" in do
        { enter1 entry (Declined Noun Mas [ (Dual,     [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Dual,     [ (Acc,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Dual,     [ (Voc,form) ]) ])
        }
  ; let entry = "gaamin" in (* v.r.sagaami.nau *)
        let form = code "gaami.nau" in do
        { enter1 entry (Declined Noun Mas [ (Dual,     [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Dual,     [ (Acc,form) ]) ])
        ; enter1 entry (Declined Noun Mas [ (Dual,     [ (Voc,form) ]) ])
        }
  ; let entry = "dh.rt" in (* ruupadh.rk *)
        let form = code "dh.rk" in do
        { enter1 entry (Declined Noun Mas [ (Singular, [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Neu [ (Singular, [ (Nom,form) ]) ])
        ; enter1 entry (Declined Noun Fem [ (Singular, [ (Nom,form) ]) ])
        }
  }
;
(* Special provision for specific adverbs usable as ifcs *)
value enter_indecl_ifcs () = do
  { let entry = "k.rtvas" in
        enter1 entry (Indifc Adv (code entry))
  ; let entry = "vibhaagazas" in
        enter1 entry (Indifc Adv (code entry))
(* tasils used as ifc *)
  ; let entry = "pramukha" in (* eg bhii.sma-dro.na-pramukhatas *)
        enter1 entry (Indifc Tas (code "pramukhatas")) (* ifc tasil *)
  ; let entry = "kaara" in (* eg "kaamakaaratas" of his/her own will *)
        enter1 entry (Indifc Tas (code "kaaratas")) (* ifc tasil *)
  ; let entry = "bhaava" in 
        enter1 entry (Indifc Tas (code "bhaavatas")) (* ifc tasil *)
  ; let entry = "yoga" in 
        enter1 entry (Indifc Tas (code "yogatas")) (* afguli-traya-yogatas *)
(* namuls used as ifc *)
  ; let entry = "utthaa" in (* ad-hoc for compound zayyotthaayam Pan{3,4,52} *)
        enter1 entry (Indifc Abs (code "utthaayam")) (* ifc .namul *) 
(*; let entry = "p.rr" in (* ad-hoc for compound go.spadapuuram Pan{3,4,32} *)
        enter1 entry (Indifc Abs (code "puuram")) (* ifc .namul *) *)
  ; let entry = "purastaat" in (* for uttarapurastaat *)
        enter1 entry (Indifc Adv (code "purastaat")) (* fake abl postposition *) 
  ; let entry = "adhastaat" in (* similarly *)
        enter1 entry (Indifc Adv (code "adhastaat")) (* postposition *) 
  ; let entry = "pazcaat" in (* dak.si.napazcaat *)
        enter1 entry (Indifc Adv (code "pazcaat")) (* postposition *) 
  ; let entry = "naama" in (* devadatta-naama *)
        enter1 entry (Indifc Prep (code "naama")) (* postposition *) 
  }
;
value enter_extra_iifcs () = do
  { let entry = "ahan" in (* for -aha- like pu.nyaahavaacanam *)
    enter1 entry (Bare Noun (code "aha"))
  ; let entry = "aakhyaa#2" in (* for -aakhya- like zaaradiiyaakhyanaamamaalaa *)
    enter1 entry (Bare Noun (code "aakhya"))
  ; let entry = "senaa" in (* for zuklasenadeva.h *)
    enter1 entry (Bare Noun (code "sena"))
  ; let entry = "da.m.s.traa" in (* for bhagnanakhada.m.s.travyaalam *)
    enter1 entry (Bare Noun (code "da.m.s.tra"))
  ; let entry = "aali" in (* for khadyotaaliivilasitanibhaa.m MD{78} *)
    enter1 entry (Bare Noun (code "aalii"))
    (* more entries are potentially concerned - for bahus of X-Y with Y fstem *)
  } 
;
(* called by [Declension.emit_decls] and [Morpho_debug.emit_decls] *)
value fake_compute_decls ((s,_ (* forget decli *)) as nmorph) part = 
  let entry = s in do  (* fake entry made from stem s - cheat *)
  { reset_nominal_databases ()  
  ; morpho_gen.val := False
  ; compute_decls_stem entry nmorph part
  ; nominal_databases () 
  }
;
      
(* For Interface - cache management *)
open Bank_lexer; 
module Gram = Camlp4.PreCast.MakeGram Bank_lexer 
;
open Bank_lexer.Token;
open Skt_morph;

value full_entry = Gram.Entry.mk "full_entry"
and entry        = Gram.Entry.mk "entry"
and gen          = Gram.Entry.mk "gen"
;
EXTEND Gram
  full_entry:
    [ [ e = entry; g = gen -> (e,g) ] ] ;
  entry:
    [ [ "["; t = TEXT; "]" -> t ] ];
  gen:
    [ [ "("; t = TEXT; ")" ->
      let gender_of = fun
        [ "m." -> Mas
        | "f." -> Fem
        | "n." -> Neu
        | s -> failwith ("Weird gender " ^ s)
        ] in
      Gender (gender_of t) ] ];
END
;
value parse_entry s =
  try Gram.parse_string full_entry Loc.ghost s with
  [ Loc.Exc_located loc e -> do
     { Format.eprintf "Wrong input: %s\n, at location %a:@." s Loc.print loc 
     ; raise e
     }
  ]
;
value update_index ic =
  try read_from_ic ic
      where rec read_from_ic ic = 
          let s = input_line ic in do
          { let ((entry,gender) as eg) = parse_entry s in 
            try compute_decls_stem entry eg ""
            with [ Sys_error m -> print_string ("Sys_error "  ^ m)
                 | _  -> print_string "Wrong input"
                 ]
          ; read_from_ic ic
	  }
  with [ End_of_file -> close_in ic ] 
;
(* Cache forms computation - used in Interface and [Restore_caches] *)
value extract_current_caches cache_txt_file = do 
  { nouns.val := Deco.empty 
  ; iics.val := Deco.empty 
  ; morpho_gen.val := False
  ; let ic = open_in cache_txt_file in update_index ic
  ; (nouns.val,iics.val)
  }
;
      
(*i end; i*)
