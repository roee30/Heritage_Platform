(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Multilingual = struct i*)

(* This module gives headers of grammar engines Declension and Conjugation
   both in roman font (English at present) and devanagarii font (Sanskrit) *)

open Skt_morph;
open Html;

type font = [ Deva | Roma ]
;
value font_of_string = fun
  [ "deva" -> Deva
  | "roma" -> Roma
  | f -> failwith ("Unknown font " ^ f)
  ] 
and string_of_font = fun
  [ Deva -> "deva" 
  | Roma -> "roma"
  ] 
;
value gender_caption gender = fun 
  [ Roma -> span3_center (match gender with 
      [ Mas -> "Masculine"
      | Fem -> "Feminine"
      | Neu -> "Neuter"
      | Deictic _ -> "All"
      ])
  | Deva -> deva12_blue_center (Encode.skt_raw_to_deva (match gender with 
      [ Mas -> "pumaan"
      | Fem -> "strii"
      | Neu -> "napu.msakam"
      | Deictic _ -> "sarvam"
      ]))
  ]
and number_caption number = fun
  [ Roma -> span3_center (match number with 
      [ Singular -> "Singular" 
      | Dual     -> "Dual" 
      | Plural   -> "Plural" 
      ])
  | Deva -> deva12_blue_center (Encode.skt_raw_to_deva (match number with 
      [ Singular -> "eka" 
      | Dual     -> "dvi" 
      | Plural   -> "bahu" 
      ]))
  ]
and case_caption case = fun
  [ Roma -> span3_center (match case with 
      [ Nom -> "Nominative"
      | Acc -> "Accusative"
      | Ins -> "Instrumental"
      | Dat -> "Dative"
      | Abl -> "Ablative"
      | Gen -> "Genitive" 
      | Loc -> "Locative"
      | Voc -> "Vocative"
      ])
  | Deva -> deva12_blue_center (Encode.skt_raw_to_deva (match case with 
      [ Nom -> "prathamaa"
      | Acc -> "dvitiiyaa"
      | Ins -> "t.rtiiyaa"
      | Dat -> "caturthii"
      | Abl -> "pa~ncamii"
      | Gen -> ".sa.s.thii" 
      | Loc -> "saptamii"
      | Voc -> "sambodhanam"
      ]))
  ]
;
value compound_name = fun
  [ Roma -> span3_center "Compound"
  | Deva -> deva12_blue_center (Encode.skt_raw_to_deva "samaasa")
  ]
and avyaya_name = fun
  [ Roma -> span3_center "Adverb"
  | Deva -> deva12_blue_center (Encode.skt_raw_to_deva "avyaya")
  ]
;
value western_pr = fun
  [ Present    -> "Present"
  | Imperative -> "Imperative" 
  | Optative   -> "Optative"
  | Imperfect  -> "Imperfect"
  ]
and indian_pr = fun
  [ Present    -> "la.t"
  | Imperative -> "lo.t" 
  | Optative   -> "vidhilif"
  | Imperfect  -> "laf"
  ]
;
value western_tense = fun
  [ Future       -> "Future" 
  | Perfect      -> "Perfect" 
  | Aorist _     -> "Aorist"
  | Injunctive _ -> "Injunctive"
  | Conditional  -> "Conditional"
  | Benedictive  -> "Benedictive"
  | Subjunctive  -> "Subjunctive" 
  ]
and indian_tense = fun
  [ Future       -> "l.r.t" 
  | Perfect      -> "li.t" 
  | Aorist _     -> "luf"
  | Injunctive _ -> "aagamaabhaavayuktaluf" 
  | Conditional  -> "l.rf"
  | Benedictive  -> "aaziirlif" 
  | Subjunctive  -> "le.t" 
  ] 
;
type gentense =
  [ Present_tense of pr_mode
  | Other_tense of tense
  ]
;
value tense_name gentense = fun
  [ Deva -> deva16_blue_center (Encode.skt_raw_to_deva s) 
            where s = match gentense with
    [ Present_tense pr -> indian_pr pr 
    | Other_tense t -> indian_tense t
    ]
  | Roma -> span2_center s where s = match gentense with
    [ Present_tense pr -> western_pr pr 
    | Other_tense t -> western_tense t
    ]
  ]
and perfut_name = fun
  [ Deva -> deva16_blue_center (Encode.skt_raw_to_deva "lu.t")
  | Roma -> span2_center "Periphrastic Future"
  ]
;
value person_name person = fun
  [ Deva -> let deva_person = match person with
                [ First -> "uttama"
                | Second -> "madhyama"
                | Third -> "prathama"
                ] in 
            deva12_blue_center 
                (Encode.skt_raw_to_deva deva_person)
  | Roma -> let roma_person = match person with
                [ First -> "First"
                | Second -> "Second"
                | Third -> "Third"
                ] in 
            span3_center roma_person
  ]
;
value conjugation_name conj = fun
  [ Deva -> let indian_conj = match conj with
                [ Primary      -> "apratyayaantadhaatu"
                | Causative    -> ".nic"
                | Intensive    -> "yaf"
                | Desiderative -> "san"
                ] in
            deva16_blue_center (Encode.skt_raw_to_deva indian_conj)
  | Roma -> let western_conj = match conj with
                [ Primary      -> "Primary"
                | Causative    -> "Causative"
                | Intensive    -> "Intensive"
                | Desiderative -> "Desiderative"
                ] in
            span2_center (western_conj ^ " Conjugation")
  ]
;
value conjugation_title narrow = fun
  [ Deva -> Encode.skt_to_deva "dhaatuvibhakti"
  | Roma -> if narrow then "Conjugation"
            else "The Sanskrit Grammarian: Conjugation"
  ]
and declension_title narrow = fun
  [ Deva -> Encode.skt_to_deva "praatipadikavibhakti"
  | Roma -> if narrow then "Declension"
            else "The Sanskrit Grammarian: Declension"
  ]
and conjugation_caption = fun
  [ Deva -> Encode.skt_to_deva "tifantaavalii"
  | Roma -> "Conjugation tables of"
  ]
and declension_caption = fun
  [ Deva -> Encode.skt_to_deva "subantaavalii"
  | Roma -> "Declension table of"
  ]
and participles_caption = fun
  [ Deva -> deva16_blue_center (Encode.skt_raw_to_deva "k.rdanta")
  | Roma -> span2_center "Participles"
  ]
and indeclinables_caption = fun
  [ Deva -> deva16_blue_center (Encode.skt_raw_to_deva "avyaya")
  | Roma -> span2_center "Indeclinable forms"
  ]
and infinitive_caption = fun
  [ Deva -> Encode.skt_to_deva "tumun"
  | Roma -> "Infinitive"
  ]
and absolutive_caption is_root = fun
  [ Deva -> Encode.skt_to_deva (if is_root then "ktvaa" else "lyap")
(* PB: absolutives in -aam should rather be labeled ".namul" *)
  | Roma -> "Absolutive"
  ]
and peripft_caption = fun
  [ Deva -> Encode.skt_to_deva "li.t"
  | Roma -> "Periphrastic Perfect"
  ]
;
value voice_mark = fun
  [ Active  -> "para"
  | Middle  -> "aatma"
  | Passive -> "karma.ni"
  ]
;
value participle_name part = fun
  [ Deva -> let indian_part = match part with
      [ Ppp    -> [ "kta" ]
      | Pppa   -> [ "ktavatu" ]
      | Ppra _ -> [ "zat.r" ] 
      | Pprm _ -> [ "zaanac" ]
      | Pprp   -> [ "zaanac"; "karma.ni" ]
      | Ppfta  -> [ "li.daadeza"; voice_mark Active ]
      | Ppftm  -> [ "li.daadeza"; voice_mark Middle ]
      | Pfuta  -> [ "lu.daadeza"; voice_mark Active ]
      | Pfutm  -> [ "lu.daadeza"; voice_mark Middle ]
      | Pfutp k -> match k with 
                   [ 1 -> [ "yat" ]
                   | 2 -> [ "aniiyar" ]
                   | 3 -> [ "tavya" ]
                   | _ -> []
                   ]
      | Action_noun -> [ "krit" ] (* "gha~n" for -a "lyu.t" for -ana *)
      ]     in
  let cat s x = s ^ " " ^ (Encode.skt_raw_to_deva x) in
  List.fold_left cat "" indian_part (* no skt punctuation so far *)
  | Roma -> let western_part = match part with
      [ Ppp     -> "Past Passive Participle"
      | Pppa    -> "Past Active Participle"
      | Ppra _  -> "Present Active Participle"
      | Pprm _  -> "Present Middle Participle"
      | Pprp    -> "Present Passive Participle"
      | Ppfta   -> "Perfect Active Participle"
      | Ppftm   -> "Perfect Middle Participle"
      | Pfuta   -> "Future Active Participle"
      | Pfutm   -> "Future Middle Participle"
      | Pfutp _ -> "Future Passive Participle"
      | Action_noun -> "Action Noun"
      ]    in western_part
  ]
;
value voice_name voice = fun
  [ Deva -> let ivoice = match voice with
                [ Active  -> "parasmaipade"
                | Middle  -> "aatmanepade"
                | Passive -> "karma.ni"
                ] in
            deva12_blue_center (Encode.skt_raw_to_deva ivoice)
  | Roma -> let wvoice = match voice with
                [ Active  -> "Active"
                | Middle  -> "Middle"
                | Passive -> "Passive"
                ] in
            span3_center wvoice
  ]
;

(*i end; i*)
