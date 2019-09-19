(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Parts = struct i*)

(* Computes the declensions of participles from stored stems.*)

open Skt_morph;
open Encode; (* [rev_code_string], [code_string] *)
open Phonetics; (* monosyllabic aug *)
open Inflected; (* [enter enter1 enter_form enter_forms access_krid register_krid] *)

value mirror = Word.mirror
;
(* Used for storing participial stems in the [participles] list. *)
(* This structure is essential for fast online computation of verbal forms. *)
(* Beware - the stem argument is a reversed word, the string is the root. *)
type memo_part =
  [ Ppp_     of conjugation and Word.word and string (* Past Passive Part *)
  | Pppa_    of conjugation and Word.word and string (* Past Active Part *)
  | Ppra_    of gana and conjugation and Word.word and Word.word and string (* Present Active Part *)
  | Pprared_ of conjugation and Word.word and string (* idem reduplicated *)
  | Pprm_    of gana and conjugation and Word.word and string (* Present Middle Part *)
  | Pprp_    of conjugation and Word.word and string (* Present Passive Part *)
  | Ppfta_   of conjugation and Word.word and string (* Perfect Active Part *)
  | Ppftm_   of conjugation and Word.word and string (* Perfect Middle Part *)
  | Pfutm_   of conjugation and Word.word and string (* Future Middle Part *)
  | Pfuta_   of conjugation and Word.word and string (* Future Active Part *)
  | Pfutp_   of conjugation and Word.word and string (* Future Passive Part *)
  ]
;
(* Special gana values for present forms of secondary conjugations *)
(*i Not clearly needed, since not printed in XML banks - replace by 0 ? i*)
value cau_gana = 12
and   des_gana = 13
and   int_gana = 14
;
(* This is to avoid redundant generation of present system participles
   when stems may come from a distinct gana. *)
value redundant_gana k = fun 
  [ "svap"  -> k=1
  | "rud#1" -> k=6
  | _ -> False
  ]
;
(* Affixing a suffix to a (reversed) stem *)
(* [fix: Word.word -> string -> Word.word] *)
value fix revstem suff = 
  Int_sandhi.int_sandhi revstem (code_string suff)
;
value rfix revstem suff = mirror (fix revstem suff)
;
value fix_augment revstem suff = aug (fix revstem suff)
;
(* NB. Internal sandhi will take care of consonant elision in e.g.
ppp "tak.s" = "tak.s"+"ta"="ta.s.ta" 
idem for cak.s tvak.s Pan{8,2,29} *)

(* Generation of unique names for kridantas, specially participial stems *)
value gensym stem n = 
  if n=0 then stem 
  else mirror [ (n+50) :: mirror stem ] 
;
(* We look up in the kridantas database if the given stem has been registered
   (possibly with some homo index) for the same (verbal,root). If not, we 
   generate the name affixing to stem the next available homo *)
value gen_stem (v,root) stem = (* stem is a bare stem with no homo index *)
  if morpho_gen.val then
     let etym = (v,code_string root) in
     let alist = access_krid stem in 
     try gensym stem (List.assoc etym alist) with
       [ Not_found -> match alist with 
          [ [] -> (* no current homonym of stem *) do 
            { register_krid stem (etym,0)
            ; stem
            }
          | [ (_,n) :: _ ] -> (* last homonym entered [stem_n] *)
            let p=n+1 in 
            if p>9 then failwith "Gensym exceeds homo index" 
            else do { register_krid stem (etym,p); gensym stem p}
          ]
       ]
  else stem
;
(* Now for participle forming paradigms *)

(* Similar to [Nouns.build_mas_at [1 :: stem]] if vat=False
   and to [Nouns.build_mas_mat stem] if vat=True *)
value build_part_at_m vat verbal stem stem_at root = (* invoked by [Ppra_] *)
  let gen_entry = gen_stem (verbal,root) stem_at in
  let krid = Krid verbal root in 
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Mas
   [ (Singular,
        [ decline Voc "an"
        ; decline Nom (if vat then "aan" else "an")
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
   ; Bare krid stem_at (* e.g. b.rhadazva *)
   ] 
;
(* Similar to [Nouns.build_mas_red] *)
value build_part_at_m_red verbal stem stem_at root = 
  let gen_entry = gen_stem (verbal,root) stem_at in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Mas
   [ (Singular,
        [ decline Voc "at"
        ; decline Nom "at"
        ; decline Acc "atam"
        ; decline Ins "ataa"
        ; decline Dat "ate"
        ; decline Abl "atas"
        ; decline Gen "atas"
        ; decline Loc "ati"
        ])
   ; (Dual, 
        [ decline Voc "atau"
        ; decline Nom "atau"
        ; decline Acc "atau"
        ; decline Ins "adbhyaam"
        ; decline Dat "adbhyaam"
        ; decline Abl "adbhyaam"
        ; decline Gen "atos"
        ; decline Loc "atos"
        ])
   ; (Plural,
        [ decline Voc "atas"
        ; decline Nom "atas"
        ; decline Acc "atas"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ] 
   ; Bare krid stem_at 
   ]
;
(* Similar to [Nouns.build_neu_at] *)
value build_part_at_n verbal stem stem_at root = 
  let gen_entry = gen_stem (verbal,root) stem_at in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Neu
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
        ; decline Voc "antii"
        ; decline Nom "atii"
        ; decline Nom "antii"
        ; decline Acc "atii"
        ; decline Acc "antii"
        ; decline Ins "adbhyaam"
        ; decline Dat "adbhyaam"
        ; decline Abl "adbhyaam"
        ; decline Gen "atos"
        ; decline Loc "atos"
        ])
   ; (Plural,
        [ decline Voc "anti"
        ; decline Nom "anti"
        ; decline Acc "anti"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ] 
   ; Bare krid stem_at 
   ]
;
(* Similar to [Nouns.build_neu_red] *)
value build_part_at_n_red verbal stem stem_at root = 
  let gen_entry = gen_stem (verbal,root) stem_at in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Neu
   [ (Singular,
        [ decline Voc "at"
        ; decline Nom "at"
        ; decline Acc "atam"
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
        [ decline Voc "ati"
        ; decline Voc "anti"
        ; decline Nom "ati"
        ; decline Nom "anti"
        ; decline Acc "ati"
        ; decline Acc "anti"
        ; decline Ins "adbhis"
        ; decline Dat "adbhyas"
        ; decline Abl "adbhyas"
        ; decline Gen "ataam"
        ; decline Loc "atsu"
        ])
   ] 
   ; Bare krid stem_at
   ]
;
(* Similar to [Nouns.build_fem_ii] *)
value build_part_ii verbal stem prati root = 
  let stem_ii = mirror [ 4 :: stem ] in 
  let gen_entry = gen_stem (verbal,root) prati in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Fem
   [ (Singular,
        [ decline Voc "i"
        ; decline Nom "ii"
        ; decline Acc "iim"
        ; decline Ins "yaa"
        ; decline Dat "yai"
        ; decline Abl "yaas"
        ; decline Gen "yaas"
        ; decline Loc "yaam"
        ])
   ; (Dual, 
        [ decline Voc "yau"
        ; decline Nom "yau"
        ; decline Acc "yau"
        ; decline Ins "iibhyaam"
        ; decline Dat "iibhyaam"
        ; decline Abl "iibhyaam"
        ; decline Gen "yos"
        ; decline Loc "yos"
        ])
   ; (Plural, 
        [ decline Voc "yas"
        ; decline Nom "yas"
        ; decline Acc "iis"
        ; decline Ins "iibhis"
        ; decline Dat "iibhyas"
        ; decline Abl "iibhyas"
        ; decline Gen "iinaam"
        ; decline Loc "ii.su"
        ])
   ]             
   ; Bare krid stem_ii (* productive ? *)
   ]
;
(* Similar to [Nouns.build_mas_a] *)
value build_part_a_m verbal stem prati root = 
  let gen_entry = gen_stem (verbal,root) prati in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Mas
   [ (Singular,
        [ decline Voc "a"
        ; decline Nom "as"
        ; decline Acc "am"
        ; decline Ins "ena"
        ; decline Dat "aaya"
        ; decline Abl "aat"
        ; decline Gen "asya"
        ; decline Loc "e"
        ])
   ; (Dual, 
        [ decline Voc "au"
        ; decline Nom "au"
        ; decline Acc "au"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, 
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aan"
        ; decline Ins "ais"
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "aanaam"
        ; decline Loc "esu"
        ])
   ]
   ; Bare krid prati
   (* what follows needs adapting [Inflected.enter_form]
   ; Avyayaf (fix stem "am") (* yathaav.rddham *)  
     possible Cvi usage: see [Nouns.iiv_krids] *)
   ] 
;
(* Similar to [Nouns.build_neu_a] *)
value build_part_a_n verbal stem prati root = 
  let gen_entry = gen_stem (verbal,root) prati in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Neu
   [ (Singular,
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
   ; (Dual, 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, 
        [ decline Voc "aani"
        ; decline Nom "aani"
        ; decline Acc "aani"
        ; decline Ins "ais"
        ; decline Dat "ebhyas"
        ; decline Abl "ebhyas"
        ; decline Gen "aanaam"
        ; decline Loc "esu"
        ])
   ]             
   ; Bare krid prati
   ]
;
(* Similar to [Nouns.build_fem_aa] *)
value build_part_aa verbal stem prati root = 
  let gen_entry = gen_stem (verbal,root) prati in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) in
  enter_forms gen_entry 
   [ Declined krid Fem
   [ (Singular,
        [ decline Voc "e"
        ; decline Nom "aa"
        ; decline Acc "aam"
        ; decline Ins "ayaa"
        ; decline Dat "aayai"
        ; decline Abl "aayaas"
        ; decline Gen "aayaas"
        ; decline Loc "aayaam"
        ])
   ; (Dual, 
        [ decline Voc "e"
        ; decline Nom "e"
        ; decline Acc "e"
        ; decline Ins "aabhyaam"
        ; decline Dat "aabhyaam"
        ; decline Abl "aabhyaam"
        ; decline Gen "ayos"
        ; decline Loc "ayos"
        ])
   ; (Plural, 
        [ decline Voc "aas"
        ; decline Nom "aas"
        ; decline Acc "aas"
        ; decline Ins "aabhis"
        ; decline Dat "aabhyas"
        ; decline Abl "aabhyas"
        ; decline Gen "aanaam"
        ; decline Loc "aasu"
        ])
   ] ]
;
(* Similar to [Nouns.build_mas_vas] *)
(* Except for proper intercalation of i *)
value build_mas_ppfa verbal stem inter stem_vas root = 
  let gen_entry = gen_stem (verbal,root) stem_vas in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) 
  and declinev case suff = (case,fix stem suffi) where 
      suffi = if inter then "i" ^ suff else suff in
  enter_forms gen_entry 
   [ Declined krid Mas
   [ (Singular,
        [ declinev Voc "van"
        ; declinev Nom "vaan"
        ; declinev Acc "vaa.msam"
        ; decline  Ins "u.saa"
        ; decline  Dat "u.se"
        ; decline  Abl "u.sas"
        ; decline  Gen "u.sas"
        ; decline  Loc "u.si"
        ])
   ; (Dual, 
        [ declinev Voc "vaa.msau"
        ; declinev Nom "vaa.msau"
        ; declinev Acc "vaa.msau"
        ; declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ; decline  Gen "u.sos"
        ; decline  Loc "u.sos"
        ])
   ; (Plural,
        [ declinev Voc "vaa.msas"
        ; declinev Nom "vaa.msas"
        ; decline  Acc "u.sas"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; decline  Gen "u.saam"
        ; declinev Loc "vatsu"
        ])
   ]
   ; Bare krid (fix stem "vat") (* eg vidvat- *)
(* ; Avyayaf (fix stem "vas") - Not dealt with by [Inflected.enter_form] *)
   ]
;
(* Similar to [Nouns.build_neu_vas] *)
value build_neu_ppfa verbal stem inter stem_vas root = 
  let gen_entry = gen_stem (verbal,root) stem_vas in
  let krid = Krid verbal root in
  let decline case suff = (case,fix stem suff) 
  and declinev case suff = (case,fix stem suffi) where 
      suffi = if inter then "i" ^ suff else suff in
  enter_forms gen_entry 
   [ Declined krid Neu
   [ (Singular,
        [ declinev Voc "vat"
        ; declinev Nom "vat"
        ; declinev Acc "vat"
        ; decline  Ins "u.saa"
        ; decline  Dat "u.se"
        ; decline  Abl "u.sas"
        ; decline  Gen "u.sas"
        ; decline  Loc "u.si"
        ])
   ; (Dual, 
        [ decline  Voc "u.sii"
        ; decline  Nom "u.sii"
        ; decline  Acc "u.sii"
        ; declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ; decline  Gen "u.sos"
        ; decline  Loc "u.sos"
        ])
   ; (Plural, 
        [ declinev Voc "vaa.msi"
        ; declinev Nom "vaa.msi"
        ; declinev Acc "vaa.msi"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; decline  Gen "u.saam"
        ; declinev Loc "vatsu"
        ])
   ]
   ; Bare krid (fix stem "vat") (* eg vidvat- *)
(* ; Avyayaf (fix stem "vas") - Not dealt with by [Inflected.enter_form] *)
   ]
;
(* Supplementary forms with intercalation of i in later language Whitney§805b *)
value build_late_ppfa verbal stem stem_vas root = 
  let gen_entry = gen_stem (verbal,root) stem_vas in
  let krid = Krid verbal root in
  let declinev case suff = (case,fix stem ("i" ^ suff)) in do
   { enter_forms gen_entry 
   [ Declined krid Mas
   [ (Singular,
        [ declinev Voc "van"
        ; declinev Nom "vaan"
        ; declinev Acc "vaa.msam"
        ])
   ; (Dual, 
        [ declinev Voc "vaa.msau"
        ; declinev Nom "vaa.msau"
        ; declinev Acc "vaa.msau"
        ; declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ])
   ; (Plural,
        [ declinev Voc "vaa.msas"
        ; declinev Nom "vaa.msas"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; declinev Loc "vatsu"
        ])
   ] 
   ; Declined krid Neu
   [ (Singular,
        [ declinev Voc "vat"
        ; declinev Nom "vat"
        ; declinev Acc "vat"
        ])
   ; (Dual, 
        [ declinev Ins "vadbhyaam"
        ; declinev Dat "vadbhyaam"
        ; declinev Abl "vadbhyaam"
        ])
   ; (Plural, 
        [ declinev Voc "vaa.msi"
        ; declinev Nom "vaa.msi"
        ; declinev Acc "vaa.msi"
        ; declinev Ins "vadbhis"
        ; declinev Dat "vadbhyas"
        ; declinev Abl "vadbhyas"
        ; declinev Loc "vatsu"
        ])
   ]    
   ; Bare krid (fix stem "vat") 
   ]
   }
;
value build_part_a part_kind stem root = 
  let prati = mirror [ 1 :: stem ] in do
  { build_part_a_m part_kind stem prati root 
  ; build_part_a_n part_kind stem prati root 
  ; build_part_aa part_kind stem prati root 
  }
and build_part_at part_kind stem stemf root = 
  let prati = fix stem "at" in do (* [Ppra_] *)
  { build_part_at_m False part_kind stem prati root 
  ; build_part_at_n part_kind stem prati root 
  ; build_part_ii part_kind stemf prati root 
  }
and build_part_at_red part_kind stem stemf root = 
  let prati = mirror [ 32 :: [ 1 :: stem ] ] in do (* [Pprared_] *)
  { build_part_at_m_red part_kind stem prati root 
  ; build_part_at_n_red part_kind stem prati root 
  ; build_part_ii part_kind stemf prati root 
  }
and build_part_vat part_kind stem stemf root = 
  let prati = mirror [ 32 :: [ 1 :: stem ] ] in do
  { build_part_at_m True part_kind stem prati root 
  ; build_part_at_n part_kind stem prati root 
  ; build_part_ii part_kind stemf prati root 
  }
and build_part_vas c stem inter stemf root = 
    let prati = fix stem (if inter then "ivas" else "vas") 
    and verbal = (c,Ppfta) in do 
  { build_mas_ppfa verbal stem inter prati root (* (i)vas *)
  ; build_neu_ppfa verbal stem inter prati root (* (i)vas *)
  ; if (root="d.rz#1" || root="vid#2" || root="viz#1") && c=Primary
    then build_late_ppfa verbal stem prati root (* i supplement Whitney§805b *)
    else ()
  ; build_part_ii verbal stemf prati root (* u.sii *)
  }
;
(* Participle stems are stored here by calls in Verbs to [record_part] below; *)
(* this is necessary for the conjugation cgi to display participle stems *)
(* That is, the internal morphology generation is done in a first pass
   generating kridanta stems. The stems are declined in a second pass,
   reading from the participles list. This data structure holds just the
   lemmas of kridanta stems corresponding to one root. Then [compute_participles]
   invoked from [Verbs.compute_conjugs] declines the stems to fill in the 
   morphology data banks for each root. This mechanism is also used by the
   conjugation engine, in order to display the kridanta stems associated to
   the argument root. Thus [participles] is always a short list just used as
   a stack and not searched, so no need of sophisticated data structure. 
   Of course it is bigger at generation time. *)

value participles = ref ([] : list memo_part)
and participles_aa = ref ([] : list memo_part)
(* the second case is to provide extra phantom forms for kridantas of roots
   accepting aa- as a preverb, in order to recognize eg meghairaacchannaa.h *)
;
value record_part memo = (* called from Verbs *)
  (* This different treatment in full generation and in single evocation
     by Conjugation between aa-prefixable roots and others is terrible *)
  (* The structure of [Conj_infos] is wrong and should be a pair of
     all admissible preverbs and of an a-list of conjugation patterns *)
  if morpho_gen.val then (* generation of forms: phantom phonemes matter *)
     if admits_aa.val then (* ugly distinction *)
          participles_aa.val := List2.union1 memo participles_aa.val
     else participles.val := List2.union1 memo participles.val
  else participles.val := List2.union1 memo participles.val (* Conjugation *)
;
(* Called by [compute_participles] *)
value build_part = fun
  [ Ppp_ c stem root -> match stem with 
    [ [ 1 :: r ] -> build_part_a (c,Ppp) r root 
    | _ -> failwith ("Weird Ppp: " ^ Canon.rdecode stem)
    ]
  | Pfutp_ c stem root -> (* k below ought to be carried by [Pfutp_] *)
    match stem with 
    [ [ 1 :: r ] -> 
      let k = match r with
              [ [ 42 :: [ 45 :: [ 1 :: [ 32 :: _ ] ] ] ] -> 3 (* -tavya *)
              | [ 42 :: [ 4 :: _ ] ] -> 2 (* -aniiya *)
              | [ 42 :: _ ] -> 1 (* -ya *) (* ambiguité possible avec -iiya ? *)
              | _ -> failwith ("Weird Pfp: " ^ Canon.rdecode stem)
              ] in
      build_part_a (c,Pfutp k) r root           
    | _ -> failwith ("Weird Pfp: " ^ Canon.rdecode stem)
    ]    
  | Pppa_ c ppstem root -> 
      let m_stem = [ 45 :: ppstem ] (* pp-v *) in
      let f_stem =  rfix m_stem "at" (* vatii *) in  
      build_part_vat (c,Pppa) m_stem f_stem root 
  | Ppra_ k c m_stem f_stem root -> 
      if redundant_gana k root then ()
      else build_part_at (c,Ppra k) m_stem f_stem root 
  | Pprared_ c stem root -> 
      let k = if c = Intensive then int_gana else 3 in
      let f_stem = rfix stem "at" (* atii *) in 
      build_part_at_red (c,Ppra k) stem f_stem root
  | Pprm_ k c stem root -> build_part_a (c,Pprm k) stem root
  | Pprp_ c stem root -> build_part_a (c,Pprp) stem root
  | Ppfta_ c stem root -> 
      let inter = if monosyllabic stem then (* intercalating i *)
                     if root="vid#1" then False
                        (* [vid#1] stem=vid [vid#2] stem=vivid *)
                     else True 
                  else if root="likh" then True (* source ? *)
                  else False
      and f_stem = rfix stem "u.s" in 
      build_part_vas c stem inter f_stem root
  | Ppftm_ c stem root -> build_part_a (c,Ppftm) stem root
  | Pfuta_ c stem root -> 
      let f_stem = rfix stem "ant" (* antii *) in
      build_part_at (c,Pfuta) stem f_stem root
  | Pfutm_ c stem root -> build_part_a (c,Pfutm) stem root
  ]
;
(* Called by [Verbs.compute_conjugs], in order to create [Install.parts_file] 
   globally for all roots by [Make_roots.make_roots]. It is also invoked
   by [Conjugation.look_up_and_display] through [Verbs.fake_compute_conjugs]. *)
value compute_participles () = do
  { List.iter build_part participles.val
  (* Now for roots admitting preverb aa *)
  ; admits_aa.val := True (* triggering phantom generation - ugly global *)
  ; List.iter build_part participles_aa.val
  }
;

(*i end; i*)
