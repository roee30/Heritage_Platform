(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Pada defines the allowed padas (Para, Atma or Ubha) for
   a given combination of root, gana, and upasarga *)

(* It is used at conjugation computation time by Verbs, in order to generate 
   root forms for attested lexicalizations of root and gana (over all
   possible upasarga usages) and at segmentation time, to filter out
   by Dispatcher the non attested combinations of gana, pada and upasarga *)

(*i module Pada = struct i*)

type voices = (* permitted padas in present system *)
  (* NB. These are distinctions within the active voice, as opposed to passive 
     ("karma.ni prayoga"). Atma is called "middle" by Western grammarians. *)
  [ Para (* parasmaipadin usage only - generated as Dictionary.Active *) 
  | Atma (* aatmanepadin  usage only - generated as Dictionary.Middle *) 
  | Ubha (* ubhayapada admits both schemes - default *)
  ]
;
exception Unattested (* when a root/pada is attested only for some pvs *)
;
value voices_of = fun 
  (* Default: invariant when prefixing by preverbs *)
  [ "ak.s" | "afg" | "aj" | "a.t" | "at" | "ad#1" | "an#2" | "am"| "argh" 
  | "ard" | "av" | "az#2" | "as#1" | "as#2" | "ifg" | "in" | "ind" 
  | "inv" | "il" | "i.s#2" | "iifkh" | "iir.s" | "uk.s" | "ujjh" | "u~nch"
  | "und" | "umbh" | "u.s" | ".rc#1" | ".rdh" | ".r.s" | "ej" | "kas" | "kiirt"
  | "kiil" | "ku.t" | "ku.t.t" | "ku.n.th" | "kunth" | "kup" | "kul" | "ku.s"
  | "kuuj" | "k.rt#1" | "k.rz" | "krand" | "kru~nc#1" | "kruz" | "klam" 
  | "klid" | "kliz" | "kvath" | "k.sar" | "k.sal" | "k.saa" | "k.si" | "k.sii"
  | "k.su" | "k.sudh#1" | "k.svi.d" | "kha~nj#1" | "khaad" | "khid"
  | "khel" | "khyaa" | "gaj" | "gad" | "garj" | "gard" | "gal" | "gaa#1" 
  | "gaa#2" | "gu~nj" | "gu.n.th" | "gup" | "gumph" | "g.rdh" 
  | "g.rr#2" | "granth" | "glai" | "ghas" | "ghu.s" | "gh.r" | "gh.r.s" 
  | "ghraa" | "cakaas" | "ca.t" | "cand" | "cam" | "car" | "cal" | "cit#1" 
  | "cumb" | "c.rt" | "chur" | "ch.rd" | "jak.s" | "jap" | "jabh#2" | "jam" 
  | "jalp" | "jas" | "jaag.r" | "jinv" | "jiiv" | "jvar" | "jval" | "tak" 
  | "tak.s" | "ta~nc" | "tam" | "tarj" | "tup" | "tu.s" | "t.rp#1" | "t.r.s#1"
  | "t.rh" | "t.rr" | "tyaj#1" | "tras" | "tru.t" | "tvak.s" | "tsar" | "da.mz"
  | "dagh" | "dabh" | "dam#1" | "dal" | "das" | "dah#1" | "daa#2" | "daa#3" 
  | "du" | "du.s" | "d.rp" | "d.rbh" | "d.rz#1" | "d.rh" | "d.rr" 
  | "dhyaa" | "draa#1" | "dru#1" | "druh#1" | "dham" | "dhaa#2" | "dhru"
  | "dhvan" | "dhv.r" | "na.t" | "nad" | "nand" | "nard" | "naz#1" 
  | "nind" | "nu#1" | "n.rt" | "pa.t" | "pat#1" | "path" | "paa#1" | "paa#2" 
  | "pii" | "piz#1" | "pi.s" | "pu.t" | "p.r#1" | "p.r#2" | "p.r.s" | "p.rr" 
  | "praa#1" | "phal" | "bal" | "b.rh#1" | "b.rh#2" | "bha~nj" | "bha.n" 
  | "bha.s" | "bhas" | "bhaa#1" | "bhii#1" | "bhuj#1" | "bhuu#1" | "bhuu.s" 
  | "bhram" | "majj" | "ma.n.d" | "mad#1" | "manth" | "mah" | "maa#3" | "mi.s" 
  | "mih" | "miil" | "mu.s#1" | "muh" | "muurch" | "m.r.d" | "mnaa"
  | "mre.d" | "mlaa" | "mluc" | "mlecch" | "yabh" | "yam" | "yas" | "yaa#1" 
  | "yu#2" | "ra.mh" | "rak.s" | "ra.n" | "rad" | "radh" | "raa#1" | "raadh"
  | "ri.s" | "ru" | "ruj#1" | "rudh#1" | "ru.s#1" | "ruh#1" | "lag" | "lafg" 
  | "lap" | "lal" | "las" | "laa" | "laa~nch" | "likh" | "liz" | "lu.n.th" 
  | "lubh" | "lul" | "vak.s" | "vac" | "vaj" | "va~nc" | "vam" | "var.n"
  | "valg" | "vaz" | "vas#1" | "vaa#2" | "vas#4" | "vaa~nch" | "vid#1" | "vidh#1"
  | "vi.s#1" | "vii#1" | "v.rj" | "v.r.s" | "v.rh" | "ven" | "vyac"| "vyadh"
  | "vraj" | "vrazc" | "za.ms" | "zak" | "zam#1" | "zam#2" | "zal" | "zaz" 
  | "zas" | "zaas" | "zi.s" | "ziil" | "zuc#1" | "zudh" | "zumbh" | "zu.s"
  | "zuu" | "z.rr" | "zcut#1" | "zraa" | "zli.s" | ".s.thiiv" 
  | "sad#1" | "sap#1" | "saa#1" | "sidh#1" | "sidh#2" | "siiv" 
  | "sur" | "s.r" | "s.rj#1" | "s.rp" | "skand" | "skhal" | "stan" | "stubh"
  | "sthag" | "snaa" | "snih#1" | "snu" | "snuh#1" | "sp.r" | "sphal" | "sphu.t"
  | "sphur" | "sm.r" | "svan" | "svap"  | "svar#1" | "svar#2" | "ha.th" 
  | "haa#1" | "hi#2" | "hi.ms" | "h.r.s" | "hras" | "hrii#1" | "hval"
  | "karakaasaara" (* denominative of cpd resembling gana 1 - tentative *)
(*| "k.subh" needed for k.subhyate *)
(*| "k.s.nu" Atma needed for sa.mk.s.ute \Pan{1,3,65} *)
(*| "krii.d" Atma needed for aakrii.date \Pan{1,3,21} *)
(*| "g.rr1"  Atma needed for avag.rr sa.mg.rr \Pan{1,3,51-52} *)
(*| "ji"     Atma needed for eg vijayate paraajayate \Pan{1,3,19} *)
(*| "jyaa1"  Atma needed for jiiyate *)
(*| "kan"    Atma needed for kaayamaana *)
(*| "gam"    Atma needed for sa.mgacchate *)
(*| "grah"   Atma needed for g.rh.niite *)
(*| "diiv#1" Atma needed for diivyate *)
(*| "van"    Atma needed for vanute *)
(*| "mah"    Atma needed for pft. maamahe *) 
(*| "m.rz"   Atma needed for pft vivam.rze Mah{18,2,42} *)
(*| "cit#1"  Atma needed for pft. cikite *)
(*| "viz1"   Atma needed for nivizate \Pan{1,3,17} *)
(*| "zram"   Atma needed for vizramate *)
(*| "kaafk.s" | "han1" occur also in Atma in BhG: kaafk.se hani.sye *)
(*| "has"  Atma needed for hasate *)
(*| "uc"   Atma needed for uuce *)
(*| "van"  Atma needed for vanute *)
(*| "zu.s" Atma for zu.syate WR epic *)
(*| "zvas#1" Atma for zvasate WR *)
(*| "nam" Atma namate WR epic *)
(*| "aap" Atma aapnute epic *)
(*| "a~nj" also Atma afkte | "naath" "praz" "sp.rz#1" idem *)
(*| Doubts: "bhuu1" could also be Atma bhavate *)
(*| "zru" also Atma in Vedic eg z.r.nu.sva & SP. r.r.nudhvam *)
(*| "yu#2" could be Atma in Maitreya S. yuyudhvam *)
(*| "krudh#1" krudhyate pm or ps or both ? *)
      -> Para (* active only *) 
  | "az#1" | "aas#2" | "indh" | "iik.s" | "ii.d" | "iir" | "iiz#1" | "ii.s" 
  | "iih"  | "edh" | "ka~nc" | "katth" | "kam" | "kamp" | "kaaz" | "kaas#1"
  | "kuu" | "k.rp" | "k.lp" (* but Henry: {cak.lpur} "ils s'arrangèrent" *)
  | "knuu" | "klav" | "k.sad" | "galbh" | "gur" | "glah" 
  | "gha.t" | "jabh#1" | "ju.s#1" | "j.rmbh" | ".damb" | ".dii" | ".dhauk" 
  | "tandr" | "tij" | "trap" | "trai" | "tvar" | "dak.s" | "day" | "diik.s" 
  | "d.r#1" | "dhii#1" | "dhuk.s" | "pa.n" | "pad#1" | "pi~nj" 
  | "pyaa" | "prath" | "pru" | "plu" | "ba.mh" | "baadh" | "bha.n.d" | "bhand" 
  | "bhaam" | "bhaa.s" | "bhraaj" | "ma.mh" | "man" | "mand#1" | "yat#1" 
  | "rabh" | "ruc#1" | "lajj" | "lamb" | "lii" | "loc" | "vand" 
  | "vas#2" | "vaaz" | "vip" | "ve.s.t" | "vrii.d" | "zafk" | "zad"
  | "zi~nj" | "zii#1" | "zrambh" | "zlaagh" | "zvit#1" | "sac" | "sev"
  | "styaa" | "spand" | "spardh" | "spaz#1" | "sphaa" | "sra.ms"
  | "sva~nj" | "haa#2" | "hu.n.d" | "h.r#2" | "hnu" | "hraad" | "hlaad" 
(*| "m.r" Ubha needed for non present tenses - see \Pan{1,3,61} for exact rule *)
(*| "smi" Ubha needed for smitavat *)
(*| "bhuj2" Ubha needed for bhunakti to govern *)
(*| "gaah" Ubha needed for gaahet epics *)
(*| "k.sam" Ubha needed for k.samati epics *)
(*| "yudh1" Ubha needed for yudhya BhG *)
(*| "diip" Ubha needed for epics *)
(*| "labh" labhati, laabhet occurs in epics *)
(*| "v.rdh#1" v.rdhati needed for Vedic *)
   (* DRP restriction: "dyut1" *) 
      -> Atma (* "deponent" verbs: middle only *)
  | _ -> Ubha (* default *) 
  (* Attested Ubha (over all ga.nas) : 
  [ "a~nc" | "arth" | "arh" | "aap" | "i" | "i.s#1" | "uc" | "uurj#1" | "uuh" 
  | ".r" | ".rj" | "ka.n.d" | "kal" | "ka.s" | "ku.n.d" | "k.r#1"
  | "k.r#2" | "k.r.s" | "kram" | "krii#1" | "krii.d" | "krudh#1" | "k.san" 
  | "k.sap#1" | "k.sam" | "k.sal" | "k.sip" | "k.sud" | "k.subh" | "k.s.nu"
  | "khan" | "gam" | "garh" | "gaah" | "guh" | "g.rr#1" | "gras" | "grah" 
  | "gha.t.t" | "cat" | "carc" | "ci" | "cint" | "cud" | "ce.s.t" | "cyu" 
  | "chad#1" | "chand" | "chid#1" | "jan" | "juu" | "j~naa#1" | "jyaa#1" 
  | "jyut" | "ta.d" | "tan#1" | "tan#2" | "tap" | "tud#1" | "tul" | "t.rd" 
  | "traa" | "daa#1" | "daaz#1" | "diz#1" | "dih" | "diip" | "duh#1" | "dev#1"
  | "draa#2" | "dvi.s#1" | "dhaa#1" | "dhaav#1" | "dhaav#2" | "dhuu#1" | "dh.r"
  | "dhva.ms" | "nam" | "nah" | "naath" | "nij" | "nii#1" | "nud" | "pac" 
  | "paz" | "pa.th" | "pii.d" | "pu.s#1" | "puu#1" | "puuj" | "puuy" | "p.rth" 
  | "prii" | "pru.s#1" | "budh#1" | "bruu" | "bhak.s" | "bhaj" 
  | "bharts" | "bhaas#1" | "bhid#1" | "bhuj#2" | "bh.r" | "bh.rjj" 
  | "mantr" | "maa#4" | "maarg" | "mi" | "mith" | "mil" | "mii" 
  | "muc#1" | "mud#1" | "m.r" | "m.rj" | "m.rdh" | "m.rz" | "m.r.s" 
  | "yaj#1" | "yam" | "yaac" | "yu#1" | "yuj#1" | "yudh#1" | "rac" | "ra~nj" 
  | "ram" | "rah" | "raaj#1" | "ri" | "ric" | "rud#1" | "rudh#2" | "lafgh" 
  | "lak.s" | "labh" | "la.s" | "lip" | "lih#1" | "lup" | "luu#1" | "vad" 
  | "van" | "vap#1" | "vap#2" | "val" | "vah#1" | "vaa#3" | "vic" | "vij" 
  | "viz#1" | "viij" | "v.r#2" | "v.rt#1" | "v.rdh#1" | "vyath" | "vyaa" | "vrii"
  | "zap" | "zaa" | "zu.s" | "zubh#1" | "z.rdh" | "zyaa" | "zram" | "zri" | "zru"
  | "zvas#1" | "sru" | "san#1" | "sa~nj" | "sah#1" | "sic" | "su#2" | "suu#1"
  | "suud" | "stambh" | "stu" | "st.rr" | "sthaa#1" | "sp.rz#1" | "sp.rh" | "smi"
  | "syand" | "svad" | "had" | "hikk" | "hu" | "huu" | "h.r#1"
  ] *)
  (* + corr. "praz" | "k.rr" | "p.rc" | "bandh" *)
  (* NB. "ah" "rip" "vadh" have no pr, "mand2" is fictitious *) 
  (* "iiz1", "lii" and "knuu" Para allowed in future *)
  ]
;
(* List of roots that admit different padas for distinct ganas:
as2 1U 4P (* 4P Vedic - may overgenerate ? *)
i 1A 2P 4A 5P
.r 1U 3P 5P
kuc 1U 6P
k.r.s 1P 6U
ghuur.n 1A 6P
jan 4A 1U
j.rr 1U 4P
jyaa1 4A 9P
.damb 1A 10P (vi-)
tap 1U 4A
daa1 2P 1U 3U
draa2 2P 4U
dh.r.s 1U 5P
nij 2A 3U
pu.s1 4U 9P
budh1 1P 4A
bhra.mz 1A 4P
man 1U 4U 8A
maa1 3A 2P
mid 1A 4P 1OP
mii 9P 4A
m.r 4A other tenses P
m.rj 1U 2P 6U
m.rd1 9P 1U
ri 4A 9U
ric 4A 7P
rud1 2P 1U 6U
van 1P 8U
vid2 2A 6U 7A
v.r1 1P 5U
zaa 3U 4P
suu1 1P 6P 2A
stambh 1U 5P 9P
svid2 1A 4P
*) 
(* More precise selection for present system *)
(* NB This will drive generation of verbal forms by Verbs. It may generate forms
   not listed in the lexicon root entry, but needed for use with some preverbs, 
   indicated in [voices_of_pv] below. 
   Incorrect associations will be captured at Reader time by Dispatcher. *)
value voices_of_gana g root = match g with 
 [ 1 -> match root with
        [ "cur" | "budh#1" | "van" | "v.r#1" | "suu#1" 
            -> Para 
        | "gave.s" | "gha.t.t" | "ghuur.n" | ".damb" | "bhra.mz" | "mid"
        | "mok.s" | "lok" | "svid#2" 
            -> Atma
        | "i" | "i.s#1" | ".r" (* ".r" Atma for pv sam \Pan{1,3,29} also "tap" *)
        | "j.rr" | "tap" | "daa#1" | "dh.r.s" | "as#2" | "kuc" | "k.r.s" 
        | "m.rj" | "m.rd#1" | "rud#1" | "stambh" 
            -> Ubha
        | "kliiba" -> Atma (* denominative verb *)
        | _ -> voices_of root (* man U (epic P) *)
        ] 
 | 2 -> match root with
       [ "daa#1" | "dyaa" | "draa#2" | "maa#1" | "m.rj" | "rud#1" | "zvas#1"
            -> Para
        | "nij" | "vid#2" -> Atma
        | _ -> voices_of root 
        ]
 | 3 -> match root with
        [ ".r" -> Para
        | "maa#1" -> Atma
        | "daa#1" | "nij" -> Ubha
        | _ -> voices_of root
        ]
 | 4 -> match root with 
        [ "as#2" | "j.rr" | "bhra.mz" | "mid" | "zaa"
        | "svid#2" -> Para
        | "i" | "jan" | "jyaa#1" | "tap" | "draa#2" | "budh#1" | "mii" | "ri"
        | "ric" |  "m.r" | "vrii" -> Atma 
        | "pu.s#1" (* | "raadh" Bergaigne vedic *) -> Ubha
        | _ -> voices_of root
        ]
 | 5 -> match root with
        [ "i" | ".r" | "dh.r.s" | "raadh" | "stambh" -> Para
        | "v.r#1" | "su#2" -> Ubha
        | _ -> voices_of root
        ]
 | 6 -> match root with
        [ "kuc" | "ghuur.n" | "suu#1" -> Para
        | "k.r.s" | "m.rj" | "rud#1" | "vid#2" -> Ubha
        | _ -> voices_of root
        ]
 | 7 -> match root with
        [ "vid#2" -> Atma
        | _ -> voices_of root
        ]
 | 8 -> match root with
        [ "man" -> Atma
        | _ -> voices_of root 
        ]
 | 9 -> match root with
        [ "jyaa#1" | "pu.s#1" | "mii" | "m.rd#1" | "ri" | "vrii" | "stambh"
            -> Para
        | _ -> voices_of root
        ]
 | 10 -> match root with
        [ "gha.t.t" | ".damb" | "mid" | "mok.s" | "lok" 
            -> Para
        | _ -> voices_of root 
        ]
 | 11 -> match root with
        [ "agada" | "agha" | "afka" | "afkha" | "adhvara" | "andha" 
        | "aparok.sa" | "apahasta" | "amitra" | "azana#2" | "azva"
        | "aakar.na" | "aakula" | "aakhyaana" | "aandola" | "aavila" | "iras"
        | "i.sudhi" | "ukta" | "udaka" | "udan" | "udazru" | "unmuula" 
        | "upahasta" | "uras" | "u.sas" | "ka.thora" | "kadartha" | "kar.na"
        | "kart.r" | "kalafka" | "kavala" | "kavi" | "kusuma"
        | "kelaa" | "k.siira" | "kha.da" | "khela" | "gadgada" | "go" | "gocara"
        | "gopaa" | "dhiira#1" 
            -> Para
        | "ajira" | "apsaras" | "abhra" | "unmanas" | "upanimantra" | "uu.sman"
        | "ka.nva" | "karu.na" | "kalaha" | "ka.s.ta" | "kuha" | "k.rcchra"
        | "k.rpa.na" | "kliiba" | "k.sepa" | "gomaya" | "citra" | "priya" 
            -> Atma
        | _ -> voices_of root (* other denominatives will take Ubha as default *)
        ]
 | _ -> voices_of root (* in particular, for non-present forms, without gana *)
 ]
;

(* Refining with potential preverb *)
value voices_of_pv upasarga gana = fun 
   (* Beware: gana only used for "tap" "i" ".r" 
      but gana=0 for non-present forms *)
(* Paninian requirements *)
[ "gam" | "svar" | "vid#1" (* "zru" | "praz" *) -> 
             if upasarga = "sam" then Ubha else Para (* \Pan{1,3,29} *)
(* "praz" used in Atma with aa- but also without pv in epics (MW) *)
(* "zru" also Atma in Vedic eg z.r.nu.sva & SP. z.r.nudhvam *)
| ".r" | "car" ->   if upasarga = "sam" then Ubha else Para (* \Pan{1,3,54} *)
| "viz#1" -> if upasarga = "ni"  then Atma else Para (* \Pan{1,3,17} *)
| "k.s.nu" -> if upasarga = "sam"  then Atma else Para (* \Pan{1,3,65} *)
| "huu" -> match upasarga with
           [ "ni" | "sam" | "upa" | "vi" -> Atma (* \Pan{1,3,30} *)
           | "aa" -> Ubha (* \Pan{1,3,31} *)
           | _ -> Para
           ]
| "yam" -> match upasarga with
           [ "aa" | "upa" -> Ubha 
           | _ -> Para (* \Pan{1,3,28} and \Pan{1,3,56} *)
           ]
| "vah#1" -> if upasarga = "pra" then Para else Ubha (* \Pan{1,3,81} *)
(* Complex vad (* \Pan{1,3,47-50,73} *) depends on meaning - now all Ubha 
| "vad" -> match upasarga with 
           [ "" | "sam" -> Ubha (* \Pan{1,3,47,48} *)
           | "anu" -> Ubha (* \Pan{1,3,49} *)
           | "pra" | "vipra" -> Ubha (* \Pan{1,3,49} *)
           | "apa" -> Ubha (* \Pan{1,3,73} *)
           | _ -> Para
           ] *)
| "g.rr#1" -> match upasarga with
           [ "ava" -> Atma (* \Pan{1,3,51} *)
           | "sam" -> Ubha (* \Pan{1,3,52} *)
           | _ -> Para
           ]
| "ji" -> match upasarga with
          [ "vi" | "paraa" -> Atma (* \Pan{1,3,19} *)
          | _ ->  Ubha (* was Para but "satyam eva jayate" *) 
          ]
| "krii.d" -> match upasarga with
          [ "aa" | "anu" | "pari" -> Atma (* \Pan{1,3,21} *)
          | "sam" -> Ubha (* \Pan{1,3,21} vaartikaa *)
          | _ -> Para (* "" | "vi" *)
          ]
| "m.rz" -> if upasarga = "pari" then Para else Ubha (* \Pan{1,3,82} *)
| "tap" when gana = 1 -> match upasarga with
                         [ "ut" | "vi" -> Ubha 
                         | _ -> Para (* \Pan{1,3,27} *)
                         ]
| "i"  -> match gana with
          [ 2 |  0 -> match upasarga with
                      [ "adhi" | "anu" | "abhi" -> Ubha 
                      | _ -> Para 
                      ]
          | _ (* 1 | 4 *) -> match upasarga with
                      [ "" -> Atma
                      | "antar" | "ut" -> Para (* gana 1 antarayati udayati *) 
                      | _ -> raise Unattested
                      ]
          ]
| "zii#1" -> if upasarga = "sam" then Ubha else Atma 
| "zram" -> if upasarga = "vi" then Ubha (* epic vizramate *) else Para
| "k.r#1" -> match upasarga with
            [ "anu" | "paraa" -> Para (* \Pan{1,3,79} *)
            |  _ -> Ubha
            ]
| "krii#1" -> match upasarga with
            [ "vi" -> Ubha (* vikrii.naati/vikrii.niite *)
            | "pari" | "ava" -> Atma 
            | _ -> Para (* \Pan{1,3,18} *)
            ]
(* Next four equivalent to marking "unused" in lexicon *)
| "ta~nc" | "saa#1" | "zam#2" | "zal" (* | "khyaa" ? *) ->
   match upasarga with 
   [ "" -> raise Unattested (* thus braa.hmasya "Ô Brahmane, tue" unrecognized *)
   | _ -> Para 
   ]  
| "loc" | "zrambh" | "hnu" -> match upasarga with 
   [ "" -> raise Unattested
   | _ -> Atma
   ]  
| ".damb" -> match upasarga with 
   [ "vi" -> Ubha
   | _ -> raise Unattested
   ]  
(* Usage, MW, WR *)
| "gha.t.t" -> if gana = 1 then 
                  if upasarga = "" then raise Unattested
                  else Atma (* only "vi" | "sam", NOT "" *)
               else (* gana = 10 *) Para 
| "bruu" -> if gana = 6 then match upasarga with
          [ "" -> Ubha (* Epics see Oberlies *)
          | _ -> raise Unattested
          ] else Ubha (* regular gana = 2 *)
| "i.s#1" when gana = 1 -> match upasarga with
          [ "" -> raise Unattested
          | _ -> Ubha
          ]
| "mantr" -> match upasarga with 
   [ "" -> Ubha
   | "aa" | "ni" -> Atma
   | "anu" | "sam" -> Para
   | _ -> raise Unattested
   ]  
| "arth" -> match upasarga with 
   [ "pra" (* Para epics *) | "sam" (* Para rare *) -> Ubha
   | "" | "abhi" | "aa" | "ni" -> Atma
   | (* "anu" ? | *) "prati" -> Para
   | _ -> raise Unattested
   ]  
| "sthaa#1" -> match upasarga with 
   [ (* "ava" | "pra" | *) "vi" | "sam" -> Atma (* \Pan{1,3,22} *)
   | _ -> Ubha (* "" | "adhi" | "anu" | "abhi" | "ava" | "aa" | "ut" | "upa" 
                | "ni" | "prati" *)
   ]  
| "vac" -> match upasarga with 
   [ "" | "pra" -> Ubha (* BhG{8,11} pravak.sye *)
   | "adhi" | "anu" | "nis" | "prati" | "vi" -> Para
   | _ -> raise Unattested
   ]  
| root -> voices_of_gana gana root
]
;

(*i end; i*)


