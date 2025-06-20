(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* module Chapter = struct *)

(* This module ensures that each individual HTML page of the DICO site
   is not too big, by slicing them into small chapters determined by prefixes
   of the vocables they define. *)

type chapters = list Word.word (* chapter boundaries *)
;

(* The chapter mechanism - slicing Dico into moderate size html pages *)
value (dico_chapters : chapters) = List.map Encode.code_string
  (* "a"  in  1.html *)
  [ "ad"   (* 2.html *)
  ; "anu"  (* 3.html *)
  ; "ap"   (* 4.html *)
  ; "abh"  (* 5.html *)
  ; "ar"   (* 6.html *)
  ; "av"   (* 7.html *)
  ; "ast"  (* 8.html *)
  ; "aa"   (* 9.html *)
  ; "aam"  (* 10.html *)
  ; "i"    (* 11.html *)
  ; "ii"   (* 12.html *)
  ; "u"    (* 13.html *)
  ; "ut"   (* 14.html *)
  ; "up"   (* 15.html *)
  ; "u.s"  (* 16.html *)
  ; ".r"   (* 17.html *)
  ; "k"    (* 18.html *)
  ; "kan"  (* 19.html *)
  ; "kaa"  (* 20.html *)
  ; "kaay" (* 21.html *)
  ; "k.r"  (* 22.html *)
  ; "k.s"  (* 23.html *)
  ; "g"    (* 24.html *)
  ; "g.r"  (* 25.html *)
  ; "c"    (* 26.html *)
  ; "j"    (* 27.html *)
  ; "jh"   (* 28.html *)
  ; "taa"  (* 29.html *)
  ; "t.r"  (* 30.html *)
  ; "d"    (* 31.html *)
  ; "di"   (* 32.html *)
  ; "dev"  (* 33.html *)
  ; "dh"   (* 34.html *)
  ; "naa"  (* 35.html *)
  ; "ni"   (* 36.html *)
  ; "nii"  (* 37.html *)
  ; "p"    (* 38.html *)
  ; "par"  (* 39.html *)
  ; "paa"  (* 40.html *)
  ; "pi"   (* 41.html *)
  ; "po"   (* 42.html *)
  ; "prat" (* 43.html *)
  ; "prab" (* 44.html *)
  ; "praa" (* 45.html *)
  ; "bal"  (* 46.html *)
  ; "bh"   (* 47.html *)
  ; "bhe"  (* 48.html *)
  ; "man"  (* 49.html *)
  ; "mar"  (* 50.html *)
  ; "mi"   (* 51.html *)
  ; "muu"  (* 52.html *)
  ; "y"    (* 53.html *)
  ; "r"    (* 54.html *)
  ; "ro"   (* 55.html *)
  ; "lam"  (* 56.html *)
  ; "v"    (* 57.html *)
  ; "vaa"  (* 58.html *)
  ; "vi"   (* 59.html *)
  ; "vip"  (* 60.html *)
  ; "vi.s" (* 61.html *)
  ; "v.r"  (* 62.html *)
  ; "z"    (* 63.html *)
  ; "zu"   (* 64.html *)
  ; ".s"   (* 65.html *)
  ; "s"    (* 66.html *)
  ; "san"  (* 67.html *)
  ; "sap"  (* 68.html *)
  ; "sar"  (* 69.html *)
  ; "sii"  (* 70.html *)
  ; "sur"  (* 71.html *)
  ; "sn"   (* 72.html *)
  ; "h"    (* 73.html *)
  ]
;
value (mw_chapters : chapters) = List.map Encode.code_string
  [ "agni"  (* 2.html *)
  ; "acira"  (* 3.html *)
  ; "atikandaka"  (* 4.html *)
  ; "adeya"  (* 5.html *)
  ; "adhyaavap"  (* 6.html *)
  ; "anaarambha.na"  (* 7.html *)
  ; "anunii"  (* 8.html *)
  ; "anu.sa.n.da"  (* 9.html *)
  ; "anti"  (* 10.html *)
  ; "apatrap"  (* 11.html *)
  ; "apaas"  (* 12.html *)
  ; "abuddha"  (* 13.html *)
  ; "abhiprastu"  (* 14.html *)
  ; "abhisa.mnam"  (* 15.html *)
  ; "abhra"  (* 16.html *)
  ; "ambhi.nii"  (* 17.html *)
  ; "aruza"  (* 18.html *)
  ; "arvaac"  (* 19.html *)
  ; "avatap"  (* 20.html *)
  ; "avas.rj"  (* 21.html *)
  ; "avo.sa"  (* 22.html *)
  ; "azvanta"  (* 23.html *)
  ; "asukha"  (* 24.html *)
  ; "ahe"  (* 25.html *)
  ; "aa"  (* 26.html *)
  ; "aacchid"  (* 27.html *)
  ; "aaditeya"  (* 28.html *)
  ; "aapaali"  (* 29.html *)
  ; "aara.t.ta"  (* 30.html *)
  ; "aav.r"  (* 31.html *)
  ; "aahitu.n.dika"  (* 32.html *)
  ; "i"  (* 33.html *)
  ; "i.s"  (* 34.html *)
  ; "ii"  (* 35.html *)
  ; "u"  (* 36.html *)
  ; "uttama"  (* 37.html *)
  ; "utpat"  (* 38.html *)
  ; "udak"  (* 39.html *)
  ; "udyam"  (* 40.html *)
  ; "upajan"  (* 41.html *)
  ; "uparuc"  (* 42.html *)
  ; "upaacar"  (* 43.html *)
  ; "ulkaa"  (* 44.html *)
  ; "uu"  (* 45.html *)
  ; ".r"  (* 46.html *)
  ; ".rr"  (* 47.html *)
  ; ".l"  (* 48.html *)
  ; ".lr"  (* 49.html *)
  ; "e"  (* 50.html *)
  ; "et.r"  (* 51.html *)
  ; "ai"  (* 52.html *)
  ; "o"  (* 53.html *)
  ; "au"  (* 54.html *)
  ; "k"  (* 55.html *)
  ; "ka.n.th"  (* 56.html *)
  ; "kapi"  (* 57.html *)
  ; "karakaayu"  (* 58.html *)
  ; "karma.sa"  (* 59.html *)
  ; "kazcana"  (* 60.html *)
  ; "kaaniita"  (* 61.html *)
  ; "kaartsna"  (* 62.html *)
  ; "kaaz"  (* 63.html *)
  ; "kiim"  (* 64.html *)
  ; "ku.na"  (* 65.html *)
  ; "kuyoga"  (* 66.html *)
  ; "kuu.t"  (* 67.html *)
  ; "k.rp"  (* 68.html *)
  ; "kela"  (* 69.html *)
  ; "ko.s.na"  (* 70.html *)
  ; "kra.s.tavya"  (* 71.html *)
  ; "k.santavya"  (* 72.html *)
  ; "k.sud"  (* 73.html *)
  ; "kh"  (* 74.html *)
  ; "khav"  (* 75.html *)
  ; "g"  (* 76.html *)
  ; "gandharva"  (* 77.html *)
  ; "gav"  (* 78.html *)
  ; "giita"  (* 79.html *)
  ; "guh"  (* 80.html *)
  ; "go"  (* 81.html *)
  ; "godha"  (* 82.html *)
  ; "graama"  (* 83.html *)
  ; "gh"  (* 84.html *)
  ; "f"  (* 85.html *)
  ; "c"  (* 86.html *)
  ; "catas.r"  (* 87.html *)
  ; "candhana"  (* 88.html *)
  ; "caara"  (* 89.html *)
  ; "citka.nakantha"  (* 90.html *)
  ; "caitra"  (* 91.html *)
  ; "ch"  (* 92.html *)
  ; "j"  (* 93.html *)
  ; "jam"  (* 94.html *)
  ; "jala.daa"  (* 95.html *)
  ; "jina"  (* 96.html *)
  ; "j~naa"  (* 97.html *)
  ; "jh"  (* 98.html *)
  ; "~n"  (* 99.html *)
  ; ".t"  (* 100.html *)
  ; ".th"  (* 101.html *)
  ; ".d"  (* 102.html *)
  ; ".dh"  (* 103.html *)
  ; ".n"  (* 104.html *)
  ; "t"  (* 105.html *)
  ; "tanaka"  (* 106.html *)
  ; "tavas"  (* 107.html *)
  ; "taavac"  (* 108.html *)
  ; "tuk"  (* 109.html *)
  ; "t.r.naafku"  (* 110.html *)
  ; "tri"  (* 111.html *)
  ; "trifkh"  (* 112.html *)
  ; "th"  (* 113.html *)
  ; "d"  (* 114.html *)
  ; "dandaza"  (* 115.html *)
  ; "dahara"  (* 116.html *)
  ; "dina"  (* 117.html *)
  ; "diirgha"  (* 118.html *)
  ; "dur"  (* 119.html *)
  ; "durdhar.sa"  (* 120.html *)
  ; "duraaka"  (* 121.html *)
  ; "devajana"  (* 122.html *)
  ; "deva.ta"  (* 123.html *)
  ; "dyuka"  (* 124.html *)
  ; "dvaa.mdvika"  (* 125.html *)
  ; "dvai"  (* 126.html *)
  ; "dh"  (* 127.html *)
  ; "dhari.ni"  (* 128.html *)
  ; "dharka.ta"  (* 129.html *)
  ; "dhuu"  (* 130.html *)
  ; "dhva~nj"  (* 131.html *)
  ; "n"  (* 132.html *)
  ; "nad"  (* 133.html *)
  ; "narda.taka"  (* 134.html *)
  ; "naagammaa"  (* 135.html *)
  ; "naarifga"  (* 136.html *)
  ; "ni.h"  (* 137.html *)
  ; "niryuktika"  (* 138.html *)
  ; "niguh"  (* 139.html *)
  ; "nimitta"  (* 140.html *)
  ; "niryat"  (* 141.html *)
  ; "ni.skira"  (* 142.html *)
  ; "niilafgu"  (* 143.html *)
  ; "naivaki"  (* 144.html *)
  ; "p"  (* 145.html *)
  ; "pa~nc"  (* 146.html *)
  ; "pa.t"  (* 147.html *)
  ; "pad"  (* 148.html *)
  ; "payora"  (* 149.html *)
  ; "paraacar"  (* 150.html *)
  ; "paridih"  (* 151.html *)
  ; "parividhaav"  (* 152.html *)
  ; "par.n"  (* 153.html *)
  ; "pavaru"  (* 154.html *)
  ; "paa.daliipura"  (* 155.html *)
  ; "paapacaka"  (* 156.html *)
  ; "paava.s.turikeya"  (* 157.html *)
  ; "pipi.svat"  (* 158.html *)
  ; "pu.n.dariika"  (* 159.html *)
  ; "pura~njara"  (* 160.html *)
  ; "pu.skaletra"  (* 161.html *)
  ; "puul"  (* 162.html *)
  ; "painya"  (* 163.html *)
  ; "prak.rrt"  (* 164.html *)
  ; "pra.nij"  (* 165.html *)
  ; "pratika"  (* 166.html *)
  ; "prativid"  (* 167.html *)
  ; "pratyabhiprasthaa"  (* 168.html *)
  ; "pradhuu"  (* 169.html *)
  ; "pramii"  (* 170.html *)
  ; "pravical"  (* 171.html *)
  ; "prasah"  (* 172.html *)
  ; "praa.mzu"  (* 173.html *)
  ; "praatikaa"  (* 174.html *)
  ; "priitu"  (* 175.html *)
  ; "ph"  (* 176.html *)
  ; "b"  (* 177.html *)
  ; "balaasa"  (* 178.html *)
  ; "bahiinara"  (* 179.html *)
  ; "bid"  (* 180.html *)
  ; "b.rh"  (* 181.html *)
  ; "brahman"  (* 182.html *)
  ; "braadhnaayanya"  (* 183.html *)
  ; "bh"  (* 184.html *)
  ; "bhand"  (* 185.html *)
  ; "bhaziraa"  (* 186.html *)
  ; "bhaava"  (* 187.html *)
  ; "bhiilabhuu.sa.naa"  (* 188.html *)
  ; "bhuu"  (* 189.html *)
  ; "bhuu.hkhaara"  (* 190.html *)
  ; "bhraj"  (* 191.html *)
  ; "m"  (* 192.html *)
  ; "ma.nittha"  (* 193.html *)
  ; "madhu"  (* 194.html *)
  ; "madhva"  (* 195.html *)
  ; "manauu"  (* 196.html *)
  ; "marb"  (* 197.html *)
  ; "mah"  (* 198.html *)
  ; "mahaaprabhaava"  (* 199.html *)
  ; "mahaazairii.sa"  (* 200.html *)
  ; "maa.msp.r.s.ta"  (* 201.html *)
  ; "maanava"  (* 202.html *)
  ; "maas"  (* 203.html *)
  ; "muku.ta"  (* 204.html *)
  ; "mummuni"  (* 205.html *)
  ; "m.r"  (* 206.html *)
  ; "m.r.saalaka"  (* 207.html *)
  ; "moci"  (* 208.html *)
  ; "y"  (* 209.html *)
  ; "yata"  (* 210.html *)
  ; "yam"  (* 211.html *)
  ; "yaak.rtka"  (* 212.html *)
  ; "yuvan"  (* 213.html *)
  ; "r"  (* 214.html *)
  ; "ra.t"  (* 215.html *)
  ; "ram"  (* 216.html *)
  ; "rasna"  (* 217.html *)
  ; "raajakineya"  (* 218.html *)
  ; "raayaana"  (* 219.html *)
  ; "ruddha"  (* 220.html *)
  ; "ro.nii"  (* 221.html *)
  ; "l"  (* 222.html *)
  ; "lataa"  (* 223.html *)
  ; "laalii"  (* 224.html *)
  ; "lok"  (* 225.html *)
  ; "v"  (* 226.html *)
  ; "va~ncati"  (* 227.html *)
  ; "vanara"  (* 228.html *)
  ; "varola"  (* 229.html *)
  ; "valbh"  (* 230.html *)
  ; "vask"  (* 231.html *)
  ; "vaaca"  (* 232.html *)
  ; "vaayu"  (* 233.html *)
  ; "vaalguda"  (* 234.html *)
  ; "vi"  (* 235.html *)
  ; "vi.mza"  (* 236.html *)
  ; "vicitra"  (* 237.html *)
  ; "vid"  (* 238.html *)
  ; "vidhaav"  (* 239.html *)
  ; "vipadumaka"  (* 240.html *)
  ; "vimala"  (* 241.html *)
  ; "vilinaatha"  (* 242.html *)
  ; "vizii"  (* 243.html *)
  ; "vizvi"  (* 244.html *)
  ; "vi.sayaka"  (* 245.html *)
  ; "vi.spanda"  (* 246.html *)
  ; "viir"  (* 247.html *)
  ; "v.rddha"  (* 248.html *)
  ; "ve.n.tha"  (* 249.html *)
  ; "veza"  (* 250.html *)
  ; "vaimaatra"  (* 251.html *)
  ; "vya~nj"  (* 252.html *)
  ; "vyah"  (* 253.html *)
  ; "vy.r"  (* 254.html *)
  ; "z"  (* 255.html *)
  ; "zata"  (* 256.html *)
  ; "zabd"  (* 257.html *)
  ; "zaraketu"  (* 258.html *)
  ; "zazamaana"  (* 259.html *)
  ; "zaa.mtanava"  (* 260.html *)
  ; "zaaha"  (* 261.html *)
  ; "zivaga.na"  (* 262.html *)
  ; "ziita"  (* 263.html *)
  ; "zu.n.d"  (* 264.html *)
  ; "zuurta"  (* 265.html *)
  ; "zai.siri"  (* 266.html *)
  ; "zyai"  (* 267.html *)
  ; "zraama"  (* 268.html *)
  ; "zriikajaaka"  (* 269.html *)
  ; "zvabhr"  (* 270.html *)
  ; ".s"  (* 271.html *)
  ; "s"  (* 272.html *)
  ; "sa.mzu.s"  (* 273.html *)
  ; "sa.msthaa"  (* 274.html *)
  ; "sakalakala"  (* 275.html *)
  ; "sa.mgha.t"  (* 276.html *)
  ; "satii"  (* 277.html *)
  ; "satak.san"  (* 278.html *)
  ; "sa.mtap"  (* 279.html *)
  ; "sapak.sa"  (* 280.html *)
  ; "sabhaaj"  (* 281.html *)
  ; "samave"  (* 282.html *)
  ; "samifg"  (* 283.html *)
  ; "sam.r"  (* 284.html *)
  ; "samphe.ta"  (* 285.html *)
  ; "saragh"  (* 286.html *)
  ; "sarva"  (* 287.html *)
  ; "sarvasuuk.sma"  (* 288.html *)
  ; "sazakala"  (* 289.html *)
  ; "sahama"  (* 290.html *)
  ; "saa.mjiiviiputra"  (* 291.html *)
  ; "saamanii"  (* 292.html *)
  ; "saar.sapa"  (* 293.html *)
  ; "sidgu.n.da"  (* 294.html *)
  ; "siila"  (* 295.html *)
  ; "sucakra"  (* 296.html *)
  ; "sund"  (* 297.html *)
  ; "suma"  (* 298.html *)
  ; "sur"  (* 299.html *)
  ; "su.sa.msad"  (* 300.html *)
  ; "suutr"  (* 301.html *)
  ; "setu"  (* 302.html *)
  ; "sodara"  (* 303.html *)
  ; "sora"  (* 304.html *)
  ; "skandha"  (* 305.html *)
  ; "stha"  (* 306.html *)
  ; "snaayu"  (* 307.html *)
  ; "sm.rta"  (* 308.html *)
  ; "svasvadha"  (* 309.html *)
  ; "svanuruupa"  (* 310.html *)
  ; "svaakta"  (* 311.html *)
  ; "h"  (* 312.html *)
  ; "hari"  (* 313.html *)
  ; "hala"  (* 314.html *)
  ; "hi.ms"  (* 315.html *)
  ; "huu"  (* 316.html *)
  ; "ho.dha"  (* 317.html *)
  ]
;

(*i TODO - put chapters in a heap for linear access time i*)
value look_up_chap w n = 
(*  let v = match w with [ [ 0 (* - *) :: stem ] -> stem | _ -> w ] in *)
  look_up n 
  where rec look_up n = fun
  [ [] -> n 
  | [ frontier :: l ] -> if Order.lexico frontier w then look_up (n+1) l else n
  ]
;
(* Enter in this table associations between a defined form and its defining 
   entry, whenever there is a chapter boundary in between. 
   In a future version this table ought to be mechanically built. *)
value vocable s = 
  let entry = fun 
    [ "Dyaus" -> "div"
    | s -> s
    ] in 
  Encode.code_skt_ref (entry s)
;
value dico_chapter s = (* defining chapter of Sanskrit word form [s] *)
  let lower = fun 
    [ [ 0 :: w ] -> w (* remove initial hyphen of suffixes *)
    | [ c :: w ] -> [ (if c>100 then c-100 else c) :: w ] (* remove capital *)
    | [] -> []
    ] in 
  let defining_word = lower (vocable s) in
  look_up_chap defining_word 1 dico_chapters
;
value cypher = string_of_int (* no cyphering so far *)
;
value dico_page chapter = (* each chapter in its own page *)
  cypher chapter ^ ".html"
;
(* Used in [Morpho_html] *)
value sh_defining_page s = dico_page (dico_chapter s)
;
value mw_defining_page_exc s mw_exceptions =
  let exc_page = Deco.assoc (Encode.rev_code_string s) mw_exceptions in
  let file_name = match exc_page with
    [ [] -> let initial = fun
       [ [ 0 :: w ] -> w (* remove initial hyphen of suffixes *)
       | [ c :: w ] -> [ (if c>100 then c-100 else c) :: w ] (* remove capital *)
       | [] -> []
       ] in let defining_word = initial (vocable s) in
            look_up_chap defining_word 1 mw_chapters
    | [ n ] -> n
    | _ -> failwith "mw_defining_page"
    ] in
  (cypher file_name) ^ ".html"
;

(* end; *)
