(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Phonetics = struct i*)
(* Notation Pr{X} gives X as the pratyaahaara notation of a set of phonemes *)
(* Sanskrit phonology *)
(* Phoneme (var.na) is implemented as integer from 1 to 49, thus consistent with 
   ZEN toolkit, with Word.letter = int. Thus the Paninian structure of 
   pratyahaaras is replaced by simple arithmetic computation. *)
(* This representation is not sufficient to represent accent, and a more 
   sophisticated phonology scheme should be used for that purpose, taking 
   into account phonetic features, such as SLP1 of the Sanskrit Library. 
   See Lies of Scharf and Higman. *)
let vowel c = (c > 0) && (c < 14)
and (* a aa i ii u uu .r .rr .l e ai o au *) (* Pr(hac) *) anusvar c = c = 14
and (* .m : anusvara standing for nasal *) (*  || c=15  candrabindu *)
  visarga c = c = 16
and (* .h *) consonant c = c > 16
and (* Pr(hal) *) phantom c = c < (-1)
  
(* -2 -3=*a -4=*i -5=*u -6=*r *)
(* final s assimilated to visarga *)
let visarg c = (c = 48) || (* s *) (c = 16)
  
(* .h *)
(* final r also assimilated to visarga *)
let visargor c = (visarg c) || (c = 43)
  
(* r *)
let rec all_consonants =
  function | c :: rest -> (consonant c) && (all_consonants rest) | [] -> true
  
let consonant_initial = function | c :: _ -> consonant c | _ -> false
  
let consonant_starts =
  function | chunk :: _ -> consonant_initial chunk | _ -> false
  
let monosyllabic =
  let rec one_vowel =
    function
    | [] -> true
    | c :: rest -> if vowel c then all_consonants rest else one_vowel rest
  in one_vowel
  
let short_vowel =
  function | 1 | 3 | 5 | 7 | 9 -> true | (* .l included *) _ -> false
and long_vowel = function | 2 | 4 | 6 | 8 -> true | _ -> false
  
let avarna c = c < 3
and (* a aa *) ivarna c = (c = 3) || (c = 4)
and (* i ii *) uvarna c = (c = 5) || (c = 6)
and (* u uu *) rivarna c = (c = 7) || (c = 8)
  
(* .r .rr *)
let not_a_vowel c = (vowel c) && (not (avarna c))
and (* c>2 and c<14 *) is_aa c = c = 2
and is_i_or_u c = (c = 3) || (c = 5)
and not_short_vowel c = (vowel c) && (not (short_vowel c))
  
(* segments a word as a list of syllables - Unused *)
let syllables =
  let rec syllables_rec accu_syl accu_pho =
    function
    | c :: rest ->
        if vowel c
        then
          (let new_syl = List.rev (c :: accu_pho)
           in syllables_rec (new_syl :: accu_syl) [] rest)
        else syllables_rec accu_syl (c :: accu_pho) rest
    | [] -> List.rev accu_syl
  in syllables_rec [] []
  
(* multi-consonant - used in Verbs for reduplicating aorist *)
(* we call (mult w) with w starting with a consonant *)
let mult =
  function | _ :: (* assumed consonant *) c :: _ -> consonant c | _ -> false
  
(* lengthens a vowel *)
let long c =
  if short_vowel c
  then if c = 9 then failwith "No long .l" else c + 1
  else if vowel c then c else failwith "Bad arg to long"
and (* shortens a vowel *) short c =
  if long_vowel c
  then c - 1
  else if vowel c then c else failwith "Bad arg to short"
  
(* lengthens the final vowel of a (reverse) stem *)
let lengthen =
  function | v :: r -> (long v) :: r | [] -> failwith "Bad arg to lengthen"
  
(* homophonic vowels *)
let savarna v1 v2 = (v1 < 9) && ((v2 < 9) && ((long v1) = (long v2)))
  
(* special version where c may be a phantom for Sandhi *)
let savarna_ph v c =
  ((vowel c) && (savarna v c)) || ((c = (-3)) && (avarna v))
  
let velar c = (c > 16) && (c < 22)
and (* gutturals    : k kh g gh f       *) palatal c = (c > 21) && (c < 27)
and (* palatals     : c ch j jh ~n      *) lingual c = (c > 26) && (c < 32)
and (* cerebrals    : .t .th .d .dh .n  *) dental c = (c > 31) && (c < 37)
and (* dentals      : t th d dh n       *) labial c = (c > 36) && (c < 42)
and (* labials      : p ph b bh m       *) semivowel c = (c > 41) && (c < 46)
and (* semi vowels  : y r l v  Pr(ya.n) *) sibilant c = (c > 45) && (c < 49)
and (* sibilants    : z .s s   Pr(zar)  *) aspirate c = c = 49
  
(* h *)
let stop c = (c > 16) && (c < 42)
  
(* sparza occlusive Whitney surd *)
(* stop c = velar c || palatal c || lingual c || dental c || labial c *)
let nasal c =
  (c = 21) || (* f *)
    ((c = 26) || (* ~n *)
       ((c = 31) || (* .n *)
          ((c = 36) || (* n *) ((c = 41) || (*  m *) (anusvar c)))))
  
(* Pr(~nam) *)
let n_or_f c = (c = 21) || (* f *) (c = 36)
  
(* n *)
let homonasal c = (* nasal homophonic to given consonant *)
  if consonant c
  then
    if velar c
    then 21
    else (* f  *)
      if palatal c
      then 26
      else (* ~n *)
        if lingual c
        then 31
        else (* .n *)
          if dental c
          then 36
          else (*  n *) if labial c then 41 else (*  m *) 14
  else (* .m *) failwith "Non consonant arg to homonasal"
  
(* vowel modifiers = anusvaara 14, candrabindu 15 and visarga 16 *)
let vowel_mod c = (c > 13) && (c < 17)
  
(* eliminate duplicate consonant in test for prosodically long in Verbs *)
let contract =
  function
  | c :: r ->
      let l =
        (match r with | c' :: r' -> if c = c' then r' else r | [] -> [])
      in c :: l
  | [] -> []
  
let voiced =
  function
  | (* voices previous phoneme with homophone *) 17 -> 19
  | (* k   -> g   *) 27 -> 29
  | (* .t  -> .d  *) 32 -> 34
  | (* t   -> d   *) 37 -> 39
  | (* p   -> b   *) (* next 6 not used by sandhi *) 18 -> 20
  | (* kh  -> gh  *) 22 -> 24
  | (* c   -> j   *) 23 -> 25
  | (* ch  -> jh  *) 28 -> 30
  | (* .th -> .dh *) 33 -> 35
  | (* th  -> dh  *) 38 -> 40
  | (* ph  -> bh  *) c -> c
  
let voiced_consonant c = (* Pr(jhaz) *)
  List.mem c [ 19; 20; 24; 25; 29; 30; 34; 35; 39; 40 ]
and mute_consonant c = (* Pr(khay) *)
  List.mem c [ 17; 27; 32; 37; 18; 22; 23; 28; 33; 38 ]
  
let is_voiced c = (* voiced phonemes *)
  (vowel c) || ((voiced_consonant c) || (List.mem c [ 42; 43; 45 ]))
  
(* y r v *)
(* Next 5 functions used in Sanskrit.adjust *)
let turns_t_to_j c = List.mem c [ 24; 25 ]
  
(* j jh *)
let turns_n_to_palatal c = (palatal c) || (c = 46)
  
(* z *)
let avagraha c = c = (-1)
  
(* elided initial a after a.h which turns to o *)
let elides_visarg_aa c =
  (voiced_consonant c) || ((nasal c) || ((semivowel c) || (aspirate c)))
  
let turns_visarg_to_o c = (elides_visarg_aa c) || (avagraha c)
  
(* Now for the phonetic grades *)
let guna =
  function
  | (* normal grade *) 1 -> [ 1 ]
  | (* a is its own guna *) 2 -> [ 2 ]
  | (* aa is its own guna and vriddhi *) 3 | 4 -> [ 10 ]
  | (* e is guna of i and ii *) 5 | 6 -> [ 12 ]
  | (* o is guna of u and uu *) 7 | 8 -> [ 1; 43 ]
  | (* ar is guna of .r and .rr *) 9 -> [ 1; 44 ]
  | (* al is guna of .l *) c -> [ c ]
  
let vriddhi =
  function
  | (* strong grade *) 1 | 2 -> [ 2 ]
  | (* aa *) 3 | 4 | 10 | 11 -> [ 11 ]
  | (* ai *) 5 | 6 | 12 | 13 -> [ 13 ]
  | (* au *) 7 | 8 -> [ 2; 43 ]
  | (* aar *) 9 -> [ 2; 44 ]
  | (* aal *) c -> [ c ]
  
(* NB. guna : int -> list int. In Panini, guna : varna -> varna, with
   guna(.r) = a and vriddhi(.r) = aa, with suutra I.1.51 that affixes "r":
     Pan(I.1.51): ura.n rapara.h || after a,i,u put r *)
(* NB. u in ura.n above explained in Kaazikaa for counterex. kheyam and geyam *)
(* Macdonnel§125 - condition for root of gana 1 to take guna of its stem *)
let gunify =
  function
  | (* arg word is reversed stem *) v :: _ when vowel v -> true
  | (* guna is used if root is vowel final *) _ :: v :: _ when short_vowel v
      -> true
  | (* or penultimate is short *) _ -> false
  
(* Augment computation *)
let augment i = (* i is initial letter of root *)
  if vowel i
  then vriddhi i
  else
    if i = 23
    then (* ch *) [ 1; 22; 23 ]
    else (* cch *)
      if consonant i
      then [ 1; i ]
      else (* a prefix of consonant *) failwith "Phonetics.augment"
  
let aug =
  function
  | (* augment last phoneme of word *) c :: word -> (augment c) @ word
  | [] -> failwith "Empty stem in aug"
  
let light =
  function
  | (* light roots end in short vowel Pan{6,1,69} *) [] -> failwith "light"
  | c :: _ -> short_vowel c
  
(* For absolutives of roots gana 10 *)
let light_10 =
  function
  | (* light roots end in short vowel Pan{1,4,11} *) [] ->
      failwith "light_10"
  | c :: r ->
      if vowel c
      then false
      else (* ? *)
        (match r with | [] -> failwith "light_10_1" | v :: _ -> short_vowel v)
  
(* Needed by [Verbs.record_part_m_th] for proper retroflexion of
   aatmanepada participles in -maana - eg kriyamaa.na *)
(* all erase last phoneme - used in denominative verbs *)
let trunc_a = function | 1 :: w -> w | _ -> failwith "trunc_a"
and trunc_aa = function | 2 :: w -> w | _ -> failwith "trunc_aa"
and trunc_ii = function | 4 :: w -> w | _ -> failwith "trunc_ii"
and trunc_u = function | 5 :: w -> w | _ -> failwith "trunc_u"
  
let trunc =
  function | _ :: w -> w | w -> failwith ("trunc " ^ (Canon.rdecode w))
  
(* Unused 
(* Stem has short vowel in last syllable *)
value rec brief = fun  
  [ [] -> failwith "Stem with no vowel (brief)"
  | [ c ] -> if vowel c then short_vowel c
             else failwith "Stem with no vowel (brief)"
  | [ c :: r ] -> if vowel c then short_vowel c
                  else brief r
  ] 
; 
(* Sandhi of preverb aa- *)
(* Unused, but simulated by Inflected - related to asandhi below. *)
value mkphantom = fun (* arg is vowel not avarna and not .rr or .l *)
  [ 1 | 2   -> [ -3 ]   (* aa-a *)
  | 3 | 4   -> [ -4 ]   (* aa-i *)
  | 5 | 6   -> [ -5 ]   (* aa-u *)
  | 7       -> [ -6 ]   (* aar *)
  | 10 | 11 -> [ 11 ]   (* ai *)
  | 12 | 13 -> [ 13 ]   (* au *)
  | _       -> failwith "mkphantom"
  ]
; *)
(* Sandhi of a and aa with initial vowel (or phantom) (for [Sandhi]) *)
(* arg is (vowel not avarna and not .rr or .l) or -2,-4,-5,-6,-7,-8 *)
(*i Should be deprecated i*)
let asandhi =
  function
  | 3 | 4 | (-4) | (-7) -> [ 10 ]
  | (* e for i, ii and e-phantoms *i *I *) 5 | 6 | (-5) | (-8) -> [ 12 ]
  | (* o for u, uu and o-phantoms *u *U *) 7 -> [ 1; 43 ]
  | (* ar *) (-6) -> [ 2; 43 ]
  | (* aar *) 123 -> [ 2; 22; 23 ]
  | (* aacch *) 10 | 11 -> [ 11 ]
  | (* ai *) 12 | 13 -> [ 13 ]
  | (* au *) (-2) -> []
  | (* amuissement *) _ -> failwith "asandhi"
  
let vowel_or_phantom c = (vowel c) || (phantom c)
  
(* Tests whether a word starts with a phantom phoneme (precooked aa-prefixed
   finite or participial or infinitive or abs-ya root form) 
   Used by Morpho, Inflected. Copied in Dispatcher. *)
let phantomatic = function | c :: _ -> (c < (-2)) || (c = 123) | _ -> false
and
  (* Amuitic forms start with -2 = [-] which elides preceding -a or -aa from Pv *)
  amuitic = function | (-2) :: _ -> true | _ -> false
  
(* Following 4 functions are used in stem computations in Verbs. *)
(* For m.rj-like verbs (Whitney§219-a) Panini{8,2,36} 
   "bhraaj" "m.rj" "yaj1" "raaj1" "vraj" "s.rj1" "bh.rjj"
   replace phoneme j=24 by j'=124 with sandhi j'+t = .s.t (j' is j going to z) 
   also in Nouns to ifcs "bhraj" "yaj2" "yaaj2" "raaj2" "s.rj2" "bh.rj" 
                 but not to sf "sraj" *)
let mrijify stem =
  match stem with
  | 24 :: r -> 124 :: r
  | _ -> failwith ("mrijify" ^ (Canon.rdecode stem))
  
(* For "duh"-like verbs (Whitney§222) "dah" "dih" "duh1" Panini{8,2,32}
   optionnellement "druh1" "muh" "snuh1" "snih1" Panini{8,2,33}
   replace phoneme h=49 by h'=149 with sandhi h'+t = gdh (h' is h going to gh) 
   ( whereas normal h goes to .dh like pp(lih)=lii.dha) *)
let duhify stem =
  match stem with
  | 49 :: r -> 149 :: r
  | _ -> failwith ("duhify " ^ (Canon.rdecode stem))
  
(* For "nah"-like verbs - h'' is h going to dh.
   Replace phoneme h=49 by h''=249 with sandhi h''+t = ddh ) *)
let nahify stem =
  match stem with
  | 49 :: r -> 249 :: r
  | _ -> failwith ("nahify " ^ (Canon.rdecode stem))
  
(* Aspiration of initial consonant of root stems ending in aspirate.
   The syllabic loop is necessary for e.g. druh -> dhruk. See Whitney§155. *)
let syll_decomp =
  function
  | c :: rest ->
      let rec decomp_rec cs c w =
        (match w with
         | c' :: rest' ->
             if consonant c'
             then decomp_rec (c :: cs) c' rest'
             else (cs, c, w)
         | [] -> (cs, c, w))
      in decomp_rec [] c rest
  | [] -> failwith "syll_decomp"
  
let mk_aspirate w = (* c-cs-vow is the syllable ending in vow *)
  let (cs, c, rest) = syll_decomp w in
  let aspc =
    match c with
    | 19 -> (* g *) 20
    | (* gh *) 34 -> (* d *) 35
    | (* dh *) (* e.g. duh {\R} dhuk *) 39 -> (* b *) 40
    | (* bh *) (* e.g. budh {\R} bhut *) _ -> c
  in (* e.g. v{\d r}dh samidh *) List2.unstack cs (aspc :: rest)
  
let asp =
  function
  | vow :: rest when vowel vow ->
      (match rest with
       | [] -> [ vow ]
       | (* idh *) _ -> vow :: (mk_aspirate rest))
  | _ -> failwith "Penultimate not vowel"
  
(* Final form of a pada *)
(* Warning - finalize does NOT replace s or r by visarga, and fails on visarga *)
let finalize rstem =
  match rstem with
  | [] -> []
  | c :: rest ->
      (match c with
       | 17 | (* k *) (* first permitted finals *) 18 | (* kh *) 21 |
           (* \.n *) 27 | (* {\d t} *) 28 | (* {\d t}h *) 31 | (* {\d n} *)
           32 | (* t *) (* e.g. marut, vi\'svajit *) 33 | (* th *) 36 |
           (* n *) 37 | (* p *) 38 | (* ph *) 41 | (* m *) 44 | (* l *)
           (* l needed for praty\=ah\=ara hal *) 45 | (* v *) (* diiv2 *) 43
           | (* r *)
           (* no visarga to keep distinction r/s for segmentation *) 48 ->
           (* s *) rstem
       | 19 | (* g *) 22 | (* c *) 23 | (* ch *) 24 | (* j *)
           (* e.g. bhi{\d s}aj; bhuj; as{\d r}j -yuj *) 25 -> (* jh *)
           (match rest with
            | 26 :: (* \~n *) ante ->
                let nasal =
                  (match ante with
                   | 1 :: 18 :: _ -> (* kha~nj *) 36
                   | (* n *) _ -> 21)
                in (* \.n *) nasal :: ante
            | 24 :: (* j *) ante | 22 :: (* c *) ante ->
                27 :: (* {\d t} *) ante
            | (* majj bh.rjj pracch *) 21 :: (* \.n *) _ -> rest
            | _ -> 17 :: (* k *) rest)
       | (* but sometimes {\d t} - eg devej *) 20 -> (* gh *)
           17 :: (* k *) (asp rest)
       | 26 -> (* \~n *) 21 :: (* \.n *) rest
       | 29 | (* {\d d} *) 30 | (* {\d d}h *) (* e. g. vri{\d d}h *)
           (* asp? *) 124 -> (* j' *) 27 :: (* {\d t} *) rest
       | (* e.g. r\=a{\d t} *) 34 -> (* d *) 32 :: (* t *) rest
       | (* e.g. suh{\d r}d *) 35 -> (* dh *) 32 :: (* t *) (asp rest)
       | (* e.g. budh, v{\d r}dh *) 39 -> (* b *) 37 :: (* p *) rest
       | 40 -> (* bh *) 37 :: (* p *) (asp rest)
       | (* e.g. kakubh *) 46 -> (* z *)
           (match rest with
            | (* .t is default and k exception (Henry, Whitney§145,218) *)
                3 :: 34 :: _ | (* -di\'s {\R} -dik *) 7 :: 34 :: _ |
                  (* -d{\d r}\'s {\R} -d{\d r}k *) 7 :: 37 :: 48 :: _
                -> (* -sp{\d r}\'s {\R} -sp{\d r}k *) 17 :: (* k *) rest
            | _ -> 27 :: (* {\d t} *) rest)
       | (* default *) (* NB optionally nak Whitney§218a *) 47 ->
           (* {\d s} *)
           (match rest with
            | 7 :: 35 :: _ -> (* -dh{\d r}{\d s} {\R} -dh{\d r}k *)
                17 :: (* k *) rest
            | (* Kane §97 *) 17 :: ante -> (* -k{\d s} {\R} -k *)
                17 :: (* k *) ante
            | (* vivik.s Kane §97 but MW: vivi.t *) _ ->
                27 :: (* {\d t} *) rest)
       | (* e.g. dvi{\d s} {\R} dvi{\d t} *) 49 -> (* h *)
           27 :: (* {\d t} *) (asp rest)
       | (* e.g. lih {\R} li{\d t} *) 149 -> (* h' *)
           17 :: (* k *) (asp rest)
       | (* -duh {\R} -dhuk , impft doh adhok, etc. *) 249 -> (* h'' *)
           32 :: (* t *) (asp rest)
       | c ->
           if vowel c
           then rstem
           else
             (let s = Canon.rdecode rstem
              in failwith ("Illegal stem " ^ (s ^ " (finalize)"))))
  
let finalizer root =
  match root with
  | [] -> []
  | c :: rest ->
      (match c with
       | 41 -> (* m *) 36 :: (* n *) rest
       | (* Whitney §143a *) _ -> finalize root)
  
(* Used in [Nouns.build_root] *)
let finalize_r stem =
  match stem with
  | [] -> []
  | c :: rest ->
      (match c with
       | 43 -> (* r *)
           (match rest with
            | c :: l ->
                if short_vowel c
                then (* giir puurbhyas Whitney §245b *) 43 :: (long c) :: l
                else stem
            | [] -> failwith "Illegal arg r to finalize_r")
       | 48 -> (* s *)
           (match rest with
            | (* Whitney §168 *)
                1 :: 43 :: 48 :: _ | (* sras *) 1 :: 45 :: 35 :: _ ->
                (* dhvas *) 34 :: (* t *) rest
            | _ -> stem)
       | (* suvas *) _ -> finalize stem)
  
(* Used in Verbs *)
let consonantal rstem = match rstem with | c :: _ -> consonant c | _ -> false
  
(* Used in Nouns *)
let bi_consonantal rstem =
  match rstem with
  | c1 :: c2 :: _ -> (consonant c1) && (consonant c2)
  | _ -> false
  

