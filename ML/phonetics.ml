(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Phonetics = struct i*)

(* Sanskrit phonology *)

value vowel c = c>0 && c<14 (* a aa i ii u uu .r .rr .l e ai o au *)(* Pr{hac} *)
and anusvar c = c=14 (* .m : anusvara standing for nasal *)
           (*  || c=15  candrabindu *)
and visarga c = c=16 (* .h *)
and consonant c = c>16 (* Pr{hal} *)
and phantom c = c<(-1) (* -2 -3=*a -4=*i -5=*u -6=*r *)
;
(* final s assimilated to visarga *)
value visarg c = c=48 (* s *) || c=16 (* .h *)
;
(* final r also assimilated to visarga *)
value visargor c = visarg c || c=43 (* r *)
;
value rec all_consonants = fun
  [ [ c :: rest ] -> consonant c && all_consonants rest
  | [] -> True
  ]
;
value consonant_initial = fun
  [ [ c :: _ ] -> consonant c 
  | _ -> False
  ]
;
value monosyllabic = one_vowel
  where rec one_vowel = fun
    [ [] -> True
    | [ c :: rest ] -> if vowel c then all_consonants rest
                       else one_vowel rest
    ]
;
value short_vowel = fun
  [ 1 | 3 | 5 | 7 | 9 -> True (* .l included *)
  | _ -> False 
  ] 
and long_vowel = fun
  [ 2 | 4 | 6 | 8 -> True 
  | _ -> False
  ]
;
value avarna c = c<3        (* a aa *)
and ivarna c = c=3 || c=4   (* i ii *)
and uvarna c = c=5 || c=6   (* u uu *)
and rivarna c = c=7 || c=8  (* .r .rr *)
;
value not_a_vowel c = vowel c && not (avarna c) (* c>2 and c<14 *)
and is_aa c = c=2
and is_i_or_u c = c=3 || c=5
and not_short_vowel c =  vowel c && not (short_vowel c)
;
(* segments a word as a list of syllables - Unused *)
value syllables = syllables_rec [] []
  where rec syllables_rec accu_syl accu_pho = fun
  [ [ c :: rest ] -> 
      if vowel c then 
         let new_syl = List.rev [ c :: accu_pho ] in 
         syllables_rec [ new_syl :: accu_syl ] [] rest 
      else syllables_rec accu_syl [ c :: accu_pho ] rest
  | [] -> List.rev accu_syl
  ]
;
(* multi-consonant - used in Verbs for reduplicating aorist *)
(* we call (mult w) with w starting with a consonant *)
value mult = fun
  [ [ _ :: [ c :: _ ] ] -> consonant c
  | _ -> False
  ]
;
(* lengthens a vowel *)
value long c = 
  if short_vowel c then 
       if c=9 then failwith "No long .l" else c+1
  else if vowel c then c
       else failwith "Bad arg to long"
(* shortens a vowel *)
and short c = 
  if long_vowel c then c-1
  else if vowel c then c
       else failwith "Bad arg to short"
;
(* lengthens the final vowel of a (reverse) stem *)
value lengthen = fun
  [ [ v :: r ] -> [ long v :: r ]
  | [] -> failwith "Bad arg to lengthen"
  ]
;
(* homophonic vowels *)
value savarna v1 v2 = v1<9 && v2<9 && (long v1 = long v2)
;
(* special version where c may be a phantom for Sandhi *)
value savarna_ph v c = (vowel c && savarna v c) || (c=(-3) && avarna v)
;
value velar   c = c > 16 && c < 22 (* gutturals    : k kh g gh f       *)
and palatal   c = c > 21 && c < 27 (* palatals     : c ch j jh ~n      *)
and lingual   c = c > 26 && c < 32 (* cerebrals    : .t .th .d .dh .n  *)
and dental    c = c > 31 && c < 37 (* dentals      : t th d dh n       *)
and labial    c = c > 36 && c < 42 (* labials      : p ph b bh m       *)
and semivowel c = c > 41 && c < 46 (* semi vowels  : y r l v  Pr{ya.n} *)
and sibilant  c = c > 45 && c < 49 (* sibilants    : z .s s   Pr{zar}  *)
and aspirate  c = c = 49 (* h *)
;
value stop c = c > 16 && c < 42
;
value nasal c = 
     c = 21 (* f *) || c =  26 (* ~n *) || c = 31 (* .n *) 
  || c = 36 (* n *) || c = 41  (*  m *) || anusvar c (* Pr{~nam} *)
;
value n_or_f c =  c = 21 (* f *) || c = 36 (* n *) 
;
value homonasal c = (* nasal homophonic to given consonant *)
  if consonant c then 
  if velar c   then 21 (* f  *) else
  if palatal c then 26 (* ~n *) else
  if lingual c then 31 (* .n *) else
  if dental c  then 36 (*  n *) else
  if labial c  then 41 (*  m *)
               else 14 (* .m *)
  else failwith "Non consonant arg to homonasal"
;
(* vowel modifiers = anusvaara 14, candrabindu 15 and visarga 16 *)
value vowel_mod c = c>13 && c<17 
;
(* eliminate duplicate consonant in test for prosodically long in Verbs *)
value contract = fun
  [ [ c :: r ] -> 
    let l = match r with
        [ [ c' :: r' ] -> if c=c' then r' else r
        | [] -> []
        ] in [ c :: l ]
  | [] -> []
  ]
;
value voiced = fun (* voices previous phoneme with homophone *)
  [ 17 -> 19 (* k   -> g   *)
  | 27 -> 29 (* .t  -> .d  *)
  | 32 -> 34 (* t   -> d   *)
  | 37 -> 39 (* p   -> b   *) 
  (* next 6 not used by sandhi *)
  | 18 -> 20 (* kh  -> gh  *) 
  | 22 -> 24 (* c   -> j   *) 
  | 23 -> 25 (* ch  -> jh  *) 
  | 28 -> 30 (* .th -> .dh *) 
  | 33 -> 35 (* th  -> dh  *) 
  | 38 -> 40 (* ph  -> bh  *) 
  | c  -> c
  ]
;
value voiced_consonant c = (* Pr{jhaz} *)
  List.mem c [ 19; 20; 24; 25; 29; 30; 34; 35; 39; 40 ]

and mute_consonant c =(* Pr{khay} *)
  List.mem c [ 17; 27; 32; 37; 18; 22; 23; 28; 33; 38 ]
;
value is_voiced c = (* voiced phonemes *)
  vowel c || voiced_consonant c || List.mem c [ 42; 43; 45 ] (* y r v *)
;
(* Next 5 functions used in Sanskrit.adjust *)
value turns_t_to_j c = List.mem c [ 24; 25 ] (* j jh *)
;
value turns_n_to_palatal c = palatal c || c=46 (* z *)
;
value avagraha c = (c = -1) (* elided initial a after a.h which turns to o *)
;
value elides_visarg_aa c =  
  voiced_consonant c || nasal c || semivowel c || aspirate c 
;
value turns_visarg_to_o c = elides_visarg_aa c || avagraha c
;
value guna = fun
  [ 1     -> [ 1 ]     (* a is its own guna *)
  | 2     -> [ 2 ]     (* aa is its own guna and vriddhi *)
  | 3 | 4 -> [ 10 ]    (* e *)
  | 5 | 6 -> [ 12 ]    (* o *)
  | 7 | 8 -> [ 1; 43 ] (* ar *)
  | 9     -> [ 1; 44 ] (* al *)
  | c     -> [ c ]
  ]
;
value vriddhi = fun
  [ 1 | 2           -> [ 2 ]     (* aa *)
  | 3 | 4 | 10 | 11 -> [ 11 ]    (* ai *)
  | 5 | 6 | 12 | 13 -> [ 13 ]    (* au *)
  | 7 | 8           -> [ 2; 43 ] (* aar *)
  | 9               -> [ 2; 44 ] (* aal *)
  | c               -> [ c ]
  ]
;

(* Macdonnel §125 - condition for root of gana 1 to take guna of its stem *)
value gunify = fun (* arg word is reversed stem *)
  [ [ v :: _ ] when vowel v -> True
  | [ _ :: [ v :: _ ] ] when short_vowel v -> True 
  | _ -> False
  ]
;
(* Augment computation *)
value augment x = (* arg is first letter of root *)
  if vowel x then vriddhi x
  else if x = 23 (* ch *) then [ 1; 22; 23 ] (* cch *)
  else if x>16 && x<50 then [ 1; x ]     (* a prefix of consonant *)
  else failwith "Phonetics.augment"
;
value aug = fun (* augment last phoneme of word *)
  [ [ c :: word ] -> augment c @ word
  | [] -> failwith "Empty stem (aug)"   
  ]
;
value light = fun (* light roots end in short vowel Pan{6,1,69} *)
   [ [] -> failwith "light" 
   | [ c :: _ ] -> short_vowel c 
   ]
;
(* For absolutives of roots gana 10 *) 
value light_10 = fun (* light roots end in short vowel Pan{1,4,11} *) 
   [ [] -> failwith "light_10"
   | [ c :: r ] -> if vowel c then False (* ? *) else match r with
       [ [] -> failwith "light_10_1"
       | [ v :: _ ] -> short_vowel v 
       ]
   ]
;
(* Needed by [Verbs.record_part_m_th] for proper retroflexion of
   aatmanepada participles in -maana - eg kriyamaa.na *)
(* all erase last phoneme - used in denominative verbs *)
value trunc_a = fun
  [ [ 1 :: w ] -> w
  | _ -> failwith "trunc_a"
  ]
and trunc_aa = fun
  [ [ 2 :: w ] -> w
  | _ -> failwith "trunc_aa"
  ]
and trunc_u = fun
  [ [ 5 :: w ] -> w
  | _ -> failwith "trunc_u"
  ]
;
value trunc = fun
  [ [ _ :: w ] -> w 
  | w -> failwith ("trunc " ^ Canon.rdecode w)
  ] 
;
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
value asandhi = fun 
  [ 3 | 4 | -4 | -7 -> [ 10 ]    (* e for i, ii and e-phantoms *i *I *)
  | 5 | 6 | -5 | -8 -> [ 12 ]    (* o for u, uu and o-phantoms *u *U *)
  | 7          -> [ 1; 43 ] (* ar *)
  | -6         -> [ 2; 43 ] (* aar *)
  | 123        -> [ 2; 22; 23 ] (* aacch *)
  | 10 | 11    -> [ 11 ]    (* ai *)
  | 12 | 13    -> [ 13 ]    (* au *)
  | -2         -> [] (* amuissement *)
  | _          -> failwith "asandhi"
  ]
;
value vowel_or_phantom c = vowel c || phantom c 
;
(* Tests whether a word starts with a phantom phoneme (precooked aa-prefixed
   finite or participial or infinitive or abs-ya root form) 
   Used by Morpho, Inflected. Copied in Dispatcher. *)
value phantomatic = fun
  [ [ c :: _ ] -> c<(-2) || c=123
  | _ -> False
  ]
(* Amuitic forms start with -2 = [-] which elides preceding -a or -aa from Pv *)
and amuitic = fun [ [ -2 :: _ ] -> True | _ -> False ]
;
(* For m.rj-like verbs (Whitney§219-a) Panini{8,2,36} 
   "bhraaj" "m.rj" "yaj1" "raaj1" "vraj" "s.rj1" "bh.rjj"
   replace phoneme j=24 by j'=124 with sandhi j'+t = .s.t (j' is j going to z) *)
value mrijify stem = match stem with
  [ [ 24 :: r ] -> [ 124 :: r ]
  | _ -> failwith ("mrijify" ^ Canon.rdecode stem)
  ]
;
(* For "duh"-like verbs (Whitney§222) "dah" "dih" "duh1" Panini{8,2,32}
   optionnellement "druh1" "muh" "snuh1" "snih1" Panini{8,2,33}
   replace phoneme h=49 by h'=149 with sandhi h'+t = gdh (h' is h going to gh) *)
value duhify stem = match stem with
  [ [ 49 :: r ] -> [ 149 :: r ]
  | _ -> failwith ("duhify " ^ Canon.rdecode stem)
  ]
;
(* For "nah"-like verbs - h'' is h going to dh.
   Replace phoneme h=49 by h''=249 with sandhi h''+t = ddh ) *)
value nahify stem = match stem with
  [ [ 49 :: r ] -> [ 249 :: r ]
  | _ -> failwith ("nahify " ^ Canon.rdecode stem)
  ]
;
(* Aspiration of initial consonant of root stems ending in aspirate.
   The syllabic loop is necessary for e.g. druh -> dhruk. See Whitney§155. *)
value syll_decomp = fun
  [ [ c :: rest ] -> decomp_rec [] c rest
      where rec decomp_rec cs c w = match w with
        [ [ c' :: rest' ] -> if consonant c' then decomp_rec [ c :: cs ] c' rest'
                             else (cs,c,w)
        | [] -> (cs,c,w)
        ]
  | [] -> failwith "syll_decomp"
  ]
;
value mk_aspirate w = (* c-cs-vow is the syllable ending in vow *)
  let (cs,c,rest) = syll_decomp w in
  let aspc = match c with
      [ 19 (* g *) -> 20 (* gh *)
      | 34 (* d *) -> 35 (* dh *) (* e.g. duh {\R} dhuk *)
      | 39 (* b *) -> 40 (* bh *) (* e.g. budh {\R} bhut *)
      | _ -> c (* e.g. v{\d r}dh samidh *)
      ] in
  List2.unstack cs [ aspc :: rest ] 
;
value asp = fun 
  [ [ vow :: rest ] when vowel vow -> [ vow :: mk_aspirate rest ]
  | _ -> failwith "Penultimate not vowel"
  ]
;
(* final form of a pada *)
(* Warning - finalize does NOT replace s or r by visarga, and fails on visarga *)
value finalize rstem = match rstem with
  [ [] -> []
  | [ c :: rest ] -> match c with 
       [ 17 (* k *) (* first permitted finals *)
       | 18 (* kh *) 
       | 21 (* \.n *) 
       | 27 (* {\d t} *)
       | 28 (* {\d t}h *)
       | 31 (* {\d n} *) 
       | 32 (* t *) (* e.g. marut, vi\'svajit *)
       | 33 (* th *) 
       | 36 (* n *) 
       | 37 (* p *) 
       | 38 (* ph *) 
       | 41 (* m *) 
       | 44 (* l *) (* l needed for praty\=ah\=ara hal *)
       | 45 (* v *) (* diiv2 *)
       | 43 (* r *) (* no visarga to keep distinction r/s for segmentation *)
       | 48 (* s *) -> rstem (* but sras -> srat ? *)
       | 19 (* g *) 
       | 22 (* c *) 
       | 23 (* ch *)
       | 24 (* j *) (* e.g. bhi{\d s}aj; bhuj; as{\d r}j -yuj *)
       | 25 (* jh *) -> match rest with 
           [ [ 26 (* \~n *) :: ante ] -> [ 21 (* \.n *) :: ante ] 
           | [ 24 (* j *) :: ante ] | [ 22 (* c *) :: ante ] 
               -> [ 27 (* {\d t} *) :: ante ] (* majj bh.rjj pracch *)
           | [ 21 (* \.n *) :: _ ] -> rest
           | _ -> [ 17 (* k *) :: rest ] (* but sometimes {\d t} - eg devej *)
           ]
       | 20 (* gh *) -> [ 17 (* k *) :: asp rest ] 
       | 26 (* \~n *) -> [ 21 (* \.n *) :: rest ] 
       | 29 (* {\d d} *) 
       | 30 (* {\d d}h *) (* e. g. vri{\d d}h *) (* asp? *)
       | 124 (* j' *) -> [ 27 (* {\d t} *) :: rest ]  (* e.g. r\=a{\d t} *)
       | 34 (* d *) -> [ 32 (* t *) :: rest ] (* e.g. suh{\d r}d *)
       | 35 (* dh *) -> [ 32 (* t *) :: asp rest ] (* e.g. budh, v{\d r}dh *)
       | 39 (* b *) -> [ 37 (* p *) :: rest ]
       | 40 (* bh *) -> [ 37 (* p *) :: asp rest ] (* e.g. kakubh *)
       | 46 (* \'s *) -> match rest with 
          (* .t is default and k exception (Henry, Whitney§145,218) *)
          [ [ 3 :: [ 34 :: _ ] ] (* -di\'s {\R} -dik *) 
          | [ 7 :: [ 34 :: _ ] ] (* -d{\d r}\'s {\R} -d{\d r}k *) 
          | [ 7 :: [ 37 :: [ 48 :: _ ] ] ] (* -sp{\d r}\'s {\R} -sp{\d r}k *) 
              -> [ 17 (* k *) :: rest ]
          | _ -> [ 27 (* {\d t} *) :: rest ] (* default *)
          (* NB optionally nak Whitney§218a *) 
          ]
       | 47 (* {\d s} *) -> match rest with  
          [ [ 7 :: [ 35 :: _ ] ] (* -dh{\d r}{\d s} {\R} -dh{\d r}k *) 
              -> [ 17 (* k *) :: rest ] (* Kane §97 *)
          | [ 17 :: ante ] (* -k{\d s} {\R} -k *) 
              -> [ 17 (* k *) :: ante ] (* vivik.s Kane §97 but MW: vivi.t *)
          | _ -> [ 27 (* {\d t} *) :: rest ] (* e.g. dvi{\d s} {\R} dvi{\d t} *)
          ]
       | 49 (* h *) -> [ 27 (* {\d t} *) :: asp rest ] (* e.g. lih {\R} li{\d t} *) 
       | 149 (* h' *) -> [ 17 (* k *) :: asp rest ] (* -duh {\R} -dhuk , impft doh adhok, etc. *)
       | 249 (* h'' *) -> [ 32 (* t *) :: asp rest ] 
       | c -> if vowel c then rstem 
              else let s = Canon.rdecode rstem in
                   failwith ("Illegal stem " ^ s ^ " (finalize)")
       ] 
  ]
;
value finalizer root = match root with
  [ [] -> []
  | [ c :: rest ] -> match c with 
       [ 41 (* m *) -> [ 36 (* n *) :: rest ] (* Whitney §143a *)
       | _ -> finalize root
       ]
  ]
;
(* Used in [Nouns.build_root] *)
value finalize_r stem = match stem with
  [ [] -> []
  | [ c :: rest ] -> match c with 
       [ 43 (* r *) -> match rest with
          [ [ c :: l ] -> if short_vowel c (* giir puurbhyas Whitney §245b *)
                          then [ 43 :: [ long c :: l ] ] 
                          else stem 
          | [] -> failwith "Illegal arg r to finalize"
          ]
       | 48 (* s *) -> match rest with
          [ [ 1 :: [ 45 :: [ 35 :: _ ] ] ] -> [ 34 (* t *) :: rest ] (* dhvas *)
          | [ 1 :: [ 45 :: _ ] ] -> stem (* suvas *)
          | _ -> [ 34 (* t *) :: rest ] (* sras *) 
          ]
       | _ -> finalize stem 
       ]
  ] 
;
(* Used in Nouns *)
value bi_consonant rstem = match rstem with
  [ [ c1 :: [ c2 :: _ ] ] -> consonant c1 && consonant c2 
  | _ -> False
  ]
;

(*i internal sandhi with vowel or 'y' according to Macdonell §59 -- unused
[value diphthong_split = fun
  [ 10 (* e *)  -> [ 42; 1 ] (* ay *)
  | 11 (* ai *) -> [ 42; 2 ] (* aay *) 
  | 12 (* o *)  -> [ 45; 1 ] (* av *) 
  | 13 (* au *) -> [ 45; 2 ] (* aav *)
  | c -> [ c ]
  ]
;] i*)

(* Caution. 
Phantom phonemes *a (-3), *i (-4), *u (-5) and *r (-6) are NOT vowels, 
you should use [vowel_or_phantom] function. 
Extra fine-grained phonemes j' (124) h' (149) and h'' (249) are consonants. *)

(*i end; i*)
