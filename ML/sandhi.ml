(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Sandhi = struct i*)

(* This module defines external sandhi for compound and sentence construction. 
   It proceeds as a finite transducer with two input tapes, one for the right
   stream of phonemes, the other for the reversal of the left stream. It is 
   deterministic, and thus makes choices in optional situations, so that 
   sandhi is a deterministic function. *)

(* This algorithm is only used by service [Sandhier], while sandhi viccheda
   proceeds by building tables in [Compile_sandhi] with the help of a clone 
   of this code, then completes the tables with optional rules, 
   making predictive sandhi a truly non-deterministic relation.
   The code below ought NOT be modified without inspection 
   of its improved clone in module [Compile_sandhi]. *)

open Phonetics; (* finalize visargor *)
open Canon; (* decode *)

value code str = Encode.code_string str
;
value visargcomp1 first = fun
  [ [] -> failwith "left arg of sandhi too short (1)"
  | [ penu :: _ ] -> match penu with
        [ 1 -> [ -1; 12; first ] (* o  -1 means erase a *)
        | 2 -> [ first ] (* visarga dropped after aa *)
        | _ -> [ 43; first ] (* visarga goes to r *)
        ]
  ]
;
value visargcomp2 = fun (* first = `r`, visarga goes to r *)
  [ [] -> raise (Failure "left arg of sandhi too short (2)")
  | [ penu :: _ ] -> match penu with
        [ 1 -> [ -1; 12; 43 ] (* "a.h+r {\R} or"  -1 means erase a *)
        | 2 -> [ 43 ] (* "aa.h+r {\R} aar" *)
        | c -> [ -1; long c; 43 ] 
        ]
  ]
;
value visargcompr = fun
  [ [] -> failwith "left arg of sandhi too short (r)"
  | [ penu :: _ ] -> [ -1; long penu; 43 ] 
  ]
;
value visargcompv first (* vowel *) = fun
  [ [] -> failwith "left arg of sandhi too short (v)"
  | [ penu :: _ ] -> match penu with
        [ 1 -> if first = 1 then [ -1; 12; -1 ] (* erase a, o, then avagraha *)
               else [ 50; first ] (* hiatus *)
        | 2 -> [ 50; first ] (* hiatus *)
        | c -> [ 43; first ] 
        ]
  ]
;
(* External sandhi core algorithm - [wl] is the reverse left word
   [wr] is the right word, result is a zip pair (left,right) of words.
   Caution. This code is used mostly by the Web interface Sandhier, 
   where phantoms may not occur in the input.
   However, phantom is tested in the code in order to keep consistency with 
   [Compile_sandhi], which builds the sandhi rules for transducers decorations.
   This function is also used for glueing preverbs in Roots. *)
(* unphantom - should instead call [Phonetics.un_phantom] *)
value uph = fun 
  [ -3 -> [ 2 ] 
  | -4 -> [ 10 ] 
  | -5 -> [ 12 ] 
  | -6 -> [ 2; 43 ] (* aar *) 
  | r -> [ r ] 
  ] 
; 
value ext_sandhi_pair wl wr = 
  match wl with 
    [ [] -> failwith "left arg of sandhi empty"
    | [ last :: before ] -> match wr with 
  (* Nota Bene : we assume [wl] to be in final sandhi form except r or s.    *)
  (* Thus in the following code all cases last = 34 (* d *) could be omitted *)
  (* for the sandhi viccheda algorithm when inflected forms are known final. *)
        [ [] -> (wl,[]) (* no visarga for final s or r *)
        | [ first :: after ] -> 
    if vowel last then
       if vowel_or_phantom first then (* first may be *e or *o, thus uph below *)
         let glue =
(* [glue] is the string replacing [[ last; first ]] with a special convention:
   when it starts with -1, it means the last letter of [before] is erased,
   which occurs only when last is visarga *)
          if savarna_ph last first then [ long last ] 
          else if avarna last then asandhi first
          else if ivarna last then [ 42 :: uph first ] (* y *)
          else if uvarna last then [ 45 :: uph first ] (* v *)
          else match last with
            [ 7 | 8 -> [ 43 :: uph first ]      (* .r {\R} r *)
            | 10 | 12 (* e o *) -> 
              if first=1 then [ last; -1 ] (* avagraha *)
              else if first=(-11) then [ 1; if last=10 then 42 else 44; 2 ] 
                   (* e+aa+a -> ayaa  o+aa+a -> avaa (preverb aa on augment) *)
              else [ 1 :: [ 50 :: uph first ] ]   (* a+hiatus *)
            | 11 (* ai *) -> [ 2 :: [ 50 :: uph first ] ] (* aa+hiatus *)
            | 13 (* au *) -> [ 2 :: [ 45 :: uph first ] ] (* aav *)
            | _ -> let message = "left arg of sandhi end illegal in " in
                   failwith (message ^ decode wl)
            ] in (before,glue @ after)
       else (wl,if first=23 (* ch *) then [ 22 :: wr ] (* cch *) else wr)
            (* c optional except when [short_vowel last] or wl=\=a or m\=a *)
    else (* we assume that last cannot be a phantom and thus is a consonant *)
       let glue = 
         if vowel first then 
            if visarg last then visargcompv first before (* may start with -1 *)
            else match last with 
                 [ 21 -> match before with 
                    [ [] -> failwith "left arg too short"
                    | [ v :: rest ] -> if short_vowel v then 
                                          [ 21 :: [ 21 :: uph first ] ] (* ff *)
                                       else [ 21 :: uph first ] 
                    ]
                 | 36 -> match before with 
                    [ [] -> failwith "left arg too short"
                    | [ v :: rest ] -> if short_vowel v then 
                                          [ 36 :: [ 36 :: uph first ] ] (* nn *)
                                       else [ 36 :: uph first ] 
                    ]
                 | c  -> [ voiced c :: uph first ] (* t {\R} d, p {\R} b  *)
                 ]
         else (* both consonant *) match first with 
     [ 49 (* h *) ->
         if visarg last then visargcomp1 first before
         else match last with
             [ 17 | 19 -> [ 19; 20 ] (* k+h {\R} ggh,  g+h {\R} ggh *)
             | 27      -> [ 29; 30 ] (* {\d t}+h {\R} {\d d}{\d d}h *)
             | 32 | 34 -> [ 34; 35 ] (* t+h {\R} ddh,  d+h {\R} ddh *)
             | 37 | 39 -> [ 39; 40 ] (* p+h {\R} bbh,  b+h {\R} bbh *)
             | 41      -> [ 14; first ] (* m+h {\R} {\d m}h *) 
                  (* but m+hm {\R} mhm and m+hn {\R} mhn preferably (Deshpande) *)
             | c       -> [ c; first ]
             ]
     | 46 (* \'s *) -> match last with
             [ 32 | 34 | 22 -> [ 22; 23 ] (* t+\'s {\R} cch  idem d c *) 
                          (* optionally [ 22; 46 ] c's see [compile_sandhi] *)
             | 36      -> [ 26; 23 ] (* n+\'s {\R} \~nch (or [ 26; 46 ] \~n\'s) *)
             | 41      -> [ 14; first ] (* m+\'s {\R} {\d m}\'s (or \~nch optional) *)
             | c       -> [ if visargor c then 16 else c; first ]
             ]
     | 36 | 41 (* n m *) ->
            if visarg last then visargcomp1 first before
            else match last with
             [ 17 | 21 -> [ 21; first ] (* k+n {\R} \.nn 'n+n -> 'nn *)
             | 27 | 29 -> [ 31; first ] (* {\d t}+n {\R} {\d n}n  {\d d}+n {\R} {\d n}n *) 
             | 32 | 34 -> [ 36; first ] (* t+n {\R} nn  d+n {\R} nn *) 
             | 37      -> [ 41; first ] (* p+n {\R} mn *)
             | 41      -> [ 14; first ] (* m+n {\R} {\d m}n *)
             | c       -> [ c; first ]  (* \.n+n {\R} \.nn etc. *)
             ]
     | 47 | 48 (* {\d s} s *) ->
               match last with
             [ 41 -> [ 14; first ] (* m+s {\R} {\d m}s *)
             | 34 -> [ 32; first ] (* d+s {\R} ts *) 
             | c  -> [ if visargor c then 16 else c; first ]
             ]
     | 37 | 38 | 17 | 18 (* p ph k kh *) ->
               match last with
             [ 41 -> [ 14; first ] (* m+p {\R} {\d m}p *)
             | 34 -> [ 32; first ] (* d+p {\R} tp *) 
             | c  -> [ if visargor c then 16 else c; first ] (* s+k {\R} {\d h}k but optional {\d s}k *)
             ]
     | 44 (* l *) -> 
            if visarg last then visargcomp1 first before
            else match last with
             [ 32 | 34 -> [ 44; 44 ] (* t+l {\R} ll  d+l {\R} ll *) 
             | 36 | 41 -> [ 44; 15; 44 ] (* n+l {\R} l\~ l (candrabindu) *)
             | c       -> [ voiced c; 44 ]
             ]
     | 42 | 45 (* y v *) -> 
            if visarg last then visargcomp1 first before
            else match last with
             [ 41 -> [ 14; first ] (* m+y {\R} {\d m}y *)
             | c  -> [ voiced c; first ]
             ]
     | 43 (* r *) ->
            if visarg last then visargcomp2 before
            else match last with
             [ 41 -> [ 14; 43 ] (* m+r {\R} {\d m}r *)
             | 43 -> visargcompr before (* Gonda §16 *)
             | c  -> [ voiced c; first ]
             ] 
     | 39 | 40 | 34 | 35 | 19 | 20 (* b bh d dh g gh *) ->
            if visarg last then visargcomp1 first before
            else match last with
             [ 41 -> [ 14; first ] (* m+b {\R} {\d m}b == mb *)
             | c  -> [ voiced c; first ]
             ]
     | 29 | 30 (* {\d d} {\d d}h *) ->
            if visarg last then visargcomp1 first before
            else match last with
             [ 41      -> [ 14; first ] (* m+{\d d} {\R} {\d m}{\d d} == {\d n}{\d d} *)
             | 32 | 34 -> [ 29; first ] (* t+{\d d} {\R} {\d d}{\d d}  d+{\d d} {\R} {\d d}{\d d} *) 
             | 36      -> [ 31; first ] (* n+{\d d} {\R} {\d n}{\d d} *)
             | c       -> [ voiced c; first ]
             ]
     | 24 | 25 (* j jh *) ->
            if visarg last then visargcomp1 first before
            else match last with
             [ 41      -> [ 14; first ] (* m+j {\R} {\d m}j == \~nj *)
             | 32 | 34 -> [ 24; first ] (* t+j {\R} jj  d+j {\R} jj *) 
             | 36 -> [ 26; first ] (* n+j {\R} \~nj *)
             | c       -> [ voiced c; first ]
             ]
     | 32 | 33 (* t th *) -> match last with 
             [ 41 -> [ 14; first ]     (* m+t {\R} {\d m}t == nt *)
             | 36 -> [ 14; 48; first ] (* n+t {\R} {\d m}st *)
             | 34 -> [ 32; first ]     (* d+t {\R} tt *)
             | c  -> [ if visargor c then 48 else c; first ] (* s+t {\R} st *)
             ]
     | 27 | 28 (* {\d t} {\d t}h *) -> match last with 
             [ 41      -> [ 14; first ] (* m+{\d t} {\R} {\d m}{\d t} == {\d n}{\d t} *)
             | 32 | 34 -> [ 27; first ] (* t+{\d t} {\R} {\d t}{\d t}  d+{\d t} {\R} {\d t}{\d t} *) 
             | 36      -> [ 14; 47; first ] (* n+{\d t} {\R} {\d m}{\d s}{\d t} *)
             | c       -> [ if visargor c then 47 else c; first ]
             ]
     | 22 | 23 (* c ch *) -> match last with 
             [ 41      -> [ 14; first ] (* m+c {\R} {\d m}c == \~nc *)
             | 32 | 34 -> [ 22; first ] (* t+c {\R} cc  d+c {\R} cc *) 
             | 36      -> [ 14; 46; first ] (* n+c {\R} {\d m}\'sc *)
             | c       -> [ if visargor c then 46 else c; first ]
             ]
  (* | 31 (* {\d n} *) missing c+.n = f.n TODO *)
     | c -> failwith ("illegal start of right arg of sandhi in " ^ decode wr)
     ] (* match first *) in (* let glue *)
       let (w1,w2) = match glue with
          [ [] -> failwith "empty glue"
          | [ -1 :: rest ] -> match before with
               [ [] -> failwith "left arg too short"
               | [ _ (* a *) :: init ] -> (init,rest)
               ]
          | _ -> (before,glue)
          ] in (w1,w2 @ after)
        ] (* match [wr] *)
    ] (* match [wl] *)
;
value ext_sandhi0 wl wr = (* No normalization *) 
  let (w1,w2) = ext_sandhi_pair wl wr in 
  List2.unstack w1 w2 (* w1 is pasted as left context of w2 *)
;
(* Only used in stand-alone module Sandhier; argument is rev of word *)
value final_sandhi = fun
  [ [] -> failwith "Empty input Sandhi"
  | [ last :: rest ] when visargor last 
       -> List.rev [ 16 :: rest ] (* final visarga *)
  | rw -> List.rev (finalize rw)
  ] 
;

(* External sandhi - Reference version - used in [Roots.follow] *)
(* [esandhi : string -> string -> word] *) 
value esandhi left right =
  let wl = List.rev (code left)
  and wr = code right in
  Encode.normalize (ext_sandhi0 wl wr) (* normalization *)
;
(* Unused directly; copied in [Compile_sandhi.match_sandhi] *)
(* [e_sandhi : string -> string -> string] *) 
value e_sandhi left right = decode (esandhi left right)
;
(* Used in [Roots.follow] and [Make_preverbs.preverbs_etym] *)
value pv_sandhi left right = 
  if left="pra" && right="ni" then "pra.ni" (* retroflexion *)
  else e_sandhi left right
and pv_sandhi0 wl wr =
  let rwl = Word.mirror wl in 
  if rwl=code "pra" && wr=code "ni" then code "pra.ni" (* retroflexion *)
  else Encode.normalize (ext_sandhi0 wl wr) (* normalization *)
;
(* tests *)
assert (e_sandhi "vane" "iva" = "vana_iva");
assert (e_sandhi "na" "chinatti" = "nacchinatti"); 
assert (e_sandhi "tat" "zariiram" = "tacchariiram");
assert (e_sandhi "tat" "lebhe" = "tallebhe");
assert (e_sandhi "tat" "zrutvaa" = "tacchrutvaa");
assert (e_sandhi "tat" "jayati" = "tajjayati");
assert (e_sandhi "tat" "mitram" = "tanmitram");
assert (e_sandhi "azvas" "asti" = "azvo'sti"); 
assert (e_sandhi "azvas" "iva" = "azva_iva");
assert (e_sandhi "punar" "iva" = "punariva");
assert (e_sandhi "punar" "suuti" = "puna.hsuuti"); 
assert (e_sandhi "punar" "janman" = "punarjanman");
assert (e_sandhi "api" "avagacchasi" = "apyavagacchasi");
assert (e_sandhi "nanu" "upavizaama.h" = "nanuupavizaama.h");
assert (e_sandhi "ubhau" "aagacchata.h" = "ubhaavaagacchata.h");
assert (e_sandhi "katham" "smarati" = "katha.msmarati");
assert (e_sandhi "sam" "hraad" = "sa.mhraad");
assert (e_sandhi "dvi.t" "hasati" = "dvi.d.dhasati");
assert (e_sandhi "ud" "h.r" = "uddh.r");
assert (e_sandhi "tat" "hema" = "taddhema");
assert (e_sandhi "taan" "tu" = "taa.mstu");
assert (e_sandhi "nara.h" "rak.sati" = "narorak.sati");
assert (e_sandhi "punar" "rak.sati" = "punaarak.sati"); 
assert (e_sandhi "gaayan" "aagacchati" = "gaayannaagacchati"); 
assert (e_sandhi "vaak" "me" = "vaafme");
assert (e_sandhi "vaag" "hasati" = "vaagghasati");
assert (e_sandhi "bahis" "k.r" = "bahi.hk.r"); (* also "bahi.sk.r" *)
assert (e_sandhi ".sa.t" "naam" = ".sa.nnaam"); (* and not ".sa.n.naam" *)
assert (e_sandhi "tat" "namas" = "tannamas"); (* but "tadnamas" also correct *)
assert (e_sandhi "kim" "hmalayati" = "ki.mhmalayati"); (* but "kimhmalayati" also correct *)
assert (e_sandhi "kim" "hnute" = "ki.mhnute"); (* but "kinhnute" also correct (metathesis) *)
assert (e_sandhi "tat" "mitram" = "tanmitram");
assert (e_sandhi "devaan" "z.r.noti" = "devaa~nch.r.noti"); 

(* Remark. [e_sandhi] is used for preverbs, and the existence of 
   *e and *o guarantees that [(external_sandhi x (external_sandhi pre y))]
   is the same as [(external_sandhi (external_sandhi x pre) y)]: *)
(* NB. form "aa|ihi" with *e-phantom generated by Inflected. *)
assert (e_sandhi "iha" "aa|ihi" = "ihehi");  (* e-phantom elim *)
assert (e_sandhi "iha" "aa" = "ihaa");
assert (e_sandhi "ihaa" "ihi" = "ihehi");
(* Idem for *o : fake sandhi "aa" "upa" = "aa|upa") generated by Inflected. *)
assert (e_sandhi "zoka" "aa|rta" = "zokaarta");

(* Context-sensitive irregularities *)

value external_sandhi left right =
  if left = "sas" || left = "sa.h" then
     match code right with 
        [ [] -> "sa.h"
        | [ first :: after ] ->
            e_sandhi (if vowel first then "sa.h" else "sa") right
        ]  
  else e_sandhi left right
;
(* Sandhier version, takes a revword and a word, and returns a word *)
value ext_sandhi rvlword rword =
  let left = match rvlword with
      [ [ 48 :: [ 1; 48 ] ] | [ 16 :: [ 1; 48 ] ] -> match rword with 
              [ [] -> [ 16 :: [ 1; 48 ] ]
              | [ first :: after ] ->
                  if vowel first then [ 16 :: [ 1; 48 ] ] else [ 1; 48 ]
              ]
      | l -> l
      ] in ext_sandhi0 left rword (* does not finalize r or s into .h *)
;
value after_dual_sandhi left right =
  match List.rev (code left)
  with [ [] -> failwith "left arg of sandhi empty"
       | [ last :: _ ] ->
         if last=4 (* ii *) || last=6 (* uu *) || last=10 (* e *)
            then (left ^ "_" ^ right) (* hiatus *)
         else e_sandhi left right
       ]
;
(* tests *)
assert (external_sandhi "sas" "gaja.h" = "sagaja.h");
assert (external_sandhi "sas" "aacaarya.h" = "sa_aacaarya.h");
assert (external_sandhi "sas" "azva.h" = "so'zva.h");
assert (external_sandhi "sas" "" = "sa.h");

assert (after_dual_sandhi "tephale" "icchaama.h" = "tephale_icchaama.h");

(* Also external sandhi does not occur after interjections 
   and is optional after initial vocatives - TODO *)

(*i end; i*)
