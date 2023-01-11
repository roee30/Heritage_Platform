(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Int_sandhi = struct i*)

(* This module defines internal sandhi operations used in morphology computations
   The code is complex - do not change without extensive tests. *)

open Phonetics; (* asp finalize visarg *)
open Canon; (* decode rdecode *)

value code str = Encode.code_string str
and mirror = Word.mirror
;
(* Retroflexion of s: for all s in w : l = w1 s w2 with w2 not empty and not 
   starting with r, look back in w1 skipping [c] such that [retrokeeps(c)]; 
   if [retroacts(c)] found then s {\R} {\d s}
   and if w2 starts with (t, th, n) then this letter becomes retroflex too. *)

value retroacts c = 
  c=17 (* k *) || c=43 (* r *) || (vowel c && c>2 && not (c=9 (* {\d l} *)))
;
value retrokeeps c = anusvar c || visarga c (* {\d h} *)
;
value rec retros = fun
  [ [] -> False
  | [ c :: l ] -> retroacts c || (retrokeeps c && retros l)
  ]
;
value rec inspects accu = fun
  [ [] -> mirror accu
  | [ c ] -> mirror [ c :: accu ]
  | [ 48 (* s *) :: [ 43 (* r *) :: l ] ] -> inspects [ 43 :: [ 48 :: accu ] ] l
  | [ 48 (* s *) :: l ] -> 
    if retros accu then match l with
       [ [] -> failwith "Illegal arg to accu"
       | [ 32 (* t *) :: r ] -> 
              inspects [ 27 (* {\d t} *) :: [ 47 (* {\d s} *) :: accu ] ] r
       | [ 33 (* th *) :: r ] -> 
              inspects [ 28 (* {\d t}h *) :: [ 47 (* {\d s} *) :: accu ] ] r
       | [ 36 (* n *) :: r ] -> 
              inspects [ 31 (* {\d n} *) :: [ 47 (* {\d s} *) :: accu ] ] r
       | l -> inspects [ 47 (* {\d s} *) :: accu ] l
       ]
    else inspects [ 48 (* s *) :: accu ] l
  | [ c :: l ] -> inspects [ c :: accu ] l
  ]
;
value retroflexs l = inspects [] l
;
(* Retroflexion of n: for all n in w : l = w1 n w2 with w2 not empty and 
   starting with [enabling(c)], look back in w1 skipping [c];
   if [retrokeepn(c)] and if [retroactn(c)] found
   then n {\R} {\d n} and if w2 starts with n if becomes {\d n} too. *)

value retroactn c = rivarna c || c=43 (* r *) || c=47 (* {\d s} *)
;
value retrokeepn c = 
  velar c || labial c || vowel c || anusvar c 
          || c=42 (* y *) || c=45 (* v *) || c=49 (* h *)
;
value rec retron = fun
  [ [] -> False
  | [ c :: rest ] -> retroactn c || (retrokeepn c && retron rest)
  ]
;
(* uses \Pan{8,3,24} *)
value enabling c =  vowel c || c=36 || c=41 || c=42 || c=45 (* n m y v *)
;
value retrn c = if c=36 then 31 (* n {\R} {\d n} *) else c
;
value rec inspectn accu = fun
  [ [] -> mirror accu
  | [ c ] -> mirror [ c :: accu ]
  | [ 36 (* n *) :: [ c :: l ] ] -> 
      if enabling c && retron accu then 
         inspectn [ retrn c :: [ 31 (* {\d n} *) :: accu ] ] l
      else inspectn [ 36 :: accu ] [ c :: l ]
  | [ c :: l ] -> inspectn [ c :: accu ] l
  ]
;
value retroflexn w = inspectn [] w
;
value ortho_code w = retroflexn (retroflexs w)
;
value ortho s = decode (ortho_code (code s))
;
(* Test examples *)
assert (ortho "nisanna" = "ni.sa.n.na");
assert (ortho "pranamati" = "pra.namati");
assert (ortho "parinindati" = "pari.nindati"); (* could be "parinindati" *)
assert (ortho "gurusu" = "guru.su");

(* Exceptions: padas not ortho *)
assert (ortho "visarpati" = "vi.sarpati"); (* should be "visarpati" *)
(* Following due to non-IE origin of stem ? *)
assert (ortho "kusuma" = "ku.suma"); (* but "kusuma" correct *)
assert (ortho "pustaka" = "pu.s.taka"); (* but "pustaka" correct *)

(* Note ortho does not transform final "s" or "r" into visarga *)

(* Homonasification necessary for present class 7 ~nk->fk *)
(* Also (very rare) normalisation of anusvara *)
value homonase c l = match l with
  [ [ 14 (* .m *) :: r ] when stop c -> [ c :: [ homonasal c :: r ] ] 
  | [ 26 (* ~n *) :: r ] when velar c -> [ c :: [ 21 (* f *) :: r ] ]
  | _ -> [ c :: l ]
  ]                  
;
(* Local combination of retron and retros, together with homonasification *)
value rec retro_join left = fun
  [ [] -> mirror left
  | [ c ] -> mirror (homonase c left)
  | [ 36 (* n *) :: [ c :: l ] ] -> 
      if enabling c && retron left then 
           retro_join [ retrn c :: [ 31 (* {\d n} *) :: left ] ] l
      else retro_join [ 36 :: left ] [ c :: l ]
  | [ 48 (* s *) :: [ 43 (* r *) :: l ] ] -> 
           retro_join [ 43 :: [ 48 :: left ] ] l
  | [ 48 (* s *) :: l ] -> 
    if retros left then match l with
      [ [] -> failwith "Illegal arg to retro_join"
      | [ 32 (* t *) :: r ] -> 
             retro_join [ 27 (* {\d t} *) :: [ 47 (* {\d s} *) :: left ] ] r
      | [ 33 (* th *) :: r ] -> 
             retro_join [ 28 (* {\d t}h *) :: [ 47 (* {\d s} *) :: left ] ] r
      | [ 36 (* n *) :: r ] -> 
             retro_join [ 31 (* {\d n} *) :: [ 47 (* {\d s} *) :: left ] ] r
      | l -> retro_join [ 47 (* {\d s} *) :: left ] l
      ]                  
    else retro_join [ 48 :: left ] l
  | [ c :: l ] -> retro_join (homonase c left) l
  ]
;
(* sandhi of -s and -.h *)
value sglue first = fun
  [ [ 1 :: _ ] -> [ -1; 12; first ] (* as -> o *)  
  |  _ -> [ 48; first ] (* keep s *)
  ]
and sglue1 first _ = [ 48; first ] (* keep s *)
;
(* Restore main phoneme from finer distinction. *)
(* We unprime a primed phoneme by modulo 100 *)
(* Codes 124, 149 and 249 ought to disappear if phonemic features introduced *)
value restore = fun 
  [ 124 -> 24 (* restores j' {\R} j *)
  | 149 | 249 -> 49 (* restores h' {\R} h and idem h'' *)
  | c -> c
  ]
;
(* Its extension to (reversed) words *)
value restore_stem = fun
  [ [ c :: r ] -> [ restore c :: r ]
  | [] -> []
  ]
;
(* Change of final consonant before consonant in internal sandhi *)
(* Gonda §19-II is not quite clear, so we keep a minimum rewrite set. *)
(* What is missing is the removal of all final consonants but one - eg vrazc *)
value cons_cons = fun 
  [ 22 (* c *) | 23 (* ch *) | 24 (* j *) | 25 (* jh *) | 46 (* \'s *) 
                 -> 17 (* k *) (* but sometimes {\d t} like in finalize *)
  | 124 (* j' *) -> 47 (* {\d s} *) 
  | 149 (* h' *) -> 49 (* h *) 
  | 26 (* \~n *) -> 21 (* \.n *)
  | 34 (* d *)   -> 32 (* t *)
  | 35 (* dh *) | 249 (* h'' *) -> 33 (* th *)
  | c            -> c
  ]
;
(* Error messages *)
value illegal_left w = 
  let mess = "Left arg of sandhi end illegal in " ^ (rdecode w) in
  failwith mess
and illegal_right w = 
  let mess = "Right arg of sandhi beginning illegal in " ^ (decode w) in 
  failwith mess
and too_short () = failwith "Left arg of int_sandhi too short"
;
(* Internal sandhi - wl is mirror of code of left string,
                     wr is code of right string.
   Result is code after internal sandhi at their junction. 
   This is a deterministic function.
   Optional rules have to be encoded elsewhere. *)
value int_sandhi wl wr = try
  match wl with 
    [ [] -> (* eg "ap" *) wr
    | [ last :: before ] -> match wr with 
        [ [] -> mirror (finalize wl)
        | [ first :: after ] -> 
          if vowel last then
             if vowel first then 
                let glue =
(* glue is the string replacing [last; first] with a special convention:
 when it starts with -1, it means the last letter (an "a") of [before] is erased,
 and when it starts with -2, it means the last letter (a vowel) of [before] 
 is lengthened *)
          if savarna last first then [ long last ] 
          else if avarna last then 
                  if ivarna first then [ 10 ] (* e *)
                  else if uvarna first then [ 12 ] (* o *)
                  else match first with
                       [ 7 -> [ 1; 43 ] (* ar *)
                       | 10 | 11 -> [ 11 ]   (* ai *)
                       | 12 | 13 -> [ 13 ]   (* au *)
                       | _ -> failwith ".rr or .l initial"
                       ]
               else if ivarna last then [ 42; first ]      (* y *)
                    (* but zrii+as=zriyas \Pan{6,4,77} *)
               else if uvarna last then [ 45; first ]      (* v *)
                    (* but bhuu+aadi=bhuuvaadi not bhvaadi irregular? *)
               else if last=7 || last=8 (* .r .rr *) then [ 43; first ] (* r *)
               else (* last diphthong *)
                       match last with
                       [ 10 (* e *)  -> [ 1; 42; (* ay *) first ] 
                       | 11 (* ai *) -> [ 2; 42; (* \=ay *) first ] 
                       | 12 (* o *)  -> [ 1; 45; (* av *) first ] 
                       | 13 (* au *) -> [ 2; 45; (* \=av *) first ] 
                       | _ -> illegal_left wl
                       ]
             (* let glue ... *) in
                retro_join before (glue @ after)
          else (* first consonant, last vowel *) match first with
               [ 23 (* ch *) when short_vowel last -> 
                  (mirror wl) @ [ 22 :: wr ] (* cch *) 
               | 42 (* y *) -> 
                  let split = match last with (* \Pan{6,1,79} *)
                              [ 12 (* o *)  -> [ 45; 1 ] (* av *)
                              | 13 (* au *) -> [ 45; 2 ] (* aav *)
                              | c -> [ c ] (* e or ai included *)
                              ] in 
                  retro_join (split @ before) wr
               | _ -> retro_join wl wr
               ]
          else (* consonant last *) (* should be analysed further *)
       if wr = [ 32 ] (* t *) then (* ad hoc *)
         let wl' = if visarg last (* s {\d h} *) then 
                      [ 32 :: before ] (* a\'s\=at impft \'s\=as *) (* *azaa.h *)
                   else finalizer wl in 
         mirror wl'
       else if all_consonants wr then mirror (finalizer wl)
       else if vowel first then retro_join [ restore last :: before ] wr 
                                (* j' {\R} j \& h' {\R} h *)
                 (* no doubling of \.n or n for internal sandhi 
                    no change of consonants before vowels, even ch/cch *)
       else (* both consonant *) let glue = match first with 
     [ 17 | 18 (* k kh *) ->
               match cons_cons last with
             [ 41      -> [ 36; first ] (* m+k {\R} nk *) (* Gonda §19-VIII *)
             | 48      -> [ 16; first ] (* s+k {\R} .hk could also be .sk [ 47; first ] *) 
             | 39 | 40 -> [ 37; first ] (* b bh {\R} p *) 
             | 33      -> [ 32; first ] (* th {\R} t *)
             | c       -> [ c; first ]
             ]
     | 19 | 20 (* g gh *) -> 
                   if visarg last then sglue first before
                   else match cons_cons last with
             [ 41 -> [ 36; first ] (* m+g {\R} ng *) (* Gonda §19-VIII *)
             | c  -> [ voiced c; first ]
             ]
     | 22 | 23 (* c ch *) -> match cons_cons last with 
             [ 41 -> [ 36; first ] (* m+c {\R} nc *) (* Gonda §19-VIII *)
             | 32 | 34 -> [ 22; first ] (* t+c {\R} cc, d+c {\R} cc *)
             | 33 -> [ 32; first ] (* th {\R} t *)
             | 36 -> [ 14; 46; first ] (* n+c {\R} {\d m}\'sc *)
             | 39 | 40 -> [ 37; first ] (* b bh {\R} p *) 
             | c  -> [ if visarg c then 46 (* \'s *) else c; first ]
             ]
     | 24 | 25 (* j jh *) -> 
                 if visarg last then sglue first before
                 else match cons_cons last with
             [ 41 -> [ 36; first ] (* m+j {\R} nj *) (* Gonda §19-VIII *)
             | 32 -> [ 24; first ] (* t+j {\R} jj *) 
             | 36 -> [ 26; first ] (* n+j {\R} \~nj *)
             | c  -> [ voiced c; first ] (* k+j {\R} gj ? *)
             ]
     | 36 (* n *) -> if visarg last then sglue1 first before (* {\d h}n {\R} rn {\R} r{\d n} *)
                     else match last with
             [ 41        -> [ 36; 36 ] (* m+n {\R} nn *) (* Gonda §19-VIII *)
             | 22        -> [ 22; 26 ] (* c+n {\R} c\~n *) (* Gonda §19-IX *)
             | 24 | 124  -> [ 24; 26 ] (* j+n {\R} j\~n *) (* Gonda §19-IX *)
             | 149 | 249 -> [ 49; 36 ] (* h'+n {\R} h+n same h'' *)
             | c         -> [ c; 36 ] (* no other change - Gonda §19-I (except retroflexion e.g. v.rk.na) *)
             ]
     | 37 | 38 (* p ph *) ->
               match cons_cons last with
             [ 33      -> [ 32; first ] (* th {\R} t *)
             | 39 | 40 -> [ 37; first ] (* b bh {\R} p *) 
             | c       -> [ if visarg c then 16 else c; first ]
             ]
     | 39 | 40 (* b bh *) -> if visarg last then sglue first before
                             else match cons_cons last with
             [ c  -> [ voiced c; first ] ]
     | 41 (* m *) -> if visarg last then sglue1 first before (* {\d h}m {\R} rm *)
                     else match last with
             [ 41 -> [ 36; 41 ] (* m+m {\R} nm *) (* Gonda §19-VIII *)
             | _  -> [ restore last; first ] (* no change Gonda §19-I *) 
             ] 
     | 42 (* y *) -> if visarg last then sglue1 first before (* {\d h}y {\R} ry *)
                     else [ restore last; first ] 
     | 43 (* r *) -> if visarg last then match before with
                        [ [ 3 :: [ 36 :: _ ] ] (* nis-r *) -> [ -1 ; 4 ; 43 ] (* n{\=\i}r *)
                        | _ -> [ restore last; first ]
                        ]
                     else match last with
             [ 41 -> [ 36; 43 ] (* m+r {\R} nr *) (* Gonda §19-VIII *)
             | _  -> [ restore last; first ] (* no other change Gonda §19-I *)
             ] 
     | 44 (* l *) -> if visarg last then sglue1 first before (* {\d h}l {\R} rl *)
                     else match last with
             [ 41 -> [ 36; 44 ] (* m+l {\R} nl *) (* Gonda §19-VIII *)
             | _  -> [ restore last; first ] (* no other change Gonda §19-I *) 
             ] 
     | 45 (* v *) -> if visarg last then sglue1 first before (* {\d h}v {\R} rv *)
                     else match last with
             [ 41 -> [ 36; 45 ] (* m+v {\R} nv *) (* Gonda §19-VIII *)
             | _  -> [ restore last; first ] (* no other change Gonda §19-I *)
             ] 
     | 46 (* \'s *) -> match cons_cons last with
             [ 32 | 33 -> [ 22; 23 ] (* t+\'s {\R} cch *) 
             | 36 | 41 -> [ 14; 46 ] (* n,m+\'s {\R} {\d m}\'s *) (* Gonda §19-VIII *)
             | 39 | 40 -> [ 37; 46 ] (* b bh {\R} p *) 
             | 48      -> [ 16; 46 ] (* s+\'s {\R} {\d h}\'s *)
             | c       -> [ c; first ]
             ]
     | 47 (* {\d s} *) ->
               match cons_cons last with
             [ 36 | 41 -> [ 14; 47 ] (* n,m+{\d s} {\R} {\d m}{\d s} *) (* Gonda §19-VIII *)
             | 48      -> [ 16; 47 ] (* s+{\d s} {\R} {\d h}{\d s} *)
             | 33      -> [ 32; 47 ] (* th {\R} t *)
             | 39 | 40 -> [ 37; 47 ] (* b bh {\R} p *) 
             | 24      -> [ 17; 47 ] (* j+{\d s} {\R} k{\d s} *) 
             | c       -> [ c; first ]
             ]
     | 48 (* s *) -> 
               match cons_cons last with
             [ 36 | 41 -> [ 14; 48 ] (* n,m+s {\R} {\d m}s *) (* Gonda §19-VIII *)
             | 47 -> match before with 
               [ [ 17 :: _ ] -> [ 47 ] (* k{\d s}+s {\R} k{\d s} *) 
               | _ -> [ 17; 47 ] (* {\d s}+s {\R} k{\d s} *) (* Gonda §19-VI *)
               ]
             | 48 -> match before with (* horrible glitch *)
               [ []    -> [ 48 ] (* se 2 sg pm as\#1 *)
               | [ 2 ] -> [ 48; 48 ] (* \=asse 2 sg pm \=as\#2 *)
               | _     -> [ 16; 48 ] (* {\d h}s *)
               ]
             | 19 | 20 | 49 -> [ 17; 47 ] (* g,h+s {\R} k{\d s} : lek{\d s}i dhok{\d s}i *) 
             | 249 
             | 33  -> [ 32; 48 ] (* th {\R} t  h''+s {\R} ts natsyati*)
             | 39 | 40 -> [ 37; 48 ] (* b bh {\R} p *) 
             | 17 -> [ 17; 47 ] (* yuj yu\~nk+se {\R} yu\.nk{\d s}e *)
             | c  -> [ c; first ]
             ]
     | 29 | 30 (* {\d d} {\d d}h *) ->
                   if visarg last then sglue first before
                   else match cons_cons last with
             [ 41 -> [ 36; first ] (* m+{\d d} {\R} n{\d d} *) (* Gonda §19-VIII *)
             | 32 -> [ 29; first ] (* t+{\d d} {\R} {\d d}{\d d} *) 
             | 36 -> [ 31; first ] (* n+{\d d} {\R} {\d n}{\d d} *)
             | c  -> [ voiced c; first ]
             ]
     | 34 (* d *) -> if visarg last then sglue first before
                     else match cons_cons last with
             [ 41 -> [ 36; first ] (* m+d {\R} nd *) (* Gonda §19-VIII *)
             | 47 -> [ 29; 29 ] (* {\d s}+d {\R} {\d d}{\d d}  ? *)
             | c  -> [ voiced c; if lingual c then 29 (* {\d d} *) else 34 ]
             ]
     | 35 (* dh *) -> if visarg last then sglue first before
                      else match last with
             [ 32 | 33 | 35 -> [ 34; 35 ] (* dh+dh {\R} ddh *)(* Gonda §19-III *)
             | 41 -> [ 36; 35 ] (* m+dh {\R} ndh *) (* Gonda §19-VIII *)
             | 49 -> [ -2; 30 ] (* h+dh {\R} {\d d}h *) (* Gonda §19-VII *) 
             | 22 | 23 | 149 -> [ 19; 35 ] (* c+dh {\R} gdh - dugdhve, vagdhi *)
             | 249 -> [ 34; 35 ] (* h''+dh {\R} ddh - naddhaa *)
             | 24 -> [ 19; 35 ] (* j+dh {\R} gdh *)(* yu\.ngdhi *)
             | 47 -> match before with 
               [ [ 17 :: _ ] -> [ -1; 29; 30 ] (* k{\d s}+dh {\R} {\d d}{\d d}h - ca{\d d}{\d d}hve *) 
               | _ -> [ 29; 30 ] (* {\d s}+dh {\R} {\d d}{\d d}h *) 
               ]
             | 46 | 124 -> [ 29; 30 ] (* \'s+dh {\R} {\d d}{\d d}h id. j' *) 
             | c  -> [ voiced c ; if lingual c then 30 (* {\d d}h *) else 35 ]
             ] 
     | 32 (* t *) -> match last with 
             [ 41 -> [ 36; 32 ] (* m+t {\R} {\d m}t = nt *) (* Gonda §19-VIII *)
             | 20 | 149 -> [ 19; 35 ] (* gh+t {\R} gdh *) (* Gonda §19-III *)
             | 19 | 22 | 24 -> [ 17; 32 ] (* g+t {\R} kt *) (* \Pan{8,4,54} *)
                      (* id c+t {\R} kt *) (* Gonda §19-V ? *)
                      (* id j+t {\R} kt *) (* yukta anakti bhunakti *) 
             | 23 ->  match before with 
               [ [ 22 :: _ ] -> [ -1; 47; 27 ] (* cch+t {\R} {\d s}{\d t}  eg p.r{\d s}{\d t}a *) 
               | _ -> [ 47; 27 ] (* ch+t {\R} {\d s}{\d t} *) (* ? *)
               ]
             | 25 -> [ 24; 35 ] (* jh+t {\R} jdh *) (* Gonda §19-III *)
             | 27 | 29 -> [ 27; 27 ] (* {\d t}+t {\R} {\d t}{\d t}  {\d d}+t {\R} {\d t}{\d t} *) 
             | 28 -> [ 27; 28 ] (* {\d t}h+t {\R} {\d t}{\d t}h *) 
             | 30 -> [ 29; 30 ] (* {\d d}h+t {\R} {\d d}{\d d}h *) (* Gonda §19-III ? *)
             | 33 -> [ 32; 33 ] (* th+t {\R} tth *)
             | 34 -> [ 32; 32 ] (* d+t {\R} tt *) 
             | 35 | 249 -> [ 34; 35 ] (* dh+t {\R} ddh *) (* Gonda §19-III *)
             | 38 -> [ 37; 33 ] (* ph+t {\R} pth *) 
             | 39 -> [ 37; 32 ] (* b {\R} p *) 
             | 40 -> [ 39; 35 ] (* bh+t {\R} bdh *) (* Gonda §19-III *)
             | 46               (* \'s+t {\R} {\d s}{\d t} *)
             | 124 -> [ 47; 27 ] (* j'+t {\R} {\d s}{\d t}  eg m{\d r}j {\R} m\=ar{\d s}{\d t}i *) 
             | 47 -> match before with 
               [ [ 17 :: _ ] -> [ -1; 47; 27 ] (* k{\d s}+t {\R} {\d s}{\d t}  eg ca{\d s}{\d t}e *) 
               | _ -> [ 47; 27 ] (* {\d s}+t {\R} {\d s}{\d t} *) (* Gonda §19-V *)
               ]
             | 49 -> [ -2; 30 ] (* h+t {\R} {\d d}h *) (* Gonda §19-VII *) 
             | c  -> [ if visarg c then 48 (* s *) else c; first ]
             ]
     | 33 (* th *) -> match last with 
             [ 41 -> [ 36; first ] (* m+th {\R} {\d m}th = nth *)(* Gonda §19-VIII *)
             | 149              (* h'+t {\R} gdh *) 
             | 20 -> [ 19; 35 ] (* gh+th {\R} gdh *) (* Gonda §19-III *)
             | 22 | 23 -> [ 17; 33 ] (* c+th {\R} kth *) (* Gonda §19-V *)
             | 24 -> [ 17; 33 ] (* j+th {\R} kth *) 
             | 25 -> [ 24; 35 ] (* jh+th {\R} jdh *) (* Gonda §19-III *)
             | 27 | 28 | 29 -> [ 27; 28 ] (* {\d t}(h)+th {\R} {\d t}{\d th}  {\d d}+th {\R} {\d t}{\d th} *) 
             | 30 -> [ 29; 30 ] (* {\d d}h+th {\R} {\d d}{\d d}h *) (* Gonda §19-III ? *)
             | 33               (* th+th {\R} tth *)
             | 34 -> [ 32; 33 ] (* d+th {\R} tth *) (* ? *)
             | 35 | 249 -> [ 34; 35 ] (* dh+th {\R} ddh *) (* Gonda §19-III *)
             | 39 -> [ 37; 33 ] (* b {\R} p *) 
             | 40 -> [ 39; 35 ] (* bh+th {\R} bdh *) (* Gonda §19-III *)
             | 124              (* j'+th {\R} {\d s}{\d t}h eg iya{\d s}{\d t}ha *) 
             | 46 -> [ 47; 28 ] (* \'s+th {\R} {\d s}{\d t}h *)
             | 47 -> match before with 
               [ [ 17 :: _ ] -> [ -1; 47; 28 ] (* k{\d s}+th {\R} {\d s}{\d t}h *) 
               | _ -> [ 47; 28 ] (* {\d s}+th {\R} {\d s}{\d t}h *) (* Gonda §19-V *)
               ]
             | 49 -> [ -2; 30 ] (* h+th {\R} {\d d}h *) (* Gonda §19-VII *) 
             | c  -> [ if visarg c then 48 else restore c; first ]
             ]
     | 27 | 28 (* {\d t} {\d t}h *) -> match cons_cons last with 
             [ 41      -> [ 36; first ] (* m+{\d t} {\R} n{\d t} *) (* Gonda §19-VIII *)
             | 32 | 33 -> [ 27; first ] (* t+{\d t} {\R} {\d t}{\d t}  d+{\d t} {\R} {\d t}{\d t} *)
             | 36      -> [ 14; 47; first ] (* n+{\d t} {\R} {\d m}{\d s}{\d t} *)
             | 39 | 40 -> [ 37; first ] (* b bh {\R} p *) 
             | c       -> [ if visarg c then 47 else c; first ]
             ]
     | 49 (* h *) ->
         if visarg last then sglue first before
         else match cons_cons last with
             [ 17      -> [ 19; 20 ] (* k+h {\R} ggh *)
             | 27      -> [ 29; 30 ] (* {\d t}+h {\R} {\d d}{\d d}h *)
             | 32 | 33 -> [ 34; 35 ] (* t+h {\R} ddh  d+h {\R} ddh *)
             | 37      -> [ 39; 40 ] (* p+h {\R} bbh *)
             | 41      -> [ 36; 49 ] (* m+h {\R} nh *) (* Gonda §19-VIII *)
             | c       -> [ c; 49 ]
             ]
     | _ -> illegal_right wr
     ]  (* let glue *) in 
        let (w1,w2) = match glue with
          [ [] -> failwith "empty glue"
          | [ -1 :: rest ] -> match before with
             [ [] -> too_short ()
             | [ _ (* a *) :: init ] -> (init, rest @ after)
             ] (* as {\R} o *)
          | [ -2 :: rest ] -> match before with
             [ [] -> too_short ()
             | [ 7 (* {\d r} *) :: init ] -> (before, rest @ after) 
             | [ c :: init ] -> (w, rest @ after) 
                 where w = if vowel c then [ long c :: init ] (* guu.dha *)
                           else before (* rara{\d m}h+tha {\R} rara{\d m}{\d d}ha *) 
             ] (* Gonda §19-VII *)
          | _ -> (before, glue @ after)
          ] in retro_join w1 w2
        ] (* match wr *)
    ] (* match wl *)
with [ Failure s -> failwith mess
       where mess = s ^ " in int_sandhi of " ^ (rdecode wl) ^ "&" ^ (decode wr) ]
;
value internal_sandhi left right =
  decode (int_sandhi (mirror (code left)) (code right))
;

(* tests *)
assert (internal_sandhi "ne" "ati" = "nayati"); 
assert (internal_sandhi "budh" "ta" = "buddha"); 
assert (internal_sandhi "rundh" "dhve" = "runddhve"); 
assert (internal_sandhi "d.rz" "ta" = "d.r.s.ta");
assert (internal_sandhi "dvi.s" "ta" = "dvi.s.ta");
assert (internal_sandhi "dvi.s" "dhvam" = "dvi.d.dhvam"); 
assert (internal_sandhi "han" "si" = "ha.msi");
assert (internal_sandhi "labh" "sye" = "lapsye"); (* I will take *)
assert (internal_sandhi "yaj" "na" = "yaj~na");
assert (internal_sandhi "han" "ka" = "hanka"); 
assert (internal_sandhi "gam" "va" = "ganva");
assert (internal_sandhi "lih" "ta" = "lii.dha");
assert (internal_sandhi "manas" "su" = "mana.hsu");
assert (internal_sandhi "jyotis" "stoma" = "jyoti.h.s.toma");
assert (internal_sandhi "manas" "bhis" = "manobhis");
assert (internal_sandhi "bhas" "ya" = "bhasya"); 
assert (internal_sandhi "bho" "ya" = "bhavya");
assert (internal_sandhi "sraj" "su" = "srak.su"); 
assert (internal_sandhi "yuj" "ta" = "yukta"); 
assert (internal_sandhi "yu~nj" "te" = "yufkte"); 
assert (internal_sandhi "tad" "" = "tat"); 
assert (internal_sandhi "vid" "aam" = "vidaam"); 
assert (internal_sandhi "nis" "rasa" = "niirasa"); 
assert (internal_sandhi "hi.ms" "aa" = "hi.msaa"); (* not hi.m.saa *)
assert (internal_sandhi "praa~nc" "s" = "praaf");
let adoh = duhify (Encode.rev_code_string "adoh") in 
assert (decode (int_sandhi adoh (code "t")) = "adhok");  (* she milked - not "adho.t" *) 

(* Not fully correct - still to be improved 
   Special cases - to be accommodated at proper point in the derivation 
   cf. Macdonell §60 footnote 1 p 26
d is assimilated before primary suffix -na: ad+na -> anna
t and d are assimilated before secondary suffixes -mat and -maya: vidyunmat m.rnmaya *)

(*i end; i*)
