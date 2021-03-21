(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Chunking mechanism for guessing partial padapatha from list of chunks. *)
(* Essential for maximum parallelism in segmentation                      *)

(*i module Chunker = struct i*)

value avagraha_expand encode s = 
  match encode s with (* avagraha reverts to a *)
  [ [ -1 :: l ] -> [ 1 :: l ]  (* only initial avagraha reverts to a *)
  | x -> x
  ]
;
(* Preprocessing of corpus to prepare partial padapatha from list of chunks. *)
(* This is extremely important from the segmenter complexity point of view   *)
(* Since it takes hints at parallel treatment from non-ambiguous blanks.     *)

exception Hiatus 
;
exception Glue
;
(* We raise Glue below when there are multiple ways to obtain the current break,
   in which case we do not profit of the sandhi hint. Furthermore, this is 
   incomplete, notably when one of the sandhied forms is a vocative. *)
(* Chunk [w] is adjusted for padapatha in view of next character [c] *)
(* No attempt is made to change [c] and thus "tacchrutvaa" is not chunkable. *)
(* This function defines the maximal separability of devanaagarii into chunks
   but is not always able to go as far as creating the full padapaa.tha *)
value adjust c w = match Word.mirror w with 
  [ [] -> failwith "adjust"
  | [ last :: rest ] -> match last with
        [ 14 (* .m *) -> Word.mirror [ 41 (* m *) :: rest ] (* revert .m to m *)
             (* note: .m coming from sandhi of n is followed by sibilant 
                and chunking is allowed only after this sibilant *)
        | 11 (* ai *) when c = 43  (* r *) -> raise Hiatus 
             (* For ai.h+r -> ai r Whitney§179 never 2 consecutive r *)
             (* Thus "i.s.tai ruupavaan" or "i.s.tai\_ruupavaan"
                But "tasmai raajaa" must be written "tasmairaajaa" *)
             (* NB for 10 (* e *) or 13 (* au *) we do not do the same treatment
                in view of the rare padas ending in es or aus, 
                thus "te rasasaarasafgrahavidhim" is allowed, and 
                gacche.h+raajaa may be written gacche\_raajaa or gaccheraajaa*)
        | 12 (* o *) -> if rest = [ 40 ] (* bh from bhos -> bho *) then 
                           Encode.code_string "bhos" (* "bho raama" "bho bhos" *)
                        else if rest = [ 49; 1 ] (* aho *) then 
                           Encode.code_string "aho" (* "aho raama" *)
                        else if Phonetics.turns_visarg_to_o c || c=1 
                             (* zivoham must be entered as zivo'ham (avagraha) *)
                             then Word.mirror [ 16 :: [ 1 :: rest ] ] 
                             (* restore visarga, assuming original a.h form *)
                        else w 
        | 1 (* a *) -> if c=1 then w else
                       if Phonetics.vowel c then raise Hiatus else w
        | 2 (* aa *) -> if Phonetics.vowel c then raise Hiatus else 
                        if Phonetics.elides_visarg_aa c then raise Hiatus else 
                        w (* Hiatus except c surd unaspirate ? *) 
                        (* NB "punaaramate" but not "punaa ramate" *)
        | 4 (* ii *) (* possible visarga vanishes - but "n.rpatiiraajati" *) 
        | 6 (* uu *) -> (* if c=43 (* r *) then raise Hiatus else *) w
        (* next 4 rules attempt to revert [last] to 'd' in view of [c] *)
        | 34 (* d *) -> if c=35 (* dh *) then raise Glue else 
                        if Phonetics.is_voiced c 
                           then Word.mirror [ 32 :: rest ] (* d -> t *)
                        else w
        | 24 (* j *) -> if Phonetics.turns_t_to_j c (* tat+jara -> tajjara *)
                           then Word.mirror [ 32 :: rest ] (* j -> t *)
                        else w
        | 26 (* ~n *) -> match rest with
             [ [ 26 (* ~n *) :: ante ] -> match ante with
                 (* optional doubling of ~n in front of vowel *)
                 [ [ v :: _ ] -> if Phonetics.short_vowel v && Phonetics.vowel c
                                    then Word.mirror rest 
                                 else failwith "adjust"
                 | _ -> failwith "adjust"
                 ]
             | _ -> if c=23 (* ch could come from ch or z *)
                       then raise Glue 
                    else if Phonetics.turns_n_to_palatal c 
                            (* taan+zaastravimukhaan -> taa~nzaastravimukhaan *)
                            then Word.mirror [ 36 (* n *) :: rest ] (* n -> ~n *)
                         else w 
             ]
        | 29 (* .d *) -> if c=30 (* .dh *) then raise Glue else 
                         if Phonetics.is_voiced c 
                            then Word.mirror [ 27 :: rest ] (* .d -> .t *)
                         else w
        | 39 (* b *) -> if c=40 (* bh *) then raise Glue else 
                        if Phonetics.is_voiced c 
                           then Word.mirror [ 37 :: rest ] (* b -> p *)
                        else w
        | 19 (* g *) -> if c=20 (* gh *) then raise Glue else 
                        if Phonetics.is_voiced c (* vaak+vazya *)
                           then Word.mirror [ 17 :: rest ] (* g -> k *)
                        else w
        | 36 (* n *) -> match rest with
             [ [ 36 (* n *) :: ante ] -> match ante with
                   (* optional doubling of n in front of vowel *)
                 [ [ v :: _ ] -> if Phonetics.short_vowel v && Phonetics.vowel c
                                    then Word.mirror rest (* gacchann eva *)
                                 else w
                 | _ -> failwith "adjust"
                 ]
             | _ -> if c=36 (* n *) || c=41 (* m *)
                       then raise Glue (* since d|m->nn and n|m -> nm *)
                         (* Word.mirror [ 32 :: rest ] (* n -> t *) *)
                         (* incompleteness: raajan naasiin vocatif raajan *)
                    else w
             ]
        | 22 (* c *) -> if c=22 then Word.mirror [ 32 :: rest ] (* c -> t *)
                        else if c=23 || c=46 (* ch could come from ch or z *)
                             then raise Glue else w
        | 44 (* l *) -> if c=last 
                           then Word.mirror [ 32 :: rest ] (* l -> t *)
                        else w
        | 21 (* f *) -> match rest with
             [ [ 21 (* f *) :: ante ] -> match ante with
                   (* optional doubling of f in front of vowel *)
                 [ [ v :: _ ] -> if Phonetics.short_vowel v && Phonetics.vowel c
                                    then Word.mirror rest 
                                 else failwith "adjust"
                 | _ -> failwith "adjust"
                 ]
             | _ -> if c=41 (* m *) (* vaak+mayi *)
                       then Word.mirror [ 17 :: rest ] (* f -> k *)
                    else w
             ]
        (* NB if last is y, r or v and c is vowel, then it may come from resp. 
           {i,ii}, {.r,.rr}, {u,uu} and this choice means that we cannot make 
           a chunk break here *)
        | 42 (* y *) | 45 (* v *) -> if Phonetics.vowel c then raise Glue 
                                     else w (* will fail *)
        | 43 (* r *) -> if Phonetics.turns_visarg_to_o c || Phonetics.vowel c 
                           then Word.mirror [ 16 :: rest ] (* visarg restored *)
                        else w (* pb punar pitar etc *)
        | 46 (* z *) -> match rest with
              [ [ 14 (* .m *) :: b ] -> if c=22 || c=23 (* c ch *) then
                                           Word.mirror [ 36 (* n *) :: b ] 
                                        else w 
              | [ 26 (* ~n *) :: _ ] -> if c=46 (* z *) then
                                           Word.mirror [ 36 (* n *) :: rest ] 
                                        else w 
                                        (* c=23 (* ch *) could come from z *)
              | _ -> if c=22 || c=23 (* c ch *) then
                     Word.mirror [ 16 (* .h *) :: rest ] else w
              ]
        | 47 (* .s *) -> match rest with
              [ [ 14 (* .m *) :: b ] -> if c=27 || c=28 (* .t .th *) then
                                           Word.mirror [ 36 (* n *) :: b ]
                                        else w 
              | _ -> w
              ]
        | 48 (* s *) -> match rest with
              [ [ 14 (* .m *) :: b ] -> if c=32 || c=33 (* t th *) then
                                           Word.mirror [ 36 (* n *) :: b ] 
                                        else w 
              | _ -> w
              ]
        | _ -> w
        ]
  ] 
;
(* Called from [Sanskrit.read_processed_skt_stream] for use in [read_sanskrit]
   with argument [read_chunk=sanskrit_chunk encode] *)
value chunker read_chunk l = (* l is list of chunks separated by blanks   *)
                             (* returns list of chunks in terminal sandhi *)
  let rec pad_rec = fun (* returns (c,l) with c first char of first pada in l *)
    [ [] -> (-1,[])
    | [ chk :: chks ] -> 
      let (c,padas) = pad_rec chks 
      and w = read_chunk chk (* initial avagraha reverts to a *) in 
      (List.hd w (* next c *), 
       try let pada = if c=(-1) then w (* last chunk *)
                      else adjust c w in
           [ pada :: padas ]
       with 
        [ Hiatus -> match padas with 
          [ [] -> failwith "chunker"
          | [ p :: lp ] -> let conc = w @ [ 50 :: p ] in (* [w_p] *)
                           [ conc :: lp ] (* hiatus indicates a word boundary *)
          ]
        | Glue -> match padas with 
          [ [] -> failwith "chunker"
          | [ p :: lp ] -> let conc = w @ p in
                           [ conc :: lp ] (* we lose the boundary indication *)
          ]
        ])
    ] in
  let (_,padas) = pad_rec l in padas
;

(*i end; i*)
