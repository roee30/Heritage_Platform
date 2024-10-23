(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Gérard Huet & Pawan Goyal                       *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This segmenter is inspired from old module Segmenter, but uses a graph 
   structure for the sharing of phased segments given with their offset. *)

open List2; (* unstack ass subtract *)
open Auto.Auto; (* auto rule choices State *)

(* used by Interface : [Viccheda = Segment Phases Machine Segment_control] 
      where [Machine = Dispatch Transducers Lemmas] 
      where [Lemmas = Load_morphs.Morphs Prel Phases] *)
module Segment
  (Phases: sig  
         type phase
         and phases = list phase;
         value unknown : phase;
         value aa_phase : phase -> phase;
         value preverb_phase : phase -> bool; 
         value ii_phase : phase -> bool;
         value un_lopa : phase -> phase;
         end)
  (Eilenberg: sig (* To be instanciated by Dispatcher *)
         value transducer : Phases.phase -> auto; 
         value initial : Phases.phases;
         value dispatch : Word.word -> Phases.phase -> Phases.phases;
         value accepting : Phases.phase -> bool;
         type input = Word.word (* input sentence represented as a word *)
         and transition = (* junction relation *)
            [ Euphony of rule (* [(w,rev u,v)] such that [u|v -> w] *)
            | Id              (* identity or no sandhi *)
            ]
         and segment = (Phases.phase * Word.word * transition)
         and output = list segment; 
         value validate : output -> output; (* consistency check *) 
         value sanitize_sa : bool -> output -> output;
         value check_id_sandhi : Word.word -> int -> bool;
         value accrue : segment -> output -> output;
         value access : Phases.phase -> Word.word -> option (auto * Word.word);
         end)
  (Control: sig value star : ref bool; (* chunk= if star then word+ else word *)
            end) 
  = struct

open Phases;
open Eilenberg;
open Control; (* star *)

(* The summarizing structure sharing sub-solutions *)
(* It represents the union of all solutions *)

(* 859 attested as last sentence in Pancatantra *)
value max_input_length = 1000
and max_seg_rows = 1000 
;
exception Overflow (* length of sentence exceeding array size *)
;
(* segments of a given phase *)
type phased_segment = (phase * list (Word.word * list Word.word)) 
                                 (* (segment, mandatory prefixes of following segment) *)
and segments = list phased_segment (* partially forgetting sandhi *)
;
value null = ([] : segments) (* initialisation of graph entry *)
and null_visual = ([]: list (Word.word * list Word.word * phase * int))
                       (* [ (word, v's of next segment, phase, offset) ] *)
and null_visual_conf =  ([]: list (Word.word * phase * int * bool))
                             (* [ (word, phase, offset, is_conflicting) ] *)
;
(* This is the graph on padas of the union of all solutions *)
(* We guarantee that every arc of the graph belongs to at least one bona fide
   segmentation. But every path in this graph is not a valid segmentation. 
   A path must pass global sandhi verification to qualify as valid. *)
(* NB. Valid segmentations may contain unrecognized segments. *)
value graph = Array.make max_input_length null (* global over chunks *)
and visual  = Array.make max_seg_rows null_visual
and visual_conf  = Array.make max_seg_rows null_visual_conf
and visual_width = Array.make max_seg_rows 0 
;
(* Checkpoints structure (sparse subgraph with mandatory positioned padas) *)
type phased_pada = (phase * Word.word) (* for checkpoints *)
and check = (int * phased_pada * bool) (* checkpoint validation *) 
;
type checks = 
  { all_checks : mutable (list check)     (* checkpoints in valid solution *)
  ; segment_checks : mutable (list check) (* checkpoints in local segment  *)
  }
;
value chkpts = { all_checks = []; segment_checks  = []}
;
(* Accessing graph entry with phase *)
value split phase = split_rec [] 
  where rec split_rec acc = fun
  [ ([ ((ph,_) as fst) :: rst ] as l) -> 
    if ph=phase then (acc,l) else split_rec [ fst :: acc ] rst
  | [] -> (acc,[])
  ]
;
value insert_right right pada = ins_rec
  where rec ins_rec acc = fun
  [ [] -> failwith "insert_right"
  | [ (p,tr) :: rst ] -> 
       if p=pada then let tr' = [ right :: tr ] in
                      unstack [ (p,tr') :: acc ] rst
       else ins_rec [ (p,tr) :: acc ] rst
  ]
;
value get_pada pada = getrec where rec getrec = fun
  [ [] -> None
  | [ (p,tr) :: rest ] -> if p=pada then (Some tr) else getrec rest
  ]
;
value register_pada index (phase,pada,sandhi) = 
  (* We search for bucket of given phase in graph *)
  let (al,ar) = split phase graph.(index) 
  and allowed_right = match sandhi with
      [ Id -> []
      | Euphony (w,_,v) -> if w=v then [] else v
      ] in 
  let pada_right = (pada,[ allowed_right ]) in
  let update_graph ar' = graph.(index) := unstack al ar' in
  match ar with  
  [ [ (_,padas) :: rest ] -> (* bucket found *)
    match get_pada pada padas with
    [ Some tr -> 
         if List.mem allowed_right tr then () (* already registered *)
         else let updated_sandhi = insert_right allowed_right pada [] padas in
              update_graph [ (phase,updated_sandhi) :: rest ]
    | None -> update_graph [ (phase,[ pada_right :: padas ]) :: rest ]
    ]
  | [] -> update_graph [ (phase,[ pada_right ]) ] (* new bucket *)
  ]   
;

(* To avoid heavy functional transmission of chunk global parameters,
    we define a record of chunk parameters. Attributes offset and [sa_control] 
    are inherited, segmentable is synthesized. *)
(* [sa_control] is True iff chunk is followed by chunk starting with consonant,
   and it then authorizes its last segment to be sa or e.sa pronouns *)
type chunk_params = { offset : mutable int
                    ; segmentable : mutable bool
                    ; sa_control : mutable bool (* for inter-chunk sa check *)
                    }
;
value cur_chunk = { offset = 0; segmentable = False ; sa_control = False }
;
value set_cur_offset n = cur_chunk.offset := n
and set_segmentable b = cur_chunk.segmentable := b
and set_sa_control b = cur_chunk.sa_control := b 
;
value set_offset (offset,checkpoints) = do
  { set_cur_offset offset
  ; chkpts.all_checks := checkpoints
  }
;
value reset_graph () = for i = 0 to max_input_length-1 do 
  { graph.(i) := null }
;
value reset_visual () = for i = 0 to max_seg_rows-1 do 
  { visual.(i) := null_visual
  ; visual_conf.(i) := null_visual_conf
  ; visual_width.(i) := 0
  }
;
(* The offset permits to align each segment with the input string *)
value offset = fun
  [ Euphony (w,u,v) -> 
      let off = if w=[] then 1 (* amui/lopa from Lopa/Lopak *)
                        else Word.length w in
      off - (Word.length u + Word.length v) 
  | Id -> 0
  ]
;
value rec contains phase_w = fun
  [ [] -> False
  | [ (phase,word,_) :: rest ] -> phase_w=(phase,word) || contains phase_w rest
  ] 
;
value check_chunk position solution checkpoints = 
  (*i The following is temporary, [solution] ought to be checked progressively
     by [react], with proper pruning of backtracking. i*)
  check_rec position solution checkpoints
    where rec check_rec index sol checks = match checks with
      [ [] -> True (* all checkpoints verified *)
      | [ (pos,phase_word,select) :: more ] -> 
          (* select=True for check *)
          if index > pos then 
            if select then False 
            else check_rec index sol more (* checkpoint missed *)
          else match sol with 
          [ [] -> True (* checkpoint relevant for later chunks *)
          | [ (phase,word,sandhi) :: rest ] -> 
              let next_index = index + Word.length word + offset sandhi in
              if index < pos then check_rec next_index rest checks 
              else let (nxt_ind,ind_sols,next_sols) = all_sol_seg_ind [] sol
                 where rec all_sol_seg_ind stack = fun
                 [ [] -> (next_index,stack,[])
                 | [ ((phase2,word2,sandhi2) as seg2) :: rest2 ] -> 
                   let next_index = pos + Word.length word2 + offset sandhi2 in
                   if next_index=pos then all_sol_seg_ind [ seg2 :: stack ] rest2
                   else (next_index,[seg2 :: stack],rest2)
                 ]
              and (ind_check,next_check) = all_check_ind [] checks
              where rec all_check_ind stack = fun
                [ [] -> (stack,[])
                | ([ (pos2,phase_word2,select2) :: more2 ] as orig) ->
                   if pos2=pos then 
                      all_check_ind [ (pos2,phase_word2,select2) :: stack ] more2
                   else (stack,orig)
                ] in
              check_sols ind_sols ind_check
              where rec check_sols solspt = fun
                [ [] -> check_rec nxt_ind next_sols next_check 
                | [ (pos2,phase_word2,select2) :: more2 ] -> 
                    (select2=contains phase_word2 solspt)
                    (* Boolean select2 should be consistent with the solutions *)
                    && check_sols solspt more2
                ]
          ]
      ]       
;
(* counts the number of segmentation solutions of a chunk *)
value solutions_counter = ref 0
;
value bump_counter () = solutions_counter.val := solutions_counter.val + 1
and get_counter () = solutions_counter.val
and reset_counter () = solutions_counter.val := 0
;
value log_chunk revsol = 
  let solution = List.rev revsol 
  and position = cur_chunk.offset in
  if position >= max_input_length then raise Overflow else
  let check = check_chunk position solution chkpts.segment_checks in
    if check then (* log solution consistent with checkpoints *) do 
       { log_rec position solution 
         where rec log_rec index = fun 
         [ [] -> ()
         | [ ((phase,word,sandhi) as triple) :: rest ] -> do 
             { register_pada index triple
             ; log_rec (index + Word.length word + offset sandhi) rest
             }
         ]
       ; set_segmentable True
       ; bump_counter ()
       }
    else ()
;

(* Rest duplicated from Segmenter *)

type backtrack =
  [ Choose of phase and input and output and Word.word and choices
  | Advance of phase and input and output and Word.word
  ]
and resumption = list backtrack (* coroutine resumptions *)
;

(* The scheduler gets its phase transitions from Dispatcher.dispatch *)
value schedule phase input output w cont =
  let add phase cont = [ Advance phase input output w :: cont ] in
  let transitions = 
    if accepting phase && not star.val then [] (* Word = Sanskrit padas *) 
    else dispatch (* full.val *) w phase (* iterate Word+ *) in
  List.fold_right add transitions cont 
  (* respects dispatch order within a fair top-down search *)
; 
(* The graph segmenter as a non deterministic reactive engine:
   [phase] is the parsing phase
   [input] is the input tape represented as a [word]
   [output] is the current result of type [output]
   [back] is the backtrack stack of type [resumption]
   [occ] is the current reverse access path in the deterministic part
   the last anonymous argument is the current state of type [auto]. *)
(* Instead of functioning in coroutine with the Reader, one solution at a time,
   it computes all solutions, populating the graph structure for later display *)
value rec react phase input output back occ = fun 
  [ State (accept,det,choices) -> 
    (* we try the deterministic space before the non deterministic one *)
    let deter cont = match input with 
      [ [] -> continue cont
      | [ letter :: rest ] -> match ass letter det with  
           [ Some state -> react phase rest output cont [ letter :: occ ] state  
           | None -> continue cont
           ] 
      ] in
    let cont = if choices=[] then back (* non deterministic continuation *)
               else [ Choose phase input output occ choices :: back ] in
    (* now we look for - or + segmentation hint *)
    let (keep,cut,input') = match input with 
       [ [ 0 :: rest ] -> (* explicit "-" compound break hint *) 
              (ii_phase phase,True,rest) 
       | [ 100 :: rest ] -> (* mandatory segmentation "+" *)
              (True,True,rest)
       | _ -> (True,False,input) (* no hint in input *)
       ] in         
    if accept && keep then 
       let segment = (phase,occ,Id) in 
       let out = accrue segment output in 
       match validate out (* validate and compact partial output *) with 
       [ [] -> if cut then continue cont else deter cont 
       | contracted -> match input' with
              [ [] -> if accepting phase (* solution found *) then
                         register contracted cont
                      else continue cont 
              | [ first :: _ ] -> (* we first try the longest matching word *)
                      let cont' = schedule phase input' contracted [] cont in
                      if cut then continue cont' else
                      if check_id_sandhi occ first then (* legitimate Id *)
                         deter cont' else deter cont 
              ]
       ]
    else if cut then continue cont else deter cont 
  ] 
and choose phase input output back occ = fun  
  [ [] -> continue back 
  | [ ((w,u,v) as rule) :: others ] -> 
       let cont = if others=[] then back
                  else [ Choose phase input output occ others :: back ] in
       match subtract input w with (* try to read [w] on [input] *)
         [ Some rest -> 
           let segment = (phase,u @ occ,Euphony rule) in 
           let out = accrue segment output in
           match validate out with
           [ [] -> continue cont 
           | contracted ->
              if v=[] (* final sandhi *) then
                 if rest=[] && accepting phase (* solution found *) then
                    register contracted cont
                 else continue cont
              else continue (schedule phase rest contracted v cont)
           ]
         | None -> continue cont
         ]
  ]
and continue = fun
  [ [] -> () (* Exploration finished *) 
  | [ resume :: back ] -> match resume with
      [ Choose phase input output occ choices -> 
          choose phase input output back occ choices 
      | Advance phase input output occ -> match access phase occ with
          [ None -> continue back 
          | Some (state,v) -> react phase input output back v state
          ]
      ] 
  ]
(* CAUTION - This [continue] is completely different from the old [continue]
from Segmenter. It does not return one solution at a time in coroutine manner, 
but sweeps the whole solution space. In particular, it returns () rather than 
an optional solution. *)
and register solution cont = (* Last check for sa/e.sa inter-chunk consistency *)
  match sanitize_sa cur_chunk.sa_control solution with 
  [ [] -> continue cont 
  | chunk_sol -> do { log_chunk chunk_sol; continue cont }
  ]
;
value init_segment_initial entries sentence = 
  List.map (fun phase -> Advance phase sentence [] []) entries
; 
value segment1 chunk = continue (init_segment_initial initial chunk) 
;
value segment chunk = do
  { segment1 chunk
  ; cur_chunk.segmentable || do 
    { graph.(cur_chunk.offset) := [ (unknown,[ (Word.mirror chunk,[]) ]) ]
    ; False 
    }
  }
;
(* Splitting checkpoints into current and future ones *)
value split_check limit = split_rec []
  where rec split_rec acc checkpts = match checkpts with 
      [ [] -> (Word.mirror acc,[])
      | [ ((index,_,_) as check) :: rest ] -> 
          if index > limit then (Word.mirror acc,checkpts)
          else split_rec [ check :: acc ] rest 
      ]
;
(* We do not need to [dove_tail] like in Rank, since chunks are independent. *)
(* Returns a pair (b,n) where b is True if all chunks are segmentable so far,
   and n is the number of potential solutions *)
value segment_chunk (full,count) chunk sa_check =
    let extremity = cur_chunk.offset+Word.length chunk in
    let (local,future) = split_check extremity chkpts.all_checks in do
    { chkpts.segment_checks := local
    ; set_sa_control sa_check (* inherited from chunks recursion *)
    ; let segmentable = segment chunk 
      and local_count = get_counter () in do
      { set_segmentable False
      ; set_offset (succ extremity,future)
      ; if segmentable then do
           { reset_counter ()
           ; (full,count*local_count) (* overflow may compute modulo *)
             (* we have [local_count] segmentations of the local [chunk], and,
              chunks being independent, the total number of solutions multiply *)
           }
        else (False,count) (* unsegmentable chunk *) 
      }
    }
;
value segment_iter chunks = segment_chunks (True,1) chunks
  where rec segment_chunks acc = fun (* speedy terminal recursion *) 
    [ [ (* last *) chunk ] -> segment_chunk acc chunk False
    | [ chunk :: rest ] -> let sa_check = Phonetics.consonant_starts rest in
                           segment_chunks (segment_chunk acc chunk sa_check) rest
    | [] -> acc
    ]
;

end; (* Segment *)

