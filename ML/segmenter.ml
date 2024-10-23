(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit sentence segmenter - analyses (external) sandhi                   *)
(* Runs the segmenting transducer defined by parameter module [Eilenberg].    *)
(* Used by [Lexer], and thus by [Reader] for segmenting, tagging and parsing. *)
(* Same logic as old [Segmenter1] but modular with multiple phases            *)
(* Eilenberg is a finite Eilenberg machine, Control gives command parameters. *)

(* In the Sanskrit application, [Word.word] is (reverse of) inflected form. *)
(* Id means sandhi is optional. It is an optimisation, since it avoids 
   listing all identity sandhi rules such as [con|voy -> con.voy]. 
   Such rules are nonetheless checked as legitimate. *)
(* NB. This segmenter is used by Reader and Parser, but not by Interface, that
   uses [Graph_segmenter] instead. *)

open List2; (* unstack ass subtract *)
open Auto.Auto; (* auto rule choices State *)

module Segment 
  (Phases: sig
         type phase 
         and phases = list phase;
         value string_of_phase : phase -> string;
         value aa_phase : phase -> phase;
         value preverb_phase : phase -> bool;
         value ii_phase : phase -> bool;
         value un_lopa : phase -> phase;
         end)
  (Eilenberg: sig 
         value transducer : Phases.phase -> auto;
         value initial : Phases.phases;
         value dispatch : Word.word -> Phases.phase -> Phases.phases;
         value accepting : Phases.phase -> bool;
         type input = Word.word (* input sentence represented as a word *)
         and transition = (* junction relation *)
            [ Euphony of rule (* [(w,rev u,v)] such that [u|v -> w] *)
            | Id                   (* identity or no sandhi *)
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
open Control;

(* The summarizing structure sharing sub-solutions *)
(* It represents the union of all solutions *)
value max_input_length = 1000
;
type phased_padas = (phase * list Word.word) (* padas of given phase *)
and segments = list phased_padas (* forgetting sandhi *)
;
(* Checkpoints structure (sparse subgraph with mandatory positioned padas) *)
type phased_pada = (phase * Word.word) (* for checkpoints *)
and check = (int * phased_pada * bool) (* checkpoint validation *) 
;
value all_checks = ref ([]: list check) (* checkpoints in rest of input *)
and offset_chunk = ref 0
and segmentable_chunk = ref False 
and sa_control = ref False
;
(* Used by [Rank.segment_chunks_filter] *)
value set_offset (offset,checkpoints) = do
  { offset_chunk.val := offset 
  ; all_checks.val := checkpoints
  }
;
value set_sa_control b = sa_control.val := b
;
(* The offset permits to align the padas with the input string *)
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
(* This validation comes from the Summary mode with Interface, which sets 
   checkpoints that have to be verified for each solution. 
   This is probably temporary, [solution] ought to be checked progressively 
   by [react], with proper pruning of backtracking. *)
value check_chunk solution = 
   let position = offset_chunk.val 
   and checkpoints = all_checks.val in
   check_rec position (List.rev solution) checkpoints
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

(* Now for the segmenter proper *)
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
    if accepting phase && not star.val then [] (* Word = Sanskrit pada *) 
    else dispatch w phase (* iterate Word+ = Sanskrit vaakya *) in
  List.fold_right add transitions cont 
  (* respects dispatch order within a fair top-down search *)
; 
(* The tagging transducer interpreter as a non deterministic reactive engine:
   [phase] is the parsing phase
   [input] is the input tape represented as a [word]
   [output] is the current result of type [output]
   [back] is the backtrack stack of type [resumption]
   [occ] is the current reverse access path in the deterministic part
   the last argument is the current state of type [auto]. *)
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
       | [ 100 :: rest ] -> (* mandatory segmentation + *)
              (True,True,rest)  
       | _ -> (True,False,input) (* no hint in input *)
       ] in         
    if accept && keep then 
       let segment = (phase,occ,Id) in 
       let out = accrue segment output in 
       match validate out with 
       [ [] -> if cut then continue cont else deter cont 
       | contracted -> match input' with
           [ [] -> if accepting phase then (* potential solution found *)
                      emit contracted cont
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
                if rest=[] && accepting phase then (* potential solution found *)
                   emit contracted cont                    
                else continue cont 
              else continue (schedule phase rest contracted v cont)
           ]
         | None -> continue cont
         ]
  ]
and continue = fun
  [ [] -> None
  | [ resume :: back ] -> match resume with
      [ Choose phase input output occ choices -> 
          choose phase input output back occ choices 
      | Advance phase input output occ -> match access phase occ with
          [ None -> continue back 
          | Some (next_state,v) -> react phase input output back v next_state
          ]
      ] 
  ]
and emit solution cont =
  if check_chunk solution 
     then match sanitize_sa sa_control.val solution with
          [ [] -> continue cont
          | ok -> Some (ok,cont) (* solution found *) 
          ]
  else continue cont 
;
value init_segment_initial initial_phases sentence =  
  List.map (fun phase -> Advance phase sentence [] []) initial_phases
; 
value segment1_initial initial_phases sentence =  
  continue (init_segment_initial initial_phases sentence)
;
value init_segment seg = (* do not eta reduce! *) 
  init_segment_initial initial seg
and segment1 seg = 
  segment1_initial initial seg 
;
end; 
