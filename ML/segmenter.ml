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
         end)
  (Control: sig value star : ref bool; (* chunk= if star then word+ else word *) 
            end) 
  = struct
open Phases;
open Eilenberg;
open Control;

(* The summarizing structure sharing sub-solutions *)
(* It represents the union of all solutions *)
value max_input_length = 600
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

(* Checking for legitimate Id sandhi *)
(* Uses [sandhis_id] computed by [Compile_sandhi] *)
(* Side-effect : [Data.public_sandhis_id_file] loaded at load time. *)
value allowed_trans =
  (Gen.gobble Data.public_sandhis_id_file : Deco.deco Word.word)
;
value check_id_sandhi revl first = 
  let match_right allowed = not (List.mem [ first ] allowed) in
  try match revl with
      [ [] -> True
      | [ last :: before ] -> 
          (Phonetics.n_or_f last && Phonetics.vowel first) ||
          (* we allow an-s transition with s vowel-initial, ignoring nn rules *)
          (* this is necessary not to block transitions from the An phase *) 
          (Phonetics.vowel last && Phonetics.consonant first) || (* 8-04-21 *)
          let allowed1 = Deco.assoc [ last ] allowed_trans in
          match before with
             [ [] -> match_right allowed1 
             | [ penu :: _ ] -> 
               let allowed2 = Deco.assoc [ last :: [ penu ] ] allowed_trans in
               match_right allowed2 && match_right allowed1 
             ]
      ]
  with [ Not_found -> True ]
;
(* Examples: 
   [let st1 = Encode.code_revstring "raamas"
    and st2 = Encode.code_string "asti" in
    check_id_sandhi st1 st2 = False
 && let st1 = Encode.code_revstring "raamaa" 
    and st2 = Encode.code_string "arati" in
    check_id_sandhi st1 st2 = False
 && let st1 = Encode.code_revstring "phalam" 
    and st2 = Encode.code_string "icchaami" in
    check_id_sandhi st1 st2 = True]
*)

(* Expands phantom-initial or lopa-initial segments *)
(* phase [(aa_phase ph)] of "aa" is Pv for verbal ph, Pvkv for nominal ones *)
value accrue ((ph,revword,rule) as segment) previous_segments =
  match Word.mirror revword with 
  [ [ -2 (* [-] *) :: r ] -> match previous_segments with 
      [ [ (phase,pv,Euphony ([],u,[-2])) :: rest ] -> (* phase=Pv,Pvkv,Pvkc *)
          let v = match r with [ [ 10 (* e *) :: _ ] -> [ 10 ] 
                               | [ 12 (* o *) :: _ ] -> [ 12 ]
                               | _ -> failwith "accrue anomaly" 
                               ] in 
          (* u is [ a ] or [ aa ], v is [ e ] or [ o ] *)
          [ un_lopa_segment :: [ (phase,pv,Euphony (v,u,v )) :: rest ] ]
            where un_lopa_segment = (un_lopa ph,Word.mirror r,rule) 
       | _ -> failwith "accrue anomaly"
       ]
     (* Then phantom phonemes *)
   | [ -3 (* *a *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-3])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2 ],[ 2 ],[ 1 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 1 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -9 (* *A *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-9])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2 ],[ 2 ],[ 2 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 2 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -4 (* *i *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ -4 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 10 ],[ 2 ],[ 3 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 3 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -7 (* *I *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ -7 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 10 ],[ 2 ],[ 4 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 4 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -5 (* *u *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ -5 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 12 ],[ 2 ],[ 5 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 5 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -8 (* *U *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ -8 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 12 ],[ 2 ],[ 6 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 6 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ -6 (* *r *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ -6 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2; 43 ],[ 2 ],[ 7 ] )) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 7 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | [ 123 (* *C *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ 123 ])) :: rest ] -> 
         let w = Phonetics.sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2; 22; 23 ],[ 2 ], [ 23 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 23 :: r ],rule)
       | _ -> failwith "accrue anomaly"
       ]
  | _ -> [ segment :: previous_segments ]
  ]
;
(* Now for the segmenter proper *)
type backtrack =
  [ Choose of phase and input and output and Word.word and choices 
  | Advance of phase and input and output and Word.word
  ]
and resumption = list backtrack (* coroutine resumptions *)
;

(* Service routines *)

(* [access : phase -> word -> option (auto * word)] *)
value access phase = acc (transducer phase) []
   where rec acc state w = fun
      [ [] -> Some (state,w)  (* w is reverse of access input word *)
      | [ c :: rest ] -> match state with
           [ State (_,deter,_) -> match ass c deter with 
                [ Some next_state -> acc next_state [ c :: w ] rest
                | None -> None 
                ] 
           ]
      ]
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
