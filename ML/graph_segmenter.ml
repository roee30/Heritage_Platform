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
open List2
  
(* unstack ass subtract *)
open Auto.Auto
  
(* auto rule choices State *)
(* used by Interface : [Viccheda = Segment Phases Machine Segment_control] 
      where [Machine = Dispatch Transducers Lemmas] 
      where [Lemmas = Load_morphs.Morphs Prel Phases] *)
module Segment
  (Phases :
    sig
      type phase and phases = phase list
      
      val unknown : phase
        
      val aa_phase : phase -> phase
        
      val preverb_phase : phase -> bool
        
      val ii_phase : phase -> bool
        
      val un_lopa : phase -> phase
        
    end)
  (Eilenberg :
    sig
      (* To be instanciated by Dispatcher *)
      val transducer : Phases.phase -> auto
        
      val initial : Phases.phases
        
      val dispatch : Word.word -> Phases.phase -> Phases.phases
        
      val accepting : Phases.phase -> bool
        
      type input =
        Word.
        word
        and (* input sentence represented as a word *)
        transition =
        (* junction relation *)
        | Euphony of rule | (* [(w,rev u,v)] such that [u|v -> w] *) Id
        and (* identity or no sandhi *)
        segment =
        (Phases.phase * Word.word * transition)
        and output =
        segment list
      
      val validate : output -> output
        
      (* consistency check *)
      val sanitize_sa : bool -> output -> output
        
    end)
  (Control : sig val star : bool ref
                    end) =
  (* chunk= if star then word+ else word *)
  struct
    open Phases
      
    open Eilenberg
      
    open Control
      
    (* star *)
    (* The summarizing structure sharing sub-solutions *)
    (* It represents the union of all solutions *)
    (* 859 attested as last sentence in Pancatantra *)
    let max_input_length = 1000
    and max_seg_rows = 1000
      
    exception Overflow
      
    (* length of sentence exceeding array size *)
    (* segments of a given phase *)
    type phased_segment =
      (phase * ((Word.word * (Word.word list)) list))
      and (* (segment, mandatory prefixes of following segment) *)
      segments =
      phased_segment list
    
    (* partially forgetting sandhi *)
    let null : segments = []
    and (* initialisation of graph entry *) null_visual :
      (Word.word * (Word.word list) * phase * int) list = []
    and (* [ (word, v's of next segment, phase, offset) ] *)
      null_visual_conf : (Word.word * phase * int * bool) list = []
      
    (* [ (word, phase, offset, is_conflicting) ] *)
    (* This is the graph on padas of the union of all solutions *)
    (* We guarantee that every arc of the graph belongs to at least one bona fide
   segmentation. But every path in this graph is not a valid segmentation. 
   A path must pass global sandhi verification to qualify as valid. *)
    (* NB. Valid segmentations may contain unrecognized segments. *)
    let graph = Array.make max_input_length null
    and (* global over chunks *) visual = Array.make max_seg_rows null_visual
    and visual_conf = Array.make max_seg_rows null_visual_conf
    and visual_width = Array.make max_seg_rows 0
      
    (* Checkpoints structure (sparse subgraph with mandatory positioned padas) *)
    type phased_pada =
      (phase * Word.word)
      and (* for checkpoints *)
      check =
      (int * phased_pada * bool)
    
    (* checkpoint validation *)
    type checks =
      { mutable all_checks : check list; (* checkpoints in valid solution *)
        mutable segment_checks : check list
      }
    
    (* checkpoints in local segment  *)
    let chkpts = { all_checks = []; segment_checks = []; }
      
    (* Accessing graph entry with phase *)
    let split phase =
      let rec split_rec acc =
        function
        | ((((ph, _) as fst)) :: rst as l) ->
            if ph = phase then (acc, l) else split_rec (fst :: acc) rst
        | [] -> (acc, [])
      in split_rec []
      
    let insert_right right pada =
      let rec ins_rec acc =
        function
        | [] -> failwith "insert_right"
        | (p, tr) :: rst ->
            if p = pada
            then (let tr' = right :: tr in unstack ((p, tr') :: acc) rst)
            else ins_rec ((p, tr) :: acc) rst
      in ins_rec
      
    let get_pada pada =
      let rec getrec =
        function
        | [] -> None
        | (p, tr) :: rest -> if p = pada then Some tr else getrec rest
      in getrec
      
    let register_pada index (phase, pada, sandhi) =
      (* We search for bucket of given phase in graph *)
      let (al, ar) = split phase graph.(index)
      and allowed_right =
        match sandhi with
        | Id -> []
        | Euphony ((w, _, v)) -> if w = v then [] else v in
      let pada_right = (pada, [ allowed_right ]) in
      let update_graph ar' = graph.(index) <- unstack al ar'
      in
        match ar with
        | (_, padas) :: rest -> (* bucket found *)
            (match get_pada pada padas with
             | Some tr ->
                 if List.mem allowed_right tr
                 then ()
                 else (* already registered *)
                   (let updated_sandhi =
                      insert_right allowed_right pada [] padas
                    in update_graph ((phase, updated_sandhi) :: rest))
             | None -> update_graph ((phase, (pada_right :: padas)) :: rest))
        | [] -> update_graph [ (phase, [ pada_right ]) ]
      
    (* new bucket *)
    (* To avoid heavy functional transmission of chunk global parameters,
    we define a record of chunk parameters. Attributes offset and [sa_control] 
    are inherited, segmentable is synthesized. *)
    (* [sa_control] is True iff chunk is followed by chunk starting with consonant,
   and it then authorizes its last segment to be sa or e.sa pronouns *)
    type chunk_params =
      { mutable offset : int; mutable segmentable : bool;
        mutable sa_control : bool
      }
    
    (* for inter-chunk sa check *)
    let cur_chunk = { offset = 0; segmentable = false; sa_control = false; }
      
    let set_cur_offset n = cur_chunk.offset <- n
    and set_segmentable b = cur_chunk.segmentable <- b
    and set_sa_control b = cur_chunk.sa_control <- b
      
    let set_offset (offset, checkpoints) =
      (set_cur_offset offset; chkpts.all_checks <- checkpoints)
      
    let reset_graph () =
      for i = 0 to max_input_length - 1 do graph.(i) <- null done
      
    let reset_visual () =
      for i = 0 to max_seg_rows - 1 do
        visual.(i) <- null_visual;
        visual_conf.(i) <- null_visual_conf;
        visual_width.(i) <- 0
      done
      
    (* The offset permits to align each segment with the input string *)
    let offset =
      function
      | Euphony ((w, u, v)) ->
          let off =
            if w = []
            then 1
            else (* amui/lopa from Lopa/Lopak *) Word.length w
          in off - ((Word.length u) + (Word.length v))
      | Id -> 0
      
    let rec contains phase_w =
      function
      | [] -> false
      | (phase, word, _) :: rest ->
          (phase_w = (phase, word)) || (contains phase_w rest)
      
    let check_chunk position solution checkpoints =
      (*i The following is temporary, [solution] ought to be checked progressively
     by [react], with proper pruning of backtracking. i*)
      let rec check_rec index sol checks =
        match checks with
        | [] -> true
        | (* all checkpoints verified *) (pos, phase_word, select) :: more ->
            (* select=True for check *)
            if index > pos
            then if select then false else check_rec index sol more
            else (* checkpoint missed *)
              (match sol with
               | [] -> true
               | (* checkpoint relevant for later chunks *)
                   (phase, word, sandhi) :: rest ->
                   let next_index =
                     (index + (Word.length word)) + (offset sandhi)
                   in
                     if index < pos
                     then check_rec next_index rest checks
                     else
                       (let (nxt_ind, ind_sols, next_sols) =
                          let rec all_sol_seg_ind stack =
                            function
                            | [] -> (next_index, stack, [])
                            | (((phase2, word2, sandhi2) as seg2)) :: rest2
                                ->
                                let next_index =
                                  (pos + (Word.length word2)) +
                                    (offset sandhi2)
                                in
                                  if next_index = pos
                                  then all_sol_seg_ind (seg2 :: stack) rest2
                                  else (next_index, (seg2 :: stack), rest2)
                          in all_sol_seg_ind [] sol
                        and (ind_check, next_check) =
                          let rec all_check_ind stack =
                            function
                            | [] -> (stack, [])
                            | ((pos2, phase_word2, select2) :: more2 as orig)
                                ->
                                if pos2 = pos
                                then
                                  all_check_ind
                                    ((pos2, phase_word2, select2) :: stack)
                                    more2
                                else (stack, orig)
                          in all_check_ind [] checks in
                        let rec check_sols solspt =
                          function
                          | [] -> check_rec nxt_ind next_sols next_check
                          | (pos2, phase_word2, select2) :: more2 ->
                              (select2 = (contains phase_word2 solspt)) &&
                                (* Boolean select2 should be consistent with the solutions *)
                                (check_sols solspt more2)
                        in check_sols ind_sols ind_check))
      in check_rec position solution checkpoints
      
    (* counts the number of segmentation solutions of a chunk *)
    let solutions_counter = ref 0
      
    let bump_counter () = solutions_counter := !solutions_counter + 1
    and get_counter () = !solutions_counter
    and reset_counter () = solutions_counter := 0
      
    let log_chunk revsol =
      let solution = List.rev revsol
      and position = cur_chunk.offset
      in
        if position >= max_input_length
        then raise Overflow
        else
          (let check = check_chunk position solution chkpts.segment_checks
           in
             if check
             then (* log solution consistent with checkpoints *)
               ((let rec log_rec index =
                   function
                   | [] -> ()
                   | (((phase, word, sandhi) as triple)) :: rest ->
                       (register_pada index triple;
                        log_rec
                          ((index + (Word.length word)) + (offset sandhi))
                          rest)
                 in log_rec position solution);
                set_segmentable true;
                bump_counter ())
             else ())
      
    (* Rest duplicated from Segmenter *)
    (* Checking for legitimate Id sandhi *)
    (* Uses [sandhis_id] computed by [Compile_sandhi] *)
    (* Side-effect : [Data.public_sandhis_id_file] loaded at load time. *)
    let allowed_trans : Word.word Deco.deco =
      Gen.gobble Data.public_sandhis_id_file
      
    let check_id_sandhi revl first =
      let match_right allowed = not (List.mem [ first ] allowed)
      in
        try
          match revl with
          | [] -> true
          | last :: before ->
              ((Phonetics.n_or_f last) && (Phonetics.vowel first)) ||
                (* we allow an-s transition with s vowel-initial, ignoring nn rules *)
                (* this is necessary not to block transitions from the An phase *)
                (((Phonetics.vowel last) && (Phonetics.consonant first)) ||
                   (* 8-04-21 *)
                   (* above line necessary for last=ii or uu and first=r (deviiraajyam) *)
                   (let allowed1 = Deco.assoc [ last ] allowed_trans
                    in
                      match before with
                      | [] -> match_right allowed1
                      | penu :: _ ->
                          let allowed2 =
                            Deco.assoc [ last; penu ] allowed_trans
                          in (match_right allowed2) && (match_right allowed1)))
        with | Not_found -> true
      
    (* For treatment of phantom phonemes *)
    let sandhi_aa =
      function
      | [ 48; 1 ] -> [ 1; 2 ]
      | (* [a.h | aa -> a_aa] *) [ 43; 1 ] -> Encode.code_string "araa"
      | (* [ar | aa -> araa] *) [ c ] ->
          (match c with
           | 1 | 2 -> [ 2 ]
           | 3 | 4 -> Encode.code_string "yaa"
           | 5 | 6 -> Encode.code_string "vaa"
           | 7 | 8 | 48 -> Encode.code_string "raa"
           | 9 -> Encode.code_string "laa"
           | c -> [ Phonetics.voiced c; 2 ])
      | _ -> failwith "sandhi_aa"
      
    (* Expands phantom-initial or lopa-initial segments *)
    (* NB phase [(aa_phase ph)] of "aa" is Pv for verbal ph, Pvkv for nominal ones *)
    let accrue (((ph, revword, rule) as segment)) previous_segments =
      match Word.mirror revword with
      | (-2) :: (* [-] *) r ->
          (match previous_segments with
           | (* First Lopa *)
               (phase, pv, Euphony (([], u, [ (-2) ]))) :: rest ->
               (* phase=Pv,Pvkv,Pvkc *)
               let v =
                 (match r with
                  | 10 :: (* e *) _ -> [ 10 ]
                  | 12 :: (* o *) _ -> [ 12 ]
                  | _ -> failwith "accrue anomaly 1") in
               (* u is [ a ] or [ aa ], v is [ e ] or [ o ] *)
               let un_lopa_segment = ((un_lopa ph), (Word.mirror r), rule)
               in
                 un_lopa_segment :: (phase, pv, (Euphony ((v, u, v)))) ::
                   rest
           | _ -> failwith "accrue anomaly 2")
      | (* Then phantom phonemes *) (-3) :: (* *a *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-3) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (1 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 2 ], [ 2 ], [ 1 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 3")
      | (-9) :: (* *A *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-9) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (2 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 2 ], [ 2 ], [ 2 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 4")
      | (-4) :: (* *i *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-4) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (3 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 10 ], [ 2 ], [ 3 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 5")
      | (-7) :: (* *I *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-7) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (4 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 10 ], [ 2 ], [ 4 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 6")
      | (-5) :: (* *u *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-5) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (5 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 12 ], [ 2 ], [ 5 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 7")
      | (-8) :: (* *U *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-8) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (6 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ], (Euphony (([ 12 ], [ 2 ], [ 6 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 8")
      | (-6) :: (* *r *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ (-6) ]))) :: rest ->
               let w = sandhi_aa u in
               let new_segment = (ph, (Word.mirror (7 :: r)), rule)
               in
                 new_segment ::
                   ((aa_phase ph), [ 2 ],
                    (Euphony (([ 2; 43 ], [ 2 ], [ 7 ])))) ::
                   (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest
           | _ -> failwith "accrue anomaly 9")
      | 123 :: (* *C *) r ->
          (match previous_segments with
           | (phase, rword, Euphony ((_, u, [ 123 ]))) :: rest ->
               if preverb_phase phase
               then failwith "accrue C with aa"
               else
                 (let w = sandhi_aa u in
                  let new_seg = (ph, (Word.mirror (23 :: r)), rule)
                  in
                    new_seg ::
                      ((aa_phase ph), [ 2 ],
                       (Euphony (([ 2; 22; 23 ], [ 2 ], [ 23 ])))) ::
                      (phase, rword, (Euphony ((w, u, [ 2 ])))) :: rest)
           | _ -> failwith "accrue anomaly 10")
      | _ -> segment :: previous_segments
      
    type backtrack =
      | Choose of phase * input * output * Word.word * choices
      | Advance of phase * input * output * Word.word
      and resumption =
      backtrack list
    
    (* coroutine resumptions *)
    (* Service routines of the segmenter *)
    (* [access : phase -> word -> option (auto * word)] *)
    let access phase =
      let rec acc state w =
        function
        | [] -> Some ((state, w))
        | (* w is reverse of access input word *) c :: rest ->
            (match state with
             | State ((_, deter, _)) ->
                 (match ass c deter with
                  | Some next_state -> acc next_state (c :: w) rest
                  | None -> None))
      in acc (transducer phase) []
      
    (* The scheduler gets its phase transitions from Dispatcher.dispatch *)
    let schedule phase input output w cont =
      let add phase cont = (Advance (phase, input, output, w)) :: cont in
      let transitions =
        if (accepting phase) && (not !star)
        then []
        else (* Word = Sanskrit padas *) dispatch (* full.val *) w phase
      in (* iterate Word+ *) List.fold_right add transitions cont
      
    (* respects dispatch order within a fair top-down search *)
    (* The graph segmenter as a non deterministic reactive engine:
   [phase] is the parsing phase
   [input] is the input tape represented as a [word]
   [output] is the current result of type [output]
   [back] is the backtrack stack of type [resumption]
   [occ] is the current reverse access path in the deterministic part
   the last anonymous argument is the current state of type [auto]. *)
    (* Instead of functioning in coroutine with the Reader, one solution at a time,
   it computes all solutions, populating the graph structure for later display *)
    let rec react phase input output back occ =
      function
      | State ((accept, det, choices)) ->
          (* we try the deterministic space before the non deterministic one *)
          let deter cont =
            (match input with
             | [] -> continue cont
             | letter :: rest ->
                 (match ass letter det with
                  | Some state ->
                      react phase rest output cont (letter :: occ) state
                  | None -> continue cont)) in
          let cont =
            if choices = []
            then back
            else (* non deterministic continuation *)
              (Choose (phase, input, output, occ, choices)) :: back in
          (* now we look for - or + segmentation hint *)
          let (keep, cut, input') =
            (match input with
             | 0 :: rest -> (* explicit "-" compound break hint *)
                 ((ii_phase phase), true, rest)
             | 100 :: rest -> (* mandatory segmentation "+" *)
                 (true, true, rest)
             | _ -> (true, false, input))
          in
            (* no hint in input *)
            if accept && keep
            then
              (let segment = (phase, occ, Id) in
               let out = accrue segment output
               in
                 match validate out with
                 | (* validate and compact partial output *) [] ->
                     if cut then continue cont else deter cont
                 | contracted ->
                     (match input' with
                      | [] ->
                          if accepting phase
                          then (* solution found *) register contracted cont
                          else continue cont
                      | first :: _ ->
                          (* we first try the longest matching word *)
                          let cont' =
                            schedule phase input' contracted [] cont
                          in
                            if cut
                            then continue cont'
                            else
                              if check_id_sandhi occ first
                              then (* legitimate Id *) deter cont'
                              else deter cont))
            else if cut then continue cont else deter cont
    and choose phase input output back occ =
      function
      | [] -> continue back
      | (((w, u, v) as rule)) :: others ->
          let cont =
            if others = []
            then back
            else (Choose (phase, input, output, occ, others)) :: back
          in
            (match subtract input w with
             | (* try to read [w] on [input] *) Some rest ->
                 let segment = (phase, (u @ occ), (Euphony rule)) in
                 let out = accrue segment output
                 in
                   (match validate out with
                    | [] -> continue cont
                    | contracted ->
                        if v = []
                        then (* final sandhi *)
                          if (rest = []) && (accepting phase)
                          then (* solution found *) register contracted cont
                          else continue cont
                        else continue (schedule phase rest contracted v cont))
             | None -> continue cont)
    and continue =
      function
      | [] -> ()
      | (* Exploration finished *) resume :: back ->
          (match resume with
           | Choose (phase, input, output, occ, choices) ->
               choose phase input output back occ choices
           | Advance (phase, input, output, occ) ->
               (match access phase occ with
                | None -> continue back
                | Some ((state, v)) -> react phase input output back v state))
    and
      (* CAUTION - This [continue] is completely different from the old [continue]
from Segmenter. It does not return one solution at a time in coroutine manner, 
but sweeps the whole solution space. In particular, it returns () rather than 
an optional solution. *)
      register solution cont =
      (* Last check for sa/e.sa inter-chunk consistency *)
      match sanitize_sa cur_chunk.sa_control solution with
      | [] -> continue cont
      | chunk_sol -> (log_chunk chunk_sol; continue cont)
      
    let init_segment_initial entries sentence =
      List.map (fun phase -> Advance (phase, sentence, [], [])) entries
      
    (* Works for Complete as well as Simplified mode *)
    let segment1 chunk = continue (init_segment_initial initial chunk)
      
    let segment chunk =
      (segment1 chunk;
       (* does not assume Complete mode *)
       cur_chunk.segmentable ||
         (graph.(cur_chunk.offset) <-
            [ (unknown, [ ((Word.mirror chunk), []) ]) ];
          false))
      
    (* Splitting checkpoints into current and future ones *)
    let split_check limit =
      let rec split_rec acc checkpts =
        match checkpts with
        | [] -> ((Word.mirror acc), [])
        | (((index, _, _) as check)) :: rest ->
            if index > limit
            then ((Word.mirror acc), checkpts)
            else split_rec (check :: acc) rest
      in split_rec []
      
    (* We do not need to [dove_tail] like in Rank, since chunks are independent. *)
    (* Returns a pair (b,n) where b is True if all chunks are segmentable so far,
   and n is the number of potential solutions *)
    let segment_chunk (full, count) chunk sa_check =
      let extremity = cur_chunk.offset + (Word.length chunk) in
      let (local, future) = split_check extremity chkpts.all_checks
      in
        (chkpts.segment_checks <- local;
         set_sa_control sa_check;
         (* inherited from chunks recursion *)
         let segmentable = segment chunk
         and local_count = get_counter ()
         in
           (set_segmentable false;
            set_offset ((succ extremity), future);
            if segmentable
            then (reset_counter (); (full, (count * local_count)))
            else (* overflow may compute modulo *)
              (* we have [local_count] segmentations of the local [chunk], and,
              chunks being independent, the total number of solutions multiply *)
              (false, count)))
      
    (* unsegmentable chunk *)
    let segment_iter chunks =
      let rec segment_chunks acc =
        function
        | (* speedy terminal recursion *) (* last *) [ chunk ] ->
            segment_chunk acc chunk false
        | chunk :: rest ->
            let sa_check = Phonetics.consonant_starts rest
            in segment_chunks (segment_chunk acc chunk sa_check) rest
        | [] -> acc
      in segment_chunks (true, 1) chunks
      
  end
  

