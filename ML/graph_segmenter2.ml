(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet, Pawan Goyal & Sriram Krishnan                *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This segmenter is inspired from old module Segmenter, but uses a graph 
   structure for the sharing of phased segments given with their offset. *)

open List2; (* unstack ass subtract *)
open Auto.Auto; (* auto rule choices State *)

(* used by Interface : [Viccheda = Segment Phases Machine Segment_control] 
      where [Machine = Dispatch Transducers Lemmas] 
      where [Lemmas = Load_morphs.Morphs Prel Phases] *)
module Segment2
  (Phases: sig  
         type phase
         and phases = list phase;
         value unknown : phase;
         value aa_phase : phase -> phase;
         value preverb_phase : phase -> bool; 
         value ii_phase : phase -> bool;
         value un_lopa : phase -> phase;
         value get_string_of_phase : phase -> string;
         value compound_component: phase -> bool;
         value ii_component: phase -> bool;
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
and default_max_best_solutions = 10
and max_best_solutions = ref 10 (* Modify based on requirement. 5, 10, 50, 100 *)
;
exception Overflow (* length of sentence exceeding array size *)
;
(* segments of a given phase *)
type phased_segment = (phase * list (Word.word * list Word.word)) 
                                 (* (segment, mandatory prefixes of 
                                              following segment) *)
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
and check_segment = (int * segment) (* for saving all the rejected segments *)
;
type rejected_segments_list = list check_segment 
;
type checks = 
  { all_checks : mutable (list check)     (* checkpoints in valid solution *)
  ; segment_checks : mutable (list check) (* checkpoints in local segment  *)
  ; rejected_segments : mutable rejected_segments_list (* checkpoints to be 
                                                          rejected after choosing
                                                          the best segments *)
  }
;
value chkpts = { all_checks = []; segment_checks  = []; rejected_segments = []}
;
(* Solutions structure *)
type sols =
  { cur_offset : mutable int (* current offset *)
  ; solution : mutable string (* segmentation solution string *)
  ; number_of_chunks : mutable int (* number of chunks in the string *)
  (* list of [<confidence_value, 
               solution_string, 
               current_chunk's offset, 
               segmentation_id, 
               number_of_segments, 
               output_triplets,
               all_triplets>] *)
  ; possible_splits : 
    mutable (list (float * string * int * int * int * 
                   rejected_segments_list * rejected_segments_list))
  (* list of [<chunk_id, list of possible segmentations>] *)
  ; total_sols: 
      mutable (list (int * list (float * string * int * int * int * 
                                 rejected_segments_list * 
                                 rejected_segments_list)))
  }
;
value chunk_solutions = { cur_offset = 0; 
                          solution = ""; 
                          number_of_chunks = 0; 
                          possible_splits = []; 
                          total_sols = []
                        } (* sols *)
;
(* Current Phase structure *)
type phase2 = 
  { cur_phase : mutable Phases.phase
  }
;
value chk_phase = 
  { cur_phase = unknown} (* To check the phase of the current segment *)
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

(* Statseg *)
(* For retrieving data from the data dumped rem file which contains 
   words and frequencies in the decorated trie format *)
(* word and frequency stored as a couplet. For example, <namaH,766> *)
type word_attrbs = list (string * int) 
(* transition and frequency stored as a triplet 
   Transition : (u | v -> w) and Frequency triplet : <u,v,f>
   For example: <[1;16],[1],2637> represents <aH,a,2637> 
   letters are already converted to integers of Heritage *)
and trans_attrbs = list (string * string * int)
and freq = int
and freqs = list freq 
and wfreq = Lexmap.lexmap freqs
;
value word_freq_ref = ref (Deco.empty : wfreq)
;
value pada_freq_ref = ref (Deco.empty : wfreq)
;
value comp_freq_ref = ref (Deco.empty : wfreq)
;
value load_word_freq file = 
  (Gen.gobble file : wfreq)
;
value load_word_list file = 
  try (Gen.gobble file : word_attrbs)
  with [ _ ->  [] ]
;
value load_transition_list file = 
  try (Gen.gobble file : trans_attrbs)
  with [ _ ->  [] ]
;
(* The following .rem files were generated from the 
   parallel corpus of unsegmented-segmented sentences 
   and contain decorated tries of words, compound-components and transitions *)
(* All the words and their frequencies *)
value words_freq_file = Data.word_freq_file 
;
(* All the padas (which are not compound components) and their frequencies *)
value pada_words_freq_file = Data.pada_freq_file 
;
(* All the compound components and their frequencies *)
value comp_words_freq_file = Data.comp_freq_file 
;
value pada_freq_list = ref ([] : word_attrbs)
;
value comp_freq_list = ref ([] : word_attrbs)
;
value word_freq_list = ref ([] : word_attrbs)
;
(* List of tuples <sandhi between words, frequency> *)
value pada_transitions_list = ref ([] : trans_attrbs)
;
(* List of tuples <sandhi between compound components, frequency> *)
value comp_transitions_list = ref ([] : trans_attrbs)
;
(* To calculate the total number of transition entries in the parallel corpus *)
value calculate_transitions triplets_list = 
     loop 0 triplets_list
     where rec loop freq_sum = fun
     [ [] -> (float_of_int freq_sum)
     | [ (f, s, v) :: tl ] -> loop (freq_sum + v) tl
     ]
;
value calculate_word_freq couplets_list = 
     loop 0 0 couplets_list
     where rec loop freq_sum type_no = fun
     [ [] -> (float_of_int freq_sum, float_of_int type_no)
     | [ (f, v) :: tl ] -> loop (freq_sum + v) (type_no + 1) tl
     ]
;
(*To find the third value of a triplet given the first two elements - 
  for transitions frequencies *)
value find_third first second triplets_list =
  let compare_elements (f,s,t) = (f = first && s = second) in
  let (_,_,third) = 
    try List.find (fun x -> compare_elements x) triplets_list
    with [ Not_found -> ("","",0) ] in
  (float_of_int third)
;
(* Converts the given list to a string with given separator *)
value int_list_to_string separator int_list = 
  let rec get_strings acc = fun
  [ [] -> acc
  | [x] -> acc ^ string_of_int x
  | [hd::tl] -> let acc1 = acc ^ string_of_int hd ^ separator in
                get_strings acc1 tl
  ] in
  "[" ^ (get_strings "" int_list) ^ "]"
;
(* Get freq from weighted lexmap of given word *)
value get_freq word freq_ref = 
  let updated_word = (Morpho_html.visargify word) in
  let freq = 
    match Deco.assoc updated_word freq_ref.val with
    [ [] -> 0.0
    | e -> let (delta, freq_lst) = (List.hd e) in
           let freq = (List.hd freq_lst) in
           float_of_int freq
    ] in
  freq
;

(* NOTE: The following needs to be calculated from the file 
   while generating the .rem files rather than manually entering the values.*)
value total_words = ref 0.0 (* 403233.0 *)
;
value total_padas = ref 0.0 (* 284930.0 *)
;
value total_comps = ref 0.0 (* 118303.0 *)
;
value total_words_types = ref 0.0 (* 37015.0 *)
;
value total_padas_types = ref 0.0 (* 27704.0 *)
;
value total_comps_types = ref 0.0 (* 16130.0 *)
;
value total_pada_transitions = ref 0.0  
; (* 280622.0 *)
value total_comp_transitions = ref 0.0  
; (* 78907.0 *)
value total_pada_transitions_types = ref 0.0 
;
value total_comp_transitions_types = ref 0.0 
;
(* To return the individual elements of the transition *)
value match_transition transition = 
  match transition with
  [ Euphony (w,u,v) -> (w,u,v)
  | Id -> ([],[],[])
  ]
;
(* To get the text in WX Notation for debugging *)
value get_word rword = 
  Canon.decode_WX (Word.mirror rword)
;
(* To calculate probability for the word *)
value get_prob rword freq_ref tot_ref tot_types = 
  let freq = get_freq rword freq_ref in
  if freq = 0.0 then (1.0 /. (tot_ref +. tot_types))
  else (freq /. tot_ref)
;
(* Word's probability when using the data as the decorated trie *)
value get_pada_prob rword = 
  get_prob rword pada_freq_ref total_padas.val total_padas_types.val
;
(* Compound component's probability when using the data as the decorated trie *)
value get_comp_prob rword = 
  get_prob rword comp_freq_ref total_comps.val total_comps_types.val
;
(* Used when word and compound components are treated alike *)
value get_word_prob rword = 
  get_prob rword word_freq_ref total_words.val total_words_types.val
;
(* Transition probability from the list of <transition, frequency> couplets *)
value get_transition_prob transition transition_list tot_transitions 
                          total_transition_types = 
  let (w,u,v) = match_transition transition in
  if (u,v) = ([],[]) then (1.0 /. tot_transitions)
  else let first = (int_list_to_string ";" (List.rev u))
       and second = (int_list_to_string ";" v) in
       let trans_freq = find_third first second transition_list in
       if trans_freq = 0.0 then 
         (1.0 /. (tot_transitions +. total_transition_types))
       else (trans_freq /. tot_transitions)
;
(* To get the probability of transition between compound components *)
value get_comp_transition_prob transition = 
  get_transition_prob transition comp_transitions_list.val 
                      total_comp_transitions.val total_comp_transitions_types.val
;
(* To get the probability of transition between words *)
value get_pada_transition_prob transition = 
  get_transition_prob transition pada_transitions_list.val 
                      total_pada_transitions.val total_pada_transitions_types.val
;
value calculate_sum (word, flm) type_tot val_tot  =
  match (List.hd flm) with 
  [ (delta, freqs) -> 
      let new_type_tot = type_tot + 1 
      and new_val_tot = val_tot + (List.hd freqs) in 
      (new_type_tot, new_val_tot)
  ]
;
value rec process_deco type_tot val_tot = fun
  [ [ hd :: tl ] -> 
          let (new_type_tot, new_val_tot) = calculate_sum hd type_tot val_tot in 
          process_deco new_type_tot new_val_tot tl 
  | [] -> (float_of_int type_tot, float_of_int val_tot)
  ]
;
value assign_freq_info = do 
  { word_freq_ref.val := load_word_freq words_freq_file
  ; pada_freq_ref.val := load_word_freq pada_words_freq_file
  ; comp_freq_ref.val := load_word_freq comp_words_freq_file
  ; pada_transitions_list.val := load_transition_list Data.pada_trans_freq_file 
  ; comp_transitions_list.val := load_transition_list Data.comp_trans_freq_file 
  ; total_pada_transitions.val := calculate_transitions pada_transitions_list.val
  ; total_comp_transitions.val := calculate_transitions comp_transitions_list.val
  ; let pada_trans_len = List.length pada_transitions_list.val in 
    total_pada_transitions_types.val := float_of_int pada_trans_len 
  ; let comp_trans_len = List.length comp_transitions_list.val in 
    total_comp_transitions_types.val := float_of_int comp_trans_len 
  ; let (words_types, words) = process_deco 0 0 (Deco.contents word_freq_ref.val)  
    and (padas_types, padas) = process_deco 0 0 (Deco.contents pada_freq_ref.val)  
    and (comps_types, comps) = process_deco 0 0 (Deco.contents comp_freq_ref.val)
    in do 
    { total_words.val := words
    ; total_words_types.val := words_types
    ; total_padas.val := padas
    ; total_padas_types.val := padas_types
    ; total_comps.val := comps
    ; total_comps_types.val := comps_types
    }
  }
;
(* To check the phase of the current segment in consideration, 
   for getting the probabilities according to the segment's phase
    (iic/ifc (compound components) vs other phase (words)) *)
(* NOTE: Make sure to include certain phases which could be iics or ifcs. 
   For example: yoddhu in yoddhu-kaamaan *)
value chk_ifc phase = 
  let ifc =  if (ii_component chk_phase.cur_phase) then True
             else False in do 
  { chk_phase.cur_phase := phase
  ; ifc
  }
;
(* Assign unigram freqs for each sandhi rule 
   [get_rule_freq] returns the product of 
   the probablity of pada / compound component, and 
   the probability of its subsequent transitions *)
value get_rule_freq (phase,rword,transition) =
  let check_ifc = (chk_ifc phase) in
  let (word_prob, transition_prob) = 
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || check_ifc) 
    then ((get_comp_prob rword), (get_comp_transition_prob transition))
    else ((get_pada_prob rword), (get_pada_transition_prob transition)) in
    (* Comment the above conditions and use the following 
       if the single list for both words and compound components is used *)
    (* [let w_prob = get_word_prob rword in
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || check_ifc) 
    then (w_prob, (get_comp_transition_prob transition))
    else (w_prob, (get_pada_transition_prob transition)) in] *)
  (* The following condition is given to add '-' between compound components, 
     and ' ' between normal words *)
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word) in
  if transition_prob = 1.0 then (1.0, word)
  else
  (* confidence value for a segment depends on [word_probability] alone *)
  (word_prob, word) 
  (* confidence value for a segment depends on [transition_probability] alone *)
  (* [(transition_prob, word)] *) 
  (* confidence value for a segment depends on 
     [<word_probability * transition_probability>] *)
  (* [let cur_prob = (word_prob *. transition_prob) in
  (cur_prob, word)] *) 
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
  do {
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
  (* The following is used to get the prob of the pada and sandhi 
     and then return it to the parent function [log_chunk] *)
  let (pada_conf_val, pada_text) = get_rule_freq (phase,pada,sandhi) in
  (pada_conf_val, pada_text)
  }
;

(* To avoid heavy functional transmission of chunk global parameters,
    we define a record of chunk parameters. Attributes offset and [sa_control] 
    are inherited, segmentable is synthesized. *)
(* [sa_control] is True iff chunk is followed by chunk starting with consonant,
   and it then authorizes its last segment to be sa or e.sa pronouns *)
type chunk_params = { offset : mutable int
                    ; segmentable : mutable bool
                    ; sa_control : mutable bool (* for inter-chunk sa check *)
                    ; chunk : mutable (list Word.letter)
                    }
;
value cur_chunk = { offset = 0
                  ; segmentable = False
                  ; sa_control = False
                  ; chunk = []
                  }
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
(* [log_chunk] is split for clarity *)
value log_chunk_rec index solution = 
  log_rec index 0 1.0 "" [] solution
  where rec log_rec index no_of_seg cumu_conf vakya acc_triplet = fun
  [ [] -> (cumu_conf, vakya, no_of_seg, acc_triplet)
  | [ ((phase,word,sandhi) as triple) :: rest ] -> 
       (* The current segment is saved in chkpts.rejected_segments and will be 
          cross verified later with the segments in the best n solutions *)
       let new_segment = (index, triple) in 
       let triplet_acc = List.append [new_segment] acc_triplet in
       let new_segment_check = 
           if List.mem new_segment chkpts.rejected_segments 
           then chkpts.rejected_segments 
           else chkpts.rejected_segments @ [new_segment] in 
       let (pada_conf_val, pada_text) = do 
       { chkpts.rejected_segments := new_segment_check
       ; register_pada index triple
       } in 
       let cumulative_pada = 
           if vakya = "" then pada_text 
           else (vakya ^ "+" ^ pada_text)
       and cumulative_conf = (cumu_conf *. pada_conf_val)
       and no_of_segments = no_of_seg + 1 in
       log_rec (index + Word.length word + offset sandhi) no_of_segments 
               cumulative_conf cumulative_pada triplet_acc rest
  ]
;
(* This function is modified for calculating 
   cumulative probability of the padas and their sandhis *)
value log_chunk revsol = 
  let solution = List.rev revsol 
  and position = cur_chunk.offset in
  if position >= max_input_length then raise Overflow else
  let check = check_chunk position solution chkpts.segment_checks in
    if check then (* log solution consistent with checkpoints *) do
        { (* to generate confidence values, 
                         segmentation, 
                         number of segments, and 
                         triplets 
             for the current chunk
        ;*)  let (chunk_conf, chunk_text, no_of_segments, chunk_triplets) = 
              log_chunk_rec position solution 
        (* For affecting conf based on number of individual words *)
        ; set_segmentable True
        ; bump_counter ()
        ; (chunk_conf, chunk_text, no_of_segments, chunk_triplets)
        }
    else
    (* Hard coded values for unrecognized chunks getting here 
       after selection of segments *)
    (1.0, "", 1, [(-1, (unknown,Word.mirror [],Id))]) 
;

(* Rest duplicated from Segmenter *)

(* Checking for legitimate Id sandhi *)
(* Uses [sandhis_id] computed by [Compile_sandhi] *)
(* Side-effect : [Data.public_sandhis_id_file] loaded at load time. *)
value allowed_trans =
  (Gen.gobble Data.public_sandhis_id_file:Deco.deco Word.word)
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
          (* above line necessary for last=ii or uu and first=r (deviiraajyam) *)
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
(* For treatment of phantom phonemes *)
value sandhi_aa = fun
  [ [ 48; 1 ] -> [ 1; 2 ] (* [a.h | aa -> a_aa] *)
  | [ 43; 1 ] -> Encode.code_string "araa" (* [ar | aa -> araa] *)
  | [ c ] -> match c with
             [ 1 | 2 -> [ 2 ]
             | 3 | 4 -> Encode.code_string "yaa"
             | 5 | 6 -> Encode.code_string "vaa"
             | 7 | 8 | 48 -> Encode.code_string "raa"
             | 9     -> Encode.code_string "laa"
             | c     -> [ Phonetics.voiced c; 2 ]
             ]
  | _ -> failwith "sandhi_aa"
  ]
;
(* Expands phantom-initial or lopa-initial segments *)
(* NB phase [(aa_phase ph)] of "aa" is Pv for verbal ph, Pvkv for nominal ones *)
value accrue ((ph,revword,rule) as segment) previous_segments =
  match Word.mirror revword with  
    [ [ -2 (* [-] *) :: r ] -> match previous_segments with (* First Lopa *) 
      [ [ (phase,pv,Euphony ([],u,[-2])) :: rest ] -> (* phase=Pv,Pvkv,Pvkc *)
          let v = match r with [ [ 10 (* e *) :: _ ] -> [ 10 ] 
                               | [ 12 (* o *) :: _ ] -> [ 12 ]
                               | _ -> failwith "accrue anomaly 1" 
                               ] in 
          (* u is [ a ] or [ aa ], v is [ e ] or [ o ] *)
          [ un_lopa_segment :: [ (phase,pv,Euphony (v,u,v)) :: rest ] ]
            where un_lopa_segment = (un_lopa ph,Word.mirror r,rule) 
       | _ -> failwith "accrue anomaly 2"
       ]
        (* Then phantom phonemes *)
   | [ -3 (* *a *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-3])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2 ],[ 2 ],[ 1 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 1 :: r ],rule)
       | _ -> failwith "accrue anomaly 3"
       ]
  | [ -9 (* *A *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-9])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2 ],[ 2 ],[ 2 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 2 :: r ],rule)
       | _ -> failwith "accrue anomaly 4"
       ]
  | [ -4 (* *i *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-4])) :: rest ] -> 
         let w = sandhi_aa u in 
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 10 ],[ 2 ],[ 3 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ] 
           where new_segment = (ph,Word.mirror [ 3 :: r ],rule)
       | _ -> failwith "accrue anomaly 5"
       ]
  | [ -7 (* *I *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-7])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 10 ],[ 2 ],[ 4 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 4 :: r ],rule)
       | _ -> failwith "accrue anomaly 6"
       ]
  | [ -5 (* *u *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-5])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 12 ],[ 2 ],[ 5 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 5 :: r ],rule)
       | _ -> failwith "accrue anomaly 7"
       ]
  | [ -8 (* *U *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-8])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 12 ],[ 2 ],[ 6 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 6 :: r ],rule)
       | _ -> failwith "accrue anomaly 8"
       ]
  | [ -6 (* *r *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[-6])) :: rest ] -> 
         let w = sandhi_aa u in
         [ new_segment :: [ (aa_phase ph,[ 2 ],Euphony ([ 2; 43 ],[ 2 ],[ 7 ])) 
                       :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_segment = (ph,Word.mirror [ 7 :: r ],rule)
       | _ -> failwith "accrue anomaly 9"
       ]
  | [ 123 (* *C *) :: r ] -> match previous_segments with
       [ [ (phase,rword,Euphony (_,u,[ 123 ])) :: rest ] -> 
         if preverb_phase phase then failwith "accrue C with aa" else 
         let w = sandhi_aa u in 
         [ new_seg :: [ (aa_phase ph,[ 2 ],Euphony ([ 2; 22; 23 ],[ 2 ],[ 23 ]))
                   :: [ (phase,rword,Euphony (w,u,[ 2 ])) :: rest ] ] ]
           where new_seg = (ph,Word.mirror [ 23 :: r ],rule)
       | _ -> failwith "accrue anomaly 10"
       ]
  | _ -> [ segment :: previous_segments ]
  ]
;

type backtrack =
  [ Choose of phase and input and output and Word.word and choices
  | Advance of phase and input and output and Word.word
  ]
and resumption = list backtrack (* coroutine resumptions *)
;

(* Service routines of the segmenter *)

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
    if accepting phase && not star.val then [] (* Word = Sanskrit padas *) 
    else dispatch (* full.val *) w phase (* iterate Word+ *) in
  List.fold_right add transitions cont 
  (* respects dispatch order within a fair top-down search *)
; 
(* If the new segment has the same text as any of the existing segments, then 
   merge the new triplets with the existing triplets *)
value edit_chunk_triplets splits ((cur_conf, cur_text, cur_offset, cur_seg_id, 
                                   cur_no_of_seg, cur_output_triplets, 
                                   cur_all_triplets) as cur_split) = 
  modify_chunk_splits [] False cur_split splits
  where rec modify_chunk_splits acc replaced cur_split = fun 
  [ [] -> acc 
  | [((conf, text, offset, seg_id, no_of_seg, 
       output_triplets, all_triplets) as hd) :: tl] -> 
    if replaced then modify_chunk_splits (acc @ [hd]) replaced cur_split tl
    else if text = cur_text then 
      let updated_all_triplets = 
        merge_triplets all_triplets all_triplets cur_all_triplets
      where rec merge_triplets all_triplets new_triplets = fun 
      [ [] -> new_triplets
      | [hd1 :: tl1] -> 
          let new_triplet = if List.mem hd1 all_triplets then [] else [hd1] in
          merge_triplets all_triplets (List.append new_triplet new_triplets) tl1 
      ] in 
      let updated_split = (conf, text, offset, seg_id, no_of_seg, 
                           output_triplets, updated_all_triplets) in 
      modify_chunk_splits (acc @ [updated_split]) True cur_split tl
    else modify_chunk_splits (acc @ [hd]) replaced cur_split tl
  ]
;
(* Adding new segmentation into list of possible segmentations based on 
   number of segments and confidence values for each chunk *)
value add_chunk splits ((cur_conf,_,_,_,cur_no_of_seg,_,_) as cur_split) = 
  loop [] False cur_split splits
  where rec loop acc inserted cur_split = fun
  [ [] -> if inserted then acc else (acc @ [cur_split])
  | [((conf,text,_,_,no_of_seg,_,_) as hd) :: tl] -> 
       if inserted then loop (acc @ [hd]) inserted cur_split tl
       else if no_of_seg > cur_no_of_seg then 
         loop ((acc @ [cur_split]) @ [hd]) True cur_split tl
       else if no_of_seg = cur_no_of_seg then 
         if conf >= cur_conf then loop (acc @ [hd]) inserted cur_split tl
         else loop ((acc @ [cur_split]) @ [hd]) True cur_split tl
       else loop (acc @ [hd]) inserted cur_split tl
  ]
;
(* snd *)
value get_second_comp item = 
  match item with
  [ (_,second,_,_,_,_,_) -> second
  ]
;
(* Adding the new segmentation into the list of possible 
   segmentations for the current chunk *)
value add_to_chunk_splits (conf, text, no_of_seg, triplets) = 
  if text = "" then () (* To not add texts which are not segmentable after 
                          selection of segments by users *)
  else do
  { let temp_list = chunk_solutions.possible_splits
  ; let sol_list = List.map get_second_comp temp_list
  ; let segment_id = List.length chunk_solutions.possible_splits
  ; let new_item = (conf, text, cur_chunk.offset, segment_id, no_of_seg, 
                    triplets, (List.rev triplets))
  ; chunk_solutions.possible_splits := 
    if List.mem text sol_list 
    then edit_chunk_triplets temp_list new_item
    else add_chunk temp_list new_item
  }
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
  | chunk_sol -> do { add_to_chunk_splits (log_chunk chunk_sol); 
                      continue cont }
  ]
;
value init_segment_initial entries sentence = 
  List.map (fun phase -> Advance phase sentence [] []) entries
; 
value segment1 chunk = continue (init_segment_initial initial chunk) 
;
value segment chunk = do
  { cur_chunk.chunk := chunk (* Added to save the unsegmentable chunk for use 
                                later to form segmentation solutions *)
  ; segment1 chunk
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
    ; chunk_solutions.possible_splits := []
    ; let segmentable = segment chunk 
      and local_count = get_counter () in do
      { set_segmentable False
      ; let old_offset = cur_chunk.offset
      ; set_offset (succ extremity,future)
      ; if segmentable then do
           { reset_counter ()
           ; (full,count*local_count) (* overflow may compute modulo *)
             (* we have [local_count] segmentations of the local [chunk], and,
              chunks being independent, the total number of solutions multiply *)
           }
        (* Original chunk is provided if segmentation is not possible *)
        else do 
           { let new_segment = 
               (old_offset,(unknown,(Word.mirror cur_chunk.chunk),Id))
           ; let new_segment_check = 
               if List.mem new_segment chkpts.rejected_segments 
               then chkpts.rejected_segments 
               else chkpts.rejected_segments @ [new_segment]
           ; chkpts.rejected_segments := new_segment_check
           ; let chnk_txt = (Canon.decode_WX chunk) 
             and chnk_sgmnt = 
                   [(old_offset,(unknown,(Word.mirror cur_chunk.chunk),Id))] in 
             chunk_solutions.possible_splits := 
               [(1.0,chnk_txt,1,1,0,chnk_sgmnt,chnk_sgmnt)]
           ; (full,count) (* unsegmentable chunk *)
           }
      }
    }
;
(* For registering the best segments into the newly generated graph *)
value new_register_for_segment (index, ((phase,pada,sandhi) as hd)) = 
  register_pada index hd
;
(* To choose only the index and phased pada for saving the checkpoints *)
value get_double item = 
  match item with
  [ (index,(phase,pada,sandhi)) -> (index,(phase,pada))
  ]
;
value get_best_segments solution_list = 
  let all_segments = get_best [] solution_list
  where rec get_best acc = fun 
  [ [] -> acc 
  | [(_,_,_,chunk_all_triplets) :: tl] -> get_best (acc @ chunk_all_triplets) tl 
  ] in 
  let selected_segments = List.map get_double all_segments in 
  List.sort compare selected_segments
;
(* The graph is reset and rebuilt with only the best segments. 
   The best segments are collected during the dovetailing algorithm below *)
value rebuild_graph solution_list = 
  let _ = reset_graph () in 
  (* Loop through the solutions to register the best segments 
     and also to collect the best segments *)
  let all_segments = register_solution [] solution_list 
  where rec register_solution acc = fun 
  [ [] -> acc
  | [(_,_,_,chunk_all_triplets) :: tl] -> 
      loop chunk_all_triplets
      where rec loop = fun 
      [ [] -> register_solution (acc @ chunk_all_triplets) tl
      | [((index, triple) as hd) :: rest] -> 
          let _ = set_cur_offset (index + 1) in 
          let _ = new_register_for_segment hd in 
          loop rest
      ]
  ] in 
  let selected_segments = List.map get_double all_segments in 
  let sorted_segments = List.sort compare selected_segments in 
  (* Segments not part of the best solutions are rejected here and 
     they are used as checkpoints for further analysis *)
  let filter_func (index,(phase,pada,sandhi)) = 
    not (List.mem (index,(phase,pada)) sorted_segments) in 
  let rejected_segments = 
    add_rejected_seg [] (List.filter filter_func chkpts.rejected_segments)
    where rec add_rejected_seg acc = fun 
    [ [] -> acc 
    | [(index,(phase,word,_)) :: tl] -> 
        add_rejected_seg (acc @ [(index,(phase,word),False)]) tl
    ] in 
  (sorted_segments, rejected_segments)
;
(* Prioritizing top solutions based on confidence values for each segmentation *)
value prioritize splits ((cur_conf, cur_text, cur_output_triplets, 
                          cur_all_triplets) as cur_split) = 
  loop 1 [] False cur_split splits
  where rec loop sol_id acc inserted cur_split = fun
  [ [] -> if inserted then acc 
          else if sol_id > max_best_solutions.val then acc 
          else (acc @ [ cur_split ])
  | [ ((conf,text,output_triplets,all_triplets) as hd) :: tl ] -> 
       if sol_id > max_best_solutions.val then acc
       else if inserted then 
            loop (sol_id + 1) (acc @ [ hd ]) inserted cur_split tl
       else if conf >= cur_conf then 
            loop (sol_id + 1) (acc @ [ hd ]) inserted cur_split tl
       else (* The final condition where the new segmentation is inserted *)
            if sol_id < max_best_solutions.val then 
            loop (sol_id + 2) ((acc @ [ cur_split ]) @ [ hd ]) True cur_split tl
       else if sol_id = max_best_solutions.val then 
            loop (sol_id + 1) (acc @ [ cur_split ]) True cur_split tl 
            (* This condition is to make sure only one segmentation 
            solution is added if it will reach the maximum limit *)
       else loop sol_id acc inserted cur_split tl 
            (* Dummy condition where nothing is inserted *)
  ]
;
(* Traverse through all the chunks recursively, where loop through each of the 
   segmentations of the chunks recursively, until the last chunk, where you 
   form solutions based on higher conf values *)
value get_top_solutions top_segments = 
  (* loop over the chunks *)
  top_solutions (1.0,"",[],[]) [] top_segments
  where rec top_solutions (conf,text,output_triplets,all_triplets) acc = fun
  [ [ (* last *) (chunk_key, chunk_segments) ] -> 
       append (conf,text,output_triplets,all_triplets) acc chunk_segments
       where rec append (conf,text,output_triplets,all_triplets) acc = fun
       [ [] -> acc
       | [ (chunk_conf,chunk_text,_,_,_,
            chunk_output_triplets,chunk_all_triplets) :: rest ] ->
            let new_text = if text = "" then chunk_text 
            else if (String.get text ((String.length text) - 1)) = '-' then 
                 text ^ "" ^ chunk_text 
            else text ^ " " ^ chunk_text
            and new_conf = conf *. chunk_conf
            and new_output_triplets = 
                  List.append chunk_output_triplets output_triplets 
            and new_all_triplets = all_triplets @ chunk_all_triplets in
            append (conf, text, output_triplets, all_triplets) 
                   (prioritize acc (new_conf, new_text, new_output_triplets, 
                                    new_all_triplets)) rest
       ]
  | [ (chunk_key, chunk_segments) :: rest1 ] -> 
       (* loop over the segments of the chunks *)
       loop (conf,text,output_triplets,all_triplets) acc chunk_segments
       where rec loop (conf1, text1, output_triplets1, all_triplets1) acc = fun
       [ [] -> acc
       | [(chunk_conf,chunk_text,_,_,_,
           chunk_output_triplets,chunk_all_triplets) :: rest2] -> 
           let new_text = if text = "" then chunk_text 
           else if (String.get text1 ((String.length text1) - 1)) = '-' then 
                text1 ^ "" ^ chunk_text 
           else text1 ^ " " ^ chunk_text 
           and new_conf = conf1 *. chunk_conf
           and new_output_triplets = 
                 List.append chunk_output_triplets output_triplets1 
           and new_all_triplets = all_triplets1 @ chunk_all_triplets in
           (* Move to next chunk with updated text and conf *)
           let acc1 = top_solutions 
                      (new_conf, new_text, new_output_triplets, new_all_triplets)
                      acc rest1 in
            (* Proceed to next segment of the current chunk *)
           loop (conf1, text1, output_triplets1, all_triplets1) acc1 rest2
       ]
  | [] -> acc
  ]
;
(* called from interface for getting top solutions *)
value dove_tail segments = 
  get_top_solutions chunk_solutions.total_sols
;
(* Modified to update the global variable which stores the segmentations for all
   the chunks as a list of lists *)
value segment_iter chunks = segment_chunks (True,1) chunks
  where rec segment_chunks acc = fun (* terminal recursion *) 
  [ [ (* last *) chunk ] -> do 
      { let (full,count) = (segment_chunk acc chunk False)
      ; chunk_solutions.number_of_chunks := chunk_solutions.number_of_chunks + 1
      ; let new_sols = 
          (chunk_solutions.number_of_chunks, chunk_solutions.possible_splits) 
      ; let modified_chunk_sols = 
          List.append chunk_solutions.total_sols [ new_sols ]
      ; chunk_solutions.total_sols := modified_chunk_sols
      ; (full,count)
      }
  | [ chunk :: rest ] -> do 
      { let (full,count) = 
          (segment_chunk acc chunk (Phonetics.consonant_starts rest))
      ; chunk_solutions.number_of_chunks := chunk_solutions.number_of_chunks + 1
      ; let new_chunk_sols = 
          (chunk_solutions.number_of_chunks, chunk_solutions.possible_splits) 
      ; let modified_chunk_sols = 
          List.append chunk_solutions.total_sols [ new_chunk_sols ]
      ; chunk_solutions.total_sols := modified_chunk_sols
      ; segment_chunks (full,count) rest
      }
  | [] -> acc
  ]
;

end; (* Segment2 *)

