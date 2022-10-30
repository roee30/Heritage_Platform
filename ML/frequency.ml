(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Amba Kulkarni & Sriram Krishnan             *)
(*                                                                        *)
(* ©2022 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Used by Graph_segmenter2 while registering every segment.
   Gets the frequencies of each segment depending on whether the segment is
   a word or a compound component.  Loads frequency data from Resources:
   
   comp_freq_ref -> compound components (iic and ifc) and their frequencies
   pada_freq_ref -> words (not iic and ifc) and their frequencies
   word_freq_ref -> all words and their frequencies
   comp_transitions_list -> transitions (sandhi) between compound components
   pada_transitions_list -> transitions (sandhi) between words
   
   Calculates word and transition probabilities for each segment *)
   
open Auto.Auto; (* auto *)

module Frequency 
  (Phases: sig  
         type phase;
         value compound_component: phase -> bool;
         value ii_component: phase -> bool;
         end)
  (Eilenberg: sig (* To be instanciated by Dispatcher *)
         type transition = (* junction relation *)
         [ Euphony of rule (* [(w,rev u,v)] such that [u|v -> w] *)
         | Id              (* identity or no sandhi *)
         ];
         end)
  = struct

open Phases;
open Eilenberg;

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
(* Boolean value to keep track of whether the current segment is a compound
   component. Updated for every segment by checking the phase *)
value cur_phase_iic = ref False
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
(*  ([],[],[])*)
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
value assign_frequency = do 
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
(* NOTE: Make sure to include certain phases which could be iics or ifcs. 
   For example: yoddhu in yoddhu-kaamaan *)
(* Assign unigram freqs for each sandhi rule 
   [get_rule_freq] returns the product of 
   the probablity of pada / compound component, and 
   the probability of its subsequent transitions *)
value get_rule_freq (phase,rword,transition) =
(*  let check_ifc = (chk_ifc phase) in*)
  let (word_prob, transition_prob) = 
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || cur_phase_iic.val) 
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
  let _ = cur_phase_iic.val := (ii_component phase) in 
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

end;
