(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Amba Kulkarni & Sriram Krishnan             *)
(*                                                                        *)
(* ©2022 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Used by [Graph_segmenter2] while registering every segment in interface
   Gets the frequencies of each segment depending on whether the segment is
   a word or a compound component.  Loads frequency data from Resources:
   
   [comp_freq_ref] -> compound components (iic and ifc) and their frequencies
   [pada_freq_ref] -> words (not iic and ifc) and their frequencies
   [word_freq_ref] -> all words and their frequencies
   [pada_morphs_freq_ref] -> stem and morph analysis of compound components
   (iic and ifc) and their frequencies
   [comp_morphs_freq_ref] -> stem and morph analysis of words (not iic and ifc)
   and their frequencies
   [comp_transitions_list] -> transitions (sandhi) between compound components
   [pada_transitions_list] -> transitions (sandhi) between words
   
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
  (Morpho_anal: sig (* To be instantiated by Interface2 *)
    type morph_anal = (string * string * string * (list string))
    and morph_anal_list = list morph_anal;
    value morph_analysis_list : Phases.phase -> Word.word -> morph_anal_list;
    value pure_stem : string -> string;
  end)
= struct

open Phases;
open Eilenberg;
open Morpho_anal;

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
type morph = string
and base = string
and base_morph = string
;
type morph_frq = list (morph * base * base_morph * freqs)
;
type sm_freq = Lexmap.lexmap morph_frq
;

(* [Provided internally as fmode in interface2:
   Frequency_Word -> 
    considers joint probability of frequencies of word-forms 
   Frequency_Transition -> 
    considers joint probability of frequencies of transitions/sandhi 
   Frequency_Stem -> 
    considers joint probability of frequencies of stems 
   Frequency_Morph -> 
    considers joint probability of frequencies of morph analysis 
   Frequency_Word_Transition -> 
    considers joint probability of products of word, transition frequencies
   Frequency_Stem_Morph -> 
    considers joint probability of frequencies of 
    <stem, inf_morph, base, der_morph>] *)
type frequency_mode = 
  [ Frequency_Word 
  | Frequency_Transition 
  | Frequency_Stem 
  | Frequency_Morph 
  | Frequency_Word_Transition 
  | Frequency_Stem_Morph
  ]
;

value freq_mode = ref (Frequency_Stem_Morph : frequency_mode)
;

(* The Best modes which use Morph based frequencies will have the same 
   segmentations each one having different morphological analysis for one or 
   more segments. Whereas the best modes which use Word base frequencies 
   do not require morph level information and hence by neglecting the morph
   information, multiple segmentation solutions collapse to one owing to 
   same segmentation forms. The following two functions are used to alter 
   such functionalities based on the [frequency_mode]. *)
value word_based_freq fmode = 
  match fmode with 
  [ Frequency_Word | Frequency_Transition| Frequency_Word_Transition
    -> True
  | _ -> False
  ]
;
value morph_based_freq fmode = 
  match fmode with  
  [ Frequency_Stem | Frequency_Morph | Frequency_Stem_Morph
    -> True
  | _ -> False
  ]
;

(* Convert Frequency Type to its id used as environment variable string *)
value fmode_id_of_fmode fmode = 
  match fmode with
  [ Frequency_Word -> "w"
  | Frequency_Transition -> "t"
  | Frequency_Stem -> "s"
  | Frequency_Morph -> "m"
  | Frequency_Word_Transition -> "x"
  | Frequency_Stem_Morph -> "n"
  | _ -> "n"
  ] 
;

(* Convert id used as environment variable string to its Frequency Type *)
value fmode_of_fmode_id fmode_id = 
  match fmode_id with
  [ "w" -> Frequency_Word
  | "t" -> Frequency_Transition
  | "s" -> Frequency_Stem
  | "m" -> Frequency_Morph
  | "x" -> Frequency_Word_Transition
  | "n" -> Frequency_Stem_Morph
  | _ -> Frequency_Stem_Morph
  ] 
;

(* Frequency reference variables which will be extracted during runtime *)
value word_freq_ref = ref (Deco.empty : wfreq)
;
value pada_freq_ref = ref (Deco.empty : wfreq)
;
value comp_freq_ref = ref (Deco.empty : wfreq)
;
value pada_morphs_freq_ref = ref (Deco.empty : sm_freq)
;
value comp_morphs_freq_ref = ref (Deco.empty : sm_freq)
;

(* The values required for probability calculations
   (obtained during runtime) *)
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
; (* 784.0 *)
value total_comp_transitions_types = ref 0.0 
; (* 381.0 *)
value total_pada_morphs = ref 0.0  
; (* 291751.0 *)
value total_comp_morphs = ref 0.0  
; (* 118958.0 *)
value total_pada_morphs_types = ref 0.0 
; (* 33000.0 *)
value total_comp_morphs_types = ref 0.0 
; (* 18770.0 *)

(* Boolean value indicating whether the best of multi-tag freqs is to be 
   taken or the sum of all multi-tag freqs is to be taken. Temporarily 
   assigned manually. 
   True indicates sum of all multi-tag freqs.
   False indicates best of all multi-tag freqs *)
value phase_level_analysis = ref True
;
(* List of tuples <sandhi between words, frequency> *)
value pada_transitions_list = ref ([] : trans_attrbs)
;
(* List of tuples <sandhi between compound components, frequency> *)
value comp_transitions_list = ref ([] : trans_attrbs)
;

(* Functions to load the .rem files *)
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
value load_morph_freq file = 
  (Gen.gobble file : sm_freq)
;

(* Calculates the total frequencies of all the morph tuples 
   and also the total unique morph tuples *)
value calculate_morph_sum (word, flm) type_tot val_tot  =
  loop type_tot val_tot flm
  where rec loop typ tot = fun 
  [ [] -> (typ, tot)
  | [ (x,y) :: tl ] ->
      let (c,d) = 
      loop1 typ tot y
      where rec loop1 a b = fun 
      [ [] -> (a,b)
      | [ (_,_,_,fs) :: tl1 ] -> 
        let new_type_tot = a + 1 
        and new_val_tot = b + (List.hd fs) in 
        loop1 new_type_tot new_val_tot tl1
      ] in 
      loop c d tl
  ]
;

(* To retrieve the sum of frequencies of all morph tuples 
   and the number of such unique entries *)
value rec process_morph_deco type_tot val_tot = fun
  [ [ hd :: tl ] -> 
    let (new_type_tot, new_val_tot) = calculate_morph_sum hd type_tot val_tot in 
          process_morph_deco new_type_tot new_val_tot tl 
  | [] -> (float_of_int type_tot, float_of_int val_tot)
  ]
;

(* Calculates the total frequencies of all the words 
   and also the total unique words *)
value calculate_sum (word, flm) type_tot val_tot  =
  match (List.hd flm) with 
  [ (delta, freqs) -> 
      let new_type_tot = type_tot + 1 
      and new_val_tot = val_tot + (List.hd freqs) in 
      (new_type_tot, new_val_tot)
  ]
;
(* To retrieve the sum of frequencies of all words
   and the number of such unique entries *)
value rec process_deco type_tot val_tot = fun
  [ [ hd :: tl ] -> 
    let (new_type_tot, new_val_tot) = calculate_sum hd type_tot val_tot in 
          process_deco new_type_tot new_val_tot tl 
  | [] -> (float_of_int type_tot, float_of_int val_tot)
  ]
;
(* To calculate the total number of transition entries *)
value calculate_transitions triplets_list = 
  loop 0 triplets_list
  where rec loop freq_sum = fun
  [ [] -> (float_of_int freq_sum)
  | [ (f, s, v) :: tl ] -> loop (freq_sum + v) tl
  ]
;
(* Boolean value to keep track of whether the current segment is a compound
   component. Updated for every segment by checking the phase *)
value cur_phase_iic = ref False
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
(* Get frequency of the given tuple from the weighted lexmap of [morph_freq] *)
value morph_frequency word morph_str base_str base_morph_str freq_ref = 
  match Deco.assoc word freq_ref with
  [ [] -> 0.0
  | [ (x,e) ] -> let frq = match_morph 0 e
         where rec match_morph high = fun 
         [ [] -> high
         | [ (m,b,bm,f) :: tl ] -> 
             if ((m = morph_str) && (b = base_str) && (bm = base_morph_str))
              then match_morph (List.hd f) tl
             else match_morph high tl
         ] in 
         float_of_int frq
  ]
;
(* Updates the frequency based on whether the best of multi-tags are to be 
   considered or the sum of the multi-tags are to be considered *)
value update_freq cur_freq calc_freq = 
   if phase_level_analysis.val then 
     cur_freq +. calc_freq
   else 
     if cur_freq > calc_freq then cur_freq
     else calc_freq
;
(* The indeclinables were stored with different terminologies in the dataset 
   and hence all these other possibilities have to be considered if the morph
   type is ind. *)
value other_possibilities first second third fourth freq_ref = 
  let ind_types = [ "part."; "prep."; "conj."; "abs."; "adv."; "tasil" ] in 
  let new_highest_freq = 
  loop 0.0 ind_types 
  where rec loop acc = fun 
  [ [] -> acc
  | [ hd :: tl ] -> 
    let new_freq = 
      morph_frequency first hd third fourth freq_ref in 
    loop (update_freq new_freq acc) tl 
    (* [if new_freq > acc then loop new_freq tl
    else loop acc tl] *)
  ] in 
  new_highest_freq
;
(* Get freq from weighted lexmap of given stem *)
value get_morph_freq stem morph_str base_str base_morph_str freq_ref = 
  let word = Transduction.code_raw_WX (stem) in 
  let freq = morph_frequency word morph_str base_str base_morph_str freq_ref in
  if freq = 0.0 && morph_str = "ind."
    then other_possibilities word morph_str base_str base_morph_str freq_ref
  else freq
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
(* Extract stem, inflectional morph, base and derivational morph, 
   get the associated frequency,
   calculate the probability *)
value morph_prob phase rword freq_ref tot_morphs tot_morphs_types =
  let all_morph_list = morph_analysis_list phase (Word.mirror rword) in 
  let highest_morph_freq = loop 0.0 all_morph_list
  where rec loop acc1 = fun 
  [ [] -> acc1
  | [ (der_s, bas_m, bas_s, der_m) :: tl1 ] -> 
    let new_f = 
      loop1 0.0 der_m
      where rec loop1 acc = fun 
      [ [] -> acc
      | [ hd :: tl ] -> 
          let new_der_s = pure_stem der_s 
          and new_bas_s = pure_stem bas_s in 
          let new_freq = get_morph_freq new_der_s hd new_bas_s bas_m freq_ref in 
          loop1 (update_freq new_freq acc) tl 
      ] in 
    loop (update_freq new_f acc1) tl1 
  ] in 
  if highest_morph_freq = 0.0 then 
    (1.0 /. (tot_morphs +. tot_morphs_types))
  else (highest_morph_freq /. tot_morphs)
;
(* Calculate probability of compound component from phase and word *)
value get_comp_morph_prob phase rword = 
  morph_prob phase rword comp_morphs_freq_ref.val total_comp_morphs.val 
                 total_comp_morphs_types.val
;
(* Calculate probability of word from phase and word *)
value get_pada_morph_prob phase rword = 
  morph_prob phase rword pada_morphs_freq_ref.val total_pada_morphs.val 
                 total_pada_morphs_types.val
;
(* NOTE: Make sure to include certain phases which could be iics or ifcs. 
   For example: yoddhu in yoddhu-kaamaan *)
(* Assign unigram freqs for each sandhi rule 
   [get_word_transition_probability] returns the product of 
   the probablity of pada / compound component, and 
   the probability of its subsequent transitions *)
value get_word_transition_probability (phase,rword,transition) =
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
  let _ = cur_phase_iic.val := (ii_component phase) in 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  (* The following condition is given to add '-' between compound components, 
     and ' ' between normal words *)
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word) in
  (* confidence value for a segment depends on 
     [<word_probability * transition_probability>] *)
  let cur_prob = (word_prob *. transition_prob) in
  (cur_prob, word)
;
(* [get_transition_probability] returns the probability of the transition
   or the sandhi that happens after the given segment *)
value get_transition_probability (phase,rword,transition) = 
  let transition_prob = 
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || cur_phase_iic.val) 
    then (get_comp_transition_prob transition)
    else (get_pada_transition_prob transition) in
  let _ = cur_phase_iic.val := (ii_component phase) in 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  (* The following condition is given to add '-' between compound components, 
     and ' ' between normal words *)
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word) in
  (* confidence value for a segment depends on [transition_probability] alone *)
  (transition_prob, word)
;

(* [get_word_probability] returns the probability of the word represented
   by the segment *)
value get_word_probability (phase,rword,transition) = 
  let word_prob = 
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || cur_phase_iic.val) 
    then (get_comp_prob rword)
    else (get_pada_prob rword) in
    (* Comment the above conditions and use the following 
       if the single list for both words and compound components is used *)
    (* [let w_prob = get_word_prob rword in
    (* Check if current segment is an iic or 
       Check if previously added segment is an ii-component 
       so that the current phase is an ifc *)
    if ((compound_component phase) || check_ifc) 
    then (w_prob, (get_comp_transition_prob transition))
    else (w_prob, (get_pada_transition_prob transition)) in] *)
  let _ = cur_phase_iic.val := (ii_component phase) in 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  (* The following condition is given to add '-' between compound components, 
     and ' ' between normal words *)
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word) in
  (* confidence value for a segment depends on [word_probability] alone *)
  (word_prob, word) 
;

(* [get_stem_morph_probability] returns the probability of the tuple
   [(stem, inflectional_morph, base, derivational_morph)] which are extracted
   from the segment *)
value get_stem_morph_probability (phase,rword,transition) = 
  let morph_prob = 
    if ((compound_component phase) || cur_phase_iic.val) 
    then get_comp_morph_prob phase rword
    else get_pada_morph_prob phase rword in 
  let _ = cur_phase_iic.val := (ii_component phase) in 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word) in
  (morph_prob, word)
;

(* NOTE: Add functions to check frequency based on stem and morph alone *)

(* Assign probability calculation function based on the fmode *)
value get_probability f_mode = 
  match f_mode with 
  [ Frequency_Word -> get_word_probability
  | Frequency_Transition -> get_transition_probability
  | Frequency_Word_Transition -> get_word_transition_probability
  | Frequency_Stem | Frequency_Morph | Frequency_Stem_Morph -> 
      get_stem_morph_probability
  ]
;

(* Loads frequency files for words and compound components
   Assigns constants for words anf compound components
   (to be used for probability calculations) *)
value assign_word_frequencies = do 
 { word_freq_ref.val := load_word_freq Data.word_freq_file
 ; pada_freq_ref.val := load_word_freq Data.pada_freq_file
 ; comp_freq_ref.val := load_word_freq Data.comp_freq_file
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

(* Loads frequency files for transitions
   Assigns constants for transitions
   (to be used for probability calculations) *)
value assign_transition_frequencies = do 
 { pada_transitions_list.val := load_transition_list Data.pada_trans_freq_file
 ; comp_transitions_list.val := load_transition_list Data.comp_trans_freq_file
 ; total_pada_transitions.val := calculate_transitions pada_transitions_list.val
 ; total_comp_transitions.val := calculate_transitions comp_transitions_list.val
 ; let pada_trans_len = List.length pada_transitions_list.val in 
   total_pada_transitions_types.val := float_of_int pada_trans_len 
 ; let comp_trans_len = List.length comp_transitions_list.val in 
   total_comp_transitions_types.val := float_of_int comp_trans_len 
 }
;

(* Loads frequency files for stem and morphological analyses
   Assigns constants for stem and morphological analyses
   (to be used for probability calculations) *)
value assign_stem_morph_frequencies = do 
 { pada_morphs_freq_ref.val := load_morph_freq Data.pada_morphs_freq_ref_file
 ; comp_morphs_freq_ref.val := load_morph_freq Data.comp_morphs_freq_ref_file
 ; let (pada_morphs_types, pada_morphs) = 
     process_morph_deco 0 0 (Deco.contents pada_morphs_freq_ref.val)
   and (comp_morphs_types, comp_morphs) = 
     process_morph_deco 0 0 (Deco.contents comp_morphs_freq_ref.val)
   in do 
   { total_pada_morphs.val := pada_morphs
   ; total_pada_morphs_types.val := pada_morphs_types
   ; total_comp_morphs.val := comp_morphs
   ; total_comp_morphs_types.val := comp_morphs_types
   }
 }
;

(* Loads frequency data structures and assigns constants based on
   frequency mode being used *)
value assign_frequency f_mode = do 
 { freq_mode.val := (fmode_of_fmode_id f_mode)
 ; match freq_mode.val with 
   [ Frequency_Word -> assign_word_frequencies
   | Frequency_Transition -> assign_transition_frequencies
   | Frequency_Word_Transition -> do
     { assign_word_frequencies
     ; assign_transition_frequencies
     }
   | Frequency_Stem | Frequency_Morph | Frequency_Stem_Morph -> 
     assign_stem_morph_frequencies
   ]
 }
;

end;
