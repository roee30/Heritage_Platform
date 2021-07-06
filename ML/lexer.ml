(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Sriram Krishnan                     *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Phrase Lexer - Used by Parser, and Rank for Reader. 
   Uses Phases from Dispatcher to define phase.
   Loads the transducers, calls Dispatch to create module Disp. 
   Calls Segment to build Viccheda, the Sanskrit lexer that undoes sandhi 
   in order to produce a padapaa.tha.
   Exports various print functions for the various modes. *)

open Transduction;
open Canon;
open Skt_morph; (* verbal *) 
open Auto.Auto; (* auto State *)
open Segmenter; (* Segment *)
open Dispatcher; (* Dispatch *) 
open Word; (* word length mirror patch *)

module Lexer (* takes its prelude and control arguments as module parameters *)
  (Prel: sig value prelude : unit -> unit; end) 
  (Lexer_control: sig 
    value star : ref bool; (* chunk = if star then word+ else word *)
    value transducers_ref : ref Load_transducers.transducer_vect;
    end) = struct 

open Html;
open Web; (* ps pl abort etc. *)
open Cgi;
open Phases; (* Phases *) 
open Phases; (* phase generative *) 

module Lemmas = Load_morphs.Morphs Prel Phases
;
open Lemmas; (* [morpho tag_sort tags_of] *)
open Load_transducers; (* [transducer_vect Trans] *)

module Transducers = Trans Prel;

module Machine = Dispatch Transducers Lemmas Lexer_control;
open Machine; (* [color_of_phase transition trim_tags] *) 

module Viccheda = Segment Phases Machine Lexer_control;
     (* [all_checks init_segment continue set_offset set_sa_contro resumption] *)

value all_checks = Viccheda.all_checks
and   set_offset = Viccheda.set_offset
and   set_sa_control = Viccheda.set_sa_control
;
value un_analyzable (chunk : word) = 
  ([ (Unknown,mirror chunk,Machine.Id) ],([]:Viccheda.resumption))
;

(* Statseg *)
type phase2 = 
  { cur_phase : mutable Phases.phase
  }
;
value chk_phase = { cur_phase = unknown}	(* To check the phase of the current segment *)
;
(* To compare lists based on confidence value *)
value compare_conf (_,c1,_,_) (_,c2,_,_) = compare c1 c2
;
(* To compare lists based on string (sentence) *)
value compare_sentence (_,_,_,s1) (_,_,_,s2) = compare s1 s2
;
(* For retrieving data from the data dumped rem file *)
(* The dump file contains the words and frequencies in the decorated trie format *)
type word_attrbs = list (string * int) (* word and frequency are stored as a couplet. For example, <namaH,766> *)
and trans_attrbs = list (string * string * int) (* transition and frequency are stored as a triplet. Transition (u | v -> w) will have the frequency triplet as <u,v,f> *)
                                                (* For example: <[1;16],[1],2637> which represents <aH,a,2637>. letters are already converted to integers of Heritage *)
and freq = int
and freqs = list freq 
and wfreq = Lexmap.lexmap freqs
;
value word_freq = ref (Deco.empty : wfreq)
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
(*value datapath = Paths.skt_install_dir ^ "DATA/"
;*)
(* The following .rem files were generated from the parallel corpus of unsegmented-segmented sentences *)
value words_freq_file = Data.word_freq_file (* .rem file containing the decorated trie of all the words and their frequencies *)
;
value pada_words_freq_file = Data.pada_freq_file (* .rem files containing the decorated trie of all the padas (which are not compound components) and their frequencies *)
;
value comp_words_freq_file = Data.comp_freq_file (* .rem files containing the decorated trie of all the compound components and their frequencies *)
;
value pada_transitions_list = load_transition_list Data.pada_trans_freq_file (* This is a list of tuples <sandhi between words, frequency> *)
;
value comp_transitions_list = load_transition_list Data.comp_trans_freq_file (* This is a list of tuples <sandhi between compound components, frequency> *)
;
(* To calculate the total number of transition instances in the parallel corpus *)
value calculate_transition_freq triplets_list = 
  loop 0 triplets_list
  where rec loop freq_sum = fun
  [ [] -> (float_of_int freq_sum)
  | [(f, s, v)::tl] -> loop (freq_sum + v) tl
  ]
;
(* To calculate the overall word instances. This works with the list of couplets and not with the latest decorated trie. Hence, this is temporarily not used. *)
value calculate_word_freq couplets_list = 
  loop 0 couplets_list
  where rec loop freq_sum = fun
  [ [] -> (float_of_int freq_sum)
  | [(f, s)::tl] -> loop (freq_sum + s) tl
  ]
;
(* Check if this works fine, then keep this instead of the previous one *)
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
     do {
     (*print_string ("Current String:   " ^ "[" ^ (get_strings "" int_list) ^ "]");*)
     "[" ^ (get_strings "" int_list) ^ "]"
     }
;
(* Get freq from weighted lexmap of given word *)
value get_freq word file_name = 
  let word_freq = load_word_freq file_name in
  let updated_word = (Morpho_html.visargify word) in
  let freq = 
  match Deco.assoc updated_word word_freq with
  [ [] -> 0.0
  | e -> let (delta, freq_lst) = (List.hd e) in
         let freq = (List.hd freq_lst) in
         float_of_int freq
  ] in
  freq
;

(* NOTE: The following needs to be calculated from the file rather than manually entering the values *)
value total_words = 403233.0
;
(*value total_padas = (calculate_word_freq padas_list) (* 129423.0 *)
;*)
value total_padas = 284930.0
;
(*value total_comps = (calculate_word_freq comps_list) (* 68556.0 *)
;*)
value total_comps = 118303.0
;
value total_words_types = 37015.0
;
value total_padas_types = 27704.0
;
value total_comps_types = 16130.0
;
value total_pada_transitions = (calculate_transition_freq pada_transitions_list) (* 280622.0 *)
;
value total_comp_transitions = (calculate_transition_freq comp_transitions_list) (* 78907.0 *)
;
value total_pada_transitions_types = float_of_int (List.length pada_transitions_list)
;
value total_comp_transitions_types = float_of_int (List.length comp_transitions_list)
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
  Canon.decode_WX (mirror rword)
;
(* To calculate probability for the word *)
value get_prob rword freq_file tot_ref tot_types = 
  let freq = get_freq rword freq_file in
  if freq = 0.0 then (1.0 /. (tot_ref +. tot_types))
  else (freq /. tot_ref)
;
(* To get the word's probability when using the data as the decorated trie *)
value get_pada_prob rword = 
  get_prob rword pada_words_freq_file total_padas total_padas_types
;
(* To get the compound component's probability when using the data as the decorated trie *)
value get_comp_prob rword = 
  get_prob rword comp_words_freq_file total_comps total_comps_types
;
(* To get the compound component's probability when using the data as the decorated trie *)
value get_word_prob rword = 
  get_prob rword words_freq_file total_words total_words_types
;
(* To get the probability of transition from the list of <transition, frequency> couplets *)
value get_transition_prob transition transition_list tot_transitions total_transition_types = 
  let (w,u,v) = match_transition transition in
  if (u,v) = ([],[]) then (1.0 /. tot_transitions)
  else let first = (int_list_to_string ";" (List.rev u))
       and second = (int_list_to_string ";" v) in
       let trans_freq = find_third first second transition_list in
       if trans_freq = 0.0 then (1.0 /. (tot_transitions +. total_transition_types))
       else (trans_freq /. tot_transitions)
;
(* To get the probability of transition between compound components *)
value get_comp_transition_prob transition = 
  get_transition_prob transition comp_transitions_list total_comp_transitions total_comp_transitions_types
;
(* To get the probability of transition between words *)
value get_pada_transition_prob transition = 
  get_transition_prob transition pada_transitions_list total_pada_transitions total_pada_transitions_types
;
(* To check the phase of the current segment in consideration for getting the probabilities according to the segment's phase (iic/ifc (compound components) vs other phase (words)) *)
(* NOTE: Make sure to include certain phases which could be iics or ifcs. For example: yoddhu in yoddhu-kaamaan *)
value chk_ifc phase = 
  let ifc = 
    if (ii_component chk_phase.cur_phase)
    then True
    else False in do
    { chk_phase.cur_phase := phase
    ; ifc
    }
;
(* Assign unigram freqs for each sandhi rule 
get_rule_freq returns the product of probablities of padas or compound components and their subsequent transition *)
value get_rule_freq1 (phase,rword,transition) =
  let check_ifc = (chk_ifc phase) in
  let (word_prob, transition_prob) = 
    if ((compound_component phase) || check_ifc) (* Condition to check if previously added segment is an ii-component so that the current phase is an ifc *)
    then ((get_comp_prob rword), (get_comp_transition_prob transition))
    else ((get_pada_prob rword), (get_pada_transition_prob transition)) in
    (* Comment the above conditions and use the following if the single list for both words and compound components is used *)
    (*let w_prob = get_word_prob rword in
    if ((compound_component phase) || check_ifc) (* Condition to check if previously added segment is an ii-component so that the current phase is an ifc *)
    then (w_prob, (get_comp_transition_prob transition))
    else (w_prob, (get_pada_transition_prob transition)) in*)
  (* The following condition is given to add '-' between compound components, and ' '  between padas *)
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  let word = 
    if (ii_component phase) then (decode_word ^ "-")
    else (decode_word ^ " ") in
  if transition_prob = 1.0 then (1.0, word) (* Sriram: Check if this condition is necessary *)
  else
  (word_prob, word) (* considers confidence value for a segment as word_probability alone *)
  (*(transition_prob, word)*) (* considers confidence value for a segment as transition_probability alone *)
  (*let cur_prob = (word_prob *. transition_prob) in
  (cur_prob, word)*) (* considers confidence value for a segment as <word_probability * transition_probability> *)
;
(* base function defined to return the confidence level for each segmentation as a product of the probablities of each rule *)
(* Note: the confidence level and the complete string are added to the output *)
value add_conf_level1 (n,output) =
 let outlist = List.rev output in
 loop1 1.0 "" outlist
 where rec loop1 cl sentence = fun
 [ [] -> (n,cl,output,String.trim(sentence))
 | [l :: r] -> let (f, words) = get_rule_freq1 l in
               loop1 (cl*.f) (sentence ^ words) r
 ]
; 
(* The above works well with the confidence formula as just the product of prob(word) and prob(transition) *)

(* The confidence formula is modified to product of prob(word_1), prob(transition) and prob(word_2) in the following *)
(* The following is changed to prob(word) and prob(transition) and prob(next_word) *)
value get_rule_freq2 (phase1,rword1,transition1) (phase2,rword2,transition2) =
  let check_ifc = (chk_ifc phase1) in
  let (first_word_prob, transition_prob) = 
    if ((compound_component phase1) || check_ifc) (* Condition to check if previously added segment is an ii-component so that the current phase is an ifc *)
    then ((get_comp_prob rword1), (get_comp_transition_prob transition1))
    else ((get_pada_prob rword1), (get_pada_transition_prob transition1)) in
  let second_word_prob = 
    if (ii_component phase2 || ii_component phase1) (* This condition is put to check if the second word is an ii-component or an if-component of the first word *)
    then get_comp_prob rword2
    else get_pada_prob rword2
  in (* The following condition is given to add '-' between compound components words, and ' '  between padas *)
  let decode_word = get_word rword1 in
  let word = if (ii_component phase1) then (decode_word ^ "-")
             else (decode_word ^ " ") in
  if transition_prob = 1.0 then (1.0, word) (* Sriram: Check if this condition is necessary *)
  else ((first_word_prob *. transition_prob *. second_word_prob), word) (* considers confidence value for a segment as <first_word_probability * transition_probability * second_word_probability> *)
  (*else ((first_word_prob *. second_word_prob), word) (* considers confidence value for a segment as <first_word_probability * second_word_probability> *)*)
;
value add_conf_level2 (n,output) =
 let outlist = List.rev output in
 loop1 1.0 "" outlist
 where rec loop1 cl sentence = fun
 [ [] -> (n,cl,output,String.trim(sentence))
 | [l :: r] ->  let s = try List.hd r (* Here two items are intended to be passed to the get_rule_freq function, and hence r's head is taken *)
                        with [
                        Failure hd -> (Phases.Unknown, [], Id) (* In case it is not found, or any exception raises, then a dummy one is sent *)
                        ] 
                in let (f, words) = get_rule_freq2 l s in
                loop1 (cl*.f) (sentence ^ words) r
 (*| [o :: t :: r] -> let (f, words) = get_rule_freq o t in loop1 (cl*.f) (sentence ^ words) (t @ r)*)
 ]
;
(* To provide the solution numbers for each of the solution, so that the parser in the next page of the interface uses these numbers *)
value give_sol_numbers solution =
 loop [] 1 solution
 where rec loop acc num = fun
 [ [] -> acc
 | [l :: r] -> let (n,cl,sol,sentence) = l in
 		let acc1 = acc @ [(num,cl,sol,sentence)] in loop acc1 (num+1) r
 ]
;
(* sorts the solutions according to the confidence level *) 
value prioritize revsols =
  loop1 [] revsols
  where rec loop1 acc1 = fun
  [ [] -> (* To sort and remove duplicates based on sentence *)
          let temp_sol = List.sort_uniq compare_sentence acc1 in
          (* To make the list in descending order of confidence value *)
          let final_sol = List.rev (List.sort compare_conf temp_sol) in
          (* To give ordered solution numbers for the solutions *)
          (*give_sol_numbers*) final_sol
          (* give_sol_numbers acc1 (* For checking the performance of the original Reader, uncomment this line and comment the previous 4 lines *) *)
  |[l :: r] -> let (n,cl,sol,sentence) = add_conf_level1 l in (* if the confidence value is <word_probability * transition_probability> *)
               (*let (n,cl,sol,sentence) = add_conf_level2 l in (* if the confidence value is <word1_probability * transition_probability * word2_probability> *)*)
               let acc = acc1 @ [(n,cl,sol,sentence)] in
               loop1 acc r 
  ]
;

(* Printing *)

value table_morph_of phase = table_begin (background (color_of_phase phase)) 
;
value print_morph pvs cached seg_num gen form n tag = do
(* n is the index in the list of tags of an ambiguous form *)
  { tr_begin |> ps
  ; th_begin |> ps
  ; span_begin Latin12 |> ps
  ; Morpho_html.print_inflected_link pvs cached form (seg_num,n) gen tag 
  ; span_end |> ps
  ; th_end |> ps
  ; tr_end |> ps  
  ; n+1
  }
;
value print_tags pvs seg_num phase form tags =   
  let ptag = print_morph pvs (is_cache phase) seg_num (generative phase) form in 
  let _ = List.fold_left ptag 1 tags in ()
;
value rec scl_phase = fun
  [ Pv | Pvc | Pvv | Pvkc | Pvkv -> "pv"
  | Noun | Nouc | Nouv | Krid | Kriv | Kric | Lopak | Pron | Auxik 
         | Cache -> "noun"
  | Root | Lopa | Auxi -> "root"
  | Inde | Abso | Absv | Absc | Avy | Auxiinv -> "inde"
  | Iic | A | An | Iicv | Iicc | Iik | Iikv | Iikc | Iiif | Auxiick
        | Ai | Ani | Cachei -> "iic"
  | Iiv | Iivv | Iivc -> "iiv" 
  | Iiy -> "iiy" 
  | Peri -> "peri" 
  | Inftu -> "inftu" 
  | Kama -> "kama" 
  | Voca | Vocv | Vocc | Inv | Vok | Vokv | Vokc | Vocf -> "voca"
  | Ifc | Ifcv | Ifcc | Indifc -> "ifc"
  | Unknown -> "unknown"
  | Comp (_,ph) _ _ -> "preverbed " ^ scl_phase ph
  ]
;
value print_scl_morph pvs gen form tag = do
  { xml_begin "tag" |> ps
  ; Morpho_scl.print_scl_inflected pvs form gen tag  
  ; xml_end "tag" |> ps
  }
;
value print_scl_tags pvs phase form tags = 
  let table phase = 
      xml_begin_with_att "tags" [ ("phase",scl_phase phase) ] in do
  { table phase |> ps
  ; List.iter (print_scl_morph pvs (generative phase) form) tags 
  ; xml_end "tags" |> ps
  }
;
(* These definitions are for export to Parser.
   They betray a difficuly in the modular organisation, since Parser sees
   Lexer, but not [Load_morphs] or Dispatcher. Modules ought to be revised. *)
value tags_of = Lemmas.tags_of 
and trim_tags = Machine.trim_tags
;
(* Keeps only relevant tags with [trim_tags] *)(*i Cochonnerie i*)
value extract_lemma phase word = 
  match tags_of phase word with  
  [ Atomic tags -> tags 
  | Preverbed (_,phase) pvs form tags -> (* tags to be trimmed to [ok_tags] *)
     if pvs = [] then tags 
     else trim_tags (generative phase) form (Canon.decode pvs) tags 
  ]
; 
(* Returns the offset correction (used by SL interface) *)
value process_transition = fun  
  [ Euphony (w,u,v) ->   
    let off = if w=[] then 1 (* amui/lopa from Lopa/Lopak *)
                      else length w in
    off - (length u + length v) 
  | Id -> 0
  ]
;
value print_transition = fun
  [ Euphony (w,u,v) -> Morpho_html.print_sandhi u v w 
  | Id -> ()
  ]
;
value process_kridanta pvs seg_num phase form tags = do
  { th_begin |> ps
  ; table_morph_of phase |> pl          (* table begin *)
  ; let ok_tags = 
        if pvs = [] then tags 
        else trim_tags (generative phase) form (Canon.decode pvs) tags in do
        (* NB Existence of the segment warrants that [ok_tags] is not empty *)
  { print_tags pvs seg_num phase form ok_tags 
  ; table_end |> ps                     (* table end *) 
  ; th_end |> ps
  ; (phase, form, ok_tags) (* value used by [Parser.print_segment_roles] *)
  }}
; 
(* Same recursive structure as [Interface.print_morpho] *)
value print_morpho phase word = do  
  { table_morph_of phase |> pl          (* table begin *)  
  ; tr_begin |> ps
  ; th_begin |> ps
  ; span_begin Latin12 |> ps 
  ; let _ = 
       match tags_of phase word with 
       [ Atomic tags ->  
          process_kridanta [] 0 phase word tags
       | Preverbed (_,phase) pvs form tags -> 
          process_kridanta pvs 0 phase form tags
       ] in () 
  ; span_end |> ps
  ; th_end |> ps 
  ; tr_end |> ps
  ; table_end |> ps                      (* table end *)
  }
;
(* Segment printing with phonetics without semantics for Reader *)
value print_segment offset (phase,rword,transition) = do 
  { "[ " |> ps
  ; Morpho_html.print_signifiant_off rword offset
  ; print_morpho phase (mirror rword)
  (* Now we print the sandhi transition *)
  ; "&lang;" |> ps (* < *) 
  ; let correction = process_transition transition in do  
      { print_transition transition
      ; "&rang;]" |> pl (* >] *)
      ; html_break |> pl
      ; offset+correction+length rword
      }
  }
; 
(* Segment printing with only the sentence without the phase and transition details *)
value print_segment_words offset (phase,rword,transition) = do
  { Morpho_html.print_signifiant_off rword offset 
  ; if (ii_component phase) then ("-" |> ps)
    else (" " |> ps)
  ; let correction = process_transition transition in 
    offset+correction+(length rword)
  }
;
value get_sandhi_word (phase,rword,transition) = 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  if (ii_component phase) then (decode_word ^ "-")
  else (decode_word ^ " ")
;
value print_segment_to_file cho (phase,rword,transition) = 
  let decode_word = Canon.decode_WX (Morpho_html.visargify rword) in
  if (ii_component phase) then (output_string cho (decode_word ^ "-"))
  else (output_string cho decode_word)
;
(* Similarly for [scl_plugin] mode (without offset and transitions) *)
(* Called from [Scl_parser.print_scl_output] *)
value print_scl_segment counter (phase,rword) =  
  let word = Morpho_html.visargify rword in do
  { let solid = background (Phases.color_of_phase phase) in
    td_begin_class solid |> pl
  ; let ic = string_of_int counter in
    "<input type=\"hidden\" name=\"field" ^ ic ^ "\" value='<form wx=\""
        ^ Canon.decode_WX word ^ "\"/>" |> ps
  ; match tags_of phase (mirror rword) with 
    [ Atomic tags ->
          print_scl_tags [] phase word tags
    | Preverbed (_,phase) pvs form tags -> 
         let ok_tags = 
           if pvs = [] then tags 
           else trim_tags (generative phase) form (Canon.decode pvs) tags in
         print_scl_tags pvs phase form ok_tags
    ] 
  ; "'>" |> ps (* closes <input *) 
  ; Canon.unidevcode word |> ps
  ; td_end |> ps
  ; "\n" |> ps
  ; counter+1
  } 
; 
end;

