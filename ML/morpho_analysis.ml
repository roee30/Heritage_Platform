(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                     Gérard Huet & Sriram Krishnan                      *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Extracts Stem and Morphological Analysis. Used from Interface2 and 
   Frequency. All relevant functions for morph analyses are present.
   Possible duplicates exist in interface and lexer. *)

open Skt_morph;
open Morphology; (* [inflected] and its constructors [Noun_form], ... *)
open Naming; (* [look_up_homo homo_undo unique_kridantas lexical_kridantas
                 preverbs_structure] *)
open Morpho_string; (* [string_morph string_verbal] *)
open Dispatcher;
open Sandhi; (* [ext_sandhi] *)
open Word;

module Morpho_analysis
  (Phases: sig  
    type phase;
    value generative : phase -> bool;
  end)
  (Lemmas: sig
    type tag_sort =
    [ Atomic of lemmas 
    | Preverbed of (Phases.phase * Phases.phase) and word and word and lemmas 
    ];
    value tags_of : Phases.phase -> Word.word -> tag_sort;
  end)
  (Machine: sig
    value trim_tags : 
      bool -> Word.word -> string -> Morphology.multitag -> Morphology.multitag;
  end)

  = struct

open Phases;
open Lemmas;
open Machine;

type stem = string
and inflectional_morph = string
and base = string 
and derivational_morph = string
;
type morph_anal = (stem * derivational_morph * base * (list inflectional_morph))
and morph_anal_list = list morph_anal
;

(* Choose between the derived stem and base stem according to 
   pre-existing kridantas *)
value krt_stem stem homo bare_stem = 
  match Deco.assoc bare_stem lexical_kridantas with 
  [ [] (* not in lexicon *) -> if stem = [ 3; 32; 1 ] (* ita ifc *) 
                                 then stem 
                               else bare_stem
  | entries (* bare stem is lexicalized *) -> 
      if List.exists (fun (_,h) -> h=homo) entries
        then stem 
      else bare_stem
  ] 
;

(* Extract unique kridantas based on [bare_stem] *)
value krit_infos bare_stem = 
  Deco.assoc bare_stem unique_kridantas
;

(* Get the tuple of [derived_stem], derivational morph analysis, [base_stem]
   and list of all inflectional morph analysis (multi-tags) *)
value get_morph preverbs_str form generative_cond (delta,morphs) = 
  let stem = Word.patch delta form in (* stem may have homo index *)
  let (derived_stem, base_morph, base) = 
    if generative_cond then (* interpret stem as unique name *)
      let (homo,bare_stem) = homo_undo stem in
      try let (verbal,root) = look_up_homo homo (krit_infos bare_stem) in 
        let k_stem = krt_stem stem homo bare_stem in 
        let preverbed_k_stem = (preverbs_str ^ (Canon.decode_WX k_stem))
        and verbal_string = (string_verbal verbal)
        and preverbed_base = (preverbs_str ^ (Canon.decode_WX root)) in 
        (preverbed_k_stem, verbal_string, preverbed_base)
      with [ _ -> ((Canon.decode_WX bare_stem),"","") ]
    else 
      match morphs with
      [ [ Unanalysed ] -> ((Canon.decode_WX stem),"","") 
      | _ -> ((preverbs_str ^ (Canon.decode_WX stem)),"","")
      ] in 
  let morph_list = List.map string_morph morphs in 
  (derived_stem, base_morph, base, morph_list)
;

(* Decomposes a preverb sequence into the list of its components *)
value decomp_pvs pvs = 
  Deco.assoc pvs Naming.preverbs_structure
;

(* Generates "-" separated pre-verbs *)
value get_preverbs_string pvs form  = 
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *)(*i OBSOLETE i*)
           else pvs in
  if pv = [] then ""
  else
    let pvs_list = List.map Canon.decode_WX (decomp_pvs pv) in 
    let pvs_str = String.concat "-" pvs_list in 
    pvs_str ^ "-"
;

(* Generates the morph analysis for the given phase, form and tags *)
value get_morph_analysis pvs phase form tags = 
  let gen = generative phase in 
  let ok_tags = if pvs = [] then tags 
                else trim_tags (generative phase) form (Canon.decode pvs) tags in
  (* NB Existence of the segment warrants that [ok_tags] is not empty *)
  let preverbs_string = get_preverbs_string pvs form  in 
  let morph_fun = get_morph preverbs_string form gen in
  List.map morph_fun ok_tags
;

(* Matches tags based on phase and word as either Atomic construction or 
   Preverbed Construction *)
value morph_analysis_list phase word = 
  match tags_of phase word with 
  [ Atomic tags -> get_morph_analysis [] phase word tags 
  | Preverbed (_,ph) pvs form tags -> get_morph_analysis pvs ph form tags
  ] 
;

(* Generates a string in the same format as is printed in the interface *)
value get_morph_string derived_stem base_morph base morph_list = 
  let morphs_string = String.concat " | " morph_list in 
  "[" ^ derived_stem ^ 
  (if base_morph = "" && base = "" then "" 
  else " { " ^ base_morph ^ " }[" ^ base ^ "]")
  ^ "]{" ^ morphs_string ^ "}"
;

(* Used for debugging - to get the word and it's morph analysis *)
value morph_string word all_morph_list = 
  let form = (Canon.decode_WX (Morpho_html.visargify word)) 
  and all_morph_list_str = 
    List.map (fun (a,b,c,d) -> get_morph_string a b c d) all_morph_list in 
  let tot_morph_str = String.concat " ; " all_morph_list_str in 
  (form ^ " -> " ^ tot_morph_str ^ "<br>")
;

(* Generates a json string of all morphological analyses in a sequence *)
value get_morph_json_string derived_stem base_morph base morph_list = 
  let get_ms x = "\"" ^ x ^ "\"" in 
  let morphs_lst = List.map get_ms morph_list in 
  let morphs_lst_str = "[" ^ (String.concat ", " morphs_lst) ^ "]" in 
  "{\"derived_stem\": \"" ^ derived_stem ^ "\"" ^ 
  ",\"base\": \"" ^ base ^ "\"" ^ 
  ",\"derivatianal_morph\": \"" ^ base_morph ^ "\"" ^ 
  ",\"inflectional_morphs\": " ^ morphs_lst_str ^ "}"
;

(* Get all the morphological analyses of the given phase and word *)
value best_segments acc (phase,rword) =
  let word = Word.mirror rword in 
  let morph_list = morph_analysis_list phase word in 
  let morph_lst = 
    List.map (fun (a,b,c,d) -> get_morph_json_string a b c d) morph_list in 
  acc @ morph_lst
;

(* Get the formatted morphological analyses *)
value get_all_morphs_str final_segments = 
  let morph_json_lst = List.fold_left best_segments [] final_segments in 
  let morph_json_str = String.concat ", " morph_json_lst in 
  ("[" ^ morph_json_str ^ "]")
;

(* Uses Sandhi module for performing sandhi of two given strings *)
value sandhi_fn first second = 
  if first = "" then second 
  else
    let encode = Encode.switch_code "WX" in 
    let left_word = encode first
    and right_word = encode second in
    let rleft_word = Word.mirror left_word in 
    let result_word = ext_sandhi rleft_word right_word in 
    Canon.decode_WX result_word
;
(* Perform sandhi on preverbs separated by "-" *)
value sandhi_preverbs preverbed_str = 
  let str_lst = String.split_on_char '-' preverbed_str in 
  List.fold_left sandhi_fn "" str_lst
;

(* String replace function with regular expression *)
value replace_string reg_exp new_str in_str = 
  Str.global_replace (Str.regexp reg_exp) new_str in_str
;

(* Removes the sense information, and 
   sandhies the preverbs with the stem *)
value pure_stem stem = 
  let hash_removed_stem = replace_string "#[0-9]+" "" stem in 
  let new_stem = 
    if String.contains hash_removed_stem '-' 
      then sandhi_preverbs hash_removed_stem 
    else hash_removed_stem in 
  new_stem
;

end; (*i Morpho_analysis i*)
