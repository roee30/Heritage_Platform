(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Encode = struct i*)

(* Defines various encodings of transliterated strings into words as int lists *)

open Transduction; (* [code_raw] and similar *)
open Phonetics; (* homonasal vowel *)
exception In_error of string (* Error in user or corpus input *)
;
value is_vowel c = vowel c || avagraha c 
                   || c>100 && c<114 (* Deprecated accounts for upper case *)
;
(* anusvara substituted by nasal or normalized to 14 when original *)
(* anunaasika after vowels treated as anusvaara *)
value rec normalize = normal_rec False
  where rec normal_rec after_vow = fun
  [ [] -> []
  | [ 14 (* .m *) ] -> [ 14 ] (* and NOT m *)
  | [ 14 (* .m *) :: [ c :: l ] ] -> 
    if after_vow then
       let c' = homonasal c in [ c' :: [ c :: normal_rec (is_vowel c) l ] ]
    else raise (In_error "Anusvaara should follow vowel")
  | [ 15 (* ~~ *) :: [ c :: l ] ] -> (* 31-08-19 anunaasika normalisation *)
    if after_vow then (* anunaasika assimilated to anasvaara *)
       let c' = homonasal c in [ c' :: [ c :: normal_rec (is_vowel c) l ] ] 
    else [ 15 :: normal_rec False [ c :: l ] ]
  | [ 16 (* .h *) ] -> 
    if after_vow then [ 16 ]
    else raise (In_error "Visarga should follow vowel")
(* No change to visarga since eg praata.hsvasu.h comes from praatar|svasu.h
   This is contrary to Henry§43 note 1, corresponding to the following code: [
  | [ 16 (* .h *) :: [ c :: l ] ] -> 
    if after_vow then 
       let c' = if sibilant c then c else 16 (* du.hkha *) in
       [ c' :: [ c :: normal_rec (is_vowel c) l ] ]
    else raise (In_error "Visarga should follow vowel")] 
   Consequently, dukuula is listed after du.hstha, like in MW, but not Renou.
   Apte avoids the problem by putting all dus-X under dus.  *)
  | [ 50 :: l ] -> [ 50 :: normal_rec False l ]  (* hiatus *)
  | [ c :: l ] -> [ c :: normal_rec (is_vowel c) l ] 
  ]
;
value code_string    str = normalize (code_raw str) (* standard VH *)
and   code_string_WX str = normalize (code_raw_WX str)
and   code_string_KH str = normalize (code_raw_KH str) 
and   code_string_SL str = normalize (code_raw_SL str)
and   code_skt_ref   str = normalize (code_rawu str) (* with upper letters *)
and   code_skt_ref_d str = normalize (code_rawd str) (* no diacritics *)
;
(* Switching code function according to transliteration convention *)
value switch_code = fun (* normalizes anusvaara in its input *)
  [ "VH" -> code_string    (* [Canon.decode]    *)
  | "WX" -> code_string_WX (* [Canon.decode_WX] *)
  | "KH" -> code_string_KH (* [Canon.decode_KH] *)
  | "SL" -> code_string_SL (* [Canon.decode_SL] *)
  | _ -> failwith "Unknown transliteration scheme"
  ] 
;
(* Switching code function according to transliteration convention *)
value switch_code_no_norm = fun (* does not normalize anusvaara in its input *)
  [ "VH" -> code_raw    (* [Canon.decode]    *)
  | "WX" -> code_raw_WX (* [Canon.decode_WX] *)
  | "KH" -> code_raw_KH (* [Canon.decode_KH] *)
  | "SL" -> code_raw_SL (* [Canon.decode_SL] *)
  | _ -> failwith "Unknown transliteration scheme"
  ]
;
value rev_code_string str = Word.mirror (code_string str)
;
(* [anchor : string -> string] -- used in [Morpho_html.url] and Sanskrit *)
value anchor t =
  let canon c = if c>100 then Canon.canon_upper_html c 
                         else Canon.canon_html c in
  let catenate c (s,b) = (* similar to Canon.catenate *)
      let b'= c>0 && c<14  (* Phonetics.vowel c *) in
      let hiatus = if b && b' then "_" ^ s else s in
      (canon c ^ hiatus , b') in
  let word = code_skt_ref t in
  let (s,_) = List.fold_right catenate word ("",False) in s 
;
(* strips from word stack (revcode) homonym index if any *)
value strip w = match w with 
  [ [ last :: rest ] -> if last>50 then rest (* remove homonymy index *)
                        else w 
  | [] -> failwith "Empty stem to strip"
  ]
;
value rstem w = strip (Word.mirror w)
;
value rev_strip w = Word.mirror (rstem w) (* [compute_mw_links] *)
;
(* Builds revword normalised stem from entry string of root *)
(* Used by [Verbs.revstem], [Nouns.enter_iic], [Print_dict] *)
value rev_stem str = strip (rev_code_string str)
;
(* Takes a reversed word and returns its canonical name (homo,stem) *)
value decompose w = match w with
  [ [ last :: rest ] -> 
       if last>50 then (last-50,Word.mirror rest) 
                  else (0,Word.mirror w)
  | [] -> failwith "Empty stem to decompose"
  ]
;
(* Temporary - encoding of homo as last character of word *)
(* This induces ugly code. Should be replaced by pairs (int,skt/word) *)
value decompose_str str = 
  decompose (rev_code_string str) (* ugly multiple reversals *)
;
value normal_stem str = Word.mirror (rev_stem str)
;
value normal_stem_str str = Canon.decode (normal_stem str) (* horror *)
;
(* strips homonymy index of raw input - similar awful double reversal *)
(* Same function, with skt input, is [Subst.stripped_code_skt] *)
(* This is only called at compile lexicon time by [Dictionary/Print_html] 
   through below [skt_strip_to_deva] *)
value code_strip_raw s =  rev_strip (code_raw s)
;
(* A cleaner solution to name spaces would be to have type lexeme = (word * int) 
   and "x#5" represented as (x,5) (0 if no homophone) *)

value skt_to_deva     str = try Canon.unidevcode (code_string str) with
                                [ Failure _ -> failwith str ]
and skt_raw_to_deva   str = try Canon.unidevcode (code_raw str) with
                                [ Failure _ -> failwith ("raw " ^ str) ]
and skt_strip_to_deva str = try Canon.unidevcode (code_strip_raw str) with
                                [ Failure _ -> failwith ("raw stripped " ^ str) ]
;
value skt_to_roma str = Canon.uniromcode (code_string str) 
;
(* Following not needed since [Transduction.skt_to_html] is more direct 
[value skt_to_roma         str = Canon.uniromcode (code_string str) 
 and skt_raw_to_roma       str = Canon.uniromcode (code_raw str) 
 and skt_raw_strip_to_roma str = Canon.uniromcode (code_strip_raw str)] *) 

(* diff with string in Velthuis transliteration - caution: argument swap *)
value diff_str str w = Word.diff w (code_string str) 
;
(*i end; i*)

