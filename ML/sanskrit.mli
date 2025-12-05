(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Sanskrit : sig i*)
type skt

(* abstract *)
type pada = skt list and sloka = pada list

val string_of_skt : skt -> string
  
(* input *)
val skt_of_string : string -> skt
  
(* faking - debug and [Subst.record_tad] *)
val aa_preverb : skt
  
val privative : skt -> bool
  
val i_root : skt
  
val ita_part : skt
  
val dagh_root : skt
  
val daghna_part : skt
  
val arcya_absolutive : skt
  
val kaara : skt
  
val trad_skt : string -> skt
  
val trad_sanscrit : string -> sloka
  
val trad_skt_list : string -> skt list
  
val maha_epic : skt
  
val rama_epic : skt
  
val skt_to_tex : skt -> string
  
val skt_to_devnag : skt -> string
  
val skt_to_html : skt -> string
  
val skt_raw_to_deva : skt -> string
  
val skt_strip_to_deva : skt -> string
  
val skt_to_anchor : skt -> string
  
val raw_sanskrit_word : skt -> Word.word
  
val sanskrit_word : skt -> Word.word
  
val rev_stem_skt : skt -> Word.word
  
val normal_stem : skt -> Word.word
  
val clean_up : skt -> skt
  
val normal_stem_skt : skt -> string
  
val code_skt_ref : skt -> Word.word
  
val code_skt_ref_d : skt -> Word.word
  
val decode_skt : Word.word -> skt
  
val read_sanskrit : (string -> Word.word) -> string -> Word.word list
  
val read_raw_sanskrit : (string -> Word.word) -> string -> Word.word list
  
val read_raw_sanskrit_corpus :
  (string -> Word.word) -> string -> Word.word list
  

