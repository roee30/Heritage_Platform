(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Sanskrit : sig i*)

type skt  (* abstract *)
;
type pada = list skt
and sloka = list pada
;
value string_of_skt : skt -> string; (* input *)
value skt_of_string : string -> skt; (* faking - debug and [Subst.record_tad] *)
value aa_preverb : skt;
value privative : skt -> bool;
value i_root : skt;
value ita_part : skt;
value dagh_root : skt;
value daghna_part : skt;
value arcya_absolutive : skt;
value trad_skt : string -> skt;
value trad_sanscrit : string -> sloka; 
value trad_skt_list : string -> list skt; 
value maha_epic : skt;
value rama_epic : skt;
value skt_to_tex : skt -> string;
value skt_to_dev : skt -> string;
value skt_to_html : skt -> string;
value skt_raw_to_deva : skt -> string;
value skt_raw_strip_to_deva : skt -> string;
value skt_to_anchor : skt -> string;
value raw_sanskrit_word : skt -> Word.word;
value sanskrit_word : skt -> Word.word;
value rev_stem_skt : skt -> Word.word;
value normal_stem : skt -> Word.word;
value clean_up : skt -> skt;
value normal_stem_skt : skt -> string;
value code_skt_ref : skt -> Word.word;
value code_skt_ref_d : skt -> Word.word;
value decode_skt : Word.word -> skt;
value read_sanskrit : (string -> Word.word) -> string -> list Word.word;
value read_raw_sanskrit : (string -> Word.word) -> string -> list Word.word;

(*i end; i*)
