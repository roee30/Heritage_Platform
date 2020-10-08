(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* The Sanskrit lexical processor *)

(*i module Sanskrit = struct i*)

open Skt_lexer; 

type skt = string
and encoding = string -> list int
;

(* Recognize a Sanskrit sentence as either a pada or a sloka *)
type pada = list skt
and sloka = list pada
;
(* Dangerous - keeps the accent and chars + - dollar *)
value string_of_skt s = s (* coercion [skt -> string] *)
;
(* Unsafe - debugging mostly, but also [Print_html.print_skt_px_ac] *)
value skt_of_string s = s (* coercion [string -> skt] *)
;
value aa_preverb = "aa"
and privative p = List.mem p [ "a"; "an#1" ] (* privative prefixes *)
;
(* Sanskrit word used in computations *)
(* Fragile: assumes fixed entry in lexicon *)
value i_root = "i"  (* [Subst.record_ifc2] *)
and ita_part = "ita" (* id *)
and dagh_root = "dagh" (* id *) 
and daghna_part = "daghna" (* id - accent needed *)
and arcya_absolutive = "arcya" (* [Subst.record_noun_gen] *)
and kaara = "kaara"
;
module Gramskt = Camlp4.PreCast.MakeGram Skt_lexer 
;
open Skt_lexer.Token
;
(* Entry points *)
value skt       = Gramskt.Entry.mk "skt"
and skt1        = Gramskt.Entry.mk "skt1"
and pada        = Gramskt.Entry.mk "pada"
and sloka_line  = Gramskt.Entry.mk "sloka_line"
and sloka       = Gramskt.Entry.mk "sloka"
and sanscrit    = Gramskt.Entry.mk "sanscrit"
and prefix      = Gramskt.Entry.mk "prefix"
and skt_list    = Gramskt.Entry.mk "skt_list"
and prefix_list = Gramskt.Entry.mk "prefix_list"
and sanscrit_corpus = Gramskt.Entry.mk "sanscrit_corpus" (* hack *)
;

EXTEND Gramskt
  skt: (* chunk of Sanskrit letters in Velthuis romanisation *)
    [ [ id = IDENT; "_"; s = skt -> id ^ "_" ^ s (* hiatus (underscore) *)
      | id = IDENT; "#"; n = INT -> id ^ "#" ^ n (* homonym index *)
      | id = IDENT -> id (* possible avagraha is initial quote *)
      | n = INT -> n (* numerals eg -tama *)
    ] ] ; 
  skt1:
    [ [ s = skt; `EOI -> s ] ] ;
  pada: (* non-empty list of pre-chunks separated by blanks *)
    [ [ el = LIST1 skt -> el ] ] ; 
  sanscrit: (* pada lines with "|" or "!" forcing terminal sandhi *)
    [ [ p = pada; "|"; "|"  -> [ p ]
      | p = pada; "|"; `EOI -> [ p ] 
      | p = pada; "|"; sl = sanscrit -> [ p :: sl ] 
      | p = pada; "!"; sl = sanscrit -> [ p :: sl ] (* voc, interj *)
      | p = pada; `EOI -> [ p ] 
      | `EOI -> failwith "Empty sanskrit input"
    ] ] ;
  sanscrit_corpus: (* pada lines with "|" or "!" encoded with "" *)
    [ [ p = pada; "|"; "|"  -> [ p ]
      | p = pada; "|"; `EOI -> [ p ] 
      | p = pada; "|"; sl = sanscrit_corpus -> [ p :: [ [""] :: sl ] ]
        (* horrible encoding for corpus mode [restore_danda] *)
      | p = pada; "!"; sl = sanscrit_corpus -> [ p :: [ [""] :: sl ] ] (* voc, interj *)
      | p = pada; `EOI -> [ p ] 
      | `EOI -> failwith "Empty sanskrit input"
    ] ] ;
  skt_list :
    [ [ el = LIST1 skt SEP ","; `EOI -> el ] ] ;
END
;
value trad_string entry t = 
  try Gramskt.parse_string entry Loc.ghost t with
  [ Loc.Exc_located loc e -> do
     { Format.eprintf "\nIn string \"%s\", at location %s :\n%!"
                      t (Loc.to_string loc)
     ; raise e
     } 
  ] 
;
value trad_skt = trad_string skt1
  and trad_sanscrit = trad_string sanscrit
  and trad_skt_list = trad_string skt_list
;

value maha_epic = "Mahaabhaarata" (* for [Print_html] *)
  and rama_epic = "Raamaaya.na"
;
value skt_to_tex = Transduction.skt_to_tex;    (* romanisation Tex diacritics *)
value skt_to_devnag = Transduction.skt_to_devnag; (* for Tex with devnag *)
value skt_to_html = Transduction.skt_to_html;  (* romanisation *)

(* Encoding functions skt -> word *)
value raw_sanskrit_word = Transduction.code_raw; (* no normalisation no accent*) 
value sanskrit_word = Encode.code_string; (* normalisation *)
value skt_raw_to_deva = Encode.skt_raw_to_deva; (* devanagari unicode *)
value skt_strip_to_deva = Encode.skt_strip_to_deva; (* id *)
value skt_to_anchor = Encode.anchor; (* hypertext anchor encoding *)
value rev_stem_skt = Encode.rev_stem; (* normalised revword *)
value normal_stem = Encode.normal_stem; (* normalised stem as word *)

(* Cleaning up by removing accents - used in [Print_dict] *)
value clean_up s = Canon.decode (Transduction.code_raw s)
;
(* Following used in [Print_dict] and [Subst] -- ought to disappear *)
value normal_stem_skt = Encode.normal_stem_str; (* normalised stem as string *)

value code_skt_ref = Encode.code_skt_ref;
value code_skt_ref_d = Encode.code_skt_ref_d;
value decode_skt = Canon.decode
;
open Chunker (* [chunker avagraha_expand] *)
;
value sanskrit_sentence strm = 
  try Gramskt.parse sanscrit Loc.ghost strm with
  [ Loc.Exc_located loc Exit -> raise (Encode.In_error "Exit")
  | Loc.Exc_located loc (Error.E msg) 
    -> raise (Encode.In_error ("(Lexical) " ^ msg))
  | Loc.Exc_located loc (Stream.Error msg) 
    -> raise (Encode.In_error ("(Stream) " ^ msg))
  | Loc.Exc_located loc (Failure s) -> raise (Encode.In_error s)
  | Loc.Exc_located loc ex -> raise ex
  ]
and sanskrit_sentence_corpus strm = 
  try Gramskt.parse sanscrit_corpus Loc.ghost strm with
  [ Loc.Exc_located loc Exit -> raise (Encode.In_error "Exit")
  | Loc.Exc_located loc (Error.E msg) 
    -> raise (Encode.In_error ("(Lexical) " ^ msg))
  | Loc.Exc_located loc (Stream.Error msg) 
    -> raise (Encode.In_error ("(Stream) " ^ msg))
  | Loc.Exc_located loc (Failure s) -> raise (Encode.In_error s)
  | Loc.Exc_located loc ex -> raise ex
  ]
;
(* No chunk processing, each chunk is assumed to be in terminal sandhi 
   already. But normalizes away anusvara, contrarily to its name *)
(* encode is [raw_sanskrit_word], [raw_sanskrit_word_KH], etc. *)
value read_raw_skt_stream encode strm = 
  let process = List.map encode in
  match sanskrit_sentence strm with
  [ [ l ] -> process l 
  | lines -> List.fold_right concat lines []
             where concat line lines = process line @ lines 
  ]
;
value read_raw_skt_stream_corpus encode strm = 
  let process = List.map encode in
  match sanskrit_sentence_corpus strm with
  [ [ l ] -> process l 
  | lines -> List.fold_right concat lines []
             where concat line lines = process line @ lines 
  ]
;
(* Chunk processing, recognizing mandatory hyatus but also allowing blank
   spacing when the sandhi is identity *) 
value read_processed_skt_stream encode strm = 
  let process = chunker (avagraha_expand encode) in
  match sanskrit_sentence strm with 
  [ [ l ] -> process l
  | lines -> List.fold_right concat lines []
             where concat line lines = process line @ lines 
  ]
;

(* Now general readers with encoding parameter of type [string -> word] *)

(* [read_sanskrit : encoding -> string -> list word] *)
(* Assumes sandhi is not undone between chunks - spaces are not significant *)
(* Generalizes [read_VH False] to all transliterations *)
value read_sanskrit encode str = (* [encode : string -> word] *)
  read_processed_skt_stream encode (Stream.of_string str)
;
(* Assumes sandhi is undone between chunks (partial padapatha) *)
(* Generalizes [read_VH True] to all transliterations *)
value read_raw_sanskrit encode str = (* [encode : string -> word] *)
  read_raw_skt_stream encode (Stream.of_string str)
and read_raw_sanskrit_corpus encode str = (* [encode : string -> word] *)
  read_raw_skt_stream_corpus encode (Stream.of_string str)
;
(*i end; i*)

