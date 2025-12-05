(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* The Sanskrit lexical processor *)
(*i module Sanskrit = struct i*)
open Skt_lexer
  
type skt = string and encoding = string -> int list

(* Recognize a Sanskrit sentence as either a pada or a sloka *)
type pada = skt list and sloka = pada list

(* Dangerous - keeps the accent and chars + - dollar *)
let string_of_skt s = s
  
(* coercion [skt -> string] *)
(* Unsafe - debugging mostly, but also [Print_html.print_skt_px_ac] *)
let skt_of_string s = s
  
(* coercion [string -> skt] *)
let aa_preverb = "aa"
and privative p = List.mem p [ "a"; "an#1" ]
  
(* privative prefixes *)
(* Sanskrit word used in computations *)
(* Fragile: assumes fixed entry in lexicon *)
let i_root = "i"
and (* [Subst.record_ifc2] *) ita_part = "ita"
and (* id *) dagh_root = "dagh"
and (* id *) daghna_part = "daghna"
and (* id - accent needed *) arcya_absolutive = "arcya"
and (* [Subst.record_noun_gen] *) kaara = "kaara"
  
module Gramskt = Camlp4.PreCast.MakeGram(Skt_lexer)
  
open Skt_lexer.Token
  
(* Entry points *)
let skt = Gramskt.Entry.mk "skt"
and skt1 = Gramskt.Entry.mk "skt1"
and pada = Gramskt.Entry.mk "pada"
and sloka_line = Gramskt.Entry.mk "sloka_line"
and sloka = Gramskt.Entry.mk "sloka"
and sanscrit = Gramskt.Entry.mk "sanscrit"
and prefix = Gramskt.Entry.mk "prefix"
and skt_list = Gramskt.Entry.mk "skt_list"
and prefix_list = Gramskt.Entry.mk "prefix_list"
and sanscrit_corpus = Gramskt.Entry.mk "sanscrit_corpus"
  
(* hack *)
let _ =
  (Gramskt.extend (skt : 'skt Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* chunk of Sanskrit letters in Velthuis romanisation *)
                (* hiatus (underscore) *) (* homonym index *)
                (* possible avagraha is initial quote *)
                Gramskt.Stoken
                  (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gramskt.Action.mk
                   (fun (n : Gramskt.Token.t) (_loc : Gramskt.Loc.t) ->
                      (let n = Gramskt.Token.extract_string n in n : 'skt))));
               ([ Gramskt.Stoken
                    (((function | IDENT ((_)) -> true | _ -> false),
                      "IDENT _")) ],
                (Gramskt.Action.mk
                   (fun (id : Gramskt.Token.t) (_loc : Gramskt.Loc.t) ->
                      (let id = Gramskt.Token.extract_string id in id : 'skt))));
               ([ Gramskt.Stoken
                    (((function | IDENT ((_)) -> true | _ -> false),
                      "IDENT _"));
                  Gramskt.Skeyword "#";
                  Gramskt.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gramskt.Action.mk
                   (fun (n : Gramskt.Token.t) _ (id : Gramskt.Token.t)
                      (_loc : Gramskt.Loc.t) ->
                      (let n = Gramskt.Token.extract_string n in
                       let id = Gramskt.Token.extract_string id
                       in id ^ ("#" ^ n) : 'skt))));
               ([ Gramskt.Stoken
                    (((function | IDENT ((_)) -> true | _ -> false),
                      "IDENT _"));
                  Gramskt.Skeyword "_"; Gramskt.Sself ],
                (Gramskt.Action.mk
                   (fun (s : 'skt) _ (id : Gramskt.Token.t)
                      (_loc : Gramskt.Loc.t) ->
                      (let id = Gramskt.Token.extract_string id
                       in id ^ ("_" ^ s) : 'skt)))) ]) ]))
        ());
   Gramskt.extend (* numerals eg -tama *) (skt1 : 'skt1 Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (skt : 'skt Gramskt.Entry.t));
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (s : 'skt)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (s : 'skt1)
                      | _ -> assert false))) ]) ]))
        ());
   Gramskt.extend (pada : 'pada Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* non-empty list of pre-chunks separated by blanks *)
                Gramskt.Slist1
                  (Gramskt.Snterm
                     (Gramskt.Entry.obj (skt : 'skt Gramskt.Entry.t))) ],
                (Gramskt.Action.mk
                   (fun (el : 'skt list) (_loc : Gramskt.Loc.t) ->
                      (el : 'pada)))) ]) ]))
        ());
   Gramskt.extend (sanscrit : 'sanscrit Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* pada lines with "|" or "!" forcing terminal sandhi *)
                (* voc, interj *)
                Gramskt.Stoken
                  (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (_loc : Gramskt.Loc.t)
                      ->
                      match __camlp4_0 with
                      | EOI -> (failwith "Empty sanskrit input" : 'sanscrit)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> ([ p ] : 'sanscrit)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "!"; Gramskt.Sself ],
                (Gramskt.Action.mk
                   (fun (sl : 'sanscrit) _ (p : 'pada) (_loc : Gramskt.Loc.t)
                      -> (p :: sl : 'sanscrit))));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|"; Gramskt.Sself ],
                (Gramskt.Action.mk
                   (fun (sl : 'sanscrit) _ (p : 'pada) (_loc : Gramskt.Loc.t)
                      -> (p :: sl : 'sanscrit))));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|";
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) _ (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> ([ p ] : 'sanscrit)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|"; Gramskt.Skeyword "|" ],
                (Gramskt.Action.mk
                   (fun _ _ (p : 'pada) (_loc : Gramskt.Loc.t) ->
                      ([ p ] : 'sanscrit)))) ]) ]))
        ());
   Gramskt.extend (sanscrit_corpus : 'sanscrit_corpus Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* pada lines with "|" or "!" encoded with "" *)
                (* horrible encoding for corpus mode [restore_danda] *)
                (* voc, interj *)
                Gramskt.Stoken
                  (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (_loc : Gramskt.Loc.t)
                      ->
                      match __camlp4_0 with
                      | EOI ->
                          (failwith "Empty sanskrit input" :
                            'sanscrit_corpus)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> ([ p ] : 'sanscrit_corpus)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "!"; Gramskt.Sself ],
                (Gramskt.Action.mk
                   (fun (sl : 'sanscrit_corpus) _ (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      (p :: [ "" ] :: sl : 'sanscrit_corpus))));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|"; Gramskt.Sself ],
                (Gramskt.Action.mk
                   (fun (sl : 'sanscrit_corpus) _ (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      (p :: [ "" ] :: sl : 'sanscrit_corpus))));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|";
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) _ (p : 'pada)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> ([ p ] : 'sanscrit_corpus)
                      | _ -> assert false)));
               ([ Gramskt.Snterm
                    (Gramskt.Entry.obj (pada : 'pada Gramskt.Entry.t));
                  Gramskt.Skeyword "|"; Gramskt.Skeyword "|" ],
                (Gramskt.Action.mk
                   (fun _ _ (p : 'pada) (_loc : Gramskt.Loc.t) ->
                      ([ p ] : 'sanscrit_corpus)))) ]) ]))
        ());
   Gramskt.extend (skt_list : 'skt_list Gramskt.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gramskt.Slist1sep
                    ((Gramskt.Snterm
                        (Gramskt.Entry.obj (skt : 'skt Gramskt.Entry.t))),
                    (Gramskt.Skeyword ","));
                  Gramskt.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gramskt.Action.mk
                   (fun (__camlp4_0 : Gramskt.Token.t) (el : 'skt list)
                      (_loc : Gramskt.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (el : 'skt_list)
                      | _ -> assert false))) ]) ]))
        ()))
  
let trad_string entry t =
  try Gramskt.parse_string entry Loc.ghost t
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" t
         (Loc.to_string loc);
       raise e)
  
let trad_skt = trad_string skt1
and trad_sanscrit = trad_string sanscrit
and trad_skt_list = trad_string skt_list
  
let maha_epic = "Mahaabhaarata"
and (* for [Print_html] *) rama_epic = "Raamaaya.na"
  
let skt_to_tex = Transduction.skt_to_tex
  
(* romanisation Tex diacritics *)
let skt_to_devnag = Transduction.skt_to_devnag
  
(* for Tex with devnag *)
let skt_to_html = Transduction.skt_to_html
  
(* romanisation *)
(* Encoding functions skt -> word *)
let raw_sanskrit_word = Transduction.code_raw
  
(* no normalisation no accent*)
let sanskrit_word = Encode.code_string
  
(* normalisation *)
let skt_raw_to_deva = Encode.skt_raw_to_deva
  
(* devanagari unicode *)
let skt_strip_to_deva = Encode.skt_strip_to_deva
  
(* id *)
let skt_to_anchor = Encode.anchor
  
(* hypertext anchor encoding *)
let rev_stem_skt = Encode.rev_stem
  
(* normalised revword *)
let normal_stem = Encode.normal_stem
  
(* normalised stem as word *)
(* Cleaning up by removing accents - used in [Print_dict] *)
let clean_up s = Canon.decode (Transduction.code_raw s)
  
(* Following used in [Print_dict] and [Subst] -- ought to disappear *)
let normal_stem_skt = Encode.normal_stem_str
  
(* normalised stem as string *)
let code_skt_ref = Encode.code_skt_ref
  
let code_skt_ref_d = Encode.code_skt_ref_d
  
let decode_skt = Canon.decode
  
open Chunker
  
(* [chunker avagraha_expand] *)
let sanskrit_sentence strm =
  try Gramskt.parse sanscrit Loc.ghost strm
  with | Loc.Exc_located (loc, Exit) -> raise (Encode.In_error "Exit")
  | Loc.Exc_located (loc, (Error.E msg)) ->
      raise (Encode.In_error ("(Lexical) " ^ msg))
  | Loc.Exc_located (loc, (Stream.Error msg)) ->
      raise (Encode.In_error ("(Stream) " ^ msg))
  | Loc.Exc_located (loc, (Failure s)) -> raise (Encode.In_error s)
  | Loc.Exc_located (loc, ex) -> raise ex
and sanskrit_sentence_corpus strm =
  try Gramskt.parse sanscrit_corpus Loc.ghost strm
  with | Loc.Exc_located (loc, Exit) -> raise (Encode.In_error "Exit")
  | Loc.Exc_located (loc, (Error.E msg)) ->
      raise (Encode.In_error ("(Lexical) " ^ msg))
  | Loc.Exc_located (loc, (Stream.Error msg)) ->
      raise (Encode.In_error ("(Stream) " ^ msg))
  | Loc.Exc_located (loc, (Failure s)) -> raise (Encode.In_error s)
  | Loc.Exc_located (loc, ex) -> raise ex
  
(* No chunk processing, each chunk is assumed to be in terminal sandhi 
   already. But normalizes away anusvara, contrarily to its name *)
(* encode is [raw_sanskrit_word], [raw_sanskrit_word_KH], etc. *)
let read_raw_skt_stream encode strm =
  let process = List.map encode
  in
    match sanskrit_sentence strm with
    | [ l ] -> process l
    | lines ->
        let concat line lines = (process line) @ lines
        in List.fold_right concat lines []
  
let read_raw_skt_stream_corpus encode strm =
  let process = List.map encode
  in
    match sanskrit_sentence_corpus strm with
    | [ l ] -> process l
    | lines ->
        let concat line lines = (process line) @ lines
        in List.fold_right concat lines []
  
(* Chunk processing, recognizing mandatory hyatus but also allowing blank
   spacing when the sandhi is identity *)
let read_processed_skt_stream encode strm =
  let process = chunker (avagraha_expand encode)
  in
    match sanskrit_sentence strm with
    | [ l ] -> process l
    | lines ->
        let concat line lines = (process line) @ lines
        in List.fold_right concat lines []
  
(* Now general readers with encoding parameter of type [string -> word] *)
(* [read_sanskrit : encoding -> string -> list word] *)
(* Assumes sandhi is not undone between chunks - spaces are not significant *)
(* Generalizes [read_VH False] to all transliterations *)
let read_sanskrit encode str = (* [encode : string -> word] *)
  read_processed_skt_stream encode (Stream.of_string str)
  
(* Assumes sandhi is undone between chunks (partial padapatha) *)
(* Generalizes [read_VH True] to all transliterations *)
let read_raw_sanskrit encode str = (* [encode : string -> word] *)
  read_raw_skt_stream encode (Stream.of_string str)
and read_raw_sanskrit_corpus encode str = (* [encode : string -> word] *)
  read_raw_skt_stream_corpus encode (Stream.of_string str)
  

