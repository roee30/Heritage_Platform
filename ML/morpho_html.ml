(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This module contains various service utilities for CGI programs *)

(*i module Morpho_html = struct i*)

open Html;
open Web; (* ps etc. *)
open Multilingual; (* Roma Deva *)

module Out_chan = struct value chan = Web.output_channel; end;
module Morpho = Morpho.Morpho_out Out_chan;

(* This loads dynamically the MW exceptions database *)
value mw_defining_page s =
  let mw_exceptions = 
    try (Gen.gobble public_mw_exc_file : Deco.deco int)  
    with [ _ -> failwith "mw_exceptions" ] in
    Chapters.mw_defining_page_exc s mw_exceptions
;
(* Absolute url on local site *)
value url s = 
  let (page,pref) = match lexicon_toggle.val with
    [ "SH" -> (web_dico_url ^ Chapters.sh_defining_page s,"") 
    | "MW" -> (mw_dico_url ^ mw_defining_page s,"H_") 
    | _ -> failwith "Unknown lexicon"
    ] in 
  page ^ "#" ^ pref ^ Encode.anchor s
;
value url_cache s = 
  mw_dico_url ^ mw_defining_page s ^ "#" ^ Encode.anchor s
;
(* Romanisation of Sanskrit *)
value skt_roma s = italics (Transduction.skt_to_html s)
(* Function [skt_roma] differs from [Encode.skt_to_roma] 
   because it does not go through encoding [s] as a word,
   and the complications of dealing with possible hiatus. *)
;
value skt_red s = html_red (skt_roma s)
;
value skt_anchor cached font form = (* for Declension Conjugation *)
  let s = match font with
          [ Deva -> deva20_blue_center (Encode.skt_raw_strip_to_deva form)
          | Roma -> skt_roma form (* no stripping in Roma *)
          ]
  and url_function = if cached then url_cache else url in
  anchor Navy_ (url_function form) s 
;
value skt_anchor_R cached = skt_anchor cached Roma (* for Declension, Indexer *)
(*i [and skt_anchor_D = skt_anchor Deva] unused i*) 
and skt_anchor_R2 s s' = anchor Navy_ (url s) (skt_roma s') (* for Indexer *)
;
value no_hom entry = (* low-level string hacking *)
  match (String.sub entry ((String.length entry)-1) 1) with
  [ "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" -> False
  | _ -> True
  ]
;
(* Used for printing MW in indexing mode *)
(* Note the difference between word and entry, word is the normalized form 
   of entry. We need entry to link to the MW page, where it is unnormalized *)
value skt_anchor_M word entry page cache = 
  let anchor_used = if cache then anchor_graph else anchor in
  let anc = mw_dico_url ^ page ^ "#" ^ entry in
  let anchor_mw = anchor_used Navy_ anc in
  let vocable = if no_hom entry then word 
                else let pos = (String.length entry)-1 in 
                     word ^ "#" ^ (String.sub entry pos 1) in
  anchor_mw (skt_roma vocable)
;
value skt_graph_anchor_R cache form =
  let s = skt_roma form in
  let url_function = if cache then url_cache else url in
  anchor_graph Navy_ (url_function form) s 
;

value print_stem w = ps (Canon.uniromcode w) (* w in lexicon or not *)
and print_chunk w = ps (Canon.uniromcode w)
and print_entry w = ps (skt_anchor_R False (Canon.decode w)) (* w in lexicon *)
and print_ext_entry ps w = ps (skt_anchor_R False (Canon.decode w)) (* idem *)
and print_cache w = ps (skt_anchor_R True (Canon.decode w))
and print_graph_entry w = ps (skt_graph_anchor_R False (Canon.decode w))
and print_graph_cache w = ps (skt_graph_anchor_R True (Canon.decode w))
;

(* Used in [Indexer] and [Lemmatizer] *)
value print_inflected gen word inverse = do
  { Morpho.print_inv_morpho print_entry print_stem print_chunk word (0,0) 
                            gen inverse 
  ; pl html_break 
  }
;
(* Used in [Lexer.print_morph] *)
value print_inflected_link pvs cached = 
  let print_fun = if cached then print_cache else print_entry in 
  Morpho.print_inv_morpho_link pvs print_fun print_stem print_chunk 
;
(* Variant for compound tags used in [Lexer.print_morph_tad] *)
value print_inflected_link_tad pvs cached = 
  let print_fun = if cached then print_cache else print_entry in 
  Morpho.print_inv_morpho_link_tad pvs print_fun print_stem print_chunk 
;
(* Used in [Interface] to print the lemmas *)
value print_graph_link pvs cached = 
  let print_fun = if cached then print_graph_cache else print_graph_entry in
  Morpho.print_inv_morpho_link pvs print_fun print_stem print_chunk
;
(* Used in [Interface] to print the lemmas for taddhitaantas *)
value print_graph_link_tad pvs cached = 
  let print_fun = if cached then print_graph_cache else print_graph_entry in
  Morpho.print_inv_morpho_link_tad pvs print_fun print_stem print_chunk
;
(* Final visarga form for display: final s and r are replaced by visarga.
   There is some information loss here, since -ar and -a.h do not have the 
   same behaviour with external sandhi, eg punar-api, antar-a'nga, antar-gata, 
   etc. For this reason the morphological tables do not keep forms in terminal 
   sandhi, and distinguish forms ended in -as and -ar.
   It should not be applied to stems, only to padas *)
value visargify rw = Word.mirror
  (match rw with
      [ [ 48 (* s *) :: r ] | [ 43 (* r *) :: r ] -> [ 16 (* .h *) :: r ] 
      | _ -> rw
      ])
;
value final w = visargify (Word.mirror w) (* Declension, Conjugation *)
; 
value print_final rw = print_chunk (visargify rw) (* Interface *)
;
value hdecode word = Transduction.skt_to_html (Canon.decode word)
;
value html_blue_off offset text = 
  (* Temporary use of title attribute for XHTML 1.0 Strict offset recording, *)
  (* should be replaced by data-offset in future HTML 5 compliance. *)
  (* This is only needed for the SL annotator interface. *)
  (* It has the unpleasant side effect of showing offsets on mouse over. *)
  let offset_attr offset = ("title",string_of_int offset) in
  (elt_begin_attrs [ offset_attr offset ] "span" Blue_)  ^ text ^ span_end 
;
(* indicates offset of segment in attribute "title" of [Blue_] span *)
value blue_word_off word offset = (* deprecated *)
  html_blue_off offset (emph (hdecode word))
;
value print_sandhi u v w = do 
  { ps (html_magenta (hdecode (visargify u))) (* visarga form *)
  ; ps (html_green "|")
  ; ps (html_magenta (hdecode v))
  ; ps (html_blue " &rarr; ") (* -> *)
  ; ps (html_red (hdecode w))
  }
;
value print_signifiant rword =
  let word = visargify rword in (* visarga form : final s and r visarged *) 
  ps (html_blue (hdecode word)) 
;
(* used in [Lexer.print_segment] with offset indication *)
value print_signifiant_off rword offset = 
  let word = visargify rword in (* visarga form : final s and r visarged *) 
  ps (blue_word_off word offset)
;
(* used in [Lexer.print_proj] *)
value print_signifiant_yellow rword = do
  { ps th_begin 
  ; pl (table_begin_style (background Yellow) [ padding5 ])
  ; ps td_begin
  ; print_signifiant rword 
  ; ps td_end
  ; ps table_end
  ; ps th_end
  }
;

(*i end; i*)

