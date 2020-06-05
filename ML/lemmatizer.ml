(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin lemmatizer for searching the inflected forms databases         *)
(* This CGI is triggered by page [index_page] in [dico_dir].              *)
(* Reads its input in shell variable [QUERY_STRING] URI-encoded.          *)
(* Prints an HTML document of lemma information on [stdout].              *)
(*i Test: (csh): setenv QUERY_STRING "q=eti&c=Verb"; lemmatizer          i*)
(*i Web: http://skt_server_url/cgi-bin/sktlemmatizer?q=eti&c=Verb        i*)

(*i module Lemmatizer = struct i*)

open Html;
open Web; (* ps pl etc. *)
open Cgi;

value ps = print_string
;
value pl s = do { ps s; print_newline () }
;
value display_rom_red s = html_red (Transduction.skt_to_html s)
;
value back_ground = background Chamois
; 
value prelude lang = do
  { pl http_header
  ; page_begin heritage_dictionary_title
  ; pl (body_begin back_ground)
  ; print_title_solid Mauve (Some lang) stem_title_en
  }
; 
value postlude lang = 
  page_end lang True 
;
value abor = abort default_language
;
value give_up phase = 
  let mess = "Missing " ^ phase ^ " morphology" in do
  { abor Control.sys_err_mess mess; exit 0 }
;
value load_inflected phase = 
  let file = match phase with 
      [ "Noun"    -> Data.public_nouns_file 
      | "Pron"    -> Data.public_pronouns_file
      | "Verb"    -> Data.public_roots_file
      | "Part"    -> Data.public_parts_file
      | "Inde"    -> Data.public_inde_file
      | "Absya"   -> Data.public_absya_file
      | "Abstvaa" -> Data.public_abstvaa_file
      | "Iic"     -> Data.public_iics_file
      | "Iiv"     -> Data.public_iivs_file
      | "Ifc"     -> Data.public_ifcs_file 
      | "Piic"    -> Data.public_piics_file
      | "Voca"    -> Data.public_vocas_file
      | _ -> raise (Control.Fatal "Unexpected phase") (* Pv Auxi Eort *)
      ] in 
  try (Gen.gobble file : Morphology.inflected_map)
  with [ _ -> give_up phase ]
;
value generative = fun
  [ "Part" | "Piic" -> True | _ -> False ]
;
value answer_begin () = do
  { pl center_begin 
  ; pl (table_begin_style (centered Yellow) [ noborder; ("cellspacing","20pt") ])
  ; ps tr_begin
  ; ps th_begin  
  }
;
value answer_end () = do
  { ps th_end
  ; ps tr_end
  ; pl table_end
  ; pl center_end 
  ; pl html_break 
  }
;
value unvisarg_rev = fun (* we revert a final visarga to s *)
  [ [ 16 :: w ] -> [ 48 :: w ] 
  | w -> w
  ]
;
value unvisarg word = Word.mirror (unvisarg_rev (Word.mirror word))
(* thus we may input raama.h and search for raamas in the morphological tables
   but we can't input puna.h or anta.h and search for punar or antar or also 
   verbal ninyu.h, stored as ninyur even though it is displayed as ninyu.h *)
;
(* Main *)
value lemmatizer_engine () = 
  let query = Sys.getenv "QUERY_STRING" in
  let env = create_env query in 
  let translit =  get "t" env Paths.default_transliteration 
  and lex = get "lex" env Paths.default_lexicon
  and url_encoded_entry = get "q" env ""
  and url_encoded_cat = get "c" env "Noun" in
  let str = decode_url url_encoded_entry (* in translit *)
  and cat = decode_url url_encoded_cat 
  and lang = language_of lex 
  and encode = Encode.switch_code translit (* normalized input *) in do
  { prelude lang
  ; try let word = unvisarg (encode str) in 
        let inflected_cat = load_inflected cat 
        and gen = generative cat in 
        let react inflected = do
          { ps (display_rom_red (Canon.decode word)) (* in romanized *)
          ; ps (span_begin Latin12)
          ; match Deco.assoc word inflected with
            [ [] -> do 
              { ps (" not found as a " ^ cat ^ " form")
              ; pl html_break
              }
            | le -> do 
              { ps " lemmatizes as:"
              ; pl html_break
              ; let pi =  Morpho_html.print_inflected gen word in 
                List.iter pi le
              }
            ]  
          ; ps span_end
          } in do
        { answer_begin () 
        ; react inflected_cat
        ; answer_end ()
        ; postlude lang
        }
    with [ Stream.Error _ -> abor "Illegal transliteration " str ]
  }
;
value safe_lemmatizer_engine () = 
  try lemmatizer_engine () 
  with (* sanitized service *)
  [ Encode.In_error s   -> abor "Wrong_input " s
  | Exit (* Sanskrit *) -> abor "Wrong character in input - " "use ASCII" 
  | Sys_error s         -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s      -> abor Control.stream_err_mess s (* file pb *)
  | Invalid_argument s  -> abor Control.fatal_err_mess s (* sub *)
  | Failure s           -> abor Control.fatal_err_mess s (* anomaly *)
  | Control.Fatal s     -> abor Control.fatal_err_mess s (* anomaly *)
  | Not_found           -> abor Control.fatal_err_mess "assoc" (* assoc *)
  | End_of_file         -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | _                   -> abor Control.fatal_err_mess "Unexpected anomaly" 
  ]
;
safe_lemmatizer_engine ()
;

(*i end; i*)
