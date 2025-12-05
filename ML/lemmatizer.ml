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
open Html
  
open Web
  
(* ps pl etc. *)
open Cgi
  
let ps = print_string
  
let pl s = (ps s; print_newline ())
  
let display_rom_red s = html_red (Transduction.skt_to_html s)
  
let back_ground = background Chamois
  
let prelude lang =
  (pl http_header;
   page_begin heritage_dictionary_title;
   pl (body_begin back_ground);
   print_title_solid Mauve (Some lang) stem_title_en)
  
let postlude lang = page_end lang true
  
let abor = abort default_language
  
let give_up phase =
  let mess = "Missing " ^ (phase ^ " morphology")
  in (abor Control.sys_err_mess mess; exit 0)
  
let load_inflected phase =
  let file =
    match phase with
    | "Noun" -> Data.public_nouns_file
    | "Pron" -> Data.public_pronouns_file
    | "Verb" -> Data.public_roots_file
    | "Part" -> Data.public_parts_file
    | "Inde" -> Data.public_inde_file
    | "Absya" -> Data.public_absya_file
    | (* absolutives in -ya, infinitives *) "Abstvaa" -> Data.
        public_abstvaa_file
    | (* absolutives in -tvaa, infini. *) "Iic" -> Data.public_iics_file
    | "Iiv" -> Data.public_iivs_file
    | "Ifc" -> Data.public_ifcs_file
    | "Piic" -> Data.public_piics_file
    | "Voca" -> Data.public_vocas_file
    | _ -> raise (Control.Fatal "Unexpected phase")
  in
    (* Pv Auxi Eort *)
    try (Gen.gobble file : Morphology.inflected_map)
    with | _ -> give_up phase
  
let generative = function | "Part" | "Piic" -> true | _ -> false
  
let answer_begin () =
  (center_begin |> pl;
   (table_begin_style (centered Yellow) [ noborder; ("cellspacing", "20pt") ])
     |> pl;
   tr_begin |> ps;
   th_begin |> ps)
  
let answer_end () =
  (th_end |> ps;
   tr_end |> ps;
   table_end |> pl;
   center_end |> pl;
   html_break |> pl)
  
let unvisarg_mirror =
  function | (* we revert a final visarga to s *) 16 :: w -> 48 :: w | w -> w
  
let unvisarg word = Word.mirror (unvisarg_mirror (Word.mirror word))
  
(* thus we may input raama.h and search for raamas in the morphological tables
   but we can't input puna.h or anta.h and search for punar or antar or also 
   verbal ninyu.h, stored as ninyur even though it is displayed as ninyu.h *)
(* Main *)
let lemmatizer_engine () =
  let query = Sys.getenv "QUERY_STRING" in
  let env = create_env query in
  let translit = get "t" env Paths.default_transliteration
  and lex = get "lex" env Paths.default_lexicon
  and font = get "font" env Paths.default_display_font in
  let ft = font_of_string font in (* Deva vs Roma print *)
  let () = toggle_sanskrit_font ft and url_encoded_entry = get "q" env ""
  and url_encoded_cat = get "c" env "Noun" in
  let str = decode_url url_encoded_entry
  and (* in translit *) cat = decode_url url_encoded_cat
  and lang = language_of_string lex
  and encode = Encode.switch_code translit
  in
    (* normalized input *)
    (prelude lang;
     try
       let word = unvisarg (encode str) in
       let inflected_cat = load_inflected cat and gen = generative cat in
       let react inflected =
         (ps (display_rom_red (Canon.decode word));
          (* in romanized *)
          ps (span_begin Latin12);
          (match Deco.assoc word inflected with
           | [] -> (ps (" not found as a " ^ (cat ^ " form")); pl html_break)
           | le ->
               (ps " lemmatizes as:";
                pl html_break;
                let pi = Morpho_html.print_inflected gen word
                in List.iter pi le));
          ps span_end)
       in
         (answer_begin (); react inflected_cat; answer_end (); postlude lang)
     with | Stream.Error _ -> abor "Illegal transliteration " str)
  
let safe_lemmatizer_engine () =
  try lemmatizer_engine ()
  with | (* sanitized service *) Encode.In_error s -> abor "Wrong_input " s
  | Exit -> (* Sanskrit *) abor "Wrong character in input - " "use ASCII"
  | Sys_error s -> abor Control.sys_err_mess s
  | (* file pb *) Stream.Error s -> abor Control.stream_err_mess s
  | (* file pb *) Invalid_argument s -> abor Control.fatal_err_mess s
  | (* sub *) Failure s | Control.Fatal s -> abor Control.fatal_err_mess s
  | (* anomaly *) Not_found -> abor Control.fatal_err_mess "assoc"
  | (* assoc *) End_of_file -> abor Control.fatal_err_mess "EOF"
  | (* EOF *) _ -> abor Control.fatal_err_mess "Unexpected anomaly"
  
let _ = safe_lemmatizer_engine ()
  

