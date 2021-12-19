(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This program creates the page [reader_page] (Sanskrit Reader Interface) 
   invoking the CGI sktreader alias reader. Invoked without language argument,
   it is itself the CGI [skt_heritage] invokable separately. *)

(*i module Mk_reader_page = struct i*)

open Html;
open Web; (* ps pl abort etc. *)
open Cgi; (* [create_env get] *)
 
value back_ground = background Chamois
;
value out_mode = ref None
;
value set_cho () = Arg.parse 
  [ ("-fr", Arg.Unit (fun () -> out_mode.val:=Some French), "French")
  ; ("-en", Arg.Unit (fun () -> out_mode.val:=Some English), "English")
  ; ("",    Arg.Unit (fun () -> out_mode.val:=None), "default language for cgi")
  ]
  (fun s -> raise (Arg.Bad s)) 
  "Usage: mk_reader_page -en or mk_reader_page -fr or mk_reader_page"
;
value print_cache_policy cache_active = do
  { " Cache " |> ps 
  ; let options = 
      [ (" On ","t",cache_active="t")  (* Cache active *)
      ; (" Off ","f",cache_active="f") (* Ignore cache *)
      ] in
    option_select_default "cache" options |> pl
  }
;
value reader_input_area_default =
  text_area "text" 1 screen_char_length 
;
value reader_input_area = reader_input_area_default ""
;
value reader_page () = do
  { set_cho ()
  ; let (lang,query) = match out_mode.val with
      [ Some lang -> do 
        { open_html_file (reader_page lang) reader_meta_title; (lang,"") }
      | None -> do
        { reader_prelude ""; (default_language, Sys.getenv "QUERY_STRING") }
      ] in try 
    let env = create_env query in
    let url_encoded_input = get "text" env "" 
    and url_encoded_mode  = get "mode" env "g"
    and url_encoded_topic = get "topic" env ""
    and st = get "st" env "t" (* default vaakya rather than isolated pada *)
    and us = get "us" env "f" (* default input sandhied *)
    and cache_active = get "cache" env cache_active.val
    and translit = get "t" env Paths.default_transliteration 
    and font = get "font" env Paths.default_display_font in
    (* Contextual information from past discourse *)
    let topic_mark = decode_url url_encoded_topic 
    and text = decode_url url_encoded_input in

    (* Corpus parameters *)
    let corpus_permission = Cgi.decoded_get Params.corpus_permission "" env in
    let corpus_dir = Cgi.decoded_get Params.corpus_dir "" env in
    let sentence_no = Cgi.decoded_get Params.sentence_no "" env in do

  { body_begin back_ground |> pl 
  ; print_title (Some lang) reader_title
  ; h3_begin C3 |> pl
  ; if Web_corpus.(permission_of_string corpus_permission = Annotator) then
      "Corpus annotator permission - " ^ corpus_dir |> pl
    else ()
  ; h3_end |> pl
  ; center_begin |> pl
  ; cgi_reader_begin reader_cgi "convert" |> pl
  ; print_lexicon_select (lexicon_of lang)
  ; if cache_allowed then print_cache_policy cache_active else ()
  ; html_break |> pl
  ; "Text " |> pl 
  ; option_select_default "st" 
        [ (" Sentence ","t",st="t") 
        ; ("   Word   ","f",st="f")  
        ] |> pl 
  ; " Format " |> pl
  ; option_select_default "us" 
        [ (" Unsandhied ","t",us="t") 
        ; ("  Sandhied  ","f",us="f") 
        ] |> pl
  (* Sanskrit printer Deva/Roma *)
  ; print_sanskrit_font_select font
  ; html_break |> pl  
  ; reader_input_area_default text |> ps
  ; html_break |> pl 
  ; "Input convention " |> ps
  ; transliteration_switch_default translit "trans" |> ps
  ; " Optional topic " |> pl (* For the moment assumed singular *)
  ; option_select_default "topic"
        [ (" Masculine ","m",topic_mark="m")  
        ; (" Feminine  ","f",topic_mark="f")  
        ; ("  Neuter   ","n",topic_mark="n")
        ; ("   Void    ","" ,topic_mark="") 
        ] |> pl
  ; " Mode " |> pl
  ; option_select_default_id "mode_id" "mode"
      (interaction_modes_default url_encoded_mode) |> pl

  (* Corpus parameters *)
  ; hidden_input Params.corpus_permission corpus_permission |> pl
  ; hidden_input Params.corpus_dir corpus_dir |> pl
  ; hidden_input Params.sentence_no sentence_no |> pl

  ; html_break |> pl 
  ; submit_input "Read" |> pl
  ; reset_input "Reset" |> pl
  ; cgi_end |> pl
  ; center_end |> pl 
  ; match out_mode.val with
    [ Some lang -> close_html_file lang True 
    | None ->
      do { close_page_with_margin (); page_end default_language True }
    ]
  }
    with 
    [ Sys_error s         -> abort lang Control.sys_err_mess s (* file pb *)
    | Stream.Error s      -> abort lang Control.stream_err_mess s (* file pb *)
    | Exit (* Sanskrit *) -> abort lang "Wrong character in input" "" 
    | Invalid_argument s  -> abort lang Control.fatal_err_mess s (* sub *)
    | Failure s           -> abort lang Control.fatal_err_mess s (* anomaly *)
    | End_of_file         -> abort lang Control.fatal_err_mess "EOF" (* EOF *)
    | Not_found           -> let s = "You must choose a parsing option" in
                             abort lang "Unset button in form - " s
    | Control.Fatal s     -> abort lang Control.fatal_err_mess s (* anomaly *)
    | Control.Anomaly s   -> abort lang Control.fatal_err_mess ("Anomaly: " ^ s)
    | _ -> abort lang Control.fatal_err_mess "Unexpected anomaly" 
    ]
 }
;

reader_page ()
;

(*i end; i*)
