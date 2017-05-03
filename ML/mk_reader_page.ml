(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This program creates the page [reader_page] (Sanskrit Reader Interface) 
   invoking the CGI sktreader alias reader. Invoked without language argument,
   it is itself the CGI [skt_heritage] invokable separately. *)

(*i module Mk_reader_page = struct i*)

open Html;
open Web; (* ps pl abort etc. *)
open Cgi; (* [create_env get] *)
 
value back_ground = background Chamois
  (*[ obs if Install.narrow_screen then background Chamois else Pict_hare ]*)
;
value out_mode = ref None
;
value set_cho () = Arg.parse 
  [ ("-fr", Arg.Unit (fun () -> out_mode.val:=Some French), "French")
  ; ("-en", Arg.Unit (fun () -> out_mode.val:=Some English), "English")
  ; ("",    Arg.Unit (fun () -> out_mode.val:=None), "cgi output on stdout")
  ]
  (fun s -> raise (Arg.Bad s)) 
  "Usage: mk_reader_page -en or mk_reader_page -fr or mk_reader_page"
;
value print_cache_policy cache_active = do
  { ps " Cache "
  ; let options = 
      [ (" On ","t",cache_active="t")  (* Cache active *)
      ; (" Off ","f",cache_active="f") (* Ignore cache *)
      ] in
    pl (option_select_default "cache" options)
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
       { reader_prelude ""; (English,Sys.getenv "QUERY_STRING") }
      ] in try 
    let env = create_env query in
    let url_encoded_input = get "text" env "" 
    and url_encoded_mode  = get "mode" env "g"
    and url_encoded_topic = get "topic" env ""
    and st = get "st" env "t" (* default vaakya rather than isolated pada *)
    and cp = get "cp" env default_mode
    and us = get "us" env "f" (* default input sandhied *)
    and cache_active = get "cache" env cache_active.val
    and translit = get "t" env Paths.default_transliteration in
    (* Contextual information from past discourse *)
    let topic_mark = decode_url url_encoded_topic 
    and text = decode_url url_encoded_input in do
  { pl (body_begin back_ground) 
  ; print_title (Some lang) reader_title
  ; pl center_begin 
  ; pl (cgi_reader_begin reader_cgi "convert") 
  ; print_lexicon_select (lexicon_of lang)
  ; if cache_allowed then print_cache_policy cache_active else ()
  ; pl html_break 
  ; pl "Text " 
  ; pl (option_select_default "st" 
        [ (" Sentence ","t",st="t") 
        ; ("   Word   ","f",st="f")  
(*i OBS ; (" Nyaya Cpd","n",st="n")  i*)
        ])
  ; pl " Format "
  ; pl (option_select_default "us" 
        [ (" Unsandhied ","t",us="t") 
        ; ("  Sandhied  ","f",us="f") 
        ])
  ; pl " Parser strength "
  ; pl (option_select_default "cp"
        [ ("  Full  ","t",cp="t") 
        ; (" Simple ","f",cp="f")
(*      ; ("Experiment","e",cp="e")  deprecated *)
        ])
  ; pl html_break  
  ; ps (reader_input_area_default text)
  ; pl html_break 
  ; ps "Input convention "
  ; ps (transliteration_switch_default translit "trans")
  ; pl " Optional topic " (* For the moment assumed singular *)
  ; pl (option_select_default "topic"
        [ (" Masculine ","m",topic_mark="m")  
        ; (" Feminine  ","f",topic_mark="f")  
        ; ("  Neuter   ","n",topic_mark="n")
        ; ("   Void    ","" ,topic_mark="") 
        ])
  ; pl " Mode "
  ; pl (option_select_default_id "mode_id" "mode"
        (interaction_modes_default url_encoded_mode))
  ; pl html_break 
  ; pl (submit_input "Read") 
  ; pl (reset_input "Reset")
  ; pl cgi_end
  ; pl center_end 
  ; match out_mode.val with
    [ Some lang -> close_html_file lang True 
    | None -> do { close_page_with_margin (); page_end English True }
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
