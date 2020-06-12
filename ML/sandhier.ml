(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sandhi Engine cgi *)
(* It gives the most common sandhi solution, but not the optional forms *)
(* This stand-alone module is not used by the rest of the system *)

(*i module Sandhier = struct i*)

open Sandhi; (* [final_sandhi] [ext_sandhi] *)
open Int_sandhi; (* [int_sandhi] *)
open Html;
open Web; (* ps pl abort etc. *)
open Cgi;

value title = h1_title (if narrow_screen then "Sandhi" 
                         else "The Sandhi Engine")
and meta_title = title "Sanskrit Sandhi Engine"
;
value display_rom_red s = html_red (Transduction.skt_to_html s)
and   display_dev_red s = html_devared (Encode.skt_to_deva s) 
;
value sandhi_engine () = do
  { pl http_header
  ; page_begin meta_title 
  ; pl (body_begin (background Chamois))
  ; pl title
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in 
   try
    let url_encoded_left = get "l" env ""
    and url_encoded_right = get "r" env ""
    and url_encoded_kind = get "k" env "external"
    and translit = get "t" env Paths.default_transliteration 
    and lex = get "lex" env Paths.default_lexicon in 
    let left_str = decode_url url_encoded_left 
    and right_str = decode_url url_encoded_right 
    and lang = language_of_string lex 
    and encode = Encode.switch_code translit in
    let left_word = encode left_str
    and right_word = encode right_str in
    let rleft_word = Word.mirror left_word 
    and final = (right_word = []) in 
    let result_word = match url_encoded_kind with
        [ "external" -> 
            if final then final_sandhi rleft_word
            else ext_sandhi rleft_word right_word
        | "internal" -> 
            if final then raise (Control.Fatal "Empty right component")
            else int_sandhi rleft_word right_word
        | _ -> raise (Control.Fatal "Unexpected kind")
        ] in
    let kind = if final then "final" else url_encoded_kind in 
    let left = Canon.decode left_word   (* = [left_str] *)
    and right = Canon.decode right_word (* = [right_str] *)
    and result = Canon.decode result_word in do
    { ps (span_begin C1)
    ; ps ("The " ^ kind ^ " sandhi of ")
    ; ps (display_rom_red left)
    ; if final then () else do
         { ps " and "
         ; ps (display_rom_red right)
         }
    ; ps " is "
    ; ps (display_rom_red result)
    ; ps span_end (* C1 *)
    ; ps center_begin
    ; ps (span_skt_begin Deva20c)
    ; ps (display_dev_red left)
    ; ps " | "
    ; if final then () else ps (display_dev_red right)
    ; ps " = "
    ; ps (display_dev_red result)
    ; ps span_end (* Deva20c *)
    ; ps center_end
    ; ps (span_begin C1)
    ; ps "NB. Other sandhi solutions may be allowed"
    ; ps span_end (* C1 *)
    ; page_end lang True
    } 
   with [ Stream.Error _ -> raise Exit
        | Not_found -> failwith "parameter missing ?"
        ] 
  }
;
value safe_engine () =
  let abor = abort default_language in
  try sandhi_engine () with 
  [ Sys_error s        -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s     -> abor Control.stream_err_mess s (* file pb *)
  | Encode.In_error s  -> abor "Wrong_input " s
  | Invalid_argument s -> abor Control.fatal_err_mess s (* sub *)
  | Failure s          -> abor Control.fatal_err_mess s (* anomaly *) 
  | Not_found          -> abor Control.fatal_err_mess "assoc" (* assoc *)
  | End_of_file        -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | Control.Fatal s    -> abor "Wrong parameters " s
  | Exit               -> abor "Wrong character in input - " 
                               "check input convention" (* Sanskrit *)
  | _                  -> abor Control.fatal_err_mess "Unexpected anomaly" 
  ]
;
safe_engine ()
;
(*i end; i*)
