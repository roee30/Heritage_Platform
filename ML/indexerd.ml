(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin indexerd for indexing in sanskrit dico without diacritics.     *)
(* This CGI is triggered by page [index.html] in [dico_dir].              *)
(* Reads its input in shell variable [QUERY_STRING] URI-encoded.          *)
(*i Test in csh with: setenv QUERY_STRING "query=yoga"; indexerd         i*)
(*i Web invocation is http://skt_server_url/cgi-bin/sktsearch?query=str   i*)

(*i module Indexerd = struct i*)

open Html;
open Web; (* ps pl etc. *)
open Cgi;

value answer_begin () = do
  { pl (table_begin Yellow_cent)
  ; ps tr_begin
  ; ps th_begin
  }
;
value answer_end () = do
  { ps th_end
  ; ps tr_end
  ; pl table_end
  ; pl html_paragraph
  }
;
value back_ground = background Chamois
;
value prelude () = do
  { pl http_header
  ; page_begin heritage_dictionary_title
  ; pl (body_begin back_ground)
  ; pl html_paragraph
  ; print_title_solid Mauve (Some Html.French) dico_title_fr 
  }
; 
value postlude () = do
  { ()
  ; page_end Html.French True 
  }
;
value print_word c = pl (Morpho_html.skt_anchor_R False (Canon.decode_ref c))
;
(* Each dummy is mapped to a list of words - all the words which
   give back the dummy by normalisation such as removing diacritics *)
value read_dummies () =
  (Gen.gobble public_dummies_file : Deco.deco Word.word)
;
value index_engine () = 
  let abor = abort Html.French (* may not preserve the current lang *) in
  try let dummies_deco = read_dummies () in do
     { prelude () 
     ; let query = Sys.getenv "QUERY_STRING" in
       let alist = create_env query in
       (* We do not assume transliteration, just ordinary roman letters *)
       (* TODO: adapt to MW search along Indexer *)
       let url_encoded_entry = List.assoc "q" alist in
       let str = decode_url url_encoded_entry in 
       try let word = Encode.code_skt_ref_d str (* normalization *) in do
           { answer_begin ()
           ; ps (div_begin Latin12)
           ; let words = Deco.assoc word dummies_deco in
             match words with
               [ [] -> do { ps (Morpho_html.skt_red str)
                          ; ps " not found in Heritage dictionary"
                          ; ps html_break; pl html_break
                          }
               | _ -> List.iter print_word words
               ]  
           ; ps div_end (* Latin12 *)
           ; answer_end ()
           ; postlude ()
           }
       with [ Stream.Error _ -> abor "Illegal input " str ]
     }
 with 
  [ Sys_error s        -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s     -> abor Control.stream_err_mess s (* file pb *)
  | Invalid_argument s -> abor Control.fatal_err_mess s (* sub *)
  | Failure s          -> abor Control.fatal_err_mess s (* anomaly *)
  | Control.Fatal s    -> abor Control.fatal_err_mess s (* anomaly *)
  | Not_found          -> abor Control.fatal_err_mess "assoc" (* assoc *)
  | End_of_file        -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | Encode.In_error s  -> abor "Wrong_input " s
  | Exit               -> abor "Wrong character in input - " "use ASCII" (* Sanskrit *)
  | _                  -> abor Control.fatal_err_mess "Unexpected anomaly" (* ? *)
  ]
;
index_engine ()
;
(*i end; i*)
