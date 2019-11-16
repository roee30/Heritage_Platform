(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Reader summarizing interface. User aid with unrecognized segs. *)

open Html;
open Web; (* ps pl abort etc. [remote_server_host] *)
open Cgi;
open Phases;
open Checkpoints; (* [phase_encode] *) 

module Prel = struct (* Interface's lexer prelude *)
 value prelude_user () = do
  { pl http_header
  ; page_begin user_aid_meta_title 
  ; pl (body_begin Chamois_back)
  ; pl user_aid_title
  ; open_page_with_margin 15
  }
 ;
 end (* Prel *)
;
value rpc = remote_server_host 
and remote = ref False (* local invocation of cgi by default *)
;
value string_point (offset,len_chunk) (k,(phase,rword),select) =
  let pada = Canon.rdecode rword in
  let updated_k = if k < offset then k else (k-len_chunk-1) in
  string_of_int updated_k ^ "," ^ phase_encode phase ^ ",{" ^ pada ^ "},{" 
                          ^ bool_encode select ^ "}"
;
value rec string_points off = fun (* [off = (offset,len_chunk)] *)
  [ [] -> ""
  | [ last ] -> string_point off last 
  | [ first :: rest ] -> string_point off first ^ "|" ^ string_points off rest
  ]
;
value call_partial text (offset,len_chunk) cpts = 
  let list_points = match cpts with 
       [ [] -> []
       | [ _ :: rest ] -> rest
       ] in
  let cgi = graph_cgi ^ "?" ^ text ^ ";cpts=" ^ 
            (string_points (offset,len_chunk) list_points) in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
value string_point_orig (k,(phase,rword),select) =
  let pada = Canon.rdecode rword in
    string_of_int k ^ "," ^ phase_encode phase ^ ",{" ^ pada ^ "},{"
                    ^ bool_encode select ^ "}"
;
value rec string_points_orig = fun 
  [ [] -> ""
  | [ last ] -> string_point_orig last 
  | [ first :: rest ] -> string_point_orig first  ^ "|" ^ string_points_orig rest
  ]
;
value cpt_partial cpts = 
  let list_points = match cpts with 
      [ [] -> []
      | [ _ :: rest ] -> rest
      ] in 
  string_points_orig list_points
;
(* Parsing mandatory checkpoints *)
open Checkpoints;

value sort_check cpts = 
  let compare_index (a,_,_) (b,_,_) = compare a b in 
  List.sort compare_index cpts
;
value rec find_chunk chunks ind = fun
  [ 0 -> ind
  | l -> match chunks with
         [ [ a :: rest ] -> find_chunk rest (ind+1) (l-((List.length a)+1)) 
         | _ -> -1
         ]
  ]
;
value user_cgi_begin cgi =
  xml_begin_with_att "form" 
    [ ("action",cgi); ("method","get") ] (* input conversion script *)
  ^ xml_begin "div"
;
value arguments trs lex cache st us cp input topic abs corpus sent_id link_num =
  let corpus_link = match corpus with
      [ "" -> ""
      | _ -> ";corpus=" ^ corpus ^ ";sentenceNumber=" ^ sent_id ^ 
             ";linkNumber=" ^ link_num
      ] in
  "t=" ^ trs ^ ";lex=" ^ lex ^ ";cache=" ^ cache ^ ";st=" ^ st ^ 
  ";us=" ^ us ^ ";cp=" ^ cp ^ ";text=" ^ input ^ ";topic=" ^ topic ^ 
  ";abs=" ^ abs ^ corpus_link
;
value print_hidden topic st cp us lex cache abs translit corpus sent_id 
                   link_num = do
  { pl (hidden_input "topic" topic)
  ; pl (hidden_input "st" st)
  ; pl (hidden_input "cp" cp)
  ; pl (hidden_input "us" us)
  ; pl (hidden_input "t" translit)
  ; pl (hidden_input "lex" lex)
  ; pl (hidden_input "cache" cache)
  ; pl (hidden_input "abs" abs)
  ; match corpus with
    [ "" -> ()
    | corpus_val -> do
    { pl (hidden_input "corpus" corpus_val)
    ; pl (hidden_input "sentenceNumber" sent_id)
    ; pl (hidden_input "linkNumber" link_num)
    }
    ]
  }
;
value read_guess_index () = 
  (Gen.gobble Data.public_guess_auto : Deco.deco (string * string)) 
;
value read_mw_index () = 
  (Gen.gobble Data.public_mw_index_file: Deco.deco (string * string * string))
;

value rec mw_sol cur_sol word = fun
 [ [] -> cur_sol
 | [ (entry,lex,page) :: rest ] -> let updated_sol = 
    match lex with
    [ "Noun" | "Ind." -> cur_sol ^ Morpho_html.skt_anchor_M word entry page False
    | _ -> cur_sol
    ] in mw_sol updated_sol word rest
 ]
;

(* function to find only the gender *)
value find_gen morph = String.sub morph (String.length morph -2) 2 
;

value print_word word (entry,morph) = 
   let final_ent = word ^ entry in
   let mw_index = read_mw_index () in 
   let words = List.rev (Deco.assoc (Encode.code_string final_ent) mw_index) in 
   let header = td_begin ^ (table_begin Deep_sky_back) ^ 
                tr_begin ^ th_begin
   and (sol, is_checked) = match words with
         [ [] -> (" [" ^ Html.anchor_begin ^ Morpho_html.skt_roma final_ent ^ 
                         xml_end "a" ^ "]", False)
         | _ -> let mw_solution = mw_sol "" final_ent words in
                  if (String.length mw_solution) > 0 then 
                       (" [" ^ mw_solution ^ "]", True)
                  else (" [" ^ Html.anchor_begin ^ Morpho_html.skt_roma final_ent
                             ^ xml_end "a" ^ "]", False)
         ]
    and footer = th_end ^ tr_end ^ table_end ^ td_end in
    let radio =  (Html.radio_input_dft "guess" ("{" ^ final_ent ^ "},{" ^ 
                     find_gen morph ^ "}") "" is_checked) ^ morph in
    header ^ radio ^ sol ^ footer
;

value rec string_word sol_st word = fun
  [ [] -> sol_st
  | [ a :: rest ] -> let new_sol = sol_st ^ (print_word word a) in
                     string_word new_sol word rest
  ]
;

(* We should replace the following function by a more standard primitive *)
value normalize_end = fun
  [ [ a :: rest ] -> 
    let normalized_a = match a with
                       [ 16 -> 48 (* .h -> s *)
                       | 14 -> 41 (* .m -> m *)
                       | c -> c
                       ] in 
    [ normalized_a :: rest ]
  | other -> other
  ]
;

value aid_using translit checkpts sentence topic st cp us lex cache abs 
                corpus sent_id link_num =
  let encode = Encode.switch_code translit 
  and decode = Canon.switch_decode translit in
  let chunks = Sanskrit.read_sanskrit encode sentence in
  let devachunks = List.map Canon.unidevcode chunks in
  let devainput = String.concat " " devachunks in do
  { pl html_break
  ; pl (html_latin16 "Sentence: ")
  ; ps (deva16_blue devainput) (* devanagari *)
  ; pl html_break
  ; pl html_break
  ; pl center_begin
  ; pl (user_cgi_begin graph_cgi)
  ; print_hidden topic st cp us lex cache abs translit corpus sent_id link_num
  ; pl (xml_begin_with_att "textarea" 
      [ ("name","text"); ("rows","1"); ("cols","100") ] ^
      sentence ^ xml_end "textarea")
  ; pl html_break 
  ; pl (submit_input "Submit Revised Sentence")
  ; pl cgi_end
  ; pl html_break
  ; pl html_break
  ; pl (user_cgi_begin graph_cgi)
  ; print_hidden topic st cp us lex cache abs translit corpus sent_id link_num
  ; pl (hidden_input "text" sentence)
  ; let (offset, chunk_rev) = match checkpts with
        [ [ (k,(_,word),_) :: _ ] -> (k, Word.mirror word)
        | _ -> (0,[])
        ] in
    let len_chunk = Word.length chunk_rev
    and chunk_ind = find_chunk chunks 1 offset in do
  { pl (xml_begin_with_att "textarea" 
         [ ("name","revised"); ("rows","1"); ("cols","30") ] ^
         (decode chunk_rev) ^ xml_end "textarea")
  ; pl (hidden_input "rev_off" (string_of_int offset))
  ; pl (hidden_input "rev_ind" (string_of_int chunk_ind))
  ; pl (hidden_input "cpts" (cpt_partial checkpts))
  ; pl html_break 
  ; pl (submit_input "Submit Revised Chunk")
  ; pl cgi_end
  ; pl html_break
  ; pl html_break
  ; if List.length chunks > 1 then do
     { ps (div_begin Latin12)
     ; pl (table_begin Spacing20)
     ; pl tr_begin
     ; let rec decoded init cur_ind = fun
           [ [] -> String.sub init 0 ((String.length init)-1)
           | [ a :: rest ] -> 
               if cur_ind = chunk_ind then decoded init (cur_ind+1) rest
               else decoded (init ^ (decode a) ^ "+") (cur_ind+1) rest
           ] in
       let updated_text = decoded "" 1 chunks in
       let arg_string = arguments translit lex cache st us cp updated_text topic
                                  abs corpus sent_id link_num in
       ps (td_wrap (call_partial arg_string (offset,len_chunk) checkpts
                    ^ "Show partial solution without this chunk"))
     ; pl tr_end
     ; pl table_end
     ; ps div_end (* Latin12 *)
     }
   else ()
  (* adding new module to show the possibilities *)
  ; if Paths.platform = "Station" then do
  { ps html_paragraph 
  ; ps (div_begin Latin16)
  ; ps "Possible lemmatizations for the chunk:"
  ; ps div_end
  ; pl par_end
  ; ps (div_begin Latin12)
  ; pl (user_cgi_begin graph_cgi)
  ; print_hidden topic st cp us lex cache abs translit corpus sent_id link_num
  ; pl (hidden_input "cpts" (cpt_partial checkpts))
  ; pl (hidden_input "text" sentence)
  ; pl html_break
  ; let guess_auto = read_guess_index () in 
    let rec match_decl sol_string init = (* init is the last *) fun
    [ [] -> sol_string
    | [ a :: rest ] -> 
      let updated_init = [ a :: init ] in 
      let words = List.rev (Deco.assoc (Word.mirror updated_init) guess_auto) in
      let new_sol = match words with
          [ [] -> sol_string
          | _ -> let str_rest = Canon.decode (Word.mirror rest) in
                 let lemma = string_word "" str_rest words in
                 let this_string = tr_begin ^ lemma ^ tr_end in
                 [ this_string :: sol_string ]
          ] in
      match_decl new_sol updated_init rest
    ] in do
  { pl (table_begin_style Blue_  [ noborder; ("align","center"); spacing20 ]) 
  ; List.iter pl (match_decl [] [] (normalize_end (List.rev chunk_rev))) 
    (* [[]=sol_string, []=last] *)
  ; pl table_end
  }
  ; pl html_break
  ; pl (submit_input "Submit Morphology")
  ; pl cgi_end (* [graph_cgi] *)
  ; ps div_end (* Latin12 *)
  ; ps (par_begin Latin16)
  ; ps "Enter your own lemmatization:"
  ; pl par_end
  ; pl (user_cgi_begin graph_cgi)
  ; print_hidden topic st cp us lex cache abs translit corpus sent_id link_num
  ; pl (hidden_input "cpts" (cpt_partial checkpts))
  ; pl (hidden_input "text" sentence)
  ; pl html_break
  ; ps (text_area "guess" 1 20 "")
  ; pl html_break
  ; ps (option_select [ ("nom.","Nominative"); ("acc.","Accusative"); 
                        ("ins.","Instrumental"); ("dat.","Dative");
                        ("abl.","Ablative"); ("gen.","Genitive"); 
                        ("loc.","Locative"); ("voc.","Vocative") ])
  ; ps (option_select_label "gender" 
          [ ("m.","Masculine"); ("f.","Feminine"); ("n.","Neuter") ])
  ; ps (option_select [ ("sg.","Singular"); ("du.","Dual"); ("pl.","Plural") ])
  ; pl html_break
  ; pl (submit_input "Submit Choices")
  ; pl cgi_end
  } (* Paths.platform = "Station" *) else ()
  ; pl center_end
  ; pl html_break 
  }
}
;
value user_aid_engine () = do
  { Prel.prelude_user ()
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in
    let url_encoded_input = get "text" env ""
    and url_encoded_topic = get "topic" env ""
    and st = get "st" env "t"
    and cp = get "cp" env "t"
    and us = get "us" env "f"
    and translit = get "t" env "SL" (* default SLP1 *) 
    and lex = get "lex" env "SH" (* default Heritage *)
    and cache = get "cache" env "f" in
    let () = cache_active.val := cache in
    let corpus = get "corpus" env ""
    and sent_id = get "sentenceNumber" env "0"
    and link_num = get "linkNumber" env "0"
    and abs = get "abs" env "f" (* default local paths *) in
    let lang = language_of lex
    and input = decode_url url_encoded_input (* unnormalized string *) in
    let checkpts = 
       try let url_encoded_cpts = List.assoc "cpts" env in (* do not use get *)
           parse_cpts (decode_url url_encoded_cpts)
       with [ Not_found -> [] ] in
    try do
     { aid_using translit checkpts input url_encoded_topic st cp us lex cache
                 abs corpus sent_id link_num
     ; close_page_with_margin ()
     ; page_end lang True
     }
    with 
    [ Sys_error s         -> abort lang Control.sys_err_mess s (* file pb *)
    | Stream.Error s      -> abort lang Control.stream_err_mess s (* file pb *)
    | Encode.In_error s   -> abort lang "Wrong input " s
    | Exit (* Sanskrit *) -> abort lang "Wrong character in input" "" 
    | Invalid_argument s  -> abort lang Control.fatal_err_mess s (* sub *)
    | Failure s           -> abort lang Control.fatal_err_mess s (* anomaly *)
    | End_of_file         -> abort lang Control.fatal_err_mess "EOF" (* EOF *)
    | Not_found           -> let s = "You must choose a parsing option" in
                             abort lang "Unset button in form - " s
    | Control.Fatal s     -> abort lang Control.fatal_err_mess s (* anomaly *)
    | Control.Anomaly s   -> abort lang Control.fatal_err_mess ("Anomaly: " ^ s)
    | _                   -> abort lang Control.fatal_err_mess "Unknown anomaly" 
    ]
 }
; 
value safe_engine () =
  let abor = abort default_language in
  try user_aid_engine () with  
  [ _ -> abor Control.fatal_err_mess "Unexpected anomaly - broken session" ]
;
safe_engine () (* Should always produce a valid xhtml page *)
;
