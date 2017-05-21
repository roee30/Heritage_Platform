(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin sktreader alias Reader for segmentation, tagging and parsing. 
   Reads its input in shell variable [QUERY_STRING] URI-encoded. 
   This CGI is triggered by page [reader_page] created by [sktreader]. 
   It prints an HTML document giving segmentation/tagging of input on stdout. 

   It invokes Rank to construct the lexer Lex, compute penalties of its various 
   solutions, and return all solutions with minimal penalties. *)

(* This is mostly legacy code, being superseded by sharing Interface module *)

(*i A typical browser invocation is 
   http://skt_server_url/cgi-bin/sktreader?t=VH;text=vivekananda.h;mode=t;topic=
   Test offline in UNIX shell bash with: (beware: no space around "=")
   QUERY_STRING="t=VH;text=vivekananda.h;mode=t;topic="; ./reader     i*)

(*i module Reader = struct i*)

open Encode; (* [switch_code] *)
open Canon;
open Html; 
open Web; (* ps pl abort etc. [remote_server_host] *)
open Cgi; (* [get decode_url] *)
open Phases; (* [Phases] *)
open Rank; (* [Prel Lex segment_all iterate] *) 
open Uoh_interface; (* Interface with UoH dependency parser *) 

module Ext = UOH Lex (* [print_ext print_nn] *)
;
(* Reader interface *)
(* Mode parameter of the reader. Controled by service Reader for respectively
   tagging, shallow parsing, or dependency analysis with the UoH parser.  *)
(* Note that Summary/Interface is not a Reader/Parser mode. *)
type mode = [ Tag | Parse | Analyse ]
;
value rpc = remote_server_host  
and remote = ref False (* local invocation of cgi by default *)
;
value call_parser text sol = 
  let cgi = parser_cgi ^ "?" ^ text ^ "p;n=" ^ sol in
            (* same remark as below: this assumes mode is last parameter *)
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
value call_graph text = 
  let cgi = graph_cgi ^ "?" ^ text ^ "g" in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
    
(* Prints n-th solution *)
(* ind is relative index within kept, n is absolute index within max *)
value print_solution text ind (n,output) = do
  { pl html_break
  ; pl hr
  ; ps (span_begin Blue_)
  ; ps "Solution "; print_int n; ps " : " 
  ; ps (call_parser text (string_of_int n))
  ; ps span_end
  ; pl html_break
  ; let _ = List.fold_left Lex.print_segment 0 (List.rev output) in
    ind+1
  }
;

(**************************************************************)
(*     General display of solutions, in the various modes     *)
(**************************************************************)

value print_sols text revsols = (* stats = (kept,max) *)
  let process_sol = print_solution text in
  let _ = List.fold_left process_sol 1 revsols in ()
;

value display limit mode text saved = fun
  (* [saved] is the list of all solutions of penalty 0 when 
     [filter_mode] of [process_input] is True,
     otherwise it lists all the solutions. *)
  [ [] -> do { pl (html_blue "No solution found"); pl html_break }
  | best_sols -> 
    let kept = List.length best_sols
    and max = match limit with 
              [ Some n -> n | None -> truncation ] in do
    { if mode = Analyse then () 
      else do
         { print_sols text (*kept,max*) best_sols
         ; pl html_break
         ; pl hr 
         ; if limit = None then do
              { pl (html_blue "Output truncated at ") 
              ; ps (span_begin Red_)
              ; print_int truncation
              ; ps span_end
              ; pl (html_blue " solutions") 
              ; pl html_break
              } else ()
         }
    ; match mode with
      [ Parse -> do
         { ps (html_magenta (string_of_int kept))
         ; let mess = " solution" ^ (if kept=1 then "" else "s")
                                  ^ " kept among " in
           ps (html_blue mess)
         ; ps (html_magenta (string_of_int max))
         ; pl html_break
         ; if kept<max then do
              { pl (html_blue "Filtering efficiency: ") 
              ; let eff = (max-kept)*100/(max-1) in 
                pl (html_magenta (string_of_int eff ^ "%"))
              } else ()
         ; pl html_break
         ; match saved with 
           [ [] -> ()
           | [ (_,min_buck) :: _ ] -> do 
               (* we print only the upper layer of saved *)
             { pl html_break
             ; ps (html_red "Additional candidate solutions")
             ; let min_sols = List.rev min_buck in 
               print_sols text (*kept,max*) min_sols 
             ; pl html_break
             }
           ]
         }
      | Analyse -> match saved with 
         [ [] -> Ext.print_ext best_sols
         | [ (_,min_buck) :: _ ] -> 
           let zero_pen = List.append best_sols (List.rev min_buck) in
           Ext.print_ext zero_pen
         ]
      | _ -> ()
      ]
    }
  ]
;

(* NB This reader is parametrized by an encoding function, that parses the
   input as a list of words, according to various transliteration schemes.
   However, the use of "decode" below to compute the romanisation and devanagari
   renderings does a conversion through VH transliteration which may not be
   faithful to encodings which represent the sequence of phonemes t and h. *)
value process_input text us mode topic (input:string) encode cpts = 
  let pieces = Sanskrit.read_raw_sanskrit encode input in
  let romapieces = List.map Canon.uniromcode pieces in
  let romainput = String.concat " " romapieces in
  let chunker = if us (* sandhi undone *) then Sanskrit.read_raw_sanskrit 
                else (* blanks non-significant *) Sanskrit.read_sanskrit in
  let chunks = chunker encode input (* normalisation here *) in 
  let devachunks = List.map Canon.unidevcode chunks in
  let devainput = String.concat " " devachunks in do
  { pl (xml_begin_with_att "p" [ ("align","center") ])
  ; ps (div_begin Latin16)
  ; pl (call_graph text ^ " Show Summary of Solutions")
  ; pl (xml_end "p")
  ; pl "Input:" 
  ; ps (roma16_red_sl romainput) (* romanisation *)
  ; pl hr
  ; pl html_break
  ; pl "Sentence: "
  ; ps (deva16_blue devainput) (* devanagari *)
  ; pl html_break
  ; if mode = Analyse then () else ps "may be analysed as:"
  ; ps div_end (* Latin16 *)
  ; let all_chunks = match topic with
        [ Some topic -> chunks @ [ code_string topic ]
        | None -> chunks
        ] in
    let filter_mode = mode=Parse || mode=Analyse in
    try segment_all filter_mode all_chunks cpts with
        [ Solutions limit revsols saved ->  
           let sols = List.rev revsols in 
           display limit mode text saved sols 
        ]
  } 
;
value sort_check cpts = 
  let compare_index (a,_,_) (b,_,_) = compare a b in
  List.sort compare_index cpts
;

(* Standard format of cgi arguments *)
value arguments translit lex cache st us cp input topic abs cpts =
     "t="     ^ translit
  ^ ";lex="   ^ lex 
  ^ ";cache=" ^ cache 
  ^ ";st="    ^ st 
  ^ ";us="    ^ us 
  ^ ";cp="    ^ cp 
  ^ ";text="  ^ input 
  ^ ";topic=" ^ topic 
  ^ ";abs="   ^ abs 
  ^ ";cpts="  ^ Checkpoints.string_points cpts 
  ^ ";mode=" (* mode to be filled later *)
;
(* Faster if only segmenting: no loading of [nouns_file], [roots_file], ... *)
value reader_engine () = do
  { Prel.prelude () 
  ; let query = try Sys.getenv "QUERY_STRING" with 
                [ Not_found -> failwith "Environment required" ] in
    let env = create_env query in
    let url_encoded_input = get "text" env "" 
    and url_encoded_mode  = get "mode" env "p"
    and url_encoded_topic = get "topic" env ""
    and st = get "st" env "t" (* default vaakya rather than isolated pada *)
    and cp = get "cp" env "t" (* default Complete mode *)
    and us = get "us" env "f" (* default input sandhied *)
    and translit = get "t" env Paths.default_transliteration 
    and lex = get "lex" env Paths.default_lexicon
    and cache = get "cache" env "f" in
    let () = cache_active.val := cache
    and abs = get "abs" env "f" (* default local paths *) in
    let lang = Html.language_of lex 
    and input = decode_url url_encoded_input (* unnormalized string *)
    and uns = us="t" (* unsandhied vs sandhied corpus *)
    and encode = switch_code translit (* encoding as a normalized word *)
    and () = Html.toggle_lexicon lex
    and () = if abs="t" then remote.val:=True else () (* Web service mode *)
    and () = if st="f" then iterate.val:=False else () (* word stemmer *)
    and () = if cp="f" then complete.val:=False else () (* simplified reader *)
    and mode = match decode_url url_encoded_mode with
        [ "t" -> Tag
        | "p" -> Parse
        | "o" -> Analyse (* Analyse mode of UoH parser *) 
        | s -> raise (Failure ("Unknown mode " ^ s))  
        ] 
    (* Contextual information from past discourse *)
    and topic_mark = decode_url url_encoded_topic in
    let topic = match topic_mark with
        [ "m" -> Some "sa.h"
        | "f" -> Some "saa"
        | "n" -> Some "tat"
        | _ -> None
        ] 
    and abortl = abort lang
    and checkpoints = (* checkpoints for graph *) 
       try let url_encoded_cpts = List.assoc "cpts" env in (* do not use get *)
           Checkpoints.parse_cpts (decode_url url_encoded_cpts)
       with [ Not_found -> [] ] in     
    let cpts = sort_check checkpoints in 
    try let text = arguments translit lex cache st us cp url_encoded_input
                             url_encoded_topic abs checkpoints in do
        { (* Now we call the lexer *)
           process_input text uns mode topic input encode cpts 
        ; pl hr
	; pl html_break  
        ; close_page_with_margin () 
        ; page_end lang True
        }
    with 
    [ Sys_error s         -> abortl Control.sys_err_mess s (* file pb *)
    | Stream.Error s      -> abortl Control.stream_err_mess s (* file pb *)
    | Encode.In_error s   -> abortl "Wrong input " s  
    | Exit (* Sanskrit *) -> abortl "Wrong character in input" "" 
    | Invalid_argument s  -> abortl Control.fatal_err_mess s (* sub *)
    | Failure s           -> abortl Control.fatal_err_mess s 
    | End_of_file         -> abortl Control.fatal_err_mess "EOF" (* EOF *)
    | Not_found           -> let s = "You must choose a parsing option" in
                             abortl "Unset button in form - " s
    | Control.Fatal s     -> abortl Control.fatal_err_mess s (* fatal *) 
    | Control.Anomaly s   -> abortl Control.anomaly_err_mess ("Anomaly: " ^ s) 
    | _ -> abortl Control.anomaly_err_mess "Unexpected anomaly" 
    ]
  }
;
value safe_engine lang = 
(* In case of error, we lose the current language of the session *)
try reader_engine () with 
  [ Failure s -> abort lang Control.fatal_err_mess s 
  | _ -> abort lang Control.anomaly_err_mess "Unexpected anomaly" 
  ]
;
(* Should always produce a compliant xhtml page *)
safe_engine Html.default_language 
;

(*i end; i*)
