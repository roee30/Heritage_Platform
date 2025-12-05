(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
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
open Encode
  
(* [switch_code] *)
open Canon
  
open Html
  
open Web
  
(* ps pl abort etc. [remote_server_host] *)
open Cgi
  
(* [get decode_url] *)
open Phases
  
(* [Phases] *)
open Rank
  
(* [Prel Lex Lexer_control Transducers segment_all Solutions] *)
(* Reader interface *)
(* Mode parameter of the reader. Controled by service Reader for respectively
   tagging, shallow parsing, or dependency analysis with the UoH parser.  *)
(* Note that Interface is not a Reader/Parser mode. *)
type mode = | Tag | Parse | Analyse

let rpc = remote_server_host
and remote = ref false
  
(* local invocation of cgi by default *)
let call_parser text sol =
  let cgi = parser_cgi ^ ("?" ^ (text ^ ("p;n=" ^ sol))) in
  (* same remark as below: this assumes mode is last parameter *)
  let invocation = if !remote then rpc ^ cgi else cgi
  in anchor Green_ invocation check_sign
  
let call_graph text =
  let cgi = graph_cgi ^ ("?" ^ (text ^ "g")) in
  let invocation = if !remote then rpc ^ cgi else cgi
  in anchor Green_ invocation check_sign
  
(* Prints n-th solution *)
(* ind is relative index within kept, n is absolute index within max *)
let print_solution text ind (n, output) =
  (pl html_break;
   pl hr;
   ps (span_begin Blue_);
   ps "Solution ";
   print_int n;
   ps " : ";
   ps (call_parser text (string_of_int n));
   ps span_end;
   pl html_break;
   let _ = List.fold_left Lex.print_segment 0 (List.rev output) in ind + 1)
  
(**************************************************************)
(*     General display of solutions, in the various modes     *)
(**************************************************************)
let print_sols text revsols = (* stats = (kept,max) *)
  let process_sol = print_solution text in
  let _ = List.fold_left process_sol 1 revsols in ()
  
let display limit mode text saved =
  function
  | (* [saved] is the list of all solutions of penalty 0 when 
     [filter_mode] of [process_input] is True,
     otherwise it lists all the solutions. *)
      [] -> (pl (html_blue "No solution found"); pl html_break)
  | best_sols ->
      let kept = List.length best_sols
      and max = (match limit with | Some n -> n | None -> truncation)
      in
        (if mode = Analyse
         then ()
         else
           (print_sols text (*kept,max*) best_sols;
            pl html_break;
            pl hr;
            if limit = None
            then
              (pl (html_blue "Output truncated at ");
               ps (span_begin Red_);
               print_int truncation;
               ps span_end;
               pl (html_blue " solutions");
               pl html_break)
            else ());
         (match mode with
          | Parse ->
              (ps (html_magenta (string_of_int kept));
               (let mess =
                  " solution" ^
                    ((if kept = 1 then "" else "s") ^ " kept among ")
                in ps (html_blue mess));
               ps (html_magenta (string_of_int max));
               pl html_break;
               if kept < max
               then
                 (pl (html_blue "Filtering efficiency: ");
                  (let eff = ((max - kept) * 100) / (max - 1)
                   in pl (html_magenta ((string_of_int eff) ^ "%"))))
               else ();
               pl html_break;
               (match saved with
                | [] -> ()
                | (_, min_buck) :: _ ->
                    ((* we print only the upper layer of saved *)
                     pl html_break;
                     ps (html_red "Additional candidate solutions");
                     (let min_sols = List.rev min_buck
                      in print_sols text (*kept,max*) min_sols);
                     pl html_break)))
          | Analyse ->
              (* [best_sols: list (int * list Rank.Lex.Disp.segment)] *)
              let solutions =
                (match saved with
                 | [] -> best_sols
                 | (_, min_buck) :: _ ->
                     List.append best_sols (List.rev min_buck)) in
              let forget_transitions (phase, word, _) = (phase, word) in
              let forget_index (_, segments) =
                List.map forget_transitions segments in
              let segmentations = List.map forget_index solutions in
              let scl_font =
                (match !sanskrit_font with | Deva -> "DEV" | Roma -> "IAST")
              in Scl_parser.print_scl scl_font segmentations
          | _ -> ()))
  
(* NB This reader is parameterized by an encoding function, that parses the
   input as a list of words, according to various transliteration schemes.
   However, the use of "decode" below to compute the romanisation and devanagari
   renderings does a conversion through VH transliteration which may not be
   faithful to encodings which represent eg the sequence of phonemes t and h. *)
let process_input text us mode topic (input : string) encode cpts =
  let pieces = Sanskrit.read_raw_sanskrit encode input in
  let romapieces = List.map Canon.uniromcode pieces in
  let romainput = String.concat " " romapieces in
  let chunker =
    if us
    then (* sandhi undone *) Sanskrit.read_raw_sanskrit
    else (* blanks non-significant *) Sanskrit.read_sanskrit in
  let chunks = chunker encode input in (* normalisation here *)
  let deva_chunks = List.map Canon.unidevcode chunks in
  let deva_input = String.concat " " deva_chunks
  in
    ((*pl (xml_begin_with_att "p" [ ("align","center") ])*)
     (*; ps (div_begin Latin16)*)
     (*; pl (call_graph text ^ " Show Summary of Solutions")*)
     (*; pl (xml_end "p")*)
     (*; pl "Input:" *)
     (*; ps (roma16_red_sl romainput) (* romanisation *)*)
     (*; pl hr*)
     (*; pl html_break*)
     (*; pl "Sentence: "*)
     (*; ps (deva16_blue deva_input) (* devanagari *)*)
     (*; pl html_break*)
     (*; if mode = Analyse then () else ps "may be analysed as: ; "*)
     ps div_end;
     (* Latin16 *)
     let all_chunks =
       match topic with
       | Some topic -> chunks @ [ code_string topic ]
       | None -> chunks in
     let filter_mode = (mode = Parse) || (mode = Analyse)
     in
       try segment_all filter_mode all_chunks cpts
       with
       | Solutions (limit, revsols, saved) ->
           let sols = List.rev revsols in display limit mode text saved sols)
  
let sort_check cpts =
  let compare_index (a, _, _) (b, _, _) = compare a b
  in List.sort compare_index cpts
  
(* Standard format of cgi arguments *)
let arguments translit lex font cache st us input topic abs cpts =
  "t=" ^
    (translit ^
       (";lex=" ^
          (lex ^
             (";font=" ^
                (font ^
                   (";cache=" ^
                      (cache ^
                         (";st=" ^
                            (st ^
                               (";us=" ^
                                  (us ^
                                     (";text=" ^
                                        (input ^
                                           (";topic=" ^
                                              (topic ^
                                                 (";abs=" ^
                                                    (abs ^
                                                       (";cpts=" ^
                                                          ((Checkpoints.
                                                              string_points
                                                              cpts)
                                                             ^ ";mode=")))))))))))))))))))
  
(* mode to be filled later *)
(* Faster if only segmenting: no loading of [nouns_file], [roots_file], ... *)
let dd x = (ps "XXX"; x)
  
let reader_engine () = (*Prel.prelude () ; *)
  let query =
    try Sys.getenv "QUERY_STRING"
    with | Not_found -> failwith "Environment required" in
  let env = create_env query in
  let url_encoded_input = get "text" env ""
  and url_encoded_mode = get "mode" env "p"
  and url_encoded_topic = get "topic" env "" and st = get "st" env "t"
  and (* default vaakya rather than isolated pada *) us = get "us" env "f"
  and (* default input sandhied *) translit =
    get "t" env Paths.default_transliteration
  and lex = get "lex" env Paths.default_lexicon
  and font = get "font" env Paths.default_display_font in
  let ft = font_of_string font
  and (* Deva vs Roma print *) cache = get "cache" env "f" in
  let () = toggle_sanskrit_font ft and () = cache_active := cache
  and abs = get "abs" env "f" in (* default local paths *)
  let lang = Html.language_of_string lex
  and input = decode_url url_encoded_input
  and (* unnormalized string *) uns = us = "t"
  and (* unsandhied vs sandhied corpus *) encode = switch_code translit
  and (* encoding as a normalized word *) () = Html.toggle_lexicon lex
  and () = if abs = "t" then remote := true else ()
  and (* Web service mode *) () =
    if st = "f" then Lexer_control.star := false else ()
  and (* word stemmer *) () =
    Lexer_control.transducers_ref := Transducers.mk_transducers ()
  and mode =
    match decode_url url_encoded_mode with
    | "t" -> Tag
    | "p" -> Parse
    | "o" -> Analyse
    | (* Analyse mode of UoH parser *) s ->
        raise (Failure ("Unknown mode " ^ s))
  and (* Contextual information from past discourse *) topic_mark =
    decode_url url_encoded_topic in
  let topic =
    match topic_mark with
    | "m" -> Some "sa.h"
    | "f" -> Some "saa"
    | "n" -> Some "tat"
    | _ -> None
  and abortl = abort lang
  and checkpoints = (* checkpoints for graph *)
    try
      let url_encoded_cpts = List.assoc "cpts" env
      in
        (* do not use get *)
        Checkpoints.parse_cpts (decode_url url_encoded_cpts)
    with | Not_found -> [] in
  let cpts = sort_check checkpoints
  in
    try
      let text =
        arguments translit lex font cache st us url_encoded_input
          url_encoded_topic abs checkpoints
      in
        ((* Now we call the lexer *)
         process_input text uns mode topic input encode cpts;
         pl hr;
         pl html_break;
         close_page_with_margin ())
    with
    | (*; page_end lang True*) Sys_error s -> abortl Control.sys_err_mess s
    | (* file pb *) Stream.Error s -> abortl Control.stream_err_mess s
    | (* file pb *) Encode.In_error s -> abortl "Wrong input " s
    | Exit -> (* Sanskrit *) abortl "Wrong character in input" ""
    | Invalid_argument s -> abortl Control.fatal_err_mess s
    | (* sub *) Failure s -> abortl Control.fatal_err_mess s
    | End_of_file -> abortl Control.fatal_err_mess "EOF"
    | (* EOF *) Not_found ->
        let s = "You must choose a parsing option"
        in abortl "Unset button in form - " s
    | Control.Fatal s -> abortl Control.fatal_err_mess s
    | (* fatal *) Control.Anomaly s ->
        abortl Control.anomaly_err_mess ("Anomaly: " ^ s)
    | _ -> abortl Control.anomaly_err_mess "Unexpected anomaly"
  
let safe_engine lang =
  (* In case of error, we lose the current language of the session *)
  try reader_engine ()
  with | Failure s -> abort lang Control.fatal_err_mess s
  | _ -> abort lang Control.anomaly_err_mess "Unexpected anomaly"
  
(* Should always produce a compliant xhtml page *)
let _ = safe_engine Html.default_language
  

