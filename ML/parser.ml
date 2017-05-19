(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin callback for shallow syntax analysis *)
(* Parser is similar to Reader, but it is invoked from the green hearts
in the output of the reader, in order to give the semantic analysis of
a specific solution. It basically replays reading until this specific solution *)

(*i module Parser = struct i*)
 
open Encode;
open Canon;
open Html;
open Web; (* ps pl abort truncation etc. [remote_server_host] *)
open Cgi; (* get *)
open Checkpoints;
open Uoh_interface; (* Interface with UoH dependency parser *)

module Prel = struct (* Parser's lexer prelude *)

(* prelude is executed by Lexer when initialisation of transducers fails *)
value prelude () = do
  { pl http_header
  ; page_begin parser_meta_title 
  ; pl (body_begin Chamois_back)
  ; if scl_toggle then (* external call SCL (experimental) *)
       pl (javascript (SCLpaths.scl_url ^ javascript_tooltip))
    else ()
  ; pl parser_title
  ; open_page_with_margin 15
  }
;
end (* Prel *)
;
value iterate = ref True (* by default we read a sentence (list of words) *)
and  complete = ref True (* by default we call the fuller segmenter *)
and output_channel = ref stdout (* by default cgi output *)
;

module Lexer_control = struct
 value star = iterate;
 value full = complete;
 value out_chan = output_channel;
end (* [Lexer_control] *)
;
module Lex = Lexer.Lexer Prel Lexer_control 
(* [print_proj print_segment_roles print_ext_segment extract_lemma] *)
;
module Ext = UOH Lex 
; 
value rpc = remote_server_host 
and remote = ref False (* local invocation of cgi by default *) 
;
open Skt_morph;
open Inflected;
open Constraints; (* [roles_of sort_flatten extract] *)
open Paraphrase; (* [display_penalties print_sem print_role] *)

value query = ref "" (* ugly - stores the query string *)
;
value set_query q = query.val := q (* [Parser.parser_engine] *)
;
(* Duplicated from Rank *)
value make_groups tagger = comp_rec 1 []  
  where rec comp_rec seg stack = fun (* going forward in time *)
  [ [] -> stack (* result goes backward in time *)
  | [ (phase,rword,_) :: rest ] -> (* we ignore euphony transition *)
      let word = Word.mirror rword (* segment is mirror word *) in 
      let keep = let tags = tagger phase word in 
                 [ roles_of seg word tags :: stack ] in
      comp_rec (seg+1) keep rest
  ] 
;
value print_sols sol = 
  let xmlify_call sol = (* sol in reverse order *) 
    let projections = List.fold_left extract "" sol in
    let invoke = parser_cgi ^ "?" ^ query.val ^ ";p=" ^ projections in
    anchor Green_ invoke heart_sign in do
  { ps html_break
  ; List.iter print_role (List.rev sol)
  ; ps (xmlify_call sol)
  ; ps html_break
  }
;
value monitoring = True (* We show explicitly the penalty vector by default *)
;
value display_penalty p = "Penalty " ^ 
   if monitoring then Constraints.show_penalty p
   else string_of_int (Constraints.eval_penalty p)
;
value print_bucket (p,b_p) = do 
  { ps html_break 
  ; ps (html_green (display_penalty p))
  ; ps html_break
  ; List.iter print_sols b_p
  } 
;
value analyse query output = 
  let tagger = Lex.extract_lemma in 
  let groups = make_groups tagger output in
  let sorted_groups = sort_flatten groups in 
  let (top_groups, threshold) = truncate_groups sorted_groups in do
  { pl (xml_empty "p")
  ; let find_len = fun
      [ [ (_,[ a :: _ ]) :: _ ] -> List.length a
      | _ -> 0
      ] in
    pl (xml_empty_with_att "input" 
           [ ("type","submit"); ("value","Submit"); 
             ("onclick","unique('" ^ parser_cgi ^ "?" ^ query 
             ^ ";p=','" ^ string_of_int (find_len top_groups) ^ "')" )
           ] ^ html_break)
  ; pl (xml_empty "p") 
  ; if scl_toggle then (* Call SCL parser *) 
       Ext.print_ext [ (1,List.rev output) ] 
       else () 
  (*i DEBUG ; Sys.command "ls -l > /tmp/SKT_TEMP/junk" i*)
  ; List.iter print_bucket top_groups  
  ; match threshold with      
    [ None -> ()
    | Some p -> do
       { ps html_break 
       ; ps (html_red ("Truncated penalty " ^ string_of_int p ^ " or more")) 
       ; ps html_break 
       }
    ]
  }
;
value print_sems word morphs = do  
  { ps (span_begin Latin12)
  ; ps "{ "
  ; let bar () = ps " | " 
    and sem = Canon.decode word in 
    List2.process_list_sep (print_sem sem) bar morphs
  ; ps " }" 
  ; ps span_end 
  }
;
value print_out seg_num segment = do 
  (* Contrarily to Reader, we discard phonetic information. *)
  { ps tr_begin
  ; Lex.print_segment_roles print_sems seg_num segment 
  ; ps tr_end 
  ; seg_num+1
  }
;
value rec print_project proj = fun
    [ [] -> match proj with 
            [ [] -> () (* finished, projections exhausted *)
            | _ -> failwith "Too many projections"
            ]
    | [ (phase,rword,_) :: rest ] -> (* sandhi ignored *)
      let new_proj = Lex.print_proj phase rword proj in
      print_project new_proj rest 
    ]
;
exception Truncation (* raised if more solutions than [Web.truncation] *)
;
(* Replay reader until solution index - quick and dirty way to recreate it. *)
(* Follows the infamous exponential [Rank.dove_tail]. *)
value dove_tail_until sol_index init = 
  let init_stack = List.map (fun (_,s) -> s) init (* erasing constraints *) in
  dtrec 1 (0,[],[]) init_stack
  where rec dtrec n kept stack = (* invariant: |stack|=|init|=number of chunks *)
  if n = Web.truncation then raise Truncation
  else if n = sol_index then (* return total output *)
          List.fold_right conc stack [] 
          where conc (o,_) oo = o @ oo 
  else dtrec (n+1) kept (crank [] init stack)
         where rec crank acc ini = fun
         [ [ (_,c) :: cc ] -> match ini with
            [ [ (constraints,i) :: ii ] -> do
              { Lex.Viccheda.set_offset constraints
              ; match Lex.Viccheda.continue c with
                [ Some next -> List2.unstack acc [ next :: cc ]
                | None -> crank [ i :: acc ] ii cc
                ]
              }
            | _ -> raise (Control.Anomaly "dove_tail_until")
            ]
         | [] -> raise Truncation
         ]
;
(* From Interface: splitting checkpoints into current and future ones *)
value split_check limit = split_rec []
   where rec split_rec acc checkpts = match checkpts with
      [ [] -> (List.rev acc,[])
      | [ ((index,_,_) as check) :: rest ] -> 
          if index > limit then (List.rev acc,checkpts)
          else split_rec [ check :: acc ] rest 
      ]
;
value segment_until sol_index chunks cpts = 
   let (_,constrained_segs) = List.fold_left init ((0,cpts),[]) chunks
   where init ((offset,checkpoints),stack) chunk = do
   { let ini_cont = Lex.Viccheda.init_segment chunk in 
     let chunk_length = Word.length chunk in
     let extremity = offset+chunk_length in 
     let (local,future) = split_check extremity checkpoints in
     let chunk_constraints = (offset,local) in
     ((succ extremity,future), do 
        { Lex.Viccheda.set_offset chunk_constraints (* Sets local constraints *)
        ; let res = match Lex.Viccheda.continue ini_cont with
              [ Some c -> c 
              | None -> Lex.un_analyzable chunk
              ] in 
          [ (chunk_constraints,res) :: stack ]
        }) 
   } in
   dove_tail_until sol_index constrained_segs 
;
value stamp = 
  "Heritage" ^ " " ^ Date.version
;
value print_validate_button query = 
  let cgi = parser_cgi ^ "?" ^ query ^ ";validate=t" in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
(* Follows [Reader.process_input] *)
value process_until sol_index query topic mode_sent translit sentence 
                    cpts us encode proj sol_num query do_validate = 
  let pieces = Sanskrit.read_raw_sanskrit encode sentence in
  let romapieces = List.map Canon.uniromcode pieces in
  let romasentence = String.concat " " romapieces in
  let chunker = if us then Sanskrit.read_raw_sanskrit 
                      else Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence in 
  let devachunks = List.map Canon.unidevcode chunks in
  let devasentence = String.concat " " devachunks in do
  { pl html_break
  ; let lex_stamp = "Lexicon: " ^ stamp in 
    ps (html_green lex_stamp) (* in order to keep relation corpus/lexicon *)
  ; pl html_break
  ; pl hr
  ; pl html_break
  ; ps (roma16_red_sl romasentence) (* romanisation *)
  ; pl html_break
  ; ps (deva16_blue devasentence) (* devanagari *)
  ; pl html_break
  ; let all_chunks = match topic with
       [ Some topic -> chunks @ [ code_string topic ]
       | None -> chunks
       ] in
    try let output = segment_until sol_index all_chunks cpts in 
        let solution = List.rev output in do
        { pl html_break
        ; pl (xml_begin_with_att "table" [ noborder; padding10; spacing5 ])
        ; match proj with 
          [ None -> let _ = List.fold_left print_out 1 solution in ()
          | Some triples -> do
            { print_project triples solution
 (*i Validate action: to be restored
    If the cgi called with has [do_validate], we record the tagging in a file,
    otherwise, we print a call back with [do_validate]. 
 -- TODO [; if Paths.platform = "Station" then 
                match do_validate with
                [ "f" -> ps (td_wrap (print_validate_button query 
                             ^ html_green "Validate")) 
                | _ -> Lex.record_tagging us mode_sent translit sol_num
                                          sentence solution triples
                ]
              else ()] i*)
            }
          ]
        ; ps table_end 
        ; match proj with 
          [ None -> analyse query solution 
          | Some p -> ()
	  ]
        }
    with [ Truncation -> do
           { pl (html_red "Solution not found")
           ; pl html_break 
           }
         ]
  }
;

value sort_check cpts = 
  let compare_index (a,_,_) (b,_,_) = compare a b in 
  List.sort compare_index cpts
;

value parser_engine () = do
(* Replays Reader until given solution - dumb but reliable *)
  { Prel.prelude ()
  ; let query = Sys.getenv "QUERY_STRING" in 
    let alist = create_env query in 
    let url_encoded_input = get "text" alist "" 
    and url_encoded_sol_index = get "n" alist "1"
    and url_encoded_topic = get "topic" alist "" 
    and st = get "st" alist "t" 
    and cp = get "cp" alist "t"
    and us = get "us" alist "f"
    and translit = get "t" alist Paths.default_transliteration 
    and lex = get "lex" alist Paths.default_lexicon
    and abs = get "abs" alist "f" (* default local paths *) in
    let lang = language_of lex
    and input = decode_url url_encoded_input (* unnormalized string *)
    and uns = us="t" (* unsandhied vs sandhied corpus *)
    and mode_sent = st="t" (* default sentence mode *)
    and encode = Encode.switch_code translit (* encoding as a normalized word *)
    and () = toggle_lexicon lex
    and () = if abs="t" then remote.val:=True else () (* Web service mode *)
    and () = if st="f" then iterate.val:=False else () (* word stemmer *)
    and () = if cp="f" then complete.val:=False else () (* simplified reader *)
    and sol_index = int_of_string (decode_url url_encoded_sol_index) 
    (* For Validate mode, register number of solutions *)
    and sol_num = int_of_string (get "allSol" alist "0")
    (* Only register this solution if validate is true *)
    and do_validate = get "validate" alist "f" 
    (* Contextual information from past discourse *)
    and topic_mark = decode_url url_encoded_topic in
    let topic = match topic_mark with
        [ "m" -> Some "sa.h"
        | "f" -> Some "saa"
        | "n" -> Some "tat"
        | _ -> None
        ] 
    (* File where to store locally the taggings - only for [Station] platform *)
    and corpus_file = (* optionally transmitted by argument "out_file" *)
        try let file_name = List.assoc "out_file" alist (* do not use get *) in 
            Some file_name  
        with [ Not_found -> Some regression_file_name ] in
(* Regression disabled
   [let () = if Paths.platform = "Station" then match corpus_file with 
                [ Some file_name -> 
                   let regression_file = var_dir ^ file_name ^ ".txt" in 
                   output_channel.val := open_out_gen 
                     [ Open_wronly; Open_append; Open_creat; Open_text ] 
                     0o777 regression_file
                | None -> ()
                ]
             else () in] *) 
    let proj = (* checks for parsing mode *)
        try let url_encoded_proj = List.assoc "p" alist in (* do not use get *) 
            Some (parse_proj (decode_url url_encoded_proj))
        with [ Not_found -> do 
                 { set_query query (* query regurgitated - horror *)
                 ; None
                 } 
             ] 
    and checkpoints = (* checkpoints for graph *)
       try let url_encoded_cpts = List.assoc "cpts" alist in (* do not use get *)
           parse_cpts (decode_url url_encoded_cpts)
       with [ Not_found -> [] ] in     
    let cpts = sort_check checkpoints in 
    try do
      { process_until sol_index query topic mode_sent translit input 
                      cpts uns encode proj sol_num query do_validate
      ; close_page_with_margin ()
      ; let bandeau = not (Gen.active proj) in
        page_end lang bandeau
      }
    with [ Stream.Error _ -> abort lang "Illegal transliteration " input ]
 }
; 
value safe_engine () =
  let abor = abort default_language in
  try parser_engine () with 
  [ Sys_error s           -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s        -> abor Control.stream_err_mess s (* file pb *)
  | Encode.In_error s     -> abor "Wrong input " s
  | Exit (* Sanskrit *)   -> abor "Wrong character in input - " "use ASCII" 
  | Invalid_argument s    -> abor Control.fatal_err_mess s (* sub *)
  | Failure s             -> abor Control.fatal_err_mess s (* anomaly *)
  | End_of_file           -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | Not_found (* assoc *) -> let s = "You must choose a parsing option" in
                             abor "Unset button in form - " s
  | Control.Fatal s       -> abor Control.fatal_err_mess s (* anomaly *)
  | Control.Anomaly s     -> abor Control.fatal_err_mess ("Anomaly: " ^ s)
  | _                     -> abor Control.fatal_err_mess "Unexpected anomaly" 
  ] 
;
safe_engine () (* Should always produce a valid HTML page *)
;

(*i end; i*)
