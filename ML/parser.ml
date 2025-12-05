(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* CGI-bin callback for shallow syntax analysis *)
(* Parser is similar to Reader, but it is invoked from the green hearts
in the output of the reader, in order to give the semantic analysis of
a specific solution. It basically replays reading until this specific solution *)
(*i module Parser = struct i*)
open Encode
  
open Canon
  
open Html
  
open Web
  
(* ps pl abort truncation etc. [remote_server_host] *)
open Cgi
  
(* get *)
open Checkpoints
  
open Phases.Phases
  
(* generative *)
open Scl_parser
  
(* Interface with UoH dependency parser *)
module Prel =
  struct
    (* Parser's lexer prelude *)
    (* prelude is executed by Lexer when initialisation of transducers fails *)
    let prelude () =
      (pl http_header;
       page_begin parser_meta_title;
       pl (body_begin Chamois_back);
       if scl_toggle
       then (* external call SCL (experimental) *)
         pl (javascript (SCLpaths.scl_url ^ javascript_tooltip))
       else ();
       pl parser_title;
       open_page_with_margin 15)
      
  end
  
(* Prel *)
open Load_transducers
  
(* [transducer_vect dummy_transducer_vect Trans] *)
module Lexer_control =
  struct
    let star = ref true
      
    let transducers_ref = ref (dummy_transducer_vect : transducer_vect)
      
  end
  
(* [Lexer_control] *)
module Transducers = Trans(Prel)
  
module Lex = Lexer.Lexer(Prel)(Lexer_control)
  
(* [print_proj print_segment_roles print_ext_segment extract_lemma] *)
(* Printing functions *)
let table_labels = table_begin (background Pink)
  
let print_labels tags seg_num =
  (ps th_begin;
   (* begin labels *)
   pl table_labels;
   (let print_label n _ =
      (ps
         (cell
            (html_red ((string_of_int seg_num) ^ ("." ^ (string_of_int n)))));
       n + 1) in
    let _ = List.fold_left print_label 1 tags in ());
   ps table_end;
   ps th_end)
  
(* end labels *)
let rec color_of_role =
  function
  | (* Semantic role of lexical category *)
      Pv | Pvkc | Pvkv | Iic | Iik | Voca | Inv | Iicv | Iicc | Iikv | Iikc |
        Iiif | A | An | Vok | Vokv | Vokc | Vocv | Vocc | Vocf | Iiy | Iiv |
        Iivv | Iivc | Peri | Auxiick | Pvv | Pvc
      -> Grey
  | Noun | Nouv | Nouc | Krid | Kriv | Kric | Ifc | Ifcv | Ifcc | Pron | Kama
      | Lopak | Auxik -> Cyan
  | (* Actor or Predicate *) Root | Lopa | Auxi -> Pink
  | (* abs-tvaa in Inde *) (* Process *)
      Abso | Absv | Absc | Inde | Avy | Ai | Ani | Inftu | Auxiinv |
        (* Adverb *) Indifc
      -> Lavender
  | Unknown | Cache | Cachei -> Grey
  | Comp ((_, ph), _, _) -> color_of_role ph
and table_role_of phase = table_begin (background (color_of_role phase))
  
(* syntactico/semantical roles analysis, function of declension *)
let print_roles pr_sem phase tags form =
  (ps th_begin;
   pl (table_role_of phase);
   (let pr_roles (delta, sems) =
      (ps tr_begin;
       ps th_begin;
       (let word = Word.patch delta form in pr_sem word sems);
       ps th_end;
       ps tr_end)
    in List.iter pr_roles tags);
   ps table_end;
   ps th_end)
  
(* Segment printing without phonetics with semantics for Parser *)
let print_segment_roles print_sems seg_num (phase, rword, _) =
  let word = Word.mirror rword
  in
    (Morpho_html.print_signifiant_yellow rword;
     let (decl_phase, form, decl_tags) =
       match Lex.tags_of phase word with
       | Atomic tags -> Lex.process_kridanta [] seg_num phase word tags
       | Preverbed ((_, phase), pvs, form, tags) ->
           Lex.process_kridanta pvs seg_num phase form tags
     in
       (print_labels decl_tags seg_num;
        print_roles print_sems decl_phase decl_tags form))
  
let project n list = List.nth list (n - 1)
  
(* Ocaml's nth starts at 0 *)
let print_uni_kridanta pvs phase word multitags (n, m) =
  let (delta, polytag) = project n multitags in
  let unitag = [ project m polytag ]
  in
    (th_begin |> ps;
     (Lex.table_morph_of phase) |> pl;
     (* table of color of phase begins *)
     (let _ = (* print unique tagging *)
        Lex.print_morph pvs false 0 (generative phase) word 0 (delta, unitag)
      in ());
     table_end |> ps;
     (* table of color of phase ends *)
     th_end |> ps)
  
let print_projection phase rword index =
  (tr_begin |> ps;
   (* tr begins *)
   Morpho_html.print_signifiant_yellow rword;
   (let word = Word.mirror rword
    in
      match Lex.tags_of phase word with
      | Atomic tags -> print_uni_kridanta [] phase word tags index
      | Preverbed ((_, phase), pvs, form, tags) ->
          (* we trim out lemmas inconsistent with preverb assignment to form *)
          let trim = Lex.trim_tags (generative phase) form (Canon.decode pvs)
          in print_uni_kridanta pvs phase form (trim tags) index);
   tr_end |> ps)
  
(* tr ends *)
let print_proj phase rword =
  function
  | [] -> failwith "Projection missing"
  | n_m :: rest -> (print_projection phase rword n_m; rest)
  
(* returns the rest of projections stream *)
(* End Printing functions *)
let rpc = remote_server_host
and remote = ref false
  
(* local invocation of cgi by default *)
open Skt_morph
  
open Inflected
  
open Constraints
  
(* [roles_of sort_flatten extract] *)
open Paraphrase
  
(* [display_penalties print_sem print_role] *)
let query = ref ""
  
(* ugly - stores the query string *)
let set_query q = query := q
  
(* [Parser.parser_engine] *)
(* Duplicated from Rank *)
let make_groups tagger =
  let rec comp_rec seg stack =
    function
    | (* going forward in time *) [] -> stack
    | (* result goes backward in time *) (phase, rword, _) :: rest ->
        (* we ignore euphony transition *)
        let word = Word.mirror rword in (* segment is mirror word *)
        let lemma = tagger phase word in
        let keep = (roles_of seg word lemma) :: stack
        in comp_rec (seg + 1) keep rest
  in comp_rec 1 []
  
let print_sols sol =
  let xmlify_call sol = (* sol in reverse order *)
    let projections = List.fold_left extract "" sol in
    let invoke = parser_cgi ^ ("?" ^ (!query ^ (";p=" ^ projections)))
    in anchor Green_ invoke heart_sign
  in
    (ps html_break;
     List.iter print_role (List.rev sol);
     ps (xmlify_call sol);
     ps html_break)
  
let monitoring = true
  
(* We show explicitly the penalty vector by default *)
let display_penalty p =
  "Penalty " ^
    (if monitoring
     then Constraints.show_penalty p
     else string_of_int (Constraints.eval_penalty p))
  
let print_bucket (p, b_p) =
  (ps html_break;
   ps (html_green (display_penalty p));
   ps html_break;
   List.iter print_sols b_p)
  
let analyse query output =
  let tagger = Lex.extract_lemma in
  let groups = make_groups tagger output in
  let sorted_groups = sort_flatten groups in
  let (top_groups, threshold) = truncate_groups sorted_groups
  in
    (pl (xml_empty "p");
     (let find_len = function | (_, a :: _) :: _ -> List.length a | _ -> 0
      in
        pl
          ((xml_empty_with_att "input" (* Final call to Parser for display *)
              [ ("type", "submit"); ("value", "Submit");
                ("onclick",
                 ("unique('" ^
                    (parser_cgi ^
                       ("?" ^
                          (query ^
                             (";p=','" ^
                                ((string_of_int (find_len top_groups)) ^ "')"))))))) ])
             ^ html_break));
     pl (xml_empty "p");
     if scl_toggle
     then (* Call SCL parser *)
       (let segments = List.map (fun (ph, w, _) -> (ph, w)) output in
        let scl_font =
          match !sanskrit_font with | Deva -> "DEV" | Roma -> "IAST"
        in Scl_parser.print_scl scl_font [ List.rev segments ])
     else ();
     (*i DEBUG ; Sys.command "ls -l > /tmp/SKT_TEMP/junk" i*)
     List.iter print_bucket top_groups;
     match threshold with
     | None -> ()
     | Some p ->
         (html_break |> ps;
          (html_red ("Truncated penalty " ^ ((string_of_int p) ^ " or more")))
            |> ps;
          html_break |> ps))
  
let print_sems word morphs =
  ((span_begin Latin12) |> ps;
   "{ " |> ps;
   (let bar () = " | " |> ps
    and sem = Canon.decode word
    in List2.process_list_sep (print_sem sem) bar morphs);
   " }" |> ps;
   span_end |> ps)
  
let print_out seg_num segment =
  ((* Contrarily to Reader, we discard phonetic information. *)
   tr_begin |> ps;
   print_segment_roles print_sems seg_num segment;
   tr_end |> ps;
   seg_num + 1)
  
let rec print_project proj =
  function
  | [] ->
      (match proj with
       | [] -> ()
       | (* finished, projections exhausted *) _ ->
           failwith "Too many projections")
  | (phase, rword, _) :: rest -> (* sandhi ignored *)
      let new_proj = print_proj phase rword proj
      in print_project new_proj rest
  
exception Truncation
  
(* raised if more solutions than [Web.truncation] *)
(* Replay reader until solution index - quick and dirty way to recreate it. *)
(* Follows the infamous exponential [Rank.dove_tail]. *)
let dove_tail_until sol_index init =
  let init_stack = List.map (fun (_, s) -> s) init in
  (* erasing constraints *)
  let rec dtrec n kept stack =
    (* invariant: |stack|=|init|=number of chunks *)
    if n = Web.truncation
    then raise Truncation
    else
      if n = sol_index
      then (* return total output *)
        (let conc (o, _) oo = o @ oo in List.fold_right conc stack [])
      else
        (let rec crank acc ini =
           function
           | (_, c) :: cc ->
               (match ini with
                | (constraints, i) :: ii ->
                    (Lex.Viccheda.set_offset constraints;
                     (match Lex.Viccheda.continue c with
                      | Some next -> List2.unstack acc (next :: cc)
                      | None -> crank (i :: acc) ii cc))
                | _ -> raise (Control.Anomaly "dove_tail_until"))
           | [] -> raise Truncation
         in dtrec (n + 1) kept (crank [] init stack))
  in dtrec 1 (0, [], []) init_stack
  
(* Following two functions are same as in Rank *)
(* Splitting checkpoints into current and future ones *)
let split_check limit =
  let rec split_rec acc checkpts =
    match checkpts with
    | [] -> ((List.rev acc), [])
    | (((index, _, _) as check)) :: rest ->
        if index > limit
        then ((List.rev acc), checkpts)
        else split_rec (check :: acc) rest
  in split_rec []
  
let segment_chunk ((offset, checkpoints), stack) chunk sa_check =
  let ini_cont = Lex.Viccheda.init_segment chunk in
  let chunk_length = Word.length chunk in
  let extremity = offset + chunk_length in
  let (local, future) = split_check extremity checkpoints in
  let chunk_constraints = (offset, local)
  in
    (((succ extremity), future),
     (Lex.Viccheda.set_offset chunk_constraints;
      (* Sets local constraints *)
      Lex.Viccheda.set_sa_control sa_check;
      (* inherit from chunks recursion *)
      let res =
        match Lex.Viccheda.continue ini_cont with
        | Some c -> c
        | None -> Lex.un_analyzable chunk
      in (chunk_constraints, res) :: stack))
  
(* Follows logic of [Rank.segment_all] until solution reached *)
let segment_until sol_index chunks cpts =
  let (_, constrained_segs) =
    let rec segment_chunks acc =
      function
      | (* last *) [ chunk ] -> segment_chunk acc chunk false
      | chunk :: rest ->
          let sa_check = Phonetics.consonant_starts rest
          in segment_chunks (segment_chunk acc chunk sa_check) rest
      | [] -> acc
    in segment_chunks ((0, cpts), []) chunks
  in dove_tail_until sol_index constrained_segs
  
(* Printing stuff *)
let stamp = "Heritage" ^ (" " ^ Date.version)
  
let print_validate_button query =
  let cgi = parser_cgi ^ ("?" ^ (query ^ ";validate=t")) in
  let invocation = if !remote then rpc ^ cgi else cgi
  in anchor Green_ invocation check_sign
  
(* Follows [Reader.process_input] *)
let process_until sol_index query topic mode_sent translit sentence cpts us
                  encode proj sol_num query do_validate =
  let pieces = Sanskrit.read_raw_sanskrit encode sentence in
  let romapieces = List.map Canon.uniromcode pieces in
  let romasentence = String.concat " " romapieces in
  let chunker =
    if us then Sanskrit.read_raw_sanskrit else Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence in
  let devachunks = List.map Canon.unidevcode chunks in
  let devasentence = String.concat " " devachunks
  in
    (pl html_break;
     (let lex_stamp = "Lexicon: " ^ stamp in ps (html_green lex_stamp));
     (* in order to keep relation corpus/lexicon *)
     pl html_break;
     pl hr;
     pl html_break;
     ps (roma16_red_sl romasentence);
     (* romanisation *)
     pl html_break;
     ps (deva16_blue devasentence);
     (* devanagari *)
     pl html_break;
     let all_chunks =
       match topic with
       | Some topic -> chunks @ [ code_string topic ]
       | None -> chunks
     in
       try
         let output = segment_until sol_index all_chunks cpts in
         let solution = List.rev output
         in
           (pl html_break;
            pl (xml_begin_with_att "table" [ noborder; padding10; spacing5 ]);
            (match proj with
             | None -> let _ = List.fold_left print_out 1 solution in ()
             | Some triples -> print_project triples solution);
            ps table_end;
            match proj with | None -> analyse query solution | Some p -> ())
       with | Truncation -> pl ((html_red "Solution not found") ^ html_break))
  
let sort_check cpts =
  let compare_index (a, _, _) (b, _, _) = compare a b
  in List.sort compare_index cpts
  
let parser_engine () =
  ((* Replays Reader until given solution - dumb but reliable *)
   Prel.prelude ();
   let query = Sys.getenv "QUERY_STRING" in
   let env = create_env query in
   let url_encoded_input = get "text" env ""
   and url_encoded_sol_index = get "n" env "1"
   and url_encoded_topic = get "topic" env "" and st = get "st" env "t"
   and us = get "us" env "f"
   and translit = get "t" env Paths.default_transliteration
   and lex = get "lex" env Paths.default_lexicon
   and font = get "font" env Paths.default_display_font in
   let ft = font_of_string font in (* Deva vs Roma print *)
   let () = toggle_sanskrit_font ft and abs = get "abs" env "f" in
   (* default local paths *)
   let lang = language_of_string lex and input = decode_url url_encoded_input
   and (* unnormalized string *) uns = us = "t"
   and (* unsandhied vs sandhied corpus *) mode_sent = st = "t"
   and (* default sentence mode *) encode = Encode.switch_code translit
   and (* encoding as a normalized word *) () = toggle_lexicon lex
   and () = if abs = "t" then remote := true else ()
   and (* Web service mode *) () =
     if st = "f" then Lexer_control.star := false else ()
   and (* word stemmer *) () =
     Lexer_control.transducers_ref := Transducers.mk_transducers ()
   and sol_index = int_of_string (decode_url url_encoded_sol_index)
   and (* For Validate mode, register number of solutions *) sol_num 
     = int_of_string (get "allSol" env "0")
   and (* Only register this solution if validate is true *) do_validate 
     = get "validate" env "f"
   and (* Contextual information from past discourse *) topic_mark =
     decode_url url_encoded_topic in
   let topic =
     match topic_mark with
     | "m" -> Some "sa.h"
     | "f" -> Some "saa"
     | "n" -> Some "tat"
     | _ -> None in
   let proj = (* checks for parsing mode or final unique tags listing *)
     try
       let url_encoded_proj = List.assoc "p" env
       in
         (* do not use get *)
         Some (parse_proj (decode_url url_encoded_proj))
     with
     | Not_found -> (set_query query; (* query regurgitated - horror *) None)
   and checkpoints = (* checkpoints for graph *)
     try
       let url_encoded_cpts = List.assoc "cpts" env
       in (* do not use get *) parse_cpts (decode_url url_encoded_cpts)
     with | Not_found -> [] in
   let cpts = sort_check checkpoints
   in
     try
       (process_until sol_index query topic mode_sent translit input cpts uns
          encode proj sol_num query do_validate;
        close_page_with_margin ();
        let bandeau = not (Gen.active proj) in page_end lang bandeau)
     with | Stream.Error _ -> abort lang "Illegal transliteration " input)
  
let safe_engine () =
  let abor = abort default_language
  in
    try parser_engine ()
    with | Sys_error s -> abor Control.sys_err_mess s
    | (* file pb *) Stream.Error s -> abor Control.stream_err_mess s
    | (* file pb *) Encode.In_error s -> abor "Wrong input " s
    | Exit -> (* Sanskrit *) abor "Wrong character in input - " "use ASCII"
    | Invalid_argument s -> abor Control.fatal_err_mess s
    | (* sub *) Failure s -> abor Control.fatal_err_mess s
    | (* anomaly *) End_of_file -> abor Control.fatal_err_mess "EOF"
    | (* EOF *) Not_found -> (* assoc *)
        let s = "You must choose a parsing option"
        in abor "Unset button in form - " s
    | Control.Fatal s -> abor Control.fatal_err_mess s
    | (* anomaly *) Control.Anomaly s ->
        abor Control.fatal_err_mess ("Anomaly: " ^ s)
    | _ -> abor Control.fatal_err_mess "Unexpected anomaly"
  
let _ = safe_engine ()
  

