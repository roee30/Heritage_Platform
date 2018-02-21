(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Pawan Goyal & Gérard Huet                       *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Regression analysis - Legacy *)

(*i module Regression = struct i*)

(* Reads from stdin a previous regression file.
   For every line, reads in parsing parameters,
   parses with the current reader, prints a new trace file. *)

open Encode; (* [code_string] *)
open Constraints; (* [extract truncate_groups] *)
open Rank; (* Lex [morpho parse_solution parse_metadata segment_all complete] *)

module Prel = struct
 value prelude () = ()
;
 end (* Prel *)
;

(* TODO : Validate mode ought to store these parameters in metadata line *)
value topic = None
;

(* Adapted from Parser *)
(* Parsing projections stream in tagging mode *)
open Bank_lexer; 
module Gram = Camlp4.PreCast.MakeGram Bank_lexer 
;
open Bank_lexer.Token
;

value projs    = Gram.Entry.mk "projs"
and lproj      = Gram.Entry.mk "lproj"
and proj       = Gram.Entry.mk "proj"
and solution   = Gram.Entry.mk "solution"
and modec      = Gram.Entry.mk "modec"
and modes      = Gram.Entry.mk "modes"
and mode_sent  = Gram.Entry.mk "mode_sent"
and mode_trans = Gram.Entry.mk "mode_trans"
and quad       = Gram.Entry.mk "quad"
and max_sol    = Gram.Entry.mk "max_sol"
and sentence   = Gram.Entry.mk "sentence"
and out_phases = Gram.Entry.mk "out_phases"
and out_phase  = Gram.Entry.mk "out_phase"
and reg_metadata  = Gram.Entry.mk "reg_metadata"
;
(* A stream of projections is encoded under the form [1,2|2,3|...] *)
(* Extends the Parser grammar in specifying the validation format. *)
EXTEND Gram
  projs:
    [ [ l = lproj; `EOI -> l
      | lproj -> failwith "Wrong projections parsing\n"
    ] ];
  lproj:
    [ [ l = LIST0 proj SEP "|" -> l ] ];
  proj:
    [ [ n = INT; ","; m = INT -> (int_of_string n,int_of_string m) ] ];
  solution:
    [ [ mc = modec; ms = modes; mst = mode_sent; mt = mode_trans; s = sentence; sol = max_sol; o = out_phases -> 
        (mc,ms,mst,mt,s,sol,o) ] ];
  reg_metadata:
    [ [ v = modec; f = modes; n = sentence -> (v,f,n) ] ] ;
  modec:
    [ [ "["; t = TEXT; "]" -> t ] ];
  modes:
    [ [ "<"; t = TEXT; ">" -> t ] ];
  mode_sent:
    [ [ "|"; t = TEXT; "|" -> t ] ];
  mode_trans:
    [ [ "#"; t=TEXT; "#" -> t ] ];
  sentence:
    [ [ "("; t = TEXT; ")" -> t ] ];
  quad:
    [ [ "["; k = INT; ","; l = INT; ","; m = INT; "," ; n = INT; "]" -> 
        (int_of_string k,int_of_string l,int_of_string m,int_of_string n) ] ];
  max_sol:
    [ [ "["; k = INT; "]" -> (int_of_string k) ] ];
  out_phases:
    [ [ c = LIST0 out_phase SEP "&" -> c ] ];
  out_phase:
    [ [ "$"; t = TEXT; "$" -> t ] ];
END
;
value parse_fail s loc e = do
  { Format.eprintf "Wrong input: %s\n, at location %a:@." s Loc.print loc 
  ; raise e
  } 
;
value parse_phase s =
  try Gram.parse_string out_phases Loc.ghost s with
  [ Loc.Exc_located loc e -> parse_fail s loc e
  ]
;
value parse_metadata s =
  try Gram.parse_string reg_metadata Loc.ghost s with
  [ Loc.Exc_located loc e -> parse_fail s loc e
  ]
;
value parse_proj s =
  try Gram.parse_string projs Loc.ghost s with
  [ Loc.Exc_located loc e -> parse_fail s loc e
  ]
;
value parse_solution s =
  try Gram.parse_string solution Loc.ghost (String.sub s 0 ((String.length s)-1))
  with
  [ Loc.Exc_located loc e -> parse_fail s loc e
  ]
;
value check_tags current_sol_string tagging = 
  let pos = (String.length current_sol_string)-1 in
  let oc = parse_phase (String.sub current_sol_string 0 pos) in
  oc = tagging
;
value look_up_tags solution output tagging sol = 
  let proj = List.fold_left extract "" sol in
  let p = parse_proj proj in 
  let current_sol_string = 
     Lex.return_tagging (List.rev output) (List.rev p) in 
  if check_tags current_sol_string tagging then Some solution 
  else None
;
value search_bucket solution output tagging (p,b_p) = 
  let watch_verify max = watch_rec 0
    where rec watch_rec n =
      if n=max then None 
      else match (look_up_tags solution output tagging (List.nth b_p n))
           with [ None -> watch_rec (n+1)
                | s -> s
                ] in
  watch_verify (List.length b_p)
;
value find_proj solution output tagging sorted_groups = 
  let (top_groups, _) = truncate_groups sorted_groups in
  let watch_verify max = watch_rec 0
    where rec watch_rec n =
      if n=max then None
      else let gr = List.nth top_groups n in
           match search_bucket solution output tagging gr with
           [ None -> watch_rec (n+1)
           | s -> s 
           ] in
  watch_verify (List.length top_groups)
; 
value analyse tagging (solution,output) = 
  let tagger = Lex.extract_lemma in 
  let groups = make_groups tagger output in
  let sorted_groups = sort_flatten groups in 
  find_proj solution output tagging sorted_groups 
;

(* [analyse_results] will look for a solution consistent with taggings;
   If so will return Some(n,ind,kept,max) as given to [print_output] 
   in mode Validate, otherwise returns None. *)
value analyse_results limit taggings = fun
  [ [] -> None
  | best_sols -> 
    let kept = List.length best_sols
    and max = match limit with
              [ Some m -> m
              | None -> Web.truncation
              ] in 
    let watch_verify maxim = watch_rec 0
	where rec watch_rec n = 
          if n = maxim then None
          else match (analyse taggings (List.nth best_sols n)) with
               [ None -> watch_rec (n+1)
               | Some sol_number -> Some max
	       ] in
    watch_verify kept
  ]
;
value verify_sentence filter_mode us topic sentence encode taggings =
  let chunker = if us (* sandhi undone *) then Sanskrit.read_raw_sanskrit 
                else (* blanks non-significant *) Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence (* normalisation here *) in 
  let all_chunks = match topic with
       [ Some topic -> chunks @ [ code_string topic ]
       | None -> chunks
       ] in
   try segment_all filter_mode all_chunks [] with
      [ Solutions limit revsols saved -> 
         let sols = List.rev revsols in 
         analyse_results limit taggings sols
      ]
;
value pdiff sol modec modes mode_sent mode_trans sentence psol tagging cho chd = 
  let report = output_string cho 
  and prdiff = output_string chd 
  and modes_report = "[{" ^ modec ^ "}] <{" ^ modes ^ "}> |{" 
                     ^ mode_sent ^ "}| #{" ^ mode_trans ^ "}# " in do
  { report (modes_report ^ "({" ^ sentence ^ "}) ")
  ; match sol with
    [ Some max -> do  
       { report ("[" ^ (string_of_int max) ^ "]")
       ; match psol with
	 [ 0 -> 
             prdiff (sentence ^ " " ^ modec ^ " " ^ modes ^ " [parses now]\n")
	 | max1 -> let diff = (max1-max) in
	           match diff with
                   [ 0 -> ()
	           | d -> prdiff (sentence ^ " " ^ modec ^ " " ^ modes ^ 
                                  " changes [" ^ (string_of_int d) ^ "]\n")
	           ]
	 ]
       }
    | None -> do
       { report ("[0]")
       ; match psol with
	 [ 0 -> () (* It didn't parse before, so no need to report *)
	 | _ -> 
           prdiff (sentence ^ " " ^ modec ^ " " ^ modes ^ " [does not parse]\n")
	 ]
       }
    ]
  ; report (" " ^ print_tag tagging ^ "\n")
      where rec print_tag = fun
      [ [ a :: rest ] -> "${" ^ a ^ "}$&" ^ (print_tag rest)
      | [] -> ""
      ]
  }
;
value regression s cho chd =  
  let (mc,ms,mst,mt,sc,solc,oc) = parse_solution s in
  let _ = complete.val:= (mc = "C") 
  and _ = iterate.val:= (mst = "Sent")
  and us = (ms = "F") in
  let solr = verify_sentence True us topic sc (switch_code mt) oc in 
  pdiff solr mc ms mst mt sc solc oc cho chd
;

value get_metadata input_info = 
 let (_, filename, _) = parse_metadata input_info 
 and version = Date.version_id in
 "[{" ^ version ^ "}] <{" ^ filename ^ "}> ({" ^ Version.version_date ^ "})"
;

value main_loop ic = 
  let use_metadata = input_line ic 
  and input_info = input_line ic 
  and version = Date.version_id 
  and date = Date.date_iso in
  let (old_version,filename,old_date) = parse_metadata input_info in
  let cho = open_out_gen [ Open_wronly; Open_trunc; Open_creat; Open_text ] 
            0o777 (Web.var_dir ^ "/" ^ filename ^ "-" ^ 
            version ^ "-" ^ date ^ ".txt") 
	and chd = open_out_gen [ Open_wronly; Open_trunc; Open_creat; Open_text ]
            0o777 (Web.var_dir ^ "/diff-" ^ filename ^ ".txt") in 
  let report_meta = output_string cho 
  and report_version = output_string chd in do
  { report_meta (use_metadata ^ "\n" ^ (get_metadata input_info) ^ "\n")
  ; let diff_meta = "diff: file = " ^ filename ^ " from Version " ^ old_version 
        ^ "." ^ old_date ^ " to " ^ version ^ "." ^ date ^ "\n" in
    report_version diff_meta
  ; try read_from_ic ic
        where rec read_from_ic ic = 
          let s = input_line ic in 
          do { regression s cho chd; read_from_ic ic }
    with [ End_of_file -> do { close_out cho; close_out chd } ] 
  }
;

(* Now regression reads on stdin - no need of unsafe file opening *)
try main_loop stdin with
  [ Sys_error m -> print_string ("Sys_error " ^ m) ]
;

(* Regression reads from stdin, first two metadata lines,
then for each line extracts parameters: mode (S or C),  sandhied or not (T or F),
sentence or word (Sent or Word), then the transliteration code (VH etc),
then the sentence as entered, then the quadruple (n,ind,kept,max), then the 
taggings previously computed in mode Validate.

It then calls: [verify_sentence] with appropriate parameters, 
which will either fail (returning None), or succeed with Some(n',ind',kept',max')
Finally it writes a new version of the input in local directory in file 
   [filename-Version.version-Version.version_date.txt]
where filename is a parameter read in the metadata second line.
It also prints a diff message in file [Web.var_dir/diff-filename.txt].
*)
(* Format of metada information:
1st line gives metadata about the corpus (unalysed)
2nd line gives global parameters [version, filename, version_date].

Example of current input format:
[
%%% Corpus from regression.txt %%% 
[{263}] <{regression}> ({2012-04-04})
[{S}] <{T}> |{Sent}| #{VH}# ({devaa~nch.r.noti}) [1] ${devaan:Noun2{ acc. pl. m. }[deva]}$&${z.r.noti:Root{ pr. [5] ac. sg. 3 }[zru]}$&
[{C}] <{T}> |{Sent}| #{VH}# ({devaa~nch.r.noti}) [1] ${devaan:Noun{ acc. pl. m. }[deva]}$&${z.r.noti:Root{ pr. [5] ac. sg. 3 }[zru]}$&
] *)

(*i end; i*)
