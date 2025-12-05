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
open Encode
  
(* [code_string] *)
open Constraints
  
(* [extract truncate_groups] *)
open Rank
  
(* Lex [morpho parse_solution parse_metadata segment_all complete] *)
module Prel = struct let prelude () = ()
                        end
  
(* Prel *)
(* TODO : Validate mode ought to store these parameters in metadata line *)
let topic = None
  
(* Adapted from Parser *)
(* Parsing projections stream in tagging mode *)
open Bank_lexer
  
module Gram = Camlp4.PreCast.MakeGram(Bank_lexer)
  
open Bank_lexer.Token
  
let projs = Gram.Entry.mk "projs"
and lproj = Gram.Entry.mk "lproj"
and proj = Gram.Entry.mk "proj"
and solution = Gram.Entry.mk "solution"
and modec = Gram.Entry.mk "modec"
and modes = Gram.Entry.mk "modes"
and mode_sent = Gram.Entry.mk "mode_sent"
and mode_trans = Gram.Entry.mk "mode_trans"
and quad = Gram.Entry.mk "quad"
and max_sol = Gram.Entry.mk "max_sol"
and sentence = Gram.Entry.mk "sentence"
and out_phases = Gram.Entry.mk "out_phases"
and out_phase = Gram.Entry.mk "out_phase"
and reg_metadata = Gram.Entry.mk "reg_metadata"
  
(* A stream of projections is encoded under the form [1,2|2,3|...] *)
(* Extends the Parser grammar in specifying the validation format. *)
let _ =
  (Gram.extend (projs : 'projs Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (lproj : 'lproj Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) ->
                      (failwith "Wrong projections parsing\n" : 'projs))));
               ([ Gram.Snterm (Gram.Entry.obj (lproj : 'lproj Gram.Entry.t));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (l : 'lproj)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (l : 'projs)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (lproj : 'lproj Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0sep
                    ((Gram.Snterm
                        (Gram.Entry.obj (proj : 'proj Gram.Entry.t))),
                    (Gram.Skeyword "|")) ],
                (Gram.Action.mk
                   (fun (l : 'proj list) (_loc : Gram.Loc.t) -> (l : 'lproj)))) ]) ]))
        ());
   Gram.extend (proj : 'proj Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (m : Gram.Token.t) _ (n : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      (let m = Gram.Token.extract_string m in
                       let n = Gram.Token.extract_string n
                       in ((int_of_string n), (int_of_string m)) : 'proj)))) ]) ]))
        ());
   Gram.extend (solution : 'solution Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (modec : 'modec Gram.Entry.t));
                  Gram.Snterm (Gram.Entry.obj (modes : 'modes Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (mode_sent : 'mode_sent Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (mode_trans : 'mode_trans Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (sentence : 'sentence Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (max_sol : 'max_sol Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (out_phases : 'out_phases Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (o : 'out_phases) (sol : 'max_sol) (s : 'sentence)
                      (mt : 'mode_trans) (mst : 'mode_sent) (ms : 'modes)
                      (mc : 'modec) (_loc : Gram.Loc.t) ->
                      ((mc, ms, mst, mt, s, sol, o) : 'solution)))) ]) ]))
        ());
   Gram.extend (reg_metadata : 'reg_metadata Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (modec : 'modec Gram.Entry.t));
                  Gram.Snterm (Gram.Entry.obj (modes : 'modes Gram.Entry.t));
                  Gram.Snterm
                    (Gram.Entry.obj (sentence : 'sentence Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (n : 'sentence) (f : 'modes) (v : 'modec)
                      (_loc : Gram.Loc.t) -> ((v, f, n) : 'reg_metadata)))) ]) ]))
        ());
   Gram.extend (modec : 'modec Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "[";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword "]" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t : 'modec)))) ]) ]))
        ());
   Gram.extend (modes : 'modes Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "<";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword ">" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t : 'modes)))) ]) ]))
        ());
   Gram.extend (mode_sent : 'mode_sent Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword "|" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t : 'mode_sent)))) ]) ]))
        ());
   Gram.extend (mode_trans : 'mode_trans Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword "#" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t :
                        'mode_trans)))) ]) ]))
        ());
   Gram.extend (sentence : 'sentence Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "(";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword ")" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t : 'sentence)))) ]) ]))
        ());
   Gram.extend (quad : 'quad Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "[";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword "]" ],
                (Gram.Action.mk
                   (fun _ (n : Gram.Token.t) _ (m : Gram.Token.t) _
                      (l : Gram.Token.t) _ (k : Gram.Token.t) _
                      (_loc : Gram.Loc.t) ->
                      (let n = Gram.Token.extract_string n in
                       let m = Gram.Token.extract_string m in
                       let l = Gram.Token.extract_string l in
                       let k = Gram.Token.extract_string k
                       in
                         ((int_of_string k), (int_of_string l),
                          (int_of_string m), (int_of_string n)) :
                        'quad)))) ]) ]))
        ());
   Gram.extend (max_sol : 'max_sol Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "[";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword "]" ],
                (Gram.Action.mk
                   (fun _ (k : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let k = Gram.Token.extract_string k in int_of_string k :
                        'max_sol)))) ]) ]))
        ());
   Gram.extend (out_phases : 'out_phases Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0sep
                    ((Gram.Snterm
                        (Gram.Entry.obj (out_phase : 'out_phase Gram.Entry.t))),
                    (Gram.Skeyword "&")) ],
                (Gram.Action.mk
                   (fun (c : 'out_phase list) (_loc : Gram.Loc.t) ->
                      (c : 'out_phases)))) ]) ]))
        ());
   Gram.extend (out_phase : 'out_phase Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Skeyword "$";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (t : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let t = Gram.Token.extract_string t in t : 'out_phase)))) ]) ]))
        ()))
  
let parse_fail s loc e =
  (Format.eprintf "Wrong input: %s\n, at location %a:@." s Loc.print loc;
   raise e)
  
let parse_phase s =
  try Gram.parse_string out_phases Loc.ghost s
  with | Loc.Exc_located (loc, e) -> parse_fail s loc e
  
let parse_metadata s =
  try Gram.parse_string reg_metadata Loc.ghost s
  with | Loc.Exc_located (loc, e) -> parse_fail s loc e
  
let parse_proj s =
  try Gram.parse_string projs Loc.ghost s
  with | Loc.Exc_located (loc, e) -> parse_fail s loc e
  
let parse_solution s =
  try
    Gram.parse_string solution Loc.ghost
      (String.sub s 0 ((String.length s) - 1))
  with | Loc.Exc_located (loc, e) -> parse_fail s loc e
  
let check_tags current_sol_string tagging =
  let pos = (String.length current_sol_string) - 1 in
  let oc = parse_phase (String.sub current_sol_string 0 pos) in oc = tagging
  
let look_up_tags solution output tagging sol =
  let proj = List.fold_left extract "" sol in
  let p = parse_proj proj in
  let current_sol_string = "" (* Lex.return_tagging (List.rev output) (List.rev p) *)
  in if check_tags current_sol_string tagging then Some solution else None
  
let search_bucket solution output tagging (p, b_p) =
  let watch_verify max =
    let rec watch_rec n =
      if n = max
      then None
      else
        (match look_up_tags solution output tagging (List.nth b_p n) with
         | None -> watch_rec (n + 1)
         | s -> s)
    in watch_rec 0
  in watch_verify (List.length b_p)
  
let find_proj solution output tagging sorted_groups =
  let (top_groups, _) = truncate_groups sorted_groups in
  let watch_verify max =
    let rec watch_rec n =
      if n = max
      then None
      else
        (let gr = List.nth top_groups n
         in
           match search_bucket solution output tagging gr with
           | None -> watch_rec (n + 1)
           | s -> s)
    in watch_rec 0
  in watch_verify (List.length top_groups)
  
let analyse tagging (solution, output) =
  let tagger = Lex.extract_lemma in
  let groups = make_groups tagger output in
  let sorted_groups = sort_flatten groups
  in find_proj solution output tagging sorted_groups
  
(* [analyse_results] will look for a solution consistent with taggings;
   If so will return Some(n,ind,kept,max) as given to [print_output] 
   in mode Validate, otherwise returns None. *)
let analyse_results limit taggings =
  function
  | [] -> None
  | best_sols ->
      let kept = List.length best_sols
      and max = (match limit with | Some m -> m | None -> Web.truncation) in
      let watch_verify maxim =
        let rec watch_rec n =
          if n = maxim
          then None
          else
            (match analyse taggings (List.nth best_sols n) with
             | None -> watch_rec (n + 1)
             | Some sol_number -> Some max)
        in watch_rec 0
      in watch_verify kept
  
let verify_sentence filter_mode us topic sentence encode taggings =
  let chunker =
    if us
    then (* sandhi undone *) Sanskrit.read_raw_sanskrit
    else (* blanks non-significant *) Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence in (* normalisation here *)
  let all_chunks =
    match topic with
    | Some topic -> chunks @ [ code_string topic ]
    | None -> chunks
  in
    try segment_all filter_mode all_chunks []
    with
    | Solutions (limit, revsols, saved) ->
        let sols = List.rev revsols in analyse_results limit taggings sols
  
let pdiff sol modec modes mode_sent mode_trans sentence psol tagging cho chd
          =
  let report = output_string cho
  and prdiff = output_string chd
  and modes_report =
    "[{" ^
      (modec ^
         ("}] <{" ^
            (modes ^
               ("}> |{" ^ (mode_sent ^ ("}| #{" ^ (mode_trans ^ "}# ")))))))
  in
    (report (modes_report ^ ("({" ^ (sentence ^ "}) ")));
     (match sol with
      | Some max ->
          (report ("[" ^ ((string_of_int max) ^ "]"));
           (match psol with
            | 0 ->
                prdiff
                  (sentence ^
                     (" " ^ (modec ^ (" " ^ (modes ^ " [parses now]\n")))))
            | max1 ->
                let diff = max1 - max
                in
                  (match diff with
                   | 0 -> ()
                   | d ->
                       prdiff
                         (sentence ^
                            (" " ^
                               (modec ^
                                  (" " ^
                                     (modes ^
                                        (" changes [" ^
                                           ((string_of_int d) ^ "]\n"))))))))))
      | None ->
          (report "[0]";
           (match psol with
            | 0 -> ()
            | (* It didn't parse before, so no need to report *) _ ->
                prdiff
                  (sentence ^
                     (" " ^ (modec ^ (" " ^ (modes ^ " [does not parse]\n"))))))));
     let rec print_tag =
       function
       | a :: rest -> "${" ^ (a ^ ("}$&" ^ (print_tag rest)))
       | [] -> ""
     in report (" " ^ ((print_tag tagging) ^ "\n")))
  
let regression s cho chd =
  let (mc, ms, mst, mt, sc, solc, oc) = parse_solution s in
  (* let _ = complete := mc = "C" and _ = iterate := mst = "Sent" *)
  let us = ms = "F" in
  let solr = verify_sentence true us topic sc (switch_code mt) oc
  in pdiff solr mc ms mst mt sc solc oc cho chd
  
let get_metadata input_info =
  let (_, filename, _) = parse_metadata input_info
  and version = Date.version_id
  in
    "[{" ^
      (version ^
         ("}] <{" ^ (filename ^ ("}> ({" ^ (Version.version_date ^ "})")))))
  
(* Disabled: uses undefined Web.var_dir
let main_loop ic =
  let use_metadata = input_line ic and input_info = input_line ic
  and version = Date.version_id and date = Date.date_iso in
  let (old_version, filename, old_date) = parse_metadata input_info in
  let cho =
    open_out_gen [ Open_wronly; Open_trunc; Open_creat; Open_text ] 0o777
      (Web.var_dir ^
         ("/" ^ (filename ^ ("-" ^ (version ^ ("-" ^ (date ^ ".txt")))))))
  and chd =
    open_out_gen [ Open_wronly; Open_trunc; Open_creat; Open_text ] 0o777
      (Web.var_dir ^ ("/diff-" ^ (filename ^ ".txt"))) in
  let report_meta = output_string cho
  and report_version = output_string chd
  in
    (report_meta (use_metadata ^ ("\n" ^ ((get_metadata input_info) ^ "\n")));
     (let diff_meta =
        "diff: file = " ^
          (filename ^
             (" from Version " ^
                (old_version ^
                   ("." ^
                      (old_date ^
                         (" to " ^ (version ^ ("." ^ (date ^ "\n")))))))))
      in report_version diff_meta);
     try
       let rec read_from_ic ic =
         let s = input_line ic in (regression s cho chd; read_from_ic ic)
       in read_from_ic ic
     with | End_of_file -> (close_out cho; close_out chd))
*)
  
(* Now regression reads on stdin - no need of unsafe file opening *)
(* Disabled: depends on main_loop
let _ =
  try main_loop stdin with | Sys_error m -> print_string ("Sys_error " ^ m)
*)
  

