(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                      Gérard Huet & Amba Kulkarni                       *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This is an adaptation of module Reader from Skt Heritage engine for
   external call as a plug-in. It prints on [stdout] an html document giving 
   segmentation/tagging of its input.
  
   It computes a penalty of the various solutions, and returns all solutions
   with minimal penalties (with a further preference for the solutions
   having a minimum number of segments), using Constraints for ranking. *)

(*i module Reader_plugin = struct i*)

open Encode;
open Canon;
open Html;
open Web; (* ps, pl, etc. abort truncation *)
open Cgi;
open Morphology; 
open Phases;

module Prelude = struct
 value prelude () = ()
;
 end (* Prelude *)
;
value iterate = ref True (* by default a chunk is a list of words *)
and complete = ref True (* by default we call the fuller segmenter *)
;
module Lexer_control = struct
 value star = iterate;
 value full = complete;
 value out_chan = ref stdout; (* cgi writes on standard output channel *)
end (* [Lexer_control] *)
;
module Lex = Lexer.Lexer Prelude Lexer_control
;
type mode = [ Tag | Parse | Analyse ] (* Segmentation is now obsolete *)
;
(* Builds the penalty stack, grouping together equi-penalty items. *)
(* Beware, [make_groups] reverses the list of tags. *)
value make_groups tagger = comp_rec 1 [] 
  where rec comp_rec seg stack = fun (* going forward in time *)
  [ [] -> stack (* result goes backward in time *)
  | [ (phase,rword,_) :: rest ] -> (* we ignore euphony transition *)
      let word = List.rev rword in 
      let keep = let tags = tagger phase word in
                 [ Constraints.roles_of seg word tags :: stack ] in
      comp_rec (seg+1) keep rest
  ] 
;

(* Computes minimum penalty in Parse mode *)
value minimum_penalty output =
  let tagger = Lex.extract_lemma in
  let out = List.rev output in
  let groups = make_groups tagger out in
  if groups=[] then failwith "Empty penalty stack !" else 
  let sort_groups = Constraints.sort_flatten groups in 
  let min_pen = match sort_groups with 
                [ [] -> failwith "Empty penalty stack" 
                | [ (pen,_) :: _ ] -> pen
                ] in 
  min_pen
;
(* Same as [UoH_interface.print_ext_output] *)
value print_offline_output cho (n,output) = 
  let ps = output_string cho in  
  let print_segment = Lex.print_ext_segment ps in do
  { ps (xml_begin_with_att "solution" [ ("num", string_of_int n) ])
  ; ps "\n"
  ; List.iter print_segment (List.rev output) 
  ; ps (xml_end "solution")
  ; ps "\n"
  }
;
value print_offline_solutions cho =
  List.iter (print_offline_output cho)
;
(* Experimental segmentation plug-in of Amba Kulkarni's parser at UoH *)
(* Printing all segmentations on stdout *)
value print_offline = print_offline_solutions stdout 
;
(* Compound minimum path penalty with solution length *)
value process_output mode ((_,output) as sol) = 
  let min = minimum_penalty output in 
  let m = Constraints.eval_penalty min in
  let length_penalty = List.length output in
  ((m+length_penalty,m),sol)
;
type tagging = (Phases.phase * Word.word * Lex.Disp.transition) 
and solution = list tagging
and ranked_solution = (int (* rank *) * solution)
and bucket = (int (* length *) * list ranked_solution)
;
exception No_solution of Word.word
;
exception Solutions of option int and list ranked_solution and list bucket
(* [Solutions None sols saved] returns solutions sols within truncation limit
   [Solutions (Some n) sols saved] returns solutions sols within total n 
   saved is the list of solutions of penalty 0 and worse length penalty *)
; 
(* insert builds a triple [(p, sols, saved)] where sols is the list of all pairs
   [(m,sol)] such that ranked [sol] has minimal length penalty [p] and 
   absolute penalty [m] and [saved] is the list of all ranked sols of length 
   penalty $>$ [p] and absolute penalty 0, arranged in buckets by increasing 
   length penalty *)
value insert ((pen,min),sol) ((min_pen,sols,saved) as current) =    
  if sols = [] then (pen,[ (min,sol) ],[])
  else if pen>min_pen then if min>0 then current (* sol is thrown away *)
                           else (min_pen,sols,List2.in_bucket pen sol saved)
  else if pen=min_pen then (min_pen,[ (min,sol) :: sols ],saved)
  else (pen,[ (min,sol) ],
            let rescue = List.fold_right save sols [] in
            if rescue=[] then saved else [ (min_pen,rescue) :: saved ])
       where save (min,sol) rescued = if min=0 then [ sol :: rescued ] 
                                      else rescued
;
(* forget absolute penalties of solutions with minimal length penalty *)
value trim = List.map snd
;
(* does depth-first search in a stack of type [list (output * resumption)] *)
value dove_tail mode init = 
  dtrec 1 (0,[],[]) init 
  where rec dtrec n kept stack = (* invariant |stack|=|init| *)
  if n > truncation then 
     let (_,sols,saved) = kept in 
     raise (Solutions None (trim sols) saved)
  else do
  { let total_output = List.fold_right conc stack []
                       where conc (o,_) oo = o @ oo in
    let pen_sol = process_output mode (n,total_output) in 
    let kept_sols = insert pen_sol kept in 
    dtrec (n+1) kept_sols (crank [] init stack)
    where rec crank acc ini = fun
      [ [ (_,c) :: cc ] -> match Lex.Viccheda.continue c with
         [ Some next -> List2.unstack acc [ next :: cc ]
         | None -> match ini with
            [ [ i :: ii ] -> crank [ i :: acc ] ii cc
            | _ -> raise (Control.Anomaly "Plugin dove_tail")
                         (* impossible by invariant *)
            ]
         ]
      | [] -> let (_,sols,saved) = kept_sols in 
              raise (Solutions (Some n) (trim sols) saved)
      ]
  }
;
value segment_all mode chunks = 
   let segs = List.fold_left init [] chunks
   where init stack chunk = 
     let ini_cont = Lex.Viccheda.init_segment chunk in 
     match Lex.Viccheda.continue ini_cont with
     [ Some c -> [ c :: stack ]
     | None -> raise (No_solution chunk) 
     ] in
   dove_tail mode segs
;
value display limit mode text saved = fun
  [ [] -> ()
  | best_sols -> 
     let zero_pen = match saved with
         [ [] -> best_sols
         | [ (_,min_buck) :: _ ] -> List.append best_sols (List.rev min_buck)
         ] in 
     print_offline zero_pen
  ]
;
value process_sentence text us mode topic (sentence : string) encode = 
  let chunker = if us then Sanskrit.read_raw_sanskrit 
                else Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence in do
  { let all_chunks = match topic with
       [ Some topic -> chunks @ [ code_string topic ]
       | None -> chunks
       ] in
    try segment_all mode all_chunks with
        [ Solutions limit revsols saved -> let sols = List.rev revsols in
           let _ = display limit mode text saved sols in True
        | No_solution chunk -> False
        ]
  }
;
value encode = Encode.switch_code "WX" (* encoding in WX as a normalized word *)
;
(* adapt with abort function : string -> string -> unit *)
value abort m1 m2 = raise (Failure (m1 ^ m2))
;
(* input: string is the text to be segmented/parsed *)
(* unsandhied: bool is True is input is unsandhied False if it is sandhied *)
(* topic is (Some "sa.h") (Some "saa") (Some "tat") or None if no topic *)
(* st:bool is True if stemmer for one word, False for tagging sentence *)
(* cp:bool is True if Complete mode, False for Simplified mode *)
value reader_engine input unsandhied topic st cp = do
  { Prelude.prelude ()
  ; if st then iterate.val:=False else () (* word stemmer *)
  ; if cp then complete.val:=True else () (* complete reader *)
    (* Contextual information from past discourse *)
  ; try process_sentence "" unsandhied Analyse topic input encode 
    (* possibly use the returned bool value (success) in your control *)
    with [ Stream.Error _ -> abort "Illegal transliteration " input ]
  }
 ;
value safe_engine input unsandhied topic st cp =
  try reader_engine input unsandhied topic st cp with 
  [ Sys_error s           -> abort Control.sys_err_mess s (* file pb *)
  | Stream.Error s        -> abort Control.stream_err_mess s (* file pb *)
  | Encode.In_error s     -> abort "Wrong input " s
  | Exit (* Sanskrit *)   -> abort "Wrong character in input - " "use ASCII" 
  | Invalid_argument s    -> abort Control.fatal_err_mess s (* sub *)
  | Failure s             -> abort Control.fatal_err_mess s (* anomaly *)
  | End_of_file           -> abort Control.fatal_err_mess "EOF" (* EOF *)
  | Not_found (* assoc *) -> abort Control.fatal_err_mess "assoc" (* anomaly *)
  | Control.Fatal s       -> abort Control.fatal_err_mess s (* anomaly *)
  | Control.Anomaly s     -> abort Control.fatal_err_mess ("Anomaly: " ^ s)
  | _                     -> abort Control.fatal_err_mess "Unexpected anomaly" 
  ]
;
(* call [safe_engine input unsandhied topic st cp] with proper parameters *)  

let input = input_line stdin in 
safe_engine input False None False True
;
(* eg. rAmovanaMgacCawi -> 3 solutions; second is good *)

(*i end; i*)
