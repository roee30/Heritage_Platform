(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Pawan Goyal & Gérard Huet                       *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* This library is used by Reader. It constructs a lexer Lex, 
   indexed on parameters [iterate] and [complete]. Using the module 
   [Constraints] for ranking, it computes a penalty for each solution, 
   and returns all solutions with minimal penalties, with a further preference 
   for the solutions having a minimum number of segments.
   It manages buckets of solutions ranked by penalties and lengths. *)
(*i module Rank = struct i*)
open Constraints
  
(* [roles_of sort_flatten eval_penalty] *)
module Prel = struct let prelude () = Web.reader_prelude Web.reader_title
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
  
(* Multi-phase lexer *)
module Lex = Lexer.Lexer(Prel)(Lexer_control)
  
(* [un_analyzable Machine Viccheda] *)
(* Builds the penalty stack, grouping together equi-penalty items. *)
(* Beware, [make_groups] reverses the list of tags. *)
let make_groups tagger =
  let rec comp_rec seg stack =
    function
    | (* going forward in time *) [] -> stack
    | (* result goes backward in time *) (phase, rword, _) :: rest ->
        (* we ignore euphony transition *)
        let word = Word.mirror rword in
        let lemma = tagger phase word in
        let keep = (roles_of seg word lemma) :: stack
        in comp_rec (seg + 1) keep rest
  in comp_rec 1 []
  
(* Compute minimum penalty in Parse mode *)
let minimum_penalty output =
  let tagger = Lex.extract_lemma and out = List.rev output in
  let groups = make_groups tagger out
  in
    if groups = []
    then failwith "Empty penalty stack !!"
    else
      (let sort_groups = sort_flatten groups in
       let min_pen =
         match sort_groups with
         | [] -> failwith "Empty penalty stack"
         | (pen, _) :: _ -> pen
       in eval_penalty min_pen)
  
(* Compound minimum path penalty with solution length *)
let process_output filter_mode (((_, output) as sol)) =
  let length_penalty = if filter_mode then List.length output else 0 in
  let pen =
    let min =
      if filter_mode && !Lexer_control.star
      then minimum_penalty output
      else 0
    in (* keep all *) ((min + length_penalty), min)
  in (pen, sol)
  
type tagging =
  (Phases.Phases.phase * Word.word * Lex.Machine.transition)
  and solution =
  tagging list
  and ranked_solution =
  (int * (* rank *) solution)
  and bucket =
  (int * (* length *) (ranked_solution list))

(* [Solutions None sols saved] gives solutions sols within truncation limit;
     [Solutions (Some n) sols saved] returns solutions sols within total n,
     saved is the list of solutions of penalty 0 and worse length penalty. *)
exception Solutions of int option * ranked_solution list * bucket list
  
(* What follows is absurd combinatorial code linearizing the set of solutions
   to chunk segmentation, exponential in the length of the chunk.
   This deprecated code is legacy from the naive parser. 
   It is usable only in demos on small sentences.  *)
(* Constructs a triple (p, sols, saved) where sols is the list of all (m,sol) 
   such that ranked sol has minimal length penalty p and absolute penalty m 
   and saved is the list of all ranked sols of length penalty > p and 
   absolute penalty 0, arranged in buckets by increasing length penalty *)
let insert ((pen, min), sol) (((min_pen, sols, saved) as current)) =
  if sols = []
  then (pen, [ (min, sol) ], [])
  else
    if pen > min_pen
    then
      if min > 0
      then current
      else (* sol is thrown away *)
        (min_pen, sols, (List2.in_bucket pen sol saved))
    else
      if pen = min_pen
      then (min_pen, ((min, sol) :: sols), saved)
      else
        (let resc =
           let save (min, sol) rescued =
             if min = 0 then sol :: rescued else rescued in
           let rescue = List.fold_right save sols []
           in if rescue = [] then saved else (min_pen, rescue) :: saved
         in (pen, [ (min, sol) ], resc))
  
(* Forget absolute penalties of solutions with minimal length penalty. *)
(* Also used to erase constraints - thus do not eta-reduce !!! *)
let trim x = List.map snd x
  
(* overflow is None or (Some n) when n solutions with n<Web.truncation *)
let emit overflow (_, sols, saved) = (* really weird control structure *)
  raise (Solutions (overflow, (trim sols), saved))
  
(* Depth-first search in a stack of type [list (output * resumption)] *)
let dove_tail filter_mode init =
  let init_stack = trim init in (* erasing constraints *)
  let rec (* exits raising exception Solutions *) dtrec n kept stack =
    (* invariant: |stack|=|init|=number of chunks *)
    if n > Web.truncation
    then emit None kept
    else
      (let full_output =
         let conc (o, _) oo = o @ oo in List.fold_right conc stack [] in
       let pen_sol = process_output filter_mode (n, full_output) in
       let kept_sols = insert pen_sol kept in
       let rec crank acc ini =
         function
         | (_, c) :: cc ->
             (match ini with
              | (constraints, i) :: ii ->
                  (Lex.Viccheda.set_offset constraints;
                   (match Lex.Viccheda.continue c with
                    | Some next -> List2.unstack acc (next :: cc)
                    | None -> crank (i :: acc) ii cc))
              | _ -> raise (Control.Anomaly "dove_tail"))
         | (* imposs by invariant *) [] -> emit (Some n) kept_sols
       in dtrec (n + 1) kept_sols (crank [] init stack))
  in dtrec 1 (0, [], []) init_stack
  
(* dove-tailing finished *)
(* From [Graph_segmenter] *)
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
  
let segment_all filter_mode chunks cpts =
  let (_, constrained_segs) =
    let rec segment_chunks acc =
      function
      | (* last *) [ chunk ] -> segment_chunk acc chunk false
      | chunk :: rest ->
          let sa_check = Phonetics.consonant_starts rest
          in segment_chunks (segment_chunk acc chunk sa_check) rest
      | [] -> acc
    in segment_chunks ((0, cpts), []) chunks
  in dove_tail filter_mode constrained_segs
  

