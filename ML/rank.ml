(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Pawan Goyal & Gérard Huet                       *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This library is used by Reader and Regression. It constructs a lexer Lex, 
   indexed on parameters [iterate] and [complete]. Using the module 
   [Constraints] for ranking, it computes a penalty for each solution, 
   and returns all solutions with minimal penalties, with a further preference 
   for the solutions having a minimum number of segments.
   It manages buckets of solutions ranked by penalties and lengths. *)

(*i module Rank = struct i*)

open Constraints;  
(* [roles_of sort_flatten eval_penalty] *)

module Prel = struct  
 value prelude () = Web.reader_prelude Web.reader_title;
 end (* Prel *)
;
(* Global parameters of the lexer *)
value iterate = ref True (* by default a chunk is a list of words *)
and complete  = ref True (* by default we call the fuller segmenter *)
and output_channel = ref stdout (* by default cgi output on standard output *)
;
open Load_transducers; (* [transducer_vect dummy_transducer_vect Trans] *)

module Lexer_control = struct
 value star = iterate;
 value full = complete;
 value out_chan = output_channel;
 value transducers_ref = 
 ref (dummy_transducer_vect : transducer_vect);
end (* [Lexer_control] *)
;
module Transducers = Trans Prel 
;
(* Multi-phase lexer *)
module Lex = Lexer.Lexer Prel Lexer_control (* [un_analyzable Disp Viccheda] *)
;
(* Builds the penalty stack, grouping together equi-penalty items. *)
(* Beware, [make_groups] reverses the list of tags. *)
value make_groups tagger = comp_rec 1 [] 
  where rec comp_rec seg stack = fun (* going forward in time *)
  [ [] -> stack (* result goes backward in time *)
  | [ (phase,rword,_) :: rest ] -> (* we ignore euphony transition *)
      let word = Word.mirror rword in 
      let lemma = tagger phase word in
      let keep = [ roles_of seg word lemma :: stack ] in
      comp_rec (seg+1) keep rest
  ] 
;
(* Compute minimum penalty in Parse mode *)
value minimum_penalty output =
  let tagger = Lex.extract_lemma
  and out = List.rev output in
  let groups = make_groups tagger out in 
  if groups=[] then failwith "Empty penalty stack !!" else 
  let sort_groups = sort_flatten groups in 
  let min_pen = 
    match sort_groups with 
    [ [] -> failwith "Empty penalty stack" 
    | [ (pen,_) :: _ ] -> pen
    ] in 
  eval_penalty min_pen
;
(* Compound minimum path penalty with solution length *)
value process_output filter_mode ((_,output) as sol) = 
  let length_penalty = if filter_mode then List.length output else 0 in
  (pen,sol) where pen = 
            let min = if filter_mode && iterate.val then minimum_penalty output  
                      else 0 (* keep all *) in
            (min+length_penalty,min)
;
type tagging = (Phases.Phases.phase * Word.word * Lex.Disp.transition) 
and solution = list tagging
and ranked_solution = (int (* rank *) * solution)
and bucket = (int (* length *) * list ranked_solution)
;
  (* [Solutions None sols saved] gives solutions sols within truncation limit;
     [Solutions (Some n) sols saved] returns solutions sols within total n,
     saved is the list of solutions of penalty 0 and worse length penalty. *)
exception Solutions of option int and list ranked_solution and list bucket
;
(* What follows is absurd combinatorial code linearizing the set of solutions
   to chunk segmentation, exponential in the length of the chunk.
   This deprecated code is legacy from the naive parser. 
   It is usable only in demos on small sentences.  *)

(* Constructs a triple (p, sols, saved) where sols is the list of all (m,sol) 
   such that ranked sol has minimal length penalty p and absolute penalty m 
   and saved is the list of all ranked sols of length penalty > p and 
   absolute penalty 0, arranged in buckets by increasing length penalty *)
value insert ((pen,min),sol) ((min_pen,sols,saved) as current) =    
  if sols = [] then (pen,[ (min,sol) ],[])
  else if pen>min_pen then if min>0 then current (* sol is thrown away *)
                           else (min_pen,sols,List2.in_bucket pen sol saved)
  else if pen=min_pen then (min_pen,[ (min,sol) :: sols ],saved)
  else (pen,[ (min,sol) ],resc) where resc =
         let save (min,sol) rescued = if min=0 then [ sol :: rescued ] 
                                      else rescued in
         let rescue = List.fold_right save sols [] in
         if rescue=[] then saved else [ (min_pen,rescue) :: saved ]
;
(* Forget absolute penalties of solutions with minimal length penalty. *)
(* Also used to erase constraints - thus do not eta-reduce !!! *)
value trim x = List.map snd x 
;
(* overflow is None or (Some n) when n solutions with n<Web.truncation *)
value emit overflow (_,sols,saved) = (* really weird control structure *)
  raise (Solutions overflow (trim sols) saved)
;
(* Depth-first search in a stack of type [list (output * resumption)] *)
value dove_tail filter_mode init = 
  let init_stack = trim init (* erasing constraints *) in
  dtrec 1 (0,[],[]) init_stack (* exits raising exception Solutions *)
  where rec dtrec n kept stack = (* invariant: |stack|=|init|=number of chunks *)
  if n > Web.truncation then emit None kept 
  else let full_output = List.fold_right conc stack []
                         where conc (o,_) oo = o @ oo in  
       let pen_sol = process_output filter_mode (n,full_output) in 
       let kept_sols = insert pen_sol kept in 
       dtrec (n+1) kept_sols (crank [] init stack)
         where rec crank acc ini = fun
         [ [ (_,c) :: cc ] -> match ini with
            [ [ (constraints,i) :: ii ] -> do
              { Lex.Viccheda.set_offset constraints
              ; match Lex.Viccheda.continue c with
                [ Some next -> List2.unstack acc [ next :: cc ]
                | None -> crank [ i :: acc ] ii cc
                ]
              }
            | _ -> raise (Control.Anomaly "dove_tail") (* imposs by invariant *)
            ]
         | [] -> emit (Some n) kept_sols (* dove-tailing finished *)
         ]
;
(* From [Graph_segmenter] *)
(* Splitting checkpoints into current and future ones *)
value split_check limit = split_rec []
  where rec split_rec acc checkpts = match checkpts with
      [ [] -> (List.rev acc,[])
      | [ ((index,_,_) as check) :: rest ] -> 
          if index > limit then (List.rev acc,checkpts)
          else split_rec [ check :: acc ] rest 
      ]
;
value segment_chunk ((offset,checkpoints),stack) chunk sa_check = do
  { let ini_cont = Lex.Viccheda.init_segment chunk in 
    let chunk_length = Word.length chunk in
    let extremity = offset+chunk_length in 
    let (local,future) = split_check extremity checkpoints in
    let chunk_constraints = (offset,local) in
    ((succ extremity,future), do 
       { Lex.Viccheda.set_offset chunk_constraints (* Sets local constraints *)
       ; Lex.Viccheda.set_sa_control sa_check (* inherit from chunks recursion *)
       ; let res = match Lex.Viccheda.continue ini_cont with
             [ Some c -> c 
             | None -> Lex.un_analyzable chunk
             ] in 
         [ (chunk_constraints,res) :: stack ]
       }) 
  } 
;
value segment_all filter_mode chunks cpts = 
  let (_,constrained_segs) = segment_chunks ((0,cpts),[]) chunks
  where rec segment_chunks acc = fun
    [ [ (* last *) chunk ] -> segment_chunk acc chunk False
    | [ chunk :: rest ] -> let sa_check = Phonetics.consonant_starts rest in
                           segment_chunks (segment_chunk acc chunk sa_check) rest
    | [] -> acc
    ] in 
  dove_tail filter_mode constrained_segs (* infamous exponential dove-tailing *)
;

(*i end; i*)
