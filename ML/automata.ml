(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Automata = struct i*)
(* same as Platform's Automaton, except debugging tools, and taking the
   sandhi rules locally from Data rather than dynamically from Web *)
open Canon; (* decode rdecode *)
open Phonetics; 
open Auto.Auto; (* rule auto stack *)
open Deco;
(*#load "../ZEN/deco.cmo"; open Deco;*)

(* Generalises the structure of trie, seen as a representation of
   deterministic automaton (recognizer for prefix-shared set of strings),
   into the graph of a non-deterministic automaton, chaining external 
   sandhi with recognition of inflected forms from the inflected lexicon. *)

(* Algorithm. For every inflected form [f], and for every external sandhi rule 
   [r: u|v -> w] such that [f=x.u], construct a choice point from state [S x]
   to an iterating block B(r). [S x] is the state reachable from the initial 
   state (top of the trie) by input [x], going on the deterministic subgraph, 
   copy of the trie. The set of iterating blocks pertaining to a node are 
   grouped in a list of non-deterministic choice points. *)

(* Parser operation. The parser traverses the state tree while scanning the
   input. Assume it is at state [S x] looking at input [z]. It has the choice
   of either staying in the deterministic part (word lookup) by going to the
   deterministic transition corresponding to the first symbol in [z], with no
   output, or else choosing in the non-deterministic part a choice block B(r)
   as an epsilon move (no scanning of [z]), and then, with [r: u|v -> w], 
   recognize that [w] is a prefix of [z] (scan it or else backtrack), emit the
   parse trace [<f>-r-] where [f=inflected(x.u)], and iterate by jumping to 
   state [S v] (we assume that sandhi rules are stripped so that [S v] always 
   exists).
   A stack of [(choices,input_index)] permits to backtrack on input failure.
   The final sandhi rules [u|# -> y] are treated similarly, with \# matching
   end of input, but instead of jumping we accept and propose the parse trace
   as a legal tagging of the sentence (with possible continuation into 
   backtracking for additional solutions). On backtracking a stack of failed
   attempts may be kept, in order to restart gracefully when a word is 
   missing from the lexicon. This robustification will be essential to turn
   the parser into a bootstrapping lexicon acquisition device. *)

(* {\bf Construction of the automaton.} *)
(* Remark that it is linear in one bottom-up traversal of the inflected trie. *)

type rules = array stack
;
(* A sandhi entry is a list [[l1; l2; ... ln] with li=[si1; si2; ... sini]] *)
(* with [sij=(c1,c2,c3) where c1=code w, c2=rev (code u), c3=code v ]       *)
(* such that [u|v -> w] by external sandhi, with [i=|u|]                    *)
(* [sandhis] concerns u ended by s or .h, and i = 1 or 2                    *)
(* [sandhir] concerns u ended by r, and i = 1 or 2                          *)
(* [sandhin] concerns u ended by n, and i = 1 or 2                          *)
(* [sandhif] concerns u ended by f, and i = 1 or 2                          *)
(* [sandhio] concerns u ended by other letters, and i = 1                   *)

(* We read sandhi rules compiled by [compile_sandhi] *)
value (sandhis, sandhir, sandhin, sandhif, sandhio) = 
  (Gen.gobble Data.sandhis_file : (rules * rules * rules * rules * rules))
;
(* Special patch for preverbs automaton to ignore cesura rules *)
value filter_cesura sandhis = 
  List.map (List.filter (fun (w,_,_) -> not (List.mem 50 w))) sandhis
;
(* [flag_pv=True] for preverbs automaton *)
value get_sandhi flag_pv = fun (* argument is [mirror (code u)] *) 
  [ [] -> failwith "get_sandhi 0"
  | [ 43 (* r *) :: before ] -> match before with
        [ [] -> failwith "get_sandhi 1"
        | [ penu :: _ ] -> sandhir.(penu)
        ]
  | [ 48 (* s *) :: before ] 
  | [ 16 (* .h *) :: before ] -> match before with
        [ [] -> failwith "get_sandhi 2"
        | [ penu :: _ ] -> sandhis.(penu)
        ] 
  | [ 36 (* n *) :: before ] -> match before with
        [ [] -> failwith "get_sandhi 3"
        | [ penu :: _ ] -> sandhin.(penu)
        ] 
  | [ 21 (* f *) :: before ] -> match before with 
        [ [] -> failwith "get_sandhi 4" 
        | [ penu :: _ ] -> sandhif.(penu) 
        ] 
  | [ c :: _ ] -> if c < 0 then failwith "get_sandhi 5"
                  else if c > 49 then failwith "get_sandhi 6"
                  else let sandhis = sandhio.(c) in 
                       if flag_pv then filter_cesura sandhis else sandhis
  ]
;
(* Same as [Compile_sandhi.merge] *)
value rec merge st1 st2 = match st1 with
  [ [] -> st2
  | [ l1 :: r1 ] -> match st2 with
      [ [] -> st1
      | [ l2 :: r2 ] -> [ (List2.union l1 l2) :: (merge r1 r2) ]
      ]
  ]
;
(* We add to the stack arrays a deco rewrite set *)

(* A rewrite deco maps revu to a list of rules (w,revu,v) *)
type rewrite_set = Deco.deco rule
;
value project n = fun
  [ Deco (_,arcs) -> try List.assoc n arcs 
                     with [ Not_found -> empty ] ]
and get_rules = fun
  [ Deco (rules,_) -> rules ]
;
(* Union of two decos *)
value rec merger d1 d2 = match d1 with
    [ Deco (i1,l1) -> match d2 with
    [ Deco (i2,l2) -> Deco (i1 @ i2, mrec l1 l2)
  where rec mrec l1 l2 = match l1 with
    [ [] -> l2 
    | [ (n,d) :: l ] -> match l2 with
    [ [] -> l1
    | [ (n',d') :: l' ] -> if n<n' then [ (n,d) :: mrec l l2 ] 
                           else if n'<n then [ (n',d') :: mrec l1 l' ]
                                else [ (n, merger d d') :: mrec l l' ]
    ] ] ] ]
;

(* {\bf Automaton construction with state minimization.} *)
value hash_max = 9689 (* Mersenne 21 *) 
;
exception Overlap
;
module Auto = Share.Share (struct type domain=auto; value size=hash_max; end)
;
(* Remark - it would be incorrect to share states [State (b,d,nd)] having the 
   same [b] and [d], since [nd] may depend on upper nodes because of 
   contextual rules. *)
value hash0 = 1
and hash1 letter key sum = sum + letter*key
and hash b arcs rules = (* NB. [abs] needed because possible integer overflow *)
    (abs (arcs + Gen.dirac b + List.length rules)) mod hash_max
;
(* [flag_pv=True] for preverbs automaton *)
value build_auto flag_pv (rewrite : rewrite_set) = traverse 
  (* [traverse: word -> lexicon -> (auto * stack * rewrite_set * int)] *)
  (* The occurrence list [occ] is the reverse of the access word. *)
  where rec traverse occ = fun
  [ Trie.Trie (b,arcs) -> 
     let local_stack = if b then get_sandhi flag_pv occ else [] 
     and local_rewrite = if b then rewrite else empty in
     let f (deter, stack, rewrite, span) (n,t) = 
         let current = [ n :: occ ] in  (* current occurrence *)
         let (auto, st, rew, k) = traverse current t in
         ([ (n, auto) :: deter ], merge st stack, 
          merger (project n rew) rewrite, hash1 n k span) in
     let (deter, stack, rewrite, span) = 
         List.fold_left f ([],[],local_rewrite,hash0) arcs in
     let (h,l) = match stack with
         [ [] -> ([],[]) | [ h :: l ] -> (h,l) ] in
           (* the tail [l] of [stack] initialises the stack for upper nodes,
              its head [h] contains the list of current choice points     *) 
     let key = hash b span h in
     let s = Auto.share (State (b,List.rev deter,get_rules rewrite @ h)) key in
     (s, merge local_stack l, rewrite, key)
  ]
;
(*  *** IMPORTANT ***
The arcs in deter are in decreasing order, because of [fold_left].
We put them back in increasing order by [List.rev deter]. This is not strictly
needed, and order of siblings is not important since access is done with [assoc].
However, it is crucial to maintain proper order for operations such as [split],
which splits an automaton into vowel-initial and consonant-initial subparts. 
Thus reversal was enforced when split was introduced in V2.43. *)

(* What follows is for debugging. *)
value print_sandhi ps (w,u,v) = do
   { ps (rdecode u)
   ; ps "|"
   ; ps (decode v)
   ; ps " -> "
   ; ps (decode w)
   ; ps "\n"
   }
;
value print_cont_sandhi ps word rule = do
   { ps ("[" ^ (rdecode word) ^ "]") 
   ; print_sandhi ps rule
   }
;
value record degree assoc = (* [assoc] is a list [[ (deg1,count1); ... ]] *)
  let (left,right) = List2.zip degree assoc in
  let update = match right with 
      [ [] -> [ (degree,1) :: right ]
      | [ (d,n) :: rest ] -> if d=degree then [ (d,n+1) :: rest ]
                             else [ (degree,1) :: right ]
      ] in
  List2.unstack left update
;
value inspect_bool states = 
  let count (t,f) = fun 
     [ State (b,_,_) -> if b then (t+1,f) else (t,f+1) ] in
  List.fold_left count (0,0) states
;
value add2 (sumt,sumf) states = 
  let (t,f) = inspect_bool states in (sumt+t,sumf+f)
;
value inspect_choices spans states = 
  let degrees sp = fun
      [ State (_,_,choices) -> record (List.length choices) sp ] in
  List.fold_left degrees spans states
;
(* Print automaton statistics in file [automaton.txt]. *)
value print_stats ps memo = 
  let pi n = ps (string_of_int n) in
  let (tt,tf) = Array.fold_left add2 (0,0) memo
  and spans = Array.fold_left inspect_choices [] memo
  and print_spans = List.iter psp 
      where psp (sp,count) = do
          { pi count; ps " states of degree "; pi sp; ps "\n" }
  and print_sandhis =
        List.iter (fun l -> List.iter (print_sandhi ps) l)
  and print_cont_sandhis cont = fun
        [ [ lev1; lev2 ] -> do
             { List.iter (print_cont_sandhi ps cont) lev1 
             ; List.iter (print_sandhi ps) lev2
             }
        | [ lev1 ] -> List.iter (print_cont_sandhi ps cont) lev1 
        | [] -> ()
        | _ -> failwith "ill-formed sandhi table"
        ] in
  let print_sandhi_rules () =
      let print_table tab =
          for i=1 to 49 do { print_sandhis tab.(i) }
      and print_table_in_context tab =
          for i=1 to 49 do { print_cont_sandhis [ i ] tab.(i) } in do
      { print_table sandhio
      ; print_table_in_context sandhir
      ; print_table_in_context sandhis
      } in do
  { ps "total number of states: "; pi (tt+tf)
  ; ps " of which "; pi tt; ps " accepting\n" 
  ; ps "nondeterminism degrees:\n"; print_spans spans 
  ; ps "sandhi rules:\n"; print_sandhi_rules ()
  }
;
(* Compile builds a tagging transducer from a lexicon index. *)
(* [compile : bool -> rewrites -> Trie.trie -> Auto.auto] 
    called by [Make_automaton] and [Make_preverb_automaton] *)
value compile stats_flag rewrite lexicon = 
  let (transducer, stack, _, _) = build_auto False rewrite [] lexicon in
  match stack with
  [ [] -> do { if stats_flag then (* optional monitoring *)
                  let cho = open_out Data.automaton_stats in
                  let ps = output_string cho in do
                  { print_stats ps Auto.memo; close_out cho } 
               else () 
             ; transducer
             }
  | _ -> (* Error: some sandhi rule has action beyond one word in the lexicon *)
         raise Overlap 
  ]
;
(* Special case: preverb automaton *)
value compile_pv stats_flag rewrite lexicon = 
  let (transducer, stack, _, _) = build_auto True rewrite [] lexicon in
  match stack with
  [ [] -> do { if stats_flag then (* optional monitoring *)
                  let cho = open_out Data.automaton_stats in
                  let ps = output_string cho in do
                  { print_stats ps Auto.memo; close_out cho } 
               else () 
             ; transducer
             }
  | _ -> (* Error: some sandhi rule has action beyond one word in the lexicon *)
         raise Overlap 
  ]
;
(* (For debugging) visualise automaton (undoing sharing). *)
value rec visual = fun
  [ State (b,deter,choices) -> do
     { print_string "<State"
     ; print_string (if b then "*" else " ")
     ; List.iter print_det deter
     ; print_string " ! "
     ; List.iter (print_sandhi print_string) choices
     ; print_string " >"
     }
  ]
and print_det (n,state) = do { print_int n; visual state }
; 

(*i end; i*)
