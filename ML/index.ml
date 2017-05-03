(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Indexing utility *)

(*i module Index = struct i*)

(*i *)
open Trie; 
(* i*)

(* [extract_zip : zipper -> word] *)
value extract_zip = extract_zip_acc []
where rec extract_zip_acc suff = fun 
  [ Top -> suff
  | Zip (_,_,n,_,up) -> extract_zip_acc [ n :: suff ] up
  ]
;
exception Last of string
;
value rec previous b left z = match left with
  [ [] -> if b then extract_zip z 
          else match z with
               [ Top -> failwith "entry 'a' missing"
               | Zip (b',l',_,_,z') -> previous b' l' z'
               ]
  | [ (n,t) :: _ ] -> let w1 = extract_zip z 
                      and w2 = last_trie t in
                      w1 @ [ n :: w2 ]
  ]
;
(* Vicious hack to return first homonym if it exists - ugly *)
value next_trie_homo = next_rec [] 
  where rec next_rec pref = fun
  [ Trie (b,l) -> 
    if b then List.rev pref
    else try let _ = List.assoc 51 l (* looking for homonym \#1 *) in
             List.rev [ 51 :: pref ] (* found - we know it is accepting *)
         with (* no homonym - we keep looking for first accepting suffix *)
         [ Not_found -> match l with 
           [ [] -> failwith "next" (* should not happen if trie in normal form *)
           | [ (n,u) :: _ ] -> next_rec [ n :: pref ] u
           ]
         ] 
  ]
;
value escape w = raise (Last (Canon.decode w))
;
(* [search : (w:word) -> (t:trie) -> (string * bool * bool)] *)
(* Assert : [t] is not [Empty] *)
(* [search w t] returns either the first member of [t] with [w] as initial 
   substring with a boolean [exact] indicating if the match is exact 
   and another one [homo] marking homonymy or else raises [Last s] 
   with [s] the last member of [t] less than [w] in lexicographic order.
   Beware. Do not change this code if you do not understand fully the specs. *)
value search w t = access w t Trie.Top 
  where rec access w t z = match w with
     [ [] -> let w1 = extract_zip z 
             and w2 = next_trie_homo t in
             let exact = w2=[] 
             and homo = w2=[ 51 ] in 
             (Canon.decode (w1 @ w2),exact,homo)
     | [ n :: rest ] -> match t with
        [ Trie (b,arcs) -> match arcs with
          [ [] -> if b then escape (extract_zip z)
                  else failwith "Empty trie" 
          | _ -> let (left,right) = List2.zip n arcs in 
                 match right with
            [ [] -> let w1=extract_zip z and w2= last_trie t in 
                    escape (w1 @ w2)
            | [ (m,u) :: upper ] ->  
              if m=n then access rest u (Zip (b,left,m,upper,z)) 
              else escape (previous b left z)
            ]
          ]
        ]
     ]
;
value read_entries () = 
  (Gen.gobble Web.public_entries_file : trie)
;
value is_in_lexicon word =
  (* Checks whether entry [word] actually appears in the lexicon, *)
  (* so that a reference URL is generated in the answers or not.  *)
  (* NB: not indexed by lexical category *)
  let entries_trie = read_entries () in
  Trie.mem word entries_trie
;

(*i end; i*)
