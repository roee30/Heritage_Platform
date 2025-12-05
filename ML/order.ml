(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Order = struct i*)
(* lexicographic comparison *)
let rec lexico l1 l2 =
  match l1 with
  | [] -> true
  | c1 :: r1 ->
      if c1 = 50
      then (* hiatus *) lexico r1 l2
      else
        (match l2 with
         | [] -> false
         | c2 :: r2 ->
             if c2 = 50
             then (* hiatus *) lexico l1 r2
             else
               if c2 > 50
               then (c1 > 50) && (c1 < c2)
               else (* homonym indexes *)
                 if c1 > 50
                 then true
                 else
                   if c2 < c1
                   then false
                   else if c2 = c1 then lexico r1 r2 else true)
  
(* for use as argument to List.sort *)
let order w w' = if w = w' then 0 else if lexico w w' then (-1) else 1
  

