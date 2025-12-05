(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* The auto structure *)
module Auto :
  sig
    type rule = (Word.word * Word.word * Word.word)
    
    (* [(w,u,v)] such that [(rev u)|v -> w] *)
    type auto =
      | State of (bool * deter * choices)
      and (* [bool] is [True] for accepting states *)
      (* Possible refinement - order choices by right-hand sides of sandhi rules *)
      deter =
      (Word.letter * auto) list
      and choices =
      rule list
    
    type stack = choices list
    
  end
  

