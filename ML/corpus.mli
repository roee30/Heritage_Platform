(* Operations on the corpus tree *)

(* Either we are on leaves of the tree (constructor [Files]) or on
   branches (constructor [Dirs]).  *)
type content =
  [ Dirs of list string
  | Files of list string ]
;
(* List the content of the given corpus subdirectory.  *)
value content : string -> content
;
