(* Directory operations.  *)

(*i module Dir : sig i*)

(* [subdirs dirname] returns the list of subdirectories of [dirname].
   This list is sorted alphabetically.  *)
value subdirs : string -> list string
;
(* [files_with_ext ext dirname] returns the list of files in [dirname]
   with the extension [ext].  This list is sorted alphabetically.  *)
value files_with_ext : string -> string -> list string
;

(*i end; i*)
