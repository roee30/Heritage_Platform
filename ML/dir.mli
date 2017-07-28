(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Directory operations *)

(* [subdirs dir] returns the list of subdirectories of [dir].  The order
   of the returned list is unspecified.  Raise [Sys_error] when an
   operating system error occurs.  *)
value subdirs : string -> list string
;
(* [files_with_ext ext dir] returns the list of files in [dir] with the
   extension [ext] (e.g. ["txt"]).  The order of the returned list is
   unspecified.  Raise [Sys_error] when an operating system error
   occurs.  *)
value files_with_ext : string -> string -> list string
;
(* [split path] splits [path] into substrings corresponding to the
   subdirectories of [path].  *)
value split : string -> list string
;
