(* Directory operations *)

(* [subdirs dir cmp] returns the list of subdirectories of [dir] sorted
   according to the function [cmp].  *)
value subdirs : string -> (string -> string -> int) -> list string
;
(* [files_with_ext ext dir cmp] returns the list of files in [dir] with
   the extension [ext] sorted according to the function [cmp].  *)
value files_with_ext :
  string -> string -> (string -> string -> int) -> list string
;
