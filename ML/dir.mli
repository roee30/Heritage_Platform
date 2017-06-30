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
(* [split path] splits [path] into substrings corresponding to the
   subdirectories of [path].  *)
value split : string -> list string
;
(* URL-encode the given path (i.e. encode the directory separators).  *)
value url_encode : string -> string
;
(* URL-decode the given path (i.e. decode the directory separators).  *)
value url_decode : string -> string
;
value url_encoded_dir_sep : string
;
