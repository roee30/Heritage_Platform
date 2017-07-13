(* Directory operations *)

(* [subdirs dir] returns the list of subdirectories of [dir].  The order
   of the returned list is unspecified.  *)
value subdirs : string -> list string
;
(* [files_with_ext ext dir] returns the list of files in [dir] with the
   extension [ext] (e.g. ["txt"]).  The order of the returned list is
   unspecified.*)
value files_with_ext : string -> string -> list string
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
