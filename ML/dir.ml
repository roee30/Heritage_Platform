(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Return the list of files in the given directory with their absolute
   name.  *)
value abs_files dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.map (Filename.concat dir) files
;
value basenames files = List.map Filename.basename files
;
value subdirs dir =
  let subdirs = List.filter Sys.is_directory (abs_files dir) in
  subdirs |> basenames
;
value file_with_ext ext file =
  not (Sys.is_directory file) && Filename.check_suffix file ("." ^ ext)
;
value files_with_ext ext dir =
  let files = List.filter (file_with_ext ext) (abs_files dir) in
  files |> basenames
;
value split path = Str.split (Str.regexp_string Filename.dir_sep) path
;
