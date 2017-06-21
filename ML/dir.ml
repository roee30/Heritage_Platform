(* Return the list of files in the given directory with their absolute
   name.  *)
value abs_files dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.map (Filename.concat dir) files
;
value basenames files = List.map Filename.basename files
;
value subdirs dir cmp =
  let subdirs = List.filter Sys.is_directory (abs_files dir) in
  subdirs |> basenames |> List.sort cmp
;
value file_with_ext ext file =
  not (Sys.is_directory file) && Filename.check_suffix file ("." ^ ext)
;
value files_with_ext ext dir cmp =
  let files = List.filter (file_with_ext ext) (abs_files dir) in
  files |> basenames |> List.sort cmp
;
