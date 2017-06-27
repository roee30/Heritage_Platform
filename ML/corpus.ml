type content =
  [ Dirs of list string
  | Files of list string ]
;
value int_of_file file =
  let chop_extension file =
    try Filename.chop_extension file with [ Invalid_argument _ -> file ]
  in
  file
  |> Filename.basename
  |> chop_extension
  |> int_of_string
;
value cmp_section_file file file' =
  compare (int_of_file file) (int_of_file file')
;
value section path = List.length (Dir.split path) > 1
;
value content subdir =
  let cmp_subdir =
    if section subdir then cmp_section_file else String.compare
  in
  match Dir.subdirs (Web.corpus_dir ^ subdir) cmp_subdir with
  [ [] ->
    Files (Dir.files_with_ext "html" (Web.corpus_dir ^ subdir) cmp_section_file)
  | subdirs -> Dirs subdirs ]
;
