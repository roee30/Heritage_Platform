type content =
  [ Sections of list string
  | Sentences of list string ]
;
value chop_extension file =
  try Filename.chop_extension file with [ Invalid_argument _ -> file ]
;
value sentence_id file =
  file
  |> Filename.basename
  |> chop_extension
  |> int_of_string
;
value content subdir =
  match Dir.subdirs (Web.corpus_dir ^ subdir) String.compare with
  [ [] ->
    let cmp file file' = compare (sentence_id file) (sentence_id file') in
    Sentences (Dir.files_with_ext "html" (Web.corpus_dir ^ subdir) cmp)
  | subdirs -> Sections subdirs ]
;
type sentence_metadata = { text : list Word.word }
;
value sentence_metadata_file dir file =
  Web.corpus_dir ^ dir ^ "." ^ chop_extension file
;
value gobble_sentence_metadata dir file =
  (Gen.gobble (sentence_metadata_file dir file) : sentence_metadata)
;
value dump_sentence_metadata metadata dir file =
  Gen.dump metadata (sentence_metadata_file dir file)
;
