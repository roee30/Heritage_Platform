type content =
  [ Sections of list string
  | Sentences of list string
  ]
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
  match Dir.subdirs subdir String.compare with
  [ [] ->
    let cmp file file' = compare (sentence_id file) (sentence_id file') in
    Sentences (Dir.files_with_ext "html" subdir cmp)
  | subdirs -> Sections subdirs
  ]
;
type sentence_metadata = { text : list Word.word }
;
value sentence_metadata_file dir file = dir ^ "." ^ chop_extension file
;
value gobble_sentence_metadata dir file =
  (Gen.gobble (sentence_metadata_file dir file) : sentence_metadata)
;
value dump_sentence_metadata metadata dir file =
  Gen.dump metadata (sentence_metadata_file dir file)
;
value save_sentence ~corpus_dir ~sentence_no ~translit ~unsandhied ~text =
  let sentence_no = string_of_int sentence_no in
  let file = corpus_dir ^ sentence_no ^ ".html" in
  let sentence =
    let encode = Encode.switch_code translit in
    let chunker =
      if unsandhied then        (* sandhi undone *)
        Sanskrit.read_raw_sanskrit
      else                      (* blanks non-significant *)
        Sanskrit.read_sanskrit
    in
    { text = chunker encode text }
  in
    do
    { Web.output_channel.val := open_out file
    ; Interface.safe_engine ()
    ; dump_sentence_metadata sentence corpus_dir sentence_no
    ; close_out Web.output_channel.val
    ; Web.output_channel.val := stdout
    }
;
