value make ~corpus_dir:corpus_dir ~sentence_no:sentence_no ~translit:translit
  ~unsandhied:unsandhied ~text:text =
  let sentence_no = string_of_int sentence_no in
  let file = Web.corpus_dir ^ corpus_dir ^ sentence_no ^ ".html" in
  let metadata_file = Web.corpus_dir ^ corpus_dir ^ "." ^ sentence_no in
  let sentence =
    let encode = Encode.switch_code translit in
    let chunker =
      if unsandhied then        (* sandhi undone *)
        Sanskrit.read_raw_sanskrit
      else                      (* blanks non-significant *)
        Sanskrit.read_sanskrit
    in
    chunker encode text
  in
  let save_sentence file =
    let mode = [ Open_wronly; Open_creat; Open_trunc; Open_text ] in
    do
    { Web.output_channel.val := open_out_gen mode 0o666 file
    ; Interface.safe_engine ()
    ; Gen.dump sentence metadata_file
    ; close_out Web.output_channel.val
    ; Web.output_channel.val := stdout }
  in
  do
  { save_sentence file
  ; Corpus_manager.make corpus_dir }
;
