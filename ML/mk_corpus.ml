(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* This is an unfinished attempt to fiter out citations from Heritage
   and make a corpus document from it - unused at present *)

value abort report_error status = 
  do
  { report_error ()
  ; exit status
  }
;
value citation_regexp = Str.regexp "\\\\citation{\\(.*\\)}"
;
value extract_citation state save_sentence line line_no =
  try
    if Str.string_match citation_regexp line 0 then
      save_sentence [ ("text", Str.matched_group 1 line) :: state ]
    else
      raise Exit
  with
  [ _ ->
    abort (fun () ->
        Printf.eprintf
          "Line %d: \
           Wrong input format (expect one citation macro per line)\n" line_no
      ) 1
  ]
;
value populate_corpus dirname file =
  if dirname.val <> "" then
    let ch = open_in file in
    let (corpus_location, dirname) =
      if Filename.is_relative dirname.val then
        ("", dirname.val)
      else
        (Filename.dirname dirname.val, Filename.basename dirname.val)
    in
    let module Corp = Corpus.Make (struct value path = corpus_location; end) in
    let dirname =
      if Filename.check_suffix dirname Filename.dir_sep then
        Filename.chop_suffix dirname Filename.dir_sep
      else
        dirname
    in
    let rec aux i =
      try
        (* [let line = input_line ch in 
            let state = 
            [ (Params.corpus_dir, dirname) 
            ; (Params.sentence_no, string_of_int i) 
            ; ("t", Paths.default_transliteration)
            ] in] *)
      failwith "TODO"
        (* [do 
           { extract_citation state (Corp.save_sentence True Web.graph_cgi) line i 
           ; aux (i + 1)
           }] *)
      with
      [ End_of_file -> () ]
    in
    do
    { Corp.mkdir dirname
    ; aux 1
    ; close_in ch
    }
  else
    abort (fun () ->
        Printf.eprintf
          "Please specify the destination directory.  \
           See %s --help.\n" (Filename.basename Sys.argv.(0))
      ) 1
;
(***************)
(* Entry point *)
(***************)
value main =
  let dirname = ref "" in 
  let opts = 
    Arg.align 
      [ ("-d", Arg.Set_string dirname, 
         " Specify the destination directory") ] 
  in 
  let usage_msg =
    Filename.basename Sys.argv.(0) ^ " -d <dest_dir> <citation_file>"
  in
  Arg.parse opts (populate_corpus dirname) usage_msg
;
