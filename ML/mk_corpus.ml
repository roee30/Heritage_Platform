value url_encode s =
  let conversion_tbl =
    [ ("]", "5D") (* Must be the first element because of [Str.regexp].  *)
    ; ("!", "21")
    ; ("#", "23")
    ; ("$", "24")
    ; ("&", "26")
    ; ("'", "27")
    ; ("(", "28")
    ; (")", "29")
    ; ("*", "2A")
    ; ("+", "2B")
    ; (",", "2C")
    ; ("/", "2F")
    ; (":", "3A")
    ; (";", "3B")
    ; ("=", "3D")
    ; ("?", "3F")
    ; ("@", "40")
    ; ("[", "5B")
    ]
  in
  let url_encode = fun
    [ " " -> "+"
    | s ->
      try "%" ^ List.assoc s conversion_tbl with [ Not_found -> s ]
    ]
  in
  let special_chars =
    Str.regexp (
      "[" ^ String.concat "" (conversion_tbl |> List.split |> fst) ^ " " ^ "]"
    )
  in
  let subst s = s |> Str.matched_string |> url_encode in
  Str.global_substitute special_chars subst s
;
value query_of_env env =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ url_encode v) env)
;
value abort report_error status =
  do
  { report_error ()
  ; exit status
  }
;
value citation_regexp = Str.regexp "\\\\citation{\\(.*\\)}"
;
value extract_citation env corpus_location line line_no =
  try
    if Str.string_match citation_regexp line 0 then
      let query = query_of_env [ ("text", Str.matched_group 1 line) :: env ] in
      Corpus.save_sentence ~corpus_location ~query
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
        (Filename.dirname dirname.val ^ Filename.dir_sep,
         Filename.basename dirname.val)
    in
    let dirname = dirname ^ Filename.dir_sep in
    let rec aux i =
      try
        let line = input_line ch in
        let env =
          [ (Params.corpus_dir, dirname)
          ; (Params.sentence_no, string_of_int i)
          ; ("t", Paths.default_transliteration)
          ]
        in
        do
        { extract_citation env corpus_location line i
        ; aux (i + 1)
        }
      with
      [ End_of_file -> () ]
    in
    do
    { Corpus.mkdir ~corpus_location ~dirname
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
