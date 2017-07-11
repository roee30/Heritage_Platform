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
value citation_regexp = Str.regexp "\\\\citation{\\(.*\\)}"
;
value extract_citation save_sentence env line line_no =
  try
    if Str.string_match citation_regexp line 0 then
      let query = query_of_env [ ("text", Str.matched_group 1 line) :: env ] in
      save_sentence ~query
    else
      raise Exit
  with
  [ _ ->
    do
    { Printf.eprintf
        "Line %d: \
         Wrong input format (expect one citation macro per line)" line_no
    ; exit 1
    }
  ]
;
value populate_corpus save_corpus dir file =
  let rec aux ch i =
    try
      let line = input_line ch in
      let env =
        [ (Params.corpus_dir, dir.val)
        ; (Params.sentence_no, string_of_int i)
        ; ("t", Paths.default_transliteration)
        ]
      in
      do
      { extract_citation save_corpus env line i
      ; aux ch (i + 1)
      }
    with
    [ End_of_file -> () ]
  in
  let ch = open_in file in
  do
  { aux ch 1
  ; close_in ch
  }
;
(***************)
(* Entry point *)
(***************)
value main =
  let dir = ref "" in
  let save_corpus = Corpus.save_sentence ~corpus_location:"./" in
  (* -d is a mandatory option!  *)
  let opts =
    Arg.align
      [ ("-d", Arg.Set_string dir,
         " Set the destination directory (ending with a slash)") ]
  in
  Arg.parse opts (populate_corpus save_corpus dir)
    (Filename.basename Sys.argv.(0) ^ " [options] <citation_file>")
;
