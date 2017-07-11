value citation_regexp = Str.regexp "\\\\citation{\\(.*\\)}"
;
value extract_citation save_corpus line =
  if Str.string_match citation_regexp line 0 then
    save_corpus ~text:(Str.matched_group 1 line)
  else
    prerr_endline "Wrong input format: expected one citation macro per line."
;
value populate_corpus save_corpus file =
 let rec aux ch i =
   try
     let line = input_line ch in
     do
     { extract_citation (save_corpus ~sentence_no:i) line
     ; aux ch (i + 1) }
   with
   [ End_of_file -> () ]
 in
 let ch = open_in file in
 do
 { aux ch 1
 ; close_in ch }
;
(***************)
(* Entry point *)
(***************)
value main =
  let save_corpus =
    Corpus.save_sentence ~corpus_dir:"./"
      ~translit:Paths.default_transliteration ~unsandhied:False
  in
  Arg.parse [] (populate_corpus save_corpus) (Sys.argv.(0) ^ " <citation_file>")
;
