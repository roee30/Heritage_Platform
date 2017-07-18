module Heading : sig
  type t
  ;
  value make : string -> t
  ;
  value label : t -> string
  ;
  value compare : t -> t -> int
  ;
end = struct
  type t = string
  ;
  value make h = h
  ;
  value label h = h
  ;
  value compare h h' = String.compare (label h) (label h')
  ;
end
;
module Sentence : sig
  type t
  ;
  value make : int -> string -> string -> t
  ;
  value id : t -> int
  ;
  value analyzer : t -> string
  ;
  value state : t -> string
  ;
  value compare : t -> t -> int
  ;
  type metadata = { text : list Word.word }
  ;
  value url : t -> string
  ;
end = struct
  type t =
    { id : int
    ; analyzer : string
    ; state : string
    }
  ;
  value make id analyzer state =
    { id = id
    ; analyzer = analyzer
    ; state = state
    }
  ;
  value id s = s.id
  ;
  value analyzer s = s.analyzer
  ;
  value state s = s.state
  ;
  value compare s s' = compare (id s) (id s')
  ;
  type metadata = { text : list Word.word }
  ;
  value url sentence = analyzer sentence ^ "?" ^ state sentence
  ;
end
;
module type Location = sig
  value dir : string
  ;
end
;
module type S = sig
  (* Contents of a corpus subdirectory: either we are on leaves of the
     tree (constructor [Sentences]) or on branches (constructor
     [Headings]).  *)
  type contents =
    [ Headings of list Heading.t
    | Sentences of list Sentence.t
    ]
  ;
  (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Heading.compare] or
     [Sentence.compare] depending on the case.  *)
  value contents : string -> contents
  ;
  exception Sentence_already_exists
  ;
  value save_sentence : bool -> string -> unit
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir : string -> unit
  ;
  value gobble_metadata : string -> Sentence.t -> Sentence.metadata
  ;
  value dump_metadata : string -> Sentence.t -> Sentence.metadata -> unit
  ;
end
;
module Make (Loc : Location) : S = struct
  type contents =
    [ Headings of list Heading.t
    | Sentences of list Sentence.t
    ]
  ;
  value contents subdir =
    match Dir.subdirs subdir with
    [ [] ->
      let sentences =
        subdir
        |> Dir.files_with_ext "rem"
        |> List.map (fun x -> (Gen.gobble (subdir ^ x) : Sentence.t))
        |> List.sort Sentence.compare
      in
      Sentences sentences
    | subdirs ->
      let headings =
        subdirs
        |> List.map Heading.make
        |> List.sort Heading.compare
      in
      Headings headings
    ]
  ;
  value metadata_file dir id = dir ^ "." ^ string_of_int id
  ;
  value gobble_metadata dir sentence =
    (Gen.gobble (metadata_file dir (Sentence.id sentence)) : Sentence.metadata)
  ;
  value dump_metadata dir sentence metadata =
    Gen.dump metadata (metadata_file dir (Sentence.id sentence))
  ;
  exception Sentence_already_exists
  ;
  value save_sentence force state =
    let env = Cgi.create_env state in
    let corpus_dir = Cgi.decoded_get Params.corpus_dir "" env in
    let sentence_no = Cgi.decoded_get Params.sentence_no "" env in
    let translit = Cgi.decoded_get "t" "" env in
    let unsandhied = Cgi.decoded_get "us" "" env = "t" in
    let text = Cgi.decoded_get "text" "" env in
    let corpus_abs_dir = Loc.dir ^ corpus_dir in
    let sentence_no =
      sentence_no |> float_of_string |> int_of_float
    in
    let file = corpus_abs_dir ^ string_of_int sentence_no ^ ".rem" in
    let metadata =
      let encode = Encode.switch_code translit in
      let chunker =
        if unsandhied then        (* sandhi undone *)
          Sanskrit.read_raw_sanskrit
        else                      (* blanks non-significant *)
          Sanskrit.read_sanskrit
      in
      { Sentence.text = chunker encode text }
    in
    let sentence = Sentence.make sentence_no Web.graph_cgi state in
    if not force && Sys.file_exists file then
      raise Sentence_already_exists
    else
      do
      { dump_metadata corpus_abs_dir sentence metadata
      ; Gen.dump sentence file
      }
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir dirname =
    try Unix.mkdir (Loc.dir ^ dirname) 0o755 with
    [ Unix.Unix_error (Unix.EEXIST, _, _) ->
      raise (Heading_abbrev_already_exists (Filename.basename dirname))
    ]
  ;
end
;
