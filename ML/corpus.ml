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
  value make : int -> string -> list (string * string) -> t
  ;
  value id : t -> int
  ;
  value analyzer : t -> string
  ;
  value state : t -> list (string * string)
  ;
  value compare : t -> t -> int
  ;
  type metadata = { text : list Word.word }
  ;
end = struct
  type t =
    { id : int
    ; analyzer : string
    ; state : list (string * string)
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
end
;
module type Location = sig
  value path : string
  ;
end
;
module type S = sig
  (* Contents of a corpus subdirectory: either we are on leaves of the
     tree (constructor [Sentences]) or on branches (constructor
     [Headings]).  *)
  type contents =
    [ Empty
    | Headings of list Heading.t
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
  value save_sentence : bool -> string -> list (string * string) -> unit
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir : string -> unit
  ;
  exception No_such_sentence
  ;
  value sentence : string -> int -> Sentence.t
  ;
  value gobble_metadata : string -> Sentence.t -> Sentence.metadata
  ;
  value dump_metadata : string -> Sentence.t -> Sentence.metadata -> unit
  ;
end
;
module Make (Loc : Location) : S = struct
  type contents =
    [ Empty
    | Headings of list Heading.t
    | Sentences of list Sentence.t
    ]
  ;
  value ( /^ ) = Filename.concat
  ;
  value ( ~/ ) file = Loc.path /^ file
  ;
  value sentence_ext = "rem"
  ;
  value sentence_file subdir id =
    ~/subdir /^ Printf.sprintf "%d.%s" id sentence_ext
  ;
  exception No_such_sentence
  ;
  value sentence subdir id =
    let file = sentence_file subdir id in
    if Sys.file_exists file then (Gen.gobble file : Sentence.t) else
      raise No_such_sentence
  ;
  value contents subdir =
    let subdir = ~/subdir in
    match Dir.subdirs subdir with
    [ [] ->
      let sentences =
        subdir
        |> Dir.files_with_ext sentence_ext
        |> List.map (fun x -> (Gen.gobble (subdir /^ x) : Sentence.t))
        |> List.sort Sentence.compare
      in
      match sentences with [ [] -> Empty | sentences -> Sentences sentences ]
    | subdirs ->
      let headings =
        subdirs
        |> List.map Heading.make
        |> List.sort Heading.compare
      in
      Headings headings
    ]
  ;
  value metadata_file dir id = ~/dir /^ "." ^ string_of_int id
  ;
  value gobble_metadata dir sentence =
    (Gen.gobble (metadata_file dir (Sentence.id sentence)) : Sentence.metadata)
  ;
  value dump_metadata dir sentence metadata =
    Gen.dump metadata (metadata_file dir (Sentence.id sentence))
  ;
  exception Sentence_already_exists
  ;
  value save_sentence force analyzer state =
    let corpus_dir = Cgi.get Params.corpus_dir state "" in
    let sentence_no = Cgi.get Params.sentence_no state "" in
    let translit = Cgi.get "t" state "" in
    let unsandhied = Cgi.get "us" state "" = "t" in
    let text = Cgi.get "text" state "" in
    let sentence_no =
      try
        sentence_no |> float_of_string |> int_of_float
      with
      [ Failure s -> failwith "save_sentence"]
    in
    let file = sentence_file corpus_dir sentence_no in
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
    let sentence = Sentence.make sentence_no analyzer state in
    if not force && Sys.file_exists file then
      raise Sentence_already_exists
    else
      do
      { dump_metadata corpus_dir sentence metadata
      ; Gen.dump sentence file
      }
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir dirname =
    try Unix.mkdir ~/dirname 0o755 with
    [ Unix.Unix_error (Unix.EEXIST, _, _) ->
      raise (Heading_abbrev_already_exists (Filename.basename dirname))
    ]
  ;
end
;
