(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

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
(* move analysis stuff to a sep module... *)
module Analyzer : sig
  type t = [ Graph ]
  ;
  value path : t -> string
  ;
end = struct
  type t = [ Graph ]
  ;
  value path = fun [ Graph -> Paths.(cgi_dir_url ^ cgi_graph) ]
  ;
end
;
module Analysis : sig
  type t
  ;
  value make :
    Analyzer.t -> Html.language -> string -> Num.num -> t
  ;
  value analyzer : t -> Analyzer.t
  ;
  value lang : t -> Html.language
  ;
  value checkpoints : t -> string
  ;
  value nb_sols : t -> Num.num
  ;
end = struct
  type t =
    { analyzer : Analyzer.t
    ; lang : Html.language
    ; checkpoints : string
    ; nb_sols : Num.num
    }
  ;
  value make analyzer lang checkpoints nb_sols =
    { analyzer ; lang; checkpoints; nb_sols }
  ;
  value analyzer a = a.analyzer
  ;
  value lang a = a.lang
  ;
  value checkpoints a = a.checkpoints
  ;
  value nb_sols a = a.nb_sols
  ;
end
;
module Encoding : sig
  type t = [ Velthuis | WX | KH | SLP1 | Devanagari | IAST ]
  ;
  value to_string : t -> string
  ;
end = struct
  type t = [ Velthuis | WX | KH | SLP1 | Devanagari | IAST ]
  ;
  value to_string = fun
    [ Velthuis -> "VH"
    | WX -> "WX"
    | KH -> "KH"
    | SLP1 -> "SL"
    | Devanagari -> "deva"
    | IAST -> "roma"
    ]
  ;
end
;
(* What about metadata (date, author, history...) ?  *)
module Sentence : sig
  type t
  ;
  value make : int -> list Word.word -> bool -> Analysis.t -> t
  ;
  value id : t -> int
  ;
  value text : Encoding.t -> t -> string
  ;
  value analysis : t -> Analysis.t
  ;
  value compare : t -> t -> int
  ;
end = struct
  type t =
    { id : int
    ; text : list Word.word
    ; unsandhied : bool
    ; analysis : Analysis.t
    }
  ;
  value make id text unsandhied analysis =
    { id = id
    ; text = text
    ; unsandhied = unsandhied
    ; analysis = analysis
    }
  ;
  value id s = s.id
  ;
  value text encoding s =
    let encode_word =
      match encoding with
      [ Encoding.Velthuis | Encoding.WX | Encoding.KH | Encoding.SLP1 ->
        encoding |> Encoding.to_string |> Canon.switch_decode
      | Encoding.Devanagari -> Canon.unidevcode
      | Encoding.IAST -> Canon.uniromcode
      ]
    in
    s.text |> List.map encode_word |> String.concat " "
  ;
  value unsandhied s = s.unsandhied
  ;
  value analysis s = s.analysis
  ;
  value compare s s' = compare (id s) (id s')
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
  value save_sentence :
    bool -> string -> int -> list Word.word -> bool -> Analysis.t -> unit
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir : string -> unit
  ;
  exception No_such_sentence
  ;
  value sentence : string -> int -> Sentence.t
  ;
  type mode = [ Reader | Annotator | Manager ]
  ;
  value default_mode : mode
  ;
  value string_of_mode : mode -> string
  ;
  value mode_of_string : string -> mode
  ;
  value url : string -> mode -> Sentence.t -> string
  ;
  value citation : string -> int -> string -> bool -> string
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
  (* value gobble_metadata dir sentence = *)
  (*   (Gen.gobble (metadata_file dir (Sentence.id sentence)) : Sentence.metadata) *)
  (* ; *)
  (* value dump_metadata dir sentence metadata = *)
  (*   Gen.dump metadata (metadata_file dir (Sentence.id sentence)) *)
  (* ; *)
  exception Sentence_already_exists
  ;
  value save_sentence force dir id text unsandhied analysis =
    let file = sentence_file dir id in
    let sentence = Sentence.make id text unsandhied analysis in
    if not force && Sys.file_exists file then
      raise Sentence_already_exists
    else
      Gen.dump sentence file
  ;
  exception Heading_abbrev_already_exists of string
  ;
  value mkdir dirname =
    try Unix.mkdir ~/dirname 0o755 with
    [ Unix.Unix_error (Unix.EEXIST, _, _) ->
      raise (Heading_abbrev_already_exists (Filename.basename dirname))
    ]
  ;
  type mode = [ Reader | Annotator | Manager ]
  ;
  value default_mode = Reader
  ;
  value string_of_mode = fun
    [ Reader -> "reader"
    | Annotator -> "annotator"
    | Manager -> "manager"
    ]
  ;
  value mode_of_string = fun
    [ "annotator" -> Annotator
    | "manager" -> Manager
    | _ -> Reader
    ]
  ;
  value url dir mode sentence =
    let analysis = Sentence.analysis sentence in
    let env =
      [ (Params.corpus_mode, string_of_mode mode)
      ; ("text", Sentence.text Encoding.Velthuis sentence)
      ; ("cpts", Analysis.checkpoints analysis)
      ; (Params.corpus_dir, dir)
      ; (Params.sentence_no, sentence |> Sentence.id |> string_of_int)
      ]
    in
    let path =
      analysis
      |> Analysis.analyzer
      |> Analyzer.path
    in
    Cgi.url path ~query:(Cgi.query_of_env env)
  ;
  value citation subdir id text_str editable =
    let text = Sanskrit.read_VH False text_str in
    let mode = if editable then Annotator else Reader in
    let sentence =
      try sentence subdir id with
      [ No_such_sentence ->
        (* Citation always with language = French (i.e. lexicon = Sanskrit
           Heritage) or language should be a parameter of this
           function ?  *)
        let analysis =
          Analysis.make Analyzer.Graph Html.French "" (Num.Int 0)
        in
        (* Unsandhied or not ?  Apparently, all the citations are
           sandhied...  *)
        do
        { try mkdir subdir with
          [ Heading_abbrev_already_exists _ -> ()
          | _ -> failwith "citation"
          ]
        ; Sentence.make id text False analysis
        }
      ]
    in
    url subdir mode sentence
  ;
end
;
