(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

module Section : sig
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
module Analyzer : sig
  type t = [ Graph ]
  ;
  value path : t -> string
  ;
  value relocatable_path : t -> string
  ;
end = struct
  type t = [ Graph ]
  ;
  value path = fun [ Graph -> Paths.(cgi_dir_url ^ cgi_graph) ]
  (* GH addition: indexing citations in DICO pages with Reader invocation *)
  (* Ugly hack: the string "!CGIGRAPH" will get inserted at make DICO time in 
     Heritage Dictionary, and instanciated properly by make releasedata at 
     make install of Heritage Platform ! *)
  and relocatable_path = fun [ Graph -> "!CGIGRAPH" ]
  ;
end
;
module Analysis : sig
  type t
  ;
  value make :
    Analyzer.t -> Html.language -> string -> int (* Num.num *) -> t
  ;
  value analyzer : t -> Analyzer.t
  ;
  value lang : t -> Html.language
  ;
  value checkpoints : t -> string
  ;
  value nb_sols : t -> int (* Num.num *)
  ;
end = struct
  type t =
    { analyzer : Analyzer.t
    ; lang : Html.language
    ; checkpoints : string
    ; nb_sols : int (* Num.num *)
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
  value of_string : string -> t
  ;
  value encode : t -> string -> Word.word
  ;
  value decode : t -> Word.word -> string
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
  value rec of_string = fun
    [ "VH" -> Velthuis
    | "WX" -> WX
    | "KH" -> KH
    | "SL" -> SLP1
    | "deva" -> Devanagari
    | "roma" -> IAST
    | _ -> Velthuis
    ]
  ;
  value encode encoding = encoding |> to_string |> Encode.switch_code
  ;
  value decode = fun
  [ Velthuis | WX | KH | SLP1 as encoding ->
    encoding |> to_string |> Canon.switch_decode
  | Devanagari -> Canon.unidevcode
  | IAST -> Canon.uniromcode
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
    ; text : list Word.word (* list of padas *)
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
  value text encoding s = (* restores input including danda *)
  (* this is used to display the link to the sentence in [Corpus_manager] *)
  (* WAS [s.text |> List.map (Encoding.decode encoding) |> String.concat " "] *)
    s.text |> restore_danda_rec ""
    where rec restore_danda_rec accu = fun
       [ [] -> accu 
       | [ [] :: rest ] -> restore_danda_rec (accu ^ "|") rest (* glitch *)
       | [ chunk :: rest ] -> let str = Encoding.decode encoding chunk in
                              restore_danda_rec (accu ^ " " ^ str) rest 
       ]
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
     [Sections]).  *)
  type contents =
    [ Empty
    | Sections of list Section.t
    | Sentences of list Sentence.t
    ]
  ;
  (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Section.compare] or
     [Sentence.compare] depending on the case.  *)
  value contents : string -> contents
  ;
  exception Sentence_already_exists
  ;
  value save_sentence :
    bool -> string -> int -> list Word.word -> bool -> Analysis.t -> unit
  ;
  exception Section_already_exists of string
  ;
  value mkdir : string -> unit
  ;
  exception No_such_sentence of int
  ;
  value sentence : string -> int -> Sentence.t
  ;
  type permission = [ Reader | Annotator | Manager ]
  ;
  value default_permission : permission
  ;
  value string_of_permission : permission -> string
  ;
  value permission_of_string : string -> permission
  ;
  value url : string -> permission -> Sentence.t -> string
  ;
  value relocatable_url : string -> permission -> Sentence.t -> string
  ;
  value citation : string -> int -> string 
  ;
end
;
module Make (Loc : Location) : S = struct
  type contents =
    [ Empty
    | Sections of list Section.t
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
  exception No_such_sentence of int
  ;
  value sentence subdir id =
    let file = sentence_file subdir id in
    if Sys.file_exists file then (Gen.gobble file : Sentence.t) 
                            else raise (No_such_sentence id)
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
      let sections =
        subdirs
        |> List.map Section.make
        |> List.sort Section.compare
      in
      Sections sections
    ]
  ;
  value metadata_file dir id = ~/dir /^ "." ^ string_of_int id
  ;
  exception Sentence_already_exists
  ;
  value save_sentence force dir id text unsandhied analysis =
    let file = sentence_file dir id in
    let sentence = Sentence.make id text unsandhied analysis in
    (* Here text is saved as a list of words, empty words coding danda *)
    if not force && Sys.file_exists file then raise Sentence_already_exists 
                                         else Gen.dump sentence file
  ;
  exception Section_already_exists of string
  ;
  value mkdir dirname =
    try Unix.mkdir ~/dirname 0o755 with
    [ Unix.Unix_error (Unix.EEXIST, _, _) ->
      raise (Section_already_exists (Filename.basename dirname)) 
    ]
  ;
  type permission = [ Reader | Annotator | Manager ]
  ;
  value default_permission = Reader
  ;
  value string_of_permission = fun
    [ Reader -> "reader"
    | Annotator -> "annotator"
    | Manager -> "manager"
    ]
  ;
  value restrict_permission perm =
    match Html.target with
    [ Html.Server -> Reader
    | Html.Simputer | Html.Computer | Html.Station -> perm
    ]
  ;
  value permission_of_string s = s |> to_perm |> restrict_permission
    where to_perm = fun
      [ "annotator" -> Annotator
      | "manager" -> Manager
      | _ -> Reader
      ]
  ;
  value url dir permission sentence =
    let analysis = Sentence.analysis sentence in
    let encoding = Encoding.of_string Paths.default_transliteration in
    let env =
      [ (Params.corpus_permission, string_of_permission permission)
      ; ("text", Sentence.text encoding sentence)
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
  value relocatable_url dir permission sentence =
    let analysis = Sentence.analysis sentence in
    let env =
      [ (Params.corpus_permission, string_of_permission permission)
      ; ("text", Sentence.text Encoding.Velthuis sentence)
      ; ("t", Encoding.(to_string Velthuis))
      ; ("cpts", Analysis.checkpoints analysis)
      ; (Params.corpus_dir, dir)
      ; (Params.sentence_no, sentence |> Sentence.id |> string_of_int)
      ]
    in
    let path =
      analysis
      |> Analysis.analyzer
      |> Analyzer.relocatable_path
    in
    Cgi.url path ~query:(Cgi.query_of_env env)
  ;
value citation subdir id =
   relocatable_url subdir Reader (sentence subdir id)
;
end
;
