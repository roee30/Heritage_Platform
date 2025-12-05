(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
module Section :
  sig
    type t
    
    val make : string -> t
      
    val label : t -> string
      
    val compare : t -> t -> int
      
  end =
  struct
    type t = string
    
    let make h = h
      
    let label h = h
      
    let compare h h' = String.compare (label h) (label h')
      
  end
  
module Analyzer :
  sig
    type t = | Graph
    
    val path : t -> string
      
    val relocatable_path : t -> string
      
  end =
  struct
    type t = | Graph
    
    let path = function | Graph -> let open Paths in cgi_dir_url ^ cgi_graph
    and
      (* GH addition: indexing citations in DICO pages with Reader invocation *)
      (* Ugly hack: the string "!CGIGRAPH" will get inserted at make DICO time in 
     Heritage Dictionary, and instanciated properly by make releasedata at 
     make install of Heritage Platform ! *)
      relocatable_path = function | Graph -> "!CGIGRAPH"
      
  end
  
module Analysis :
  sig
    type t
    
    val make :
      Analyzer.t -> Html.language -> string -> int -> (* Num.num *) t
      
    val analyzer : t -> Analyzer.t
      
    val lang : t -> Html.language
      
    val checkpoints : t -> string
      
    val nb_sols : t -> int
      
  end = (* Num.num *)
  struct
    type t =
      { analyzer : Analyzer.t; lang : Html.language; checkpoints : string;
        nb_sols : int
      }
    
    (* Num.num *)
    let make analyzer lang checkpoints nb_sols =
      {
        analyzer = analyzer;
        lang = lang;
        checkpoints = checkpoints;
        nb_sols = nb_sols;
      }
      
    let analyzer a = a.analyzer
      
    let lang a = a.lang
      
    let checkpoints a = a.checkpoints
      
    let nb_sols a = a.nb_sols
      
  end
  
module Encoding :
  sig
    type t = | Velthuis | WX | KH | SLP1 | Devanagari | IAST
    
    val to_string : t -> string
      
    val of_string : string -> t
      
    val encode : t -> string -> Word.word
      
    val decode : t -> Word.word -> string
      
  end =
  struct
    type t = | Velthuis | WX | KH | SLP1 | Devanagari | IAST
    
    let to_string =
      function
      | Velthuis -> "VH"
      | WX -> "WX"
      | KH -> "KH"
      | SLP1 -> "SL"
      | Devanagari -> "deva"
      | IAST -> "roma"
      
    let rec of_string =
      function
      | "VH" -> Velthuis
      | "WX" -> WX
      | "KH" -> KH
      | "SL" -> SLP1
      | "deva" -> Devanagari
      | "roma" -> IAST
      | _ -> Velthuis
      
    let encode (encoding : t) : string -> Word.word =
      (encoding |> to_string) |> Encode.switch_code
      
    let decode =
      function
      | (Velthuis | WX | KH | SLP1 as encoding) ->
          (encoding |> to_string) |> Canon.switch_decode
      | Devanagari -> Canon.unidevcode
      | IAST -> Canon.uniromcode
      
  end
  
(* What about metadata (date, author, history...) ?  *)
module Sentence :
  sig
    type t
    
    val make : int -> Word.word list -> bool -> Analysis.t -> t
      
    val id : t -> int
      
    val text : Encoding.t -> t -> string
      
    val analysis : t -> Analysis.t
      
    val compare : t -> t -> int
      
  end =
  struct
    type t =
      { id : int; text : Word.word list; (* list of padas *)
        unsandhied : bool; analysis : Analysis.t
      }
    
    let make id text unsandhied analysis =
      { id = id; text = text; unsandhied = unsandhied; analysis = analysis; }
      
    let id s = s.id
      
    let text encoding s = (* restores input including danda *)
      (* this is used to display the link to the sentence in [Corpus_manager] *)
      (* WAS [s.text |> List.map (Encoding.decode encoding) |> String.concat " "] *)
      let rec restore_danda_rec accu =
        function
        | [] -> accu
        | [] :: rest -> restore_danda_rec (accu ^ "|") rest
        | (* glitch *) chunk :: rest ->
            let str = Encoding.decode encoding chunk
            in restore_danda_rec (accu ^ (" " ^ str)) rest
      in s.text |> (restore_danda_rec "")
      
    let unsandhied s = s.unsandhied
      
    let analysis s = s.analysis
      
    let compare s s' = compare (id s) (id s')
      
  end
  
module type Location = sig val path : string
                              end
  
module type S =
  sig
    (* Contents of a corpus subdirectory: either we are on leaves of the
     tree (constructor [Sentences]) or on branches (constructor
     [Sections]).  *)
    type contents =
      | Empty | Sections of Section.t list | Sentences of Sentence.t list
    
    (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Section.compare] or
     [Sentence.compare] depending on the case.  *)
    val contents : string -> contents
      
    exception Sentence_already_exists
      
    val save_sentence :
      bool -> string -> int -> Word.word list -> bool -> Analysis.t -> unit
      
    exception Section_already_exists of string
      
    val mkdir : string -> unit
      
    exception No_such_sentence of int
      
    val sentence : string -> int -> Sentence.t
      
    type permission = | Reader | Annotator | Manager
    
    val default_permission : permission
      
    val string_of_permission : permission -> string
      
    val permission_of_string : string -> permission
      
    val url : string -> permission -> Sentence.t -> string
      
    val relocatable_url : string -> permission -> Sentence.t -> string
      
    val citation : string -> int -> string
      
  end
  
module Make (Loc : Location) : S =
  struct
    type contents =
      | Empty | Sections of Section.t list | Sentences of Sentence.t list
    
    let ( /^ ) = Filename.concat
      
    let ( ~/ ) file = Loc.path /^ file
      
    let sentence_ext = "rem"
      
    let sentence_file subdir id =
      (( ~/ ) subdir) /^ (Printf.sprintf "%d.%s" id sentence_ext)
      
    exception No_such_sentence of int
      
    let sentence subdir id =
      let file = sentence_file subdir id
      in
        if Sys.file_exists file
        then (Gen.gobble file : Sentence.t)
        else raise (No_such_sentence id)
      
    let contents subdir =
      let subdir = ( ~/ ) subdir
      in
        match Dir.subdirs subdir with
        | [] ->
            let sentences =
              ((subdir |> (Dir.files_with_ext sentence_ext)) |>
                 (List.map (fun x -> (Gen.gobble (subdir /^ x) : Sentence.t))))
                |> (List.sort Sentence.compare)
            in
              (match sentences with
               | [] -> Empty
               | sentences -> Sentences sentences)
        | subdirs ->
            let sections =
              (subdirs |> (List.map Section.make)) |>
                (List.sort Section.compare)
            in Sections sections
      
    let metadata_file dir id = ((( ~/ ) dir) /^ ".") ^ (string_of_int id)
      
    exception Sentence_already_exists
      
    let save_sentence force dir id text unsandhied analysis =
      let file = sentence_file dir id in
      let sentence = Sentence.make id text unsandhied analysis
      in
        (* Here text is saved as a list of words, empty words coding danda *)
        if (not force) && (Sys.file_exists file)
        then raise Sentence_already_exists
        else Gen.dump sentence file
      
    exception Section_already_exists of string
      
    let mkdir dirname =
      try Unix.mkdir (( ~/ ) dirname) 0o755
      with
      | Unix.Unix_error ((Unix.EEXIST, _, _)) ->
          raise (Section_already_exists (Filename.basename dirname))
      
    type permission = | Reader | Annotator | Manager
    
    let default_permission = Reader
      
    let string_of_permission =
      function
      | Reader -> "reader"
      | Annotator -> "annotator"
      | Manager -> "manager"
      
    let restrict_permission perm =
      match Html.target with
      | Html.Server -> Reader
      | Html.Simputer | Html.Computer | Html.Station -> perm
      
    let permission_of_string s =
      let to_perm =
        function
        | "annotator" -> Annotator
        | "manager" -> Manager
        | _ -> Reader
      in (s |> to_perm) |> restrict_permission
      
    let url dir permission sentence =
      let analysis = Sentence.analysis sentence in
      let encoding = Encoding.of_string Paths.default_transliteration in
      let env =
        [ (Params.corpus_permission, (string_of_permission permission));
          ("text", (Sentence.text encoding sentence));
          ("cpts", (Analysis.checkpoints analysis));
          (Params.corpus_dir, dir);
          (Params.sentence_no, ((sentence |> Sentence.id) |> string_of_int)) ] in
      let path = (analysis |> Analysis.analyzer) |> Analyzer.path
      in Cgi.url path ~query: (Cgi.query_of_env env)
      
    let relocatable_url dir permission sentence =
      let analysis = Sentence.analysis sentence in
      let env =
        [ (Params.corpus_permission, (string_of_permission permission));
          ("text", (Sentence.text Encoding.Velthuis sentence));
          ("t", (let open Encoding in to_string Velthuis));
          ("cpts", (Analysis.checkpoints analysis));
          (Params.corpus_dir, dir);
          (Params.sentence_no, ((sentence |> Sentence.id) |> string_of_int)) ] in
      let path = (analysis |> Analysis.analyzer) |> Analyzer.relocatable_path
      in Cgi.url path ~query: (Cgi.query_of_env env)
      
    let citation subdir id =
      relocatable_url subdir Reader (sentence subdir id)
      
  end
  

