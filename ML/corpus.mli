(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Operations on the corpus tree *)
module Section : sig type t
                      val label : t -> string
                         end
  
module Analyzer :
  sig
    type t = | Graph
    
    val path : t -> string
      
    val relocatable_path : t -> string
      
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
      
  end
  
(* Num.num *)
module Encoding :
  sig
    type t = | Velthuis | WX | KH | SLP1 | Devanagari | IAST
    
    val to_string : t -> string
      
    val of_string : string -> t
      
    val encode : t -> string -> Word.word
      
    val decode : t -> Word.word -> string
      
  end
  
module Sentence :
  sig
    type t
    
    val make : int -> Word.word list -> bool -> Analysis.t -> t
      
    val id : t -> int
      
    val text : Encoding.t -> t -> string
      
    val analysis : t -> Analysis.t
      
  end
  
module type Location = sig val path : string
                              end
  
module type S =
  sig
    (* Contents of a corpus subdirectory: either it is empty (constructor
     [Empty]), otherwise we are on leaves of the tree (constructor
     [Sentences]) or on branches (constructor [Sections]).  *)
    type contents =
      | Empty | Sections of Section.t list | Sentences of Sentence.t list
    
    (* List the contents of the given corpus subdirectory.  Note that the
     returned elements are sorted according to [Section.compare] or
     [Sentence.compare] depending on the case.  Raise [Sys_error] when
     an operating system error occurs.  *)
    val contents : string -> contents
      
    (* Exception raised by [save_sentence] when the sentence to be saved
     already exists.  *)
    exception Sentence_already_exists
      
    (* Raise [Sentence_already_exists] if the sentence to be saved already
     exists and [force] is [False] and [Sys_error] when an operating
     system error occurs.  *)
    val save_sentence :
      bool -> string -> int -> Word.word list -> bool -> Analysis.t -> unit
      
    exception Section_already_exists of string
      
    (* Raise [Section_already_exists] if the given corpus directory
     already exists and [Unix.Unix_error] when an operating system error
     occurs.  *)
    val mkdir : string -> unit
      
    exception No_such_sentence of int
      
    (* Raise [No_such_sentence i] if the sentence i does not exist.  *)
    val sentence : string -> int -> Sentence.t
      
    type permission = | Reader | Annotator | Manager
    
    val default_permission : permission
      
    val string_of_permission : permission -> string
      
    val permission_of_string : string -> permission
      
    val url : string -> permission -> Sentence.t -> string
      
    val relocatable_url : string -> permission -> Sentence.t -> string
      
    (* [citation subdir id ]] returns an URL to the analysis
     of the sentence whose number is [id] in the corpus
     subdirectory [subdir]. Raise [Failure "citation"] if an
     error occurs.  *)
    val citation : string -> int -> string
      
  end
  
module Make (Loc : Location) : S
  

