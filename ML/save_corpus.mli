(* Engine that saves the sentence being analyzed and then displays the
   corpus subdirectory corresponding to the saved sentence.  *)

value make :
  ~corpus_dir:string -> ~sentence_no:int -> ~translit:string ->
  ~unsandhied:bool -> ~text:string -> unit
;
