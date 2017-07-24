(* Generation of corpus manager's pages *)

(* Generate the page displaying a view of the given corpus subdirectory.
   The output channel is as always either [stdout] for CGI output or
   a static HTML file (according to the "magic switch"
   Web.output_channel).  *)
value mk_page : string -> Web_corpus.mode -> unit
;
