(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Idir Lankri                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Generation of corpus manager's pages *)

(* Generate the page displaying a view of the given corpus subdirectory.
   The output channel is as always either [stdout] for CGI output or
   a static HTML file (according to the "magic switch"
   Web.output_channel).  NB: No error handling is done by this
   function.  *)
value mk_page : string -> Web_corpus.mode -> unit
;
