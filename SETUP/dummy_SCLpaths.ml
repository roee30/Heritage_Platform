(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                   Configuration paths for SCL parser                   *)
(*                       Gérard Huet & Amba Kulkarni                      *)
(*                                                                        *)
(**************************************************************************)

(* This will reside in ML directory at configuration time *)

value scl_url = "http://localhost/SCL/SHMT/" 
;

(* used in [Uoh_interface] *)
value svg_interface_url = "" 
and nn_parser_url = ""
and show_parses_path = ""
;
value scl_install_dir = "" 
and offline_dir = "/tmp/" (* vamana *) 
and default_output_font = "ROMAN" (* could be "DEV" *)
;
value scl_dir = ""
and offline name = offline_dir ^ name (* problematic file output *)
;
value offline_file = offline "1.txt" (* owner [_www] Apache=httpd *)
and tmp_in = offline "tmp_in"
;
