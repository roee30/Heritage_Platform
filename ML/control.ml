(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Module Control contains exceptions of global scope *)

exception Anomaly of string (* deemed impossible by logic and ML semantics *)
;
exception Warning of string (* emits a warning *)
;
exception Fatal of string (* unrecoverable fatal error *)
;
(* error reporting *)
value report_mess = "- please report - "
;
value fatal_err_mess = "Fatal error " 
and anomaly_err_mess = "Anomaly " ^ report_mess 
and sys_err_mess     = "System error " ^ report_mess 
and stream_err_mess  = "Stream error - wrong input ? " 
;

(* change if Morphology data type changes *)
value data_format_version = 1
; 
